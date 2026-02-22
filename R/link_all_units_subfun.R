#' Spatial linking subfunctions for disperseR
#' 
#' These functions handle spatial operations for linking HYSPLIT output
#' to geographic units (ZIP codes, counties, grids).
#' 
#' @name link_subfunctions
#' @importFrom sf st_as_sf st_transform st_crs st_bbox st_crop st_join 
#'   st_intersects st_coordinates st_centroid st_geometry st_set_geometry
#' @importFrom terra rast ext crs values project crop trim as.points
#'   cellFromXY xyFromCell rotate subset
#' @importFrom data.table data.table setnames setDT copy rbindlist
NULL

.normalize_code5 <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  out <- x_chr
  is_num <- !is.na(x_chr) & grepl("^[0-9]+$", x_chr)
  if (any(is_num)) {
    out[is_num] <- sprintf("%05d", as.integer(x_chr[is_num]))
  }
  out
}


#' Link parcel locations to spatial units
#'
#' @description Core linking function that converts parcel locations to 
#' spatial concentrations and links to ZIPs, counties, or grids.
#'
#' @param d data.table with lon, lat columns
#' @param link.to One of 'zips', 'counties', or 'grids'
#' @param p4string Projection string for output
#' @param zc ZCTA sf object (required for 'zips')
#' @param zc.vect Optional pre-projected ZCTA `SpatVector` or provider function
#'   returning one (used by fast ZIP extraction path).
#' @param cw Crosswalk data.table (required for 'zips')
#' @param county.sf County sf object (required for 'counties')
#' @param county.vect Optional pre-projected county `SpatVector` or provider
#'   function returning one (used by fast county extraction path).
#' @param rasterin PBL height raster
#' @param res.link. Grid resolution in meters
#' @param pbl. Apply PBL normalization
#' @param crop.usa Crop to continental USA
#' @param engine Linking engine: `"legacy"` or `"fast"`.
#'
#' @return data.table with linked concentrations
#' @keywords internal
link_to <- function(d,
                    link.to = 'zips',
                    p4string,
                    zc = NULL,
                    zc.vect = NULL,
                    cw = NULL,
                    county.sf = NULL,
                    county.vect = NULL,
                    rasterin = NULL,
                    res.link. = 12000,
                    pbl. = TRUE,
                    crop.usa = FALSE,
                    engine = c("legacy", "fast")) {
  engine <- match.arg(engine)

  # Project point coordinates efficiently (sf_project fast path, st_transform fallback)
  lon <- as.numeric(d[["lon"]])
  lat <- as.numeric(d[["lat"]])
  xy_wgs84 <- cbind(lon, lat)
  fast_project_min_rows <- getOption("disperseR.fast.project.min_rows", 50000L)
  if (!is.numeric(fast_project_min_rows) ||
      length(fast_project_min_rows) != 1 ||
      !is.finite(fast_project_min_rows) ||
      fast_project_min_rows < 1) {
    fast_project_min_rows <- 50000L
  }
  use_sf_project <- identical(engine, "fast") &&
    nrow(d) >= as.integer(fast_project_min_rows)
  target_crs <- sf::st_crs(p4string)
  target_proj <- if (!is.na(target_crs)) target_crs$wkt else p4string
  project_with_st <- function() {
    pts_sf <- sf::st_as_sf(
      data.frame(
        lon = lon,
        lat = lat
      ),
      coords = c("lon", "lat"),
      crs = 4326
    )
    sf::st_coordinates(sf::st_transform(pts_sf, crs = p4string))
  }
  if (use_sf_project) {
    pts_coords <- tryCatch(
      sf::sf_project(
        from = sf::st_crs(4326)$wkt,
        to = target_proj,
        pts = xy_wgs84
      ),
      error = function(e) project_with_st()
    )
  } else {
    pts_coords <- project_with_st()
  }
  if (!is.matrix(pts_coords) || ncol(pts_coords) < 2) {
    stop("Failed to project parcel coordinates for linking.", call. = FALSE)
  }
  pts_finite <- is.finite(pts_coords[, 1]) & is.finite(pts_coords[, 2])
  if (!any(pts_finite)) {
    warning("No valid projected parcel coordinates found for linking. Returning empty result.")
    if (link.to == "grids") {
      return(data.table::data.table(x = numeric(0), y = numeric(0), N = numeric(0)))
    }
    if (link.to == "counties") {
      return(data.table::data.table(
        statefp = character(0),
        countyfp = character(0),
        state_name = character(0),
        name = character(0),
        geoid = character(0),
        N = numeric(0)
      ))
    }
    return(data.table::data.table(ZIP = character(0), N = numeric(0)))
  }
  pts_coords_valid <- pts_coords[pts_finite, , drop = FALSE]
  
  # Get bounding box and create grid raster
  bbox <- c(
    xmin = min(pts_coords_valid[, 1]),
    ymin = min(pts_coords_valid[, 2]),
    xmax = max(pts_coords_valid[, 1]),
    ymax = max(pts_coords_valid[, 2])
  )
  xmin <- floor(bbox["xmin"] / res.link.) * res.link.
  ymin <- floor(bbox["ymin"] / res.link.) * res.link.
  xmax <- ceiling(bbox["xmax"] / res.link.) * res.link.
  ymax <- ceiling(bbox["ymax"] / res.link.) * res.link.
  
  # Create terra raster for gridding
  r <- terra::rast(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    resolution = res.link.,
    crs = p4string
  )
  terra::values(r) <- NA
  
  # Count particles per cell
  cells <- terra::cellFromXY(r, pts_coords_valid)
  cells <- as.integer(cells[!is.na(cells)])
  if (length(cells) > 0) {
    if (engine == "fast") {
      fast_count_max_nbins <- getOption("disperseR.fast.count.max_nbins", 5e7)
      if (!is.numeric(fast_count_max_nbins) ||
          length(fast_count_max_nbins) != 1 ||
          !is.finite(fast_count_max_nbins) ||
          fast_count_max_nbins < 1) {
        fast_count_max_nbins <- 5e7
      }
      nbins <- as.numeric(terra::ncell(r))
      if (is.finite(nbins) && nbins <= fast_count_max_nbins) {
        cell_counts <- tabulate(cells, nbins = as.integer(nbins))
        tab_cells <- which(cell_counts > 0L)
        tab_counts <- as.numeric(cell_counts[tab_cells])
      } else {
        cell_dt <- data.table::data.table(cell_idx = cells)
        tab_dt <- cell_dt[, .(count = .N), by = "cell_idx"]
        data.table::setorder(tab_dt, cell_idx)
        tab_cells <- tab_dt$cell_idx
        tab_counts <- as.numeric(tab_dt$count)
      }
    } else {
      tab <- table(cells)
      tab_cells <- as.numeric(names(tab))
      tab_counts <- as.numeric(tab)
    }
  } else {
    tab_cells <- integer(0)
    tab_counts <- numeric(0)
  }
  
  # Apply PBL normalization if requested
  if (length(tab_cells) > 0 && pbl. && !is.null(rasterin)) {
    pbl_layer <- subset_nc_date(hpbl_brick = rasterin, vardate = d$Pdate[1])
    pbl_layer_proj <- terra::project(pbl_layer, r)
    pbls <- pbl_layer_proj[tab_cells]
    denom <- suppressWarnings(as.numeric(pbls[, 1]))
    denom[!is.finite(denom) | denom <= 0] <- NA_real_
    r[tab_cells] <- tab_counts / denom
  } else if (length(tab_cells) > 0) {
    r[tab_cells] <- tab_counts
  } else {
    # leave all cells as NA when no valid mappings were found
    r <- r
  }

  fast_extract_min_cells <- getOption("disperseR.fast.extract.min.cells", 5000L)
  if (!is.numeric(fast_extract_min_cells) ||
      length(fast_extract_min_cells) != 1 ||
      !is.finite(fast_extract_min_cells) ||
      fast_extract_min_cells < 0) {
    fast_extract_min_cells <- 5000L
  }
  fast_extract_min_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio", 2)
  if (!is.numeric(fast_extract_min_ratio) ||
      length(fast_extract_min_ratio) != 1 ||
      !is.finite(fast_extract_min_ratio) ||
      fast_extract_min_ratio <= 0) {
    fast_extract_min_ratio <- 2
  }
  fast_crop_min_cover <- getOption("disperseR.fast.crop.min.cover_ratio", 0.98)
  if (!is.numeric(fast_crop_min_cover) ||
      length(fast_crop_min_cover) != 1 ||
      !is.finite(fast_crop_min_cover) ||
      fast_crop_min_cover < 0 ||
      fast_crop_min_cover > 1) {
    fast_crop_min_cover <- 0.98
  }
  use_fast_extract_base <- identical(engine, "fast") &&
    length(tab_cells) >= as.integer(fast_extract_min_cells)
  
  # Trim to data extent (only if there's actual data)
  if (all(is.na(terra::values(r)))) {
    warning("No valid parcel-to-cell mappings found for linking. Returning empty result.")
    if (link.to == "grids") {
      return(data.table::data.table(x = numeric(0), y = numeric(0), N = numeric(0)))
    }
    if (link.to == "counties") {
      return(data.table::data.table(
        statefp = character(0),
        countyfp = character(0),
        state_name = character(0),
        name = character(0),
        geoid = character(0),
        N = numeric(0)
      ))
    }
    return(data.table::data.table(ZIP = character(0), N = numeric(0)))
  }
  r2 <- tryCatch(
    terra::trim(r, padding = 1),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("invalid extent", msg, ignore.case = TRUE)) {
        message("Note: Raster trimming skipped (parcel footprint too sparse ",
                "for optimization). Results are unaffected -- the full raster ",
                "is used instead.")
      } else {
        warning("trim failed: ", msg, ". Using original raster.")
      }
      r
    }
  )

  # Lon/lat CRS can produce subtle boundary-intersection differences between
  # sf::st_intersects (legacy) and terra::extract (fast). Keep legacy path to
  # preserve output parity when working directly in geographic coordinates.
  if (use_fast_extract_base && terra::is.lonlat(r2)) {
    use_fast_extract_base <- FALSE
  }
  
  # Crop to USA if requested
  if (crop.usa) {
    if (requireNamespace("rnaturalearth", quietly = TRUE)) {
      usa <- rnaturalearth::ne_countries(
        scale = 110, type = "countries", 
        country = "United States of America",
        returnclass = "sf"
      )
      usa_proj <- sf::st_transform(usa, crs = p4string)
      r2 <- terra::crop(r2, terra::ext(usa_proj))
    }
  }

  maybe_crop_vect <- function(vect_in) {
    if (!inherits(vect_in, "SpatVector") || nrow(vect_in) == 0) {
      return(vect_in)
    }
    ext_vect <- terra::ext(vect_in)
    ext_r <- terra::ext(r2)
    vect_w <- ext_vect[2] - ext_vect[1]
    vect_h <- ext_vect[4] - ext_vect[3]
    if (is.finite(vect_w) && vect_w > 0 && is.finite(vect_h) && vect_h > 0) {
      cover_w <- min(ext_vect[2], ext_r[2]) - max(ext_vect[1], ext_r[1])
      cover_h <- min(ext_vect[4], ext_r[4]) - max(ext_vect[3], ext_r[3])
      cover_w <- max(cover_w, 0)
      cover_h <- max(cover_h, 0)
      cover_ratio <- (cover_w * cover_h) / (vect_w * vect_h)
      if (is.finite(cover_ratio) && cover_ratio >= fast_crop_min_cover) {
        return(vect_in)
      }
    }
    tryCatch(
      terra::crop(vect_in, ext_r),
      error = function(e) vect_in
    )
  }
  
  # Return grid data as data.table
  if (link.to == 'grids') {
    xyz <- terra::as.data.frame(r2, xy = TRUE, na.rm = TRUE)
    xyz <- data.table::setDT(xyz)
    names(xyz)[3] <- 'N'
    return(xyz)
  }
  
  get_r_sf <- local({
    r_sf_cache <- NULL
    function() {
      if (is.null(r_sf_cache)) {
        r_poly <- terra::as.polygons(r2, dissolve = FALSE, na.rm = TRUE)
        r_sf_cache <<- sf::st_as_sf(r_poly)
        names(r_sf_cache)[1] <<- "N"
      }
      r_sf_cache
    }
  })
  
  # Link to counties
  if (link.to == 'counties') {
    message("Linking to counties...")
    
    county_vect_base <- NULL
    county_vect_provider <- NULL
    county_proj <- NULL
    if (!is.null(county.vect) && is.function(county.vect)) {
      county_vect_provider <- county.vect
    } else if (!is.null(county.vect) && inherits(county.vect, "SpatVector")) {
      county_vect_base <- county.vect
    }
    if (!is.null(county.sf)) {
      target_crs <- sf::st_crs(p4string)
      county_crs <- sf::st_crs(county.sf)
      if (!is.na(county_crs) && county_crs == target_crs) {
        county_proj <- county.sf
      } else {
        county_proj <- sf::st_transform(county.sf, crs = p4string)
      }
    }
    if (is.null(county_vect_base) && is.null(county_proj)) {
      stop("county.sf or county.vect must be provided for county linking")
    }
    
    # Spatial join: aggregate raster values to counties
    use_fast_extract <- use_fast_extract_base
    county_dt <- NULL

    if (use_fast_extract) {
      if (is.null(county_vect_base) && !is.null(county_vect_provider) && is.null(county_proj)) {
        county_vect_base <- county_vect_provider()
        if (!inherits(county_vect_base, "SpatVector")) {
          stop("county.vect provider must return a SpatVector")
        }
      }
      poly_count_est <- if (!is.null(county_vect_base)) {
        max(nrow(county_vect_base), 1L)
      } else {
        max(nrow(county_proj), 1L)
      }
      cell_poly_ratio <- length(tab_cells) / poly_count_est
      if (is.finite(cell_poly_ratio) && cell_poly_ratio >= fast_extract_min_ratio) {
        if (is.null(county_vect_base) && !is.null(county_vect_provider)) {
          county_vect_base <- county_vect_provider()
          if (!inherits(county_vect_base, "SpatVector")) {
            stop("county.vect provider must return a SpatVector")
          }
        }
        county_vect <- if (!is.null(county_vect_base)) county_vect_base else terra::vect(county_proj)
        county_vect <- maybe_crop_vect(county_vect)
        county_attr <- data.table::as.data.table(county_vect)
        if (nrow(county_attr) == 0) {
          return(data.table::data.table(
            statefp = character(0),
            countyfp = character(0),
            state_name = character(0),
            name = character(0),
            geoid = character(0),
            N = numeric(0)
          ))
        }
        county_vals <- data.table::as.data.table(
          terra::extract(r2, county_vect, cells = TRUE, touches = TRUE)
        )
        if (nrow(county_vals) == 0 || ncol(county_vals) < 3) {
          return(data.table::data.table(
            statefp = character(0),
            countyfp = character(0),
            state_name = character(0),
            name = character(0),
            geoid = character(0),
            N = numeric(0)
          ))
        }
        val_col <- setdiff(names(county_vals), c("ID", "cell"))[1]
        if (is.na(val_col)) {
          return(data.table::data.table(
            statefp = character(0),
            countyfp = character(0),
            state_name = character(0),
            name = character(0),
            geoid = character(0),
            N = numeric(0)
          ))
        }
        data.table::setnames(county_vals, val_col, "N")
        county_vals <- county_vals[is.finite(N), .(ID, N)]
        if (nrow(county_vals) == 0) {
          return(data.table::data.table(
            statefp = character(0),
            countyfp = character(0),
            state_name = character(0),
            name = character(0),
            geoid = character(0),
            N = numeric(0)
          ))
        }
        county_attr[, ID := .I]
        county_dt <- merge(
          county_vals,
          county_attr[, .(ID, statefp, countyfp, state_name, name, geoid)],
          by = "ID",
          all.x = TRUE,
          sort = FALSE
        )
      } else {
        use_fast_extract <- FALSE
      }
    }

    if (!use_fast_extract) {
      if (is.null(county_proj)) {
        county_proj <- sf::st_as_sf(county_vect_base)
      }
      county_join <- sf::st_join(county_proj, get_r_sf(), join = sf::st_intersects)
      county_dt <- data.table::setDT(sf::st_drop_geometry(county_join))
    }

    # Aggregate by county (mean of overlapping cells)
    if (!"N" %in% names(county_dt)) {
      return(data.table::data.table(
        statefp = character(0),
        countyfp = character(0),
        state_name = character(0),
        name = character(0),
        geoid = character(0),
        N = numeric(0)
      ))
    }
    county_agg <- county_dt[, .(N = mean(N, na.rm = TRUE)),
                            by = .(statefp, countyfp, state_name, name, geoid)]
    county_agg <- county_agg[is.finite(N)]
    
    return(county_agg)
  }
  
  # Link to ZIP codes
  if (link.to == 'zips') {
    
    zc_vect_base <- NULL
    zc_vect_provider <- NULL
    zc_proj <- NULL
    if (!is.null(zc.vect) && is.function(zc.vect)) {
      zc_vect_provider <- zc.vect
    } else if (!is.null(zc.vect) && inherits(zc.vect, "SpatVector")) {
      zc_vect_base <- zc.vect
    }
    if (!is.null(zc)) {
      # Ensure ZCTA is properly projected
      target_crs <- sf::st_crs(p4string)
      zc_crs <- sf::st_crs(zc)
      if (!is.na(zc_crs) && zc_crs == target_crs) {
        zc_proj <- zc
      } else {
        zc_proj <- sf::st_transform(zc, crs = p4string)
      }
    }
    if (is.null(zc_vect_base) && is.null(zc_proj)) {
      stop("zc or zc.vect must be provided for ZIP linking")
    }
    
    use_fast_extract <- use_fast_extract_base
    zc_agg <- NULL

    if (use_fast_extract) {
      if (is.null(zc_vect_base) && !is.null(zc_vect_provider) && is.null(zc_proj)) {
        zc_vect_base <- zc_vect_provider()
        if (!inherits(zc_vect_base, "SpatVector")) {
          stop("zc.vect provider must return a SpatVector")
        }
      }
      poly_count_est <- if (!is.null(zc_vect_base)) {
        max(nrow(zc_vect_base), 1L)
      } else {
        max(nrow(zc_proj), 1L)
      }
      cell_poly_ratio <- length(tab_cells) / poly_count_est
      if (is.finite(cell_poly_ratio) && cell_poly_ratio >= fast_extract_min_ratio) {
        if (is.null(zc_vect_base) && !is.null(zc_vect_provider)) {
          zc_vect_base <- zc_vect_provider()
          if (!inherits(zc_vect_base, "SpatVector")) {
            stop("zc.vect provider must return a SpatVector")
          }
        }
        zc_vect <- if (!is.null(zc_vect_base)) zc_vect_base else terra::vect(zc_proj)
        zc_vect <- maybe_crop_vect(zc_vect)
        zc_attr <- data.table::as.data.table(zc_vect)
        if (nrow(zc_attr) == 0) {
          if (is.null(cw)) {
            return(data.table::data.table(ZCTA = character(0), N = numeric(0)))
          }
          return(data.table::data.table(ZIP = character(0), N = numeric(0)))
        }
        zcta_col <- intersect(c("ZCTA5CE10", "ZCTA"), names(zc_attr))[1]
        if (is.na(zcta_col)) {
          stop("Cannot find ZCTA column in shapefile")
        }
        data.table::setnames(zc_attr, zcta_col, "ZCTA", skip_absent = TRUE)

        zc_vals <- data.table::as.data.table(
          terra::extract(r2, zc_vect, cells = TRUE, touches = TRUE)
        )
        if (nrow(zc_vals) == 0 || ncol(zc_vals) < 3) {
          if (is.null(cw)) {
            return(data.table::data.table(ZCTA = character(0), N = numeric(0)))
          }
          return(data.table::data.table(ZIP = character(0), N = numeric(0)))
        }
        val_col <- setdiff(names(zc_vals), c("ID", "cell"))[1]
        if (is.na(val_col)) {
          if (is.null(cw)) {
            return(data.table::data.table(ZCTA = character(0), N = numeric(0)))
          }
          return(data.table::data.table(ZIP = character(0), N = numeric(0)))
        }
        data.table::setnames(zc_vals, val_col, "N")
        zc_vals <- zc_vals[is.finite(N), .(ID, N)]
        if (nrow(zc_vals) == 0) {
          if (is.null(cw)) {
            return(data.table::data.table(ZCTA = character(0), N = numeric(0)))
          }
          return(data.table::data.table(ZIP = character(0), N = numeric(0)))
        }
        zc_attr[, ID := .I]
        zc_dt <- merge(
          zc_vals,
          zc_attr[, .(ID, ZCTA)],
          by = "ID",
          all.x = TRUE,
          sort = FALSE
        )
        # Preserve legacy semantics by averaging all intersecting cell values
        # across duplicated ZCTA rows.
        zc_agg <- zc_dt[, .(N = mean(N, na.rm = TRUE)), by = ZCTA]
      } else {
        use_fast_extract <- FALSE
      }
    }

    if (!use_fast_extract) {
      if (is.null(zc_proj)) {
        zc_proj <- sf::st_as_sf(zc_vect_base)
      }
      # Crop ZCTAs to raster extent for efficiency
      zc_crop <- tryCatch({
        rast_bbox <- sf::st_bbox(c(
          xmin = terra::xmin(r2),
          ymin = terra::ymin(r2),
          xmax = terra::xmax(r2),
          ymax = terra::ymax(r2)
        ), crs = sf::st_crs(p4string))
        suppressWarnings(sf::st_crop(zc_proj, rast_bbox))
      }, error = function(e) {
        zc_proj
      })

      # Spatial join: aggregate raster values to ZCTAs
      zc_join <- sf::st_join(zc_crop, get_r_sf(), join = sf::st_intersects)
      zc_dt <- data.table::setDT(sf::st_drop_geometry(zc_join))

      # Handle column name variations
      zcta_col <- intersect(c("ZCTA5CE10", "ZCTA"), names(zc_dt))[1]
      if (is.na(zcta_col)) {
        stop("Cannot find ZCTA column in shapefile")
      }
      data.table::setnames(zc_dt, zcta_col, "ZCTA", skip_absent = TRUE)
      zc_agg <- zc_dt[, .(N = mean(N, na.rm = TRUE)), by = ZCTA]
    }

    zc_agg <- zc_agg[!is.na(N)]
    
    # Merge with crosswalk
    if (!is.null(cw)) {
      cw_dt <- if (isTRUE(attr(cw, "disperseR_norm5"))) {
        data.table::as.data.table(cw)
      } else {
        tmp_cw <- data.table::copy(data.table::as.data.table(cw))
        tmp_cw[, ZCTA := .normalize_code5(ZCTA)]
        tmp_cw[, ZIP := .normalize_code5(ZIP)]
        tmp_cw
      }
      if (!(data.table::haskey(cw_dt) && identical(data.table::key(cw_dt), "ZCTA"))) {
        cw_dt <- data.table::copy(cw_dt)
        data.table::setkey(cw_dt, ZCTA)
      }
      zc_agg[, ZCTA := .normalize_code5(ZCTA)]

      M <- cw_dt[zc_agg, on = "ZCTA", nomatch = 0L, allow.cartesian = TRUE]
      if ("i.N" %in% names(M)) {
        data.table::setnames(M, "i.N", "N")
      }
      # Keep rows based on required merge/value columns only.
      # Crosswalk metadata columns may legitimately contain NA and should not
      # silently drop matched ZIP rows.
      M <- M[!is.na(ZIP) & is.finite(N)]
      return(M)
    }
    
    return(zc_agg)
  }
}


#' Trim parcels with height = 0
#'
#' @param Min Input data.table
#' @return Trimmed data.table
#' @keywords internal
trim_zero <- function(Min) {
  M <- data.table::as.data.table(data.table::copy(Min))
  if (nrow(M) == 0) {
    return(M)
  }

  zero_cutoff <- M[height == 0, .(h_zero = suppressWarnings(min(hour, na.rm = TRUE))), by = particle_no]
  zero_cutoff <- zero_cutoff[is.finite(h_zero)]

  if (nrow(zero_cutoff) == 0) {
    return(M)
  }

  M <- merge(M, zero_cutoff, by = "particle_no", all.x = TRUE, sort = FALSE)
  M <- M[is.na(h_zero) | hour < h_zero]
  M[, h_zero := NULL]
  return(M)
}


#' Trim parcels below planetary boundary layer
#'
#' @param Min Input data.table with lon, lat, height, Pdate columns
#' @param rasterin PBL height SpatRaster
#' @return Trimmed data.table
#' @keywords internal
#' @importFrom terra cellFromXY
#' @importFrom lubridate month year
trim_pbl <- function(Min, rasterin) {
  old_tz <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_tz), add = TRUE)
  M <- data.table::copy(Min)
  M[, ref := seq_len(nrow(M))]
  
  # Extract month/year
  M[, Pmonth := formatC(lubridate::month(Pdate), width = 2, format = "d", flag = "0")]
  M[, Pyear := formatC(lubridate::year(Pdate), width = 4, format = "d", flag = "0")]
  
  my <- unique(M[, .(Pmonth, Pyear)])
  
  # Create coordinate matrix (assumes input is lon/lat WGS84)
  xy <- as.matrix(M[, .(lon, lat)])
  
  # Project coordinates to raster CRS if needed
  rast_crs <- terra::crs(rasterin)
  if (!terra::is.lonlat(rasterin)) {
    # Raster is in projected CRS - transform parcel coords to match
    xy_sf <- sf::st_as_sf(data.frame(lon = xy[,1], lat = xy[,2]), 
                          coords = c("lon", "lat"), crs = 4326)
    xy_proj <- sf::st_transform(xy_sf, rast_crs)
    xy <- sf::st_coordinates(xy_proj)
  }
  
  # Get cell indices
  M$rastercell <- terra::cellFromXY(rasterin, xy)
  M_dt <- stats::na.omit(M)
  
  for (m in seq_len(nrow(my))) {
    mon <- my[m, Pmonth]
    yer <- my[m, Pyear]
    idx <- M_dt$Pmonth %in% mon & M_dt$Pyear %in% yer
    if (!any(idx)) {
      next
    }
    day <- paste(yer, mon, '01', sep = '-')
    
    pbl_layer <- subset_nc_date(hpbl_brick = rasterin, vardate = day)
    
    # Extract values - terra::values returns matrix, extract first column
    pbl_vals <- as.vector(terra::values(pbl_layer))
    M_dt[idx, pbl := pbl_vals[rastercell]]
  }
  
  M_dt <- M_dt[height < pbl]
  return(Min[M_dt$ref, .(lon, lat, height, Pdate, hour)])
}

# Read HYSPLIT link input files and filter to the requested time window during load.
.read_hysp_files_for_window <- function(files.read, vec_dates) {
  if (length(files.read) == 0) {
    return(data.table::data.table())
  }

  required_cols <- c("lon", "lat", "height", "Pdate", "hour")
  vec_dates_chr <- unique(as.character(vec_dates))
  vec_dates_date <- suppressWarnings(as.Date(vec_dates_chr))
  use_date_match <- !all(is.na(vec_dates_date))
  vec_dates_int <- if (use_date_match) {
    unique(as.integer(vec_dates_date[!is.na(vec_dates_date)]))
  } else {
    integer(0)
  }

  read_one <- function(path) {
    dt <- tryCatch(
      fst::read.fst(path, as.data.table = TRUE, columns = required_cols),
      error = function(e) fst::read.fst(path, as.data.table = TRUE)
    )
    missing <- setdiff(required_cols, names(dt))
    if (length(missing) > 0) {
      stop("HYSPLIT link input file is missing required columns: ",
           paste(missing, collapse = ", "), " in file ", path, call. = FALSE)
    }
    dt[, required_cols, with = FALSE]
  }

  keep_window <- function(dt) {
    if (nrow(dt) == 0) {
      return(NULL)
    }
    keep_date <- if (inherits(dt$Pdate, "Date") && use_date_match) {
      as.integer(dt$Pdate) %in% vec_dates_int
    } else {
      as.character(dt$Pdate) %in% vec_dates_chr
    }
    out <- dt[keep_date & hour > 1L, ]
    if (nrow(out) == 0) {
      return(NULL)
    }
    out
  }

  chunks <- lapply(files.read, function(path) keep_window(read_one(path)))
  chunks <- Filter(Negate(is.null), chunks)
  if (length(chunks) == 0) {
    return(data.table::data.table())
  }
  data.table::rbindlist(chunks, use.names = TRUE, fill = FALSE)
}


#' Link dispersion to grids
#'
#' @param month_YYYYMM Month in YYYYMM format
#' @param start.date Start date
#' @param end.date End date
#' @param unit Unit data.table
#' @param duration.run.hours Duration in hours
#' @param pbl.height PBL height raster
#' @param res.link. Grid resolution
#' @param overwrite Overwrite existing files
#' @param pbl. Apply PBL normalization
#' @param crop.usa Crop to USA
#' @param return.linked.data. Return linked data
#' @param engine Linking engine: `"legacy"` or `"fast"`.
#' @return data.table with grid links
#' @keywords internal
#' @importFrom fst read.fst write.fst
#' @importFrom lubridate month year
disperser_link_grids <- function(month_YYYYMM = NULL,
                                  start.date = NULL,
                                  end.date = NULL,
                                  unit,
                                  duration.run.hours = 240,
                                  pbl.height = NULL,
                                  res.link. = 12000,
                                  overwrite = FALSE,
                                  pbl. = TRUE,
                                  crop.usa = FALSE,
                                  return.linked.data. = TRUE,
                                  engine = c("legacy", "fast")) {
  engine <- match.arg(engine)
  
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  unitID <- .disperseR_validate_id_component(as.character(unit$ID[[1]]), "unit$ID")

  ziplink_dir <- .disperseR_cache_get("ziplink_dir")
  if (is.null(ziplink_dir) || !nzchar(ziplink_dir)) {
    stop("ziplink_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    dir.create(ziplink_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    stop("ziplink_dir does not exist and could not be created: ", ziplink_dir, call. = FALSE)
  }

  hysp_dir <- .disperseR_cache_get("hysp_dir")
  if (is.null(hysp_dir) || !nzchar(hysp_dir)) {
    stop("hysp_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(hysp_dir)) {
    stop("hysp_dir does not exist: ", hysp_dir, call. = FALSE)
  }
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  
  # Parse dates
  if (is.null(start.date) | is.null(end.date)) {
    start.date <- as.Date(paste(
      substr(month_YYYYMM, 1, 4),
      substr(month_YYYYMM, 5, 6),
      '01', sep = '-'
    ))
    end.date <- seq(start.date, by = paste(1, "months"), length = 2)[2] - 1
  }
  
  if (is.null(month_YYYYMM))
    month_YYYYMM <- paste(start.date, end.date, sep = '_')
  
  month_YYYYMM <- as.character(month_YYYYMM)
  
  output_file <- file.path(
    ziplink_dir,
    paste0("gridlinks_", unitID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date) - ceiling(duration.run.hours / 24),
      to = as.Date(end.date),
      by = '1 day'
    )
    
    unit_id_regex <- .disperseR_escape_regex(unitID)
    pattern.file <- paste0(
      "_", unit_id_regex, "_(",
      paste(vec_filedates, collapse = '|'), ').*\\.fst$'
    )
    
    hysp_dir.path <- file.path(
      hysp_dir,
      unique(paste(lubridate::year(vec_filedates),
                   formatC(lubridate::month(vec_filedates), width = 2, flag = '0'),
                   sep = '/'))
    )
    
    files.read <- list.files(
      path = hysp_dir.path,
      pattern = pattern.file,
      recursive = FALSE,
      full.names = TRUE
    )

    if (length(files.read) == 0) {
      out <- data.table::data.table(x = numeric(), y = numeric(), N = numeric())
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out)
    }
    
    d <- .read_hysp_files_for_window(
      files.read = files.read,
      vec_dates = vec_dates
    )
    
    if (nrow(d) == 0) {
      out <- data.table::data.table(x = numeric(), y = numeric(), N = numeric())
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out)
    }
    
    message(Sys.time(), " Files read and combined")
    
    if (pbl. && is.null(pbl.height)) {
      stop("pbl.height must be provided when pbl. = TRUE.", call. = FALSE)
    }
    if (pbl.) {
      # trim_pbl handles coordinate projection internally
      d_trim <- trim_pbl(d, rasterin = pbl.height)
      message(Sys.time(), " PBLs trimmed")
    } else {
      d_trim <- d
    }

    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

    disp_df_link <- link_to(
      d = d_trim,
      link.to = 'grids',
      p4string = p4s,
      rasterin = pbl.height,
      res.link. = res.link.,
      pbl. = pbl.,
      crop.usa = crop.usa,
      engine = engine
    )

    message(Sys.time(), " Grids linked")
    
    out <- disp_df_link
    out$month <- as.character(month_YYYYMM)
    out$ID <- unitID
    
    if (nrow(out) != 0) {
      fst::write.fst(out, output_file)
      message(Sys.time(), " Linked grids saved to ", output_file)
    }
  } else {
    message("File ", output_file, " already exists. Use overwrite = TRUE to overwrite.")
    if (return.linked.data.)
      out <- fst::read.fst(output_file, as.data.table = TRUE)
  }
  
  if (!return.linked.data.)
    out <- data.table::data.table(x = numeric(), y = numeric(), N = numeric())
  
  out$month <- as.character(month_YYYYMM)
  out$ID <- unitID
  suppressWarnings(out[, V1 := NULL])
  return(out)
}


#' Link dispersion to counties
#'
#' @inheritParams disperser_link_grids
#' @param counties County sf object
#' @param counties.vect Optional pre-projected county `SpatVector` or provider
#'   function returning one.
#' @param engine Linking engine: `"legacy"` or `"fast"`.
#' @return data.table with county links
#' @keywords internal
disperser_link_counties <- function(month_YYYYMM = NULL,
                                     start.date = NULL,
                                     end.date = NULL,
                                     counties,
                                     counties.vect = NULL,
                                     unit,
                                     duration.run.hours = 240,
                                     pbl.height = NULL,
                                     res.link. = 12000,
                                     overwrite = FALSE,
                                     pbl. = TRUE,
                                     return.linked.data. = TRUE,
                                     engine = c("legacy", "fast")) {
  engine <- match.arg(engine)
  
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  unitID <- .disperseR_validate_id_component(as.character(unit$ID[[1]]), "unit$ID")

  if (is.null(counties)) {
    stop("counties must be provided for county linking.", call. = FALSE)
  }

  ziplink_dir <- .disperseR_cache_get("ziplink_dir")
  if (is.null(ziplink_dir) || !nzchar(ziplink_dir)) {
    stop("ziplink_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    dir.create(ziplink_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    stop("ziplink_dir does not exist and could not be created: ", ziplink_dir, call. = FALSE)
  }

  hysp_dir <- .disperseR_cache_get("hysp_dir")
  if (is.null(hysp_dir) || !nzchar(hysp_dir)) {
    stop("hysp_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(hysp_dir)) {
    stop("hysp_dir does not exist: ", hysp_dir, call. = FALSE)
  }
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  
  if (is.null(start.date) | is.null(end.date)) {
    start.date <- as.Date(paste(
      substr(month_YYYYMM, 1, 4),
      substr(month_YYYYMM, 5, 6),
      '01', sep = '-'
    ))
    end.date <- seq(start.date, by = paste(1, "months"), length = 2)[2] - 1
  }
  
  if (is.null(month_YYYYMM))
    month_YYYYMM <- paste(start.date, end.date, sep = '_')
  
  output_file <- file.path(
    ziplink_dir,
    paste0("countylinks_", unitID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date) - ceiling(duration.run.hours / 24),
      to = as.Date(end.date),
      by = '1 day'
    )

    unit_id_regex <- .disperseR_escape_regex(unitID)
    pattern.file <- paste0(
      "_", unit_id_regex, "_(",
      paste(vec_filedates, collapse = '|'), ').*\\.fst$'
    )

    hysp_dir.path <- file.path(
      hysp_dir,
      unique(paste(lubridate::year(vec_filedates),
                   formatC(lubridate::month(vec_filedates), width = 2, flag = '0'),
                   sep = '/'))
    )

    files.read <- list.files(
      path = hysp_dir.path,
      pattern = pattern.file,
      recursive = FALSE,
      full.names = TRUE
    )

    if (length(files.read) == 0) {
      out <- data.table::data.table(
        statefp = character(),
        countyfp = character(),
        state_name = character(),
        name = character(),
        geoid = character(),
        N = numeric()
      )
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out)
    }
    
    d <- .read_hysp_files_for_window(
      files.read = files.read,
      vec_dates = vec_dates
    )
    
    if (nrow(d) == 0) {
      out <- data.table::data.table(
        statefp = character(),
        countyfp = character(),
        state_name = character(),
        name = character(),
        geoid = character(),
        N = numeric()
      )
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out)
    }
    
    message(Sys.time(), " Files read and combined")
    
    if (pbl. && is.null(pbl.height)) {
      stop("pbl.height must be provided when pbl. = TRUE.", call. = FALSE)
    }
    if (pbl.) {
      # trim_pbl handles coordinate projection internally
      d_trim <- trim_pbl(d, rasterin = pbl.height)
      message(Sys.time(), " PBLs trimmed")
    } else {
      d_trim <- d
    }

    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

    disp_df_link <- link_to(
      d = d_trim,
      link.to = 'counties',
      county.sf = counties,
      county.vect = counties.vect,
      p4string = p4s,
      rasterin = pbl.height,
      res.link. = res.link.,
      pbl. = pbl.,
      engine = engine
    )

    message(Sys.time(), " Counties linked")
    
    out <- data.table::as.data.table(disp_df_link)
    out$month <- as.character(month_YYYYMM)
    out$ID <- unitID
    
    if (nrow(out) != 0) {
      fst::write.fst(out, output_file)
      message(Sys.time(), " Linked counties saved to ", output_file)
    }
  } else {
    message("File ", output_file, " already exists. Use overwrite = TRUE to overwrite.")
    if (return.linked.data.)
      out <- fst::read.fst(output_file, as.data.table = TRUE)
  }
  
  if (!return.linked.data.) {
    out <- data.table::data.table(
      statefp = character(),
      countyfp = character(),
      state_name = character(),
      name = character(),
      geoid = character(),
      N = numeric()
    )
  }
  
  out$month <- as.character(month_YYYYMM)
  out$ID <- unitID
  suppressWarnings(out[, V1 := NULL])
  return(out)
}


#' Link dispersion to ZIP codes
#'
#' @inheritParams disperser_link_grids
#' @param crosswalk. Crosswalk data.table
#' @param zcta Optional ZCTA sf object.
#' @param zcta.vect Optional pre-projected ZCTA `SpatVector` or provider
#'   function returning one.
#' @param engine Linking engine: `"legacy"` or `"fast"`.
#' @return data.table with ZIP code links
#' @keywords internal
disperser_link_zips <- function(month_YYYYMM = NULL,
                                 start.date = NULL,
                                 end.date = NULL,
                                 unit,
                                 duration.run.hours = 240,
                                 pbl.height = NULL,
                                 crosswalk. = NULL,
                                 zcta = NULL,
                                 zcta.vect = NULL,
                                 res.link. = 12000,
                                 overwrite = FALSE,
                                 pbl. = TRUE,
                                 return.linked.data. = TRUE,
                                 engine = c("legacy", "fast")) {
  engine <- match.arg(engine)
  
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  unitID <- .disperseR_validate_id_component(as.character(unit$ID[[1]]), "unit$ID")

  if (is.null(crosswalk.)) {
    stop("crosswalk. must be provided for ZIP linking.", call. = FALSE)
  }

  ziplink_dir <- .disperseR_cache_get("ziplink_dir")
  if (is.null(ziplink_dir) || !nzchar(ziplink_dir)) {
    stop("ziplink_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    dir.create(ziplink_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(ziplink_dir)) {
    stop("ziplink_dir does not exist and could not be created: ", ziplink_dir, call. = FALSE)
  }

  hysp_dir <- .disperseR_cache_get("hysp_dir")
  if (is.null(hysp_dir) || !nzchar(hysp_dir)) {
    stop("hysp_dir is not set. Run create_dirs() first.", call. = FALSE)
  }
  if (!dir.exists(hysp_dir)) {
    stop("hysp_dir does not exist: ", hysp_dir, call. = FALSE)
  }

  if (is.null(zcta) && is.null(zcta.vect)) {
    zcta <- .disperseR_cache_get("zcta")
    if (is.null(zcta)) {
      stop("zcta is not set. Run get_data(data = \"zctashapefile\") first.", call. = FALSE)
    }
  }
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  
  if (is.null(start.date) | is.null(end.date)) {
    start.date <- as.Date(paste(
      substr(month_YYYYMM, 1, 4),
      substr(month_YYYYMM, 5, 6),
      '01', sep = '-'
    ))
    end.date <- seq(start.date, by = paste(1, "months"), length = 2)[2] - 1
  }
  
  if (is.null(month_YYYYMM))
    month_YYYYMM <- paste(start.date, end.date, sep = '_')
  
  month_YYYYMM <- as.character(month_YYYYMM)
  
  zip_output_file <- file.path(
    ziplink_dir,
    paste0("ziplinks_", unitID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(zip_output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date) - ceiling(duration.run.hours / 24),
      to = as.Date(end.date),
      by = '1 day'
    )
    
    unit_id_regex <- .disperseR_escape_regex(unitID)
    pattern.file <- paste0(
      "_", unit_id_regex, "_(",
      paste(vec_filedates, collapse = '|'), ').*\\.fst$'
    )
    
    hysp_dir.path <- file.path(
      hysp_dir,
      unique(paste(lubridate::year(vec_filedates),
                   formatC(lubridate::month(vec_filedates), width = 2, flag = '0'),
                   sep = '/'))
    )
    
    files.read <- list.files(
      path = hysp_dir.path,
      pattern = pattern.file,
      recursive = FALSE,
      full.names = TRUE
    )

    if (length(files.read) == 0) {
      out <- data.table::data.table(ZIP = character(), N = numeric())
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out[, .(ZIP, N, month, ID)])
    }
    
    d <- .read_hysp_files_for_window(
      files.read = files.read,
      vec_dates = vec_dates
    )
    
    if (nrow(d) == 0) {
      out <- data.table::data.table(ZIP = character(), N = numeric())
      out$month <- as.character(month_YYYYMM)
      out$ID <- unitID
      return(out[, .(ZIP, N, month, ID)])
    }
    
    message(Sys.time(), " Files read and combined")
    
    if (pbl. && is.null(pbl.height)) {
      stop("pbl.height must be provided when pbl. = TRUE.", call. = FALSE)
    }
    if (pbl. && !is.null(pbl.height)) {
      # trim_pbl handles coordinate projection internally
      d_trim <- trim_pbl(d, rasterin = pbl.height)
      message(Sys.time(), " PBLs trimmed")
    } else {
      d_trim <- d
    }

    p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

    disp_df_link <- link_to(
      d = d_trim,
      link.to = 'zips',
      zc = zcta,
      zc.vect = zcta.vect,
      cw = crosswalk.,
      p4string = p4s,
      rasterin = pbl.height,
      res.link. = res.link.,
      pbl. = pbl.,
      engine = engine
    )

    message(Sys.time(), " ZIPs linked")
    
    out <- disp_df_link[, .(ZIP, N)]
    out[, ZIP := .normalize_code5(ZIP)]
    out$month <- as.character(month_YYYYMM)
    out$ID <- unitID
    
    if (nrow(out) != 0) {
      fst::write.fst(out, zip_output_file)
      message(Sys.time(), " Linked ZIPs saved to ", zip_output_file)
    }
  } else {
    message("File ", zip_output_file, " already exists. Use overwrite = TRUE to overwrite.")
    if (return.linked.data.)
      out <- fst::read.fst(zip_output_file, as.data.table = TRUE)
  }
  
  if (!return.linked.data.)
    out <- data.table::data.table(ZIP = character(), N = numeric())
  
  out$month <- as.character(month_YYYYMM)
  out$ID <- unitID
  out <- out[, .(ZIP, N, month, ID)]
  return(out)
}
