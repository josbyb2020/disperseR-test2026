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


#' Link parcel locations to spatial units
#'
#' @description Core linking function that converts parcel locations to 
#' spatial concentrations and links to ZIPs, counties, or grids.
#'
#' @param d data.table with lon, lat columns
#' @param link.to One of 'zips', 'counties', or 'grids'
#' @param p4string Projection string for output
#' @param zc ZCTA sf object (required for 'zips')
#' @param cw Crosswalk data.table (required for 'zips')
#' @param county.sf County sf object (required for 'counties')
#' @param rasterin PBL height raster
#' @param res.link. Grid resolution in meters
#' @param pbl. Apply PBL normalization
#' @param crop.usa Crop to continental USA
#'
#' @return data.table with linked concentrations
#' @export
link_to <- function(d,
                    link.to = 'zips',
                    p4string,
                    zc = NULL,
                    cw = NULL,
                    county.sf = NULL,
                    rasterin = NULL,
                    res.link. = 12000,
                    pbl. = TRUE,
                    crop.usa = FALSE) {

  # Convert to sf points
  pts_sf <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
  pts_proj <- sf::st_transform(pts_sf, crs = p4string)
  
  # Get bounding box and create grid raster
  bbox <- sf::st_bbox(pts_proj)
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
  
  # Convert sf points to terra vector for cell operations
  pts_coords <- sf::st_coordinates(pts_proj)
  pts_vect <- terra::vect(pts_coords, crs = p4string)
  
  # Count particles per cell
  cells <- terra::cellFromXY(r, pts_coords)
  tab <- table(cells)
  
  # Apply PBL normalization if requested
  if (pbl. && !is.null(rasterin)) {
    pbl_layer <- subset_nc_date(hpbl_brick = rasterin, vardate = d$Pdate[1])
    pbl_layer_proj <- terra::project(pbl_layer, r)
    pbls <- pbl_layer_proj[as.numeric(names(tab))]
    r[as.numeric(names(tab))] <- as.numeric(tab) / as.numeric(pbls[, 1])
  } else {
    r[as.numeric(names(tab))] <- as.numeric(tab)
  }
  
  # Trim to data extent
  r2 <- terra::trim(r, padding = 1)
  
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
  
  # Return grid data as data.table
  if (link.to == 'grids') {
    xyz <- terra::as.data.frame(r2, xy = TRUE, na.rm = TRUE)
    xyz <- data.table::setDT(xyz)
    names(xyz)[3] <- 'N'
    return(xyz)
  }
  
  # Convert raster to polygons for overlay operations
  r_poly <- terra::as.polygons(r2, dissolve = FALSE, na.rm = TRUE)
  r_sf <- sf::st_as_sf(r_poly)
  names(r_sf)[1] <- "N"
  
  # Link to counties
  if (link.to == 'counties') {
    message("Linking to counties...")
    
    # Ensure county.sf is properly projected
    if (!is.null(county.sf)) {
      county_proj <- sf::st_transform(county.sf, crs = p4string)
    } else {
      stop("county.sf must be provided for county linking")
    }
    
    # Spatial join: aggregate raster values to counties
    county_join <- sf::st_join(county_proj, r_sf, join = sf::st_intersects)
    
    # Aggregate by county (mean of overlapping cells)
    county_dt <- data.table::setDT(sf::st_drop_geometry(county_join))
    county_agg <- county_dt[, .(N = mean(N, na.rm = TRUE)), 
                            by = .(statefp, countyfp, state_name, name, geoid)]
    
    return(county_agg)
  }
  
  # Link to ZIP codes
  if (link.to == 'zips') {
    
    # Ensure ZCTA is properly projected
    zc_proj <- sf::st_transform(zc, crs = p4string)
    
    # Crop ZCTAs to raster extent for efficiency
    zc_crop <- tryCatch({
      sf::st_crop(zc_proj, sf::st_bbox(r_sf))
    }, error = function(e) {
      zc_proj
    })
    
    # Spatial join: aggregate raster values to ZCTAs
    zc_join <- sf::st_join(zc_crop, r_sf, join = sf::st_intersects)
    
    # Aggregate by ZCTA (mean of overlapping cells)
    zc_dt <- data.table::setDT(sf::st_drop_geometry(zc_join))
    
    # Handle column name variations
    zcta_col <- intersect(c("ZCTA5CE10", "ZCTA"), names(zc_dt))[1]
    if (is.na(zcta_col)) {
      stop("Cannot find ZCTA column in shapefile")
    }
    data.table::setnames(zc_dt, zcta_col, "ZCTA", skip_absent = TRUE)
    
    zc_agg <- zc_dt[, .(N = mean(N, na.rm = TRUE)), by = ZCTA]
    zc_agg <- zc_agg[!is.na(N)]
    
    # Merge with crosswalk
    if (!is.null(cw)) {
      cw_copy <- data.table::copy(cw)
      cw_copy$ZCTA <- formatC(cw_copy$ZCTA, width = 5, format = "d", flag = "0")
      zc_agg$ZCTA <- formatC(zc_agg$ZCTA, width = 5, format = "d", flag = "0")
      
      M <- merge(zc_agg, cw_copy, by = "ZCTA", all = FALSE, allow.cartesian = TRUE)
      M[, ZIP := formatC(ZIP, width = 5, format = "d", flag = "0")]
      M$ZIP <- as.character(M$ZIP)
      M <- stats::na.omit(M)
      return(M)
    }
    
    return(zc_agg)
  }
}


#' Trim parcels with height = 0
#'
#' @param Min Input data.table
#' @return Trimmed data.table
#' @export
trim_zero <- function(Min) {
  M <- data.table::copy(Min)
  
  p_zero_df <- M[height == 0, ]
  particles <- unique(p_zero_df$particle_no)
  
  for (p in particles) {
    h_zero <- p_zero_df[particle_no == p, hour]
    M[particle_no == p & hour >= h_zero, ] <- NA
  }
  M <- stats::na.omit(M)
  return(M)
}


#' Trim parcels below planetary boundary layer
#'
#' @param Min Input data.table with lon, lat, height, Pdate columns
#' @param rasterin PBL height SpatRaster
#' @return Trimmed data.table
#' @export
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
  
  my <- data.table::data.table(
    expand.grid(
      data.table::data.table(
        mo = unique(M[, Pmonth]),
        yr = unique(M[, Pyear])
      )
    )
  )
  
  # Create coordinate matrix
  xy <- as.matrix(M[, .(lon, lat)])
  
  # Get cell indices
  M$rastercell <- terra::cellFromXY(rasterin, xy)
  M_dt <- stats::na.omit(M)
  
  for (m in seq_len(nrow(my))) {
    mon <- my[m, mo]
    yer <- my[m, yr]
    day <- paste(yer, mon, '01', sep = '-')
    
    pbl_layer <- subset_nc_date(hpbl_brick = rasterin, vardate = day)
    
    # Extract values - terra::values returns matrix, extract first column
    pbl_vals <- as.vector(terra::values(pbl_layer))
    M_dt[Pmonth %in% mon & Pyear %in% yer,
         pbl := pbl_vals[M_dt[Pmonth %in% mon & Pyear %in% yer, rastercell]]]
  }
  
  M_dt <- M_dt[height < pbl]
  return(Min[M_dt$ref, .(lon, lat, height, Pdate, hour)])
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
#' @return data.table with grid links
#' @export
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
                                  return.linked.data. = TRUE) {
  
  unitID <- unit$ID
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  
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
    paste0("gridlinks_", unit$ID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date) - ceiling(duration.run.hours / 24),
      to = as.Date(end.date),
      by = '1 day'
    )
    
    pattern.file <- paste0(
      '_', gsub('[*]', '[*]', unit$ID), '_(',
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
    
    l <- lapply(files.read, fst::read.fst, as.data.table = TRUE)
    d <- data.table::rbindlist(l)
    
    if (length(d) == 0)
      return(paste("No files available to link in", month_YYYYMM))
    
    message(Sys.time(), " Files read and combined")
    
    d <- d[as.character(Pdate) %in% vec_dates & hour > 1, ]
    
    if (pbl.) {
      d_xmin <- min(d$lon)
      e_xmin <- terra::ext(pbl.height)[1]
      if (d_xmin < e_xmin - 5)
        pbl.height <- terra::rotate(pbl.height)
      
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
      crop.usa = crop.usa
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
#' @return data.table with county links
#' @export
disperser_link_counties <- function(month_YYYYMM = NULL,
                                     start.date = NULL,
                                     end.date = NULL,
                                     counties,
                                     unit,
                                     duration.run.hours = 240,
                                     pbl.height = NULL,
                                     res.link. = 12000,
                                     overwrite = FALSE,
                                     pbl. = TRUE,
                                     return.linked.data. = TRUE) {
  
  unitID <- unit$ID
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  
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
    paste0("countylinks_", unit$ID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date),
      to = as.Date(end.date),
      by = '1 day'
    )
    
    pattern.file <- paste0(
      '_', gsub('[*]', '[*]', unit$ID), '_(',
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
    
    l <- lapply(files.read, fst::read.fst, as.data.table = TRUE)
    d <- data.table::rbindlist(l)
    
    if (length(d) == 0)
      return(paste("No files available to link in", month_YYYYMM))
    
    message(Sys.time(), " Files read and combined")
    
    d <- d[as.character(Pdate) %in% vec_dates & hour > 1, ]
    
    if (pbl.) {
      d_xmin <- min(d$lon)
      e_xmin <- terra::ext(pbl.height)[1]
      if (d_xmin < e_xmin - 5)
        pbl.height <- terra::rotate(pbl.height)
      
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
      p4string = p4s,
      rasterin = pbl.height,
      res.link. = res.link.,
      pbl. = pbl.
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
#' @return data.table with ZIP code links
#' @export
disperser_link_zips <- function(month_YYYYMM = NULL,
                                 start.date = NULL,
                                 end.date = NULL,
                                 unit,
                                 duration.run.hours = 240,
                                 pbl.height = NULL,
                                 crosswalk. = NULL,
                                 res.link. = 12000,
                                 overwrite = FALSE,
                                 pbl. = TRUE,
                                 return.linked.data. = TRUE) {
  
  unitID <- unit$ID
  
  if ((is.null(start.date) | is.null(end.date)) & is.null(month_YYYYMM))
    stop("Define either start.date/end.date OR month_YYYYMM")
  if (nrow(unit) > 1)
    stop("Please supply a single unit")
  
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
    paste0("ziplinks_", unit$ID, "_", start.date, "_", end.date, ".fst")
  )
  
  if (!file.exists(zip_output_file) | overwrite) {
    
    vec_dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = '1 day'))
    vec_filedates <- seq.Date(
      from = as.Date(start.date) - ceiling(duration.run.hours / 24),
      to = as.Date(end.date),
      by = '1 day'
    )
    
    pattern.file <- paste0(
      '_', gsub('[*]', '[*]', unit$ID), '_(',
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
    
    l <- lapply(files.read, fst::read.fst, as.data.table = TRUE)
    d <- data.table::rbindlist(l)
    
    if (length(d) == 0) {
      return(paste("No files available to link in", month_YYYYMM))
    }
    
    message(Sys.time(), " Files read and combined")
    
    d <- d[as.character(Pdate) %in% vec_dates & hour > 1, ]
    
    if (pbl. && !is.null(pbl.height)) {
      d_xmin <- min(d$lon)
      e_xmin <- terra::ext(pbl.height)[1]
      if (d_xmin < e_xmin - 5) {
        pbl.height <- terra::rotate(pbl.height)
      }
      
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
      cw = crosswalk.,
      p4string = p4s,
      rasterin = pbl.height,
      res.link. = res.link.,
      pbl. = pbl.
    )
    
    message(Sys.time(), " ZIPs linked")
    
    out <- disp_df_link[, .(ZIP, N)]
    out$ZIP <- formatC(out$ZIP, width = 5, format = "d", flag = "0")
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
