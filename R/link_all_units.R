#' Link air parcels to spatial units for multiple emission sources
#'
#' @description Links all air parcels to relevant spatial scales (ZIP codes,
#' counties, or grids) by month for specified units. Reads HYSPLIT output files
#' produced by run_disperser_parallel() and performs spatial aggregation.
#'
#' Automatically uses appropriate parallelization based on OS:
#' mclapply on Unix/macOS, parLapplyLB on Windows.
#' If some tasks fail, successful links are returned with a warning summary.
#' If all tasks fail, the function errors with task-level diagnostics.
#'
#' @param units.run A data.table with columns: ID (character), uID (character),
#'   Latitude (numeric), Longitude (numeric), year (integer). ID values are used
#'   in output filenames and must be filesystem-safe across platforms (avoid `/`,
#'   backslash, or `:*?"<>|`).
#' @param link.to One of 'zips', 'counties', or 'grids'
#' @param mc.cores Number of cores for parallel computation. Default 2 (CRAN policy).
#'   Set `options(disperseR.mc.cores = parallel::detectCores())` to use all cores.
#' @param year.mons Months for linking (use get_yearmon() to create)
#' @param start.date Optional start date (alternative to year.mons)
#' @param end.date Optional end date (alternative to year.mons)
#' @param pbl_trim Trim parcels under monthly PBL heights? Default TRUE
#' @param pbl.height Monthly boundary layer heights (required if pbl_trim=TRUE)
#' @param crosswalk. Crosswalk data (required if link.to='zips')
#' @param counties. County sf object (required if link.to='counties')
#' @param hysp_dir Directory containing HYSPLIT output files (defaults to
#'   create_dirs()).
#' @param ziplink_dir Output directory for linked files (defaults to
#'   create_dirs()).
#' @param duration.run.hours Duration in hours (default 240 = 10 days)
#' @param res.link Grid resolution in meters (default 12000)
#' @param overwrite Overwrite existing files? Default FALSE
#' @param pbl.trim Legacy parameter, use pbl_trim instead
#' @param crop.usa Crop output to lower 48 states? (grids only)
#' @param return.linked.data Return linked data? Default TRUE
#' @param engine Linking engine: `"legacy"` (default, exact historical path)
#'   or `"fast"` (optimized internals with the same output contract).
#'
#' @return data.table with linked spatial data
#' @export
#' @importFrom parallel detectCores mclapply makeCluster stopCluster clusterExport parLapplyLB clusterEvalQ
#' @importFrom data.table rbindlist
link_all_units <- function(units.run,
                           link.to = "zips",
                           mc.cores = getOption("disperseR.mc.cores", 2L),
                           year.mons = NULL,
                           start.date = NULL,
                           end.date = NULL,
                           pbl_trim = TRUE,
                           pbl.height = NULL,
                           crosswalk. = NULL,
                           counties. = NULL,
                           hysp_dir = NULL,
                           ziplink_dir = NULL,
                           duration.run.hours = 240,
                           res.link = 12000,
                           overwrite = FALSE,
                           pbl.trim = NULL,
                           crop.usa = FALSE,
                           return.linked.data = TRUE,
                           engine = c("legacy", "fast")) {
  engine <- match.arg(engine)
  link_crs <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
  # Input validation
  if ((is.null(start.date) || is.null(end.date)) && is.null(year.mons)) {
    stop(
      "ERROR: Time range not specified.\n",
      "  Provide EITHER:\n",
      "    - year.mons = get_yearmon(start.year='2005', start.month='01', end.year='2005', end.month='12')\n",
      "    - OR: start.date='2005-01-01' and end.date='2005-12-31'",
      call. = FALSE
    )
  }
  if (length(link.to) != 1 || !(link.to %in% c("zips", "counties", "grids"))) {
    stop(
      "ERROR: 'link.to' must be one of: 'zips', 'counties', 'grids'\n",
      "  You provided: ", paste(link.to, collapse = ", "),
      call. = FALSE
    )
  }
  if (link.to == "zips" && is.null(crosswalk.)) {
    stop(
      "ERROR: 'crosswalk.' is required when link.to='zips'.\n",
      "  Get it with: crosswalk <- disperseR::get_data('crosswalk')",
      call. = FALSE
    )
  }
  if (link.to == "zips" && !is.null(crosswalk.) && engine == "fast") {
    crosswalk. <- data.table::copy(data.table::as.data.table(crosswalk.))
    crosswalk.[, ZCTA := .normalize_code5(ZCTA)]
    crosswalk.[, ZIP := .normalize_code5(ZIP)]
    data.table::setkey(crosswalk., ZCTA)
    attr(crosswalk., "disperseR_norm5") <- TRUE
  }
  if (link.to == "counties" && is.null(counties.)) {
    stop(
      "ERROR: 'counties.' is required when link.to='counties'.\n",
      "  Provide an sf object with county polygons.",
      call. = FALSE
    )
  }
  if (pbl_trim && is.null(pbl.height)) {
    stop(
      "ERROR: 'pbl.height' is required when pbl_trim=TRUE.\n",
      "  Get it with: pbl <- disperseR::get_data('pblheight')\n",
      "  Or set pbl_trim=FALSE to skip boundary layer trimming.",
      call. = FALSE
    )
  }

  # Backward-compat: pbl.trim was historically used internally; prefer pbl_trim.
  if (!is.null(pbl.trim)) {
    pbl_trim_effective <- isTRUE(pbl.trim)
  } else {
    pbl_trim_effective <- isTRUE(pbl_trim)
  }

  # Rotate PBL raster once if needed (avoid repeating per-unit).
  # NetCDF PBL files often use 0-360 longitude; disperseR particle data
  # uses -180..180, so rotate when the raster's western edge is near 0.
  if (pbl_trim_effective && !is.null(pbl.height)) {
    if (inherits(pbl.height, "PackedSpatRaster")) {
      pbl.height <- terra::unwrap(pbl.height)
    }
    if (terra::is.lonlat(pbl.height)) {
      e_xmin <- terra::ext(pbl.height)[1]
      if (e_xmin >= 0 && e_xmin < 5) {
        pbl.height <- terra::rotate(pbl.height)
      }
    }
  }

  zcta_prepared <- NULL
  zcta_vect_prepared <- NULL
  if (link.to == "zips") {
    zcta_prepared <- .disperseR_cache_get("zcta")
    if (is.null(zcta_prepared)) {
      stop(
        "ERROR: zcta is not set. Run get_data(data = \"zctashapefile\") first.",
        call. = FALSE
      )
    }
    target_crs <- sf::st_crs(link_crs)
    zc_crs <- sf::st_crs(zcta_prepared)
    if (is.na(zc_crs) || zc_crs != target_crs) {
      zcta_prepared <- sf::st_transform(zcta_prepared, crs = link_crs)
    }
    zcta_vect_prepared <- local({
      zcta_sf_local <- zcta_prepared
      zcta_vect_cache <- NULL
      function() {
        if (is.null(zcta_vect_cache)) {
          zcta_vect_cache <<- terra::vect(zcta_sf_local)
        }
        zcta_vect_cache
      }
    })
  }

  counties_prepared <- counties.
  counties_vect_prepared <- NULL
  if (link.to == "counties" && !is.null(counties_prepared)) {
    target_crs <- sf::st_crs(link_crs)
    counties_crs <- sf::st_crs(counties_prepared)
    if (is.na(counties_crs) || counties_crs != target_crs) {
      counties_prepared <- sf::st_transform(counties_prepared, crs = link_crs)
    }
    counties_vect_prepared <- local({
      counties_sf_local <- counties_prepared
      counties_vect_cache <- NULL
      function() {
        if (is.null(counties_vect_cache)) {
          counties_vect_cache <<- terra::vect(counties_sf_local)
        }
        counties_vect_cache
      }
    })
  }

  # If year.mons not provided, derive from start/end dates.
  if (is.null(year.mons)) {
    sd <- as.Date(start.date)
    ed <- as.Date(end.date)
    if (is.na(sd) || is.na(ed)) {
      stop("start.date and end.date must be coercible to Date (e.g., '2005-01-02')", call. = FALSE)
    }
    year.mons <- get_yearmon(
      start.year = format(sd, "%Y"),
      start.month = format(sd, "%m"),
      end.year = format(ed, "%Y"),
      end.month = format(ed, "%m")
    )
  }

  # Resolve directory paths from package cache (set by create_dirs()).
  if (is.null(hysp_dir)) {
    hysp_dir <- .disperseR_cache_get("hysp_dir")
  }
  if (is.null(ziplink_dir)) {
    ziplink_dir <- .disperseR_cache_get("ziplink_dir")
  }
  if (is.null(hysp_dir) || !nzchar(hysp_dir)) {
    stop("hysp_dir is not set. Run create_dirs() first or pass hysp_dir explicitly.", call. = FALSE)
  }
  if (is.null(ziplink_dir) || !nzchar(ziplink_dir)) {
    stop("ziplink_dir is not set. Run create_dirs() first or pass ziplink_dir explicitly.", call. = FALSE)
  }
  .disperseR_cache_set("hysp_dir", hysp_dir)
  .disperseR_cache_set("ziplink_dir", ziplink_dir)

  # Detect OS for parallelization strategy
  is_windows <- .Platform$OS.type == "windows"
  if (is_windows && mc.cores > 1) {
    pkg_path <- system.file(package = "disperseR")
    if (!nzchar(pkg_path)) {
      warning(
        "Windows parallel runs require disperseR to be installed. ",
        "Install the package (not just devtools::load_all) or set mc.cores = 1.",
        call. = FALSE
      )
      mc.cores <- 1
    }
  }
  if (is_windows && mc.cores > 1 && pbl_trim_effective && inherits(pbl.height, "SpatRaster")) {
    packed_ok <- TRUE
    pbl.height.packed <- tryCatch(
      terra::wrap(pbl.height, proxy = TRUE),
      error = function(e) {
        packed_ok <<- FALSE
        warning(
          "Could not pack pbl.height for Windows parallel transfer (",
          conditionMessage(e),
          "). Falling back to mc.cores = 1.",
          call. = FALSE
        )
        NULL
      }
    )
    if (packed_ok) {
      pbl.height <- pbl.height.packed
    } else {
      mc.cores <- 1
    }
  }
  units.run <- data.table::as.data.table(units.run)
  units.run <- unique(units.run[, .(uID, ID)])
  task_grid <- data.table::CJ(
    unit_idx = seq_len(nrow(units.run)),
    month_idx = seq_along(year.mons),
    unique = TRUE
  )
  use_parallel <- mc.cores > 1 && nrow(task_grid) > 1
  parallel_dt_threads <- getOption("disperseR.parallel.dt.threads", 1L)
  if (!is.numeric(parallel_dt_threads) ||
      length(parallel_dt_threads) != 1 ||
      !is.finite(parallel_dt_threads) ||
      parallel_dt_threads < 1) {
    parallel_dt_threads <- 1L
  }
  parallel_dt_threads <- as.integer(parallel_dt_threads)
  if (use_parallel) {
    old_dt_threads <- data.table::getDTthreads()
    data.table::setDTthreads(parallel_dt_threads)
    on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
  }
  resolve_pbl_height <- local({
    cached <- NULL
    function() {
      if (is.null(cached)) {
        cached <<- if (inherits(pbl.height, "PackedSpatRaster")) {
          tryCatch(
            terra::unwrap(pbl.height),
            error = function(e) {
              stop(
                "Failed to unpack pbl.height for linking: ",
                conditionMessage(e),
                call. = FALSE
              )
            }
          )
        } else {
          pbl.height
        }
      }
      cached
    }
  })

  # On Windows, creating a socket cluster is expensive; create once and reuse.
  cl <- NULL
  if (use_parallel && is_windows) {
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    cluster_export <- c(
      "hysp_dir", "ziplink_dir",
      "units.run", "task_grid", "year.mons",
      "link.to", "pbl.height",
      "duration.run.hours", "overwrite", "res.link",
      "pbl_trim_effective", "crop.usa", "return.linked.data", "engine",
      "parallel_dt_threads"
    )
    if (link.to == "zips") {
      cluster_export <- c(cluster_export, "crosswalk.", "zcta_prepared", "zcta_vect_prepared")
    } else if (link.to == "counties") {
      cluster_export <- c(cluster_export, "counties_prepared", "counties_vect_prepared")
    }

    parallel::clusterExport(cl, cluster_export, envir = environment())
    # Workers need ::: to reach internal cache setter (not exported).
    # This is the standard R pattern for socket-cluster workers.
    parallel::clusterEvalQ(cl, {
      library(disperseR)
      library(data.table)
      data.table::setDTthreads(parallel_dt_threads)
      disperseR:::.disperseR_cache_set("hysp_dir", hysp_dir)
      disperseR:::.disperseR_cache_set("ziplink_dir", ziplink_dir)
    })
  }

  run_task <- function(task_index) {
    task <- task_grid[task_index]
    unit <- units.run[task$unit_idx]
    month_YYYYMM <- year.mons[[task$month_idx]]
    unit_id <- as.character(unit$ID[[1]])
    pbl.height.task <- resolve_pbl_height()

    tryCatch(
      {
        if (link.to == "zips") {
          disperser_link_zips(
            month_YYYYMM = month_YYYYMM,
            unit = unit,
            pbl.height = pbl.height.task,
            crosswalk. = crosswalk.,
            zcta = zcta_prepared,
            zcta.vect = zcta_vect_prepared,
            duration.run.hours = duration.run.hours,
            overwrite = overwrite,
            res.link. = res.link,
            pbl. = pbl_trim_effective,
            return.linked.data. = return.linked.data,
            engine = engine
          )
        } else if (link.to == "counties") {
          disperser_link_counties(
            month_YYYYMM = month_YYYYMM,
            unit = unit,
            pbl.height = pbl.height.task,
            counties = counties_prepared,
            counties.vect = counties_vect_prepared,
            duration.run.hours = duration.run.hours,
            overwrite = overwrite,
            res.link. = res.link,
            pbl. = pbl_trim_effective,
            return.linked.data. = return.linked.data,
            engine = engine
          )
        } else {
          disperser_link_grids(
            month_YYYYMM = month_YYYYMM,
            unit = unit,
            pbl.height = pbl.height.task,
            duration.run.hours = duration.run.hours,
            overwrite = overwrite,
            res.link. = res.link,
            pbl. = pbl_trim_effective,
            crop.usa = crop.usa,
            return.linked.data. = return.linked.data,
            engine = engine
          )
        }
      },
      error = function(e) {
        structure(
          list(
            unit_id = unit_id,
            month = month_YYYYMM,
            message = conditionMessage(e)
          ),
          class = "disperseR_link_task_error"
        )
      }
    )
  }

  # Cross-platform parallel task runner with load balancing for uneven task runtimes.
  run_tasks <- function() {
    idx <- seq_len(nrow(task_grid))
    if (!use_parallel || length(idx) == 1) {
      return(lapply(idx, run_task))
    }
    if (is_windows) {
      return(parallel::parLapplyLB(cl, idx, run_task))
    }
    parallel::mclapply(
      idx,
      run_task,
      mc.cores = mc.cores,
      mc.preschedule = FALSE
    )
  }

  # Inform user about parallelization mode
  if (use_parallel) {
    if (is_windows) {
      message(sprintf(
        "Windows: using load-balanced socket cluster with %d workers across %d tasks",
        mc.cores, nrow(task_grid)
      ))
    } else {
      message(sprintf(
        "Unix/macOS: using load-balanced fork parallelization with %d cores across %d tasks",
        mc.cores, nrow(task_grid)
      ))
    }
  }

  task_results <- run_tasks()
  ok <- vapply(task_results, data.table::is.data.table, logical(1))

  if (any(!ok)) {
    errs <- task_results[!ok]
    error_msgs <- vapply(errs, function(entry) {
      paste0(
        "[ID=", entry$unit_id,
        " month=", entry$month,
        "] ", entry$message
      )
    }, character(1))
    if (!any(ok)) {
      stop(
        "All linking tasks failed:\n",
        paste("  -", error_msgs, collapse = "\n"),
        call. = FALSE
      )
    }
    warning(
      sum(!ok), " of ", length(task_results),
      " linking task(s) failed:\n",
      paste("  -", error_msgs, collapse = "\n"),
      call. = FALSE
    )
  }

  if (any(ok)) {
    out <- data.table::rbindlist(task_results[ok], fill = TRUE)
    if ("month" %in% names(out)) {
      out[, month := as.character(month)]
    }
  } else {
    if (link.to == "zips") {
      out <- data.table::data.table(ZIP = character(), N = numeric(), month = character(), ID = character())
    } else if (link.to == "counties") {
      out <- data.table::data.table(
        statefp = character(),
        countyfp = character(),
        state_name = character(),
        name = character(),
        geoid = character(),
        N = numeric(),
        month = character(),
        ID = character()
      )
    } else {
      out <- data.table::data.table(x = numeric(), y = numeric(), N = numeric(), month = character(), ID = character())
    }
  }

  if (nrow(out) > 0) {
    out[, comb := paste("month:", month, "unitID:", ID, sep = " ")]
  } else if (!"comb" %in% names(out)) {
    out[, comb := character()]
  }

  return(out)
}
