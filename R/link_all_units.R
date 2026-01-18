#' Link air parcels to spatial units for multiple emission sources
#'
#' @description Links all air parcels to relevant spatial scales (ZIP codes,
#' counties, or grids) by month for specified units. Reads HYSPLIT output files
#' produced by run_disperser_parallel() and performs spatial aggregation.
#' 
#' Automatically uses appropriate parallelization based on OS:
#' mclapply on Unix/macOS, parLapply on Windows.
#'
#' @param units.run A data.table with columns: ID (character), uID (character),
#'   Latitude (numeric), Longitude (numeric), year (integer)
#' @param link.to One of 'zips', 'counties', or 'grids'
#' @param mc.cores Number of cores for parallel computation. Set to 1 for serial.
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
#'
#' @return data.table with linked spatial data
#' @export
#' @importFrom parallel detectCores mclapply makeCluster stopCluster clusterExport parLapply clusterEvalQ
#' @importFrom data.table rbindlist
link_all_units <- function(units.run,
                           link.to = 'zips',
                           mc.cores = parallel::detectCores(),
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
                           return.linked.data = TRUE) {

  # Input validation
 if ((is.null(start.date) || is.null(end.date)) && is.null(year.mons)) {
    stop("Define either a start.date and an end.date OR a year.mons")
  }
  if (length(link.to) != 1 || !(link.to %in% c('zips', 'counties', 'grids'))) {
    stop("link.to should be one of 'zips', 'counties', or 'grids'")
  }
  if (link.to == 'zips' && is.null(crosswalk.)) {
    stop("crosswalk. must be provided if link.to == 'zips'")
  }
  if (link.to == 'counties' && is.null(counties.)) {
    stop("counties. must be provided if link.to == 'counties'")
  }
  if (pbl_trim && is.null(pbl.height)) {
    stop("pbl.height must be provided if pbl_trim == TRUE")
  }

  # Backward-compat: pbl.trim was historically used internally; prefer pbl_trim.
  if (!is.null(pbl.trim)) {
    pbl_trim_effective <- isTRUE(pbl.trim)
  } else {
    pbl_trim_effective <- isTRUE(pbl_trim)
  }

  # If year.mons not provided, derive from start/end dates.
  if (is.null(year.mons)) {
    sd <- as.Date(start.date)
    ed <- as.Date(end.date)
    if (is.na(sd) || is.na(ed)) {
      stop("start.date and end.date must be coercible to Date (e.g., '2005-01-02')", call. = FALSE)
    }
    year.mons <- disperseR::get_yearmon(
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

  # Detect OS for parallelization strategy
  is_windows <- .Platform$OS.type == "windows"
  use_parallel <- mc.cores > 1 && length(year.mons) > 1

  # On Windows, creating a socket cluster is expensive; create once and reuse.
  cl <- NULL
  if (use_parallel && is_windows) {
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl, c("hysp_dir", "ziplink_dir"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(disperseR)
      library(data.table)
    })
  }

  # Cross-platform parallel apply helper
  safe_mclapply <- function(X, FUN, ...) {
    if (!use_parallel || length(X) == 1) {
      # Sequential execution
      return(lapply(X, FUN, ...))
    } else if (is_windows) {
      # Windows: socket cluster (created once above)
      return(parallel::parLapply(cl, X, FUN, ...))
    } else {
      # Unix/macOS: fork-based
      return(parallel::mclapply(X, FUN, ..., mc.cores = mc.cores))
    }
  }

  # Link functions for each spatial type
  zips_link_parallel <- function(unit) {
    linked_zips <- safe_mclapply(
      year.mons,
      disperseR::disperser_link_zips,
      unit = unit,
      pbl.height = pbl.height,
      crosswalk. = crosswalk.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      pbl. = pbl_trim_effective,
      return.linked.data. = return.linked.data
    )

    linked_zips <- data.table::rbindlist(Filter(data.table::is.data.table, linked_zips))
    message(paste("Processed unit", unit$ID))

    if (nrow(linked_zips) > 0) {
      linked_zips[, month := as.character(month)]
    }
    return(linked_zips)
  }

  counties_link_parallel <- function(unit) {
    linked_counties <- safe_mclapply(
      year.mons,
      disperseR::disperser_link_counties,
      unit = unit,
      pbl.height = pbl.height,
      counties = counties.,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      pbl. = pbl_trim_effective,
      return.linked.data. = return.linked.data
    )

    linked_counties <- data.table::rbindlist(Filter(data.table::is.data.table, linked_counties))
    message(paste("Processed unit", unit$ID))

    if (nrow(linked_counties) > 0) {
      linked_counties[, month := as.character(month)]
    }
    return(linked_counties)
  }

  grids_link_parallel <- function(unit) {
    linked_grids <- safe_mclapply(
      year.mons,
      disperseR::disperser_link_grids,
      unit = unit,
      pbl.height = pbl.height,
      duration.run.hours = duration.run.hours,
      overwrite = overwrite,
      res.link. = res.link,
      pbl. = pbl_trim_effective,
      crop.usa = crop.usa,
      return.linked.data. = return.linked.data
    )

    linked_grids <- data.table::rbindlist(Filter(data.table::is.data.table, linked_grids))
    message(paste("Processed unit", unit$ID))

    if (nrow(linked_grids) > 0) {
      linked_grids[, month := as.character(month)]
    }
    return(linked_grids)
  }

  # Inform user about parallelization mode
  if (use_parallel) {
    if (is_windows) {
      message(sprintf("Windows: using socket cluster with %d workers", mc.cores))
    } else {
      message(sprintf("Unix/macOS: using fork-based parallelization with %d cores", mc.cores))
    }
  }

  # Process units
  units.run <- unique(units.run[, .(uID, ID)])

  if (link.to == 'zips') {
    out <- units.run[, zips_link_parallel(.SD), by = seq_len(nrow(units.run))]
  } else if (link.to == 'counties') {
    out <- units.run[, counties_link_parallel(.SD), by = seq_len(nrow(units.run))]
  } else if (link.to == 'grids') {
    out <- units.run[, grids_link_parallel(.SD), by = seq_len(nrow(units.run))]
  }

  if (nrow(out) > 0) {
    out[, comb := paste("month:", month, "unitID:", ID, sep = " ")]
    out[, seq_len := NULL]
  }
  
  return(out)
}
