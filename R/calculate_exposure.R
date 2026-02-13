#' Calculate exposure from linked HYSPLIT outputs
#'
#' @description Takes linked HYSPLIT outputs combined using 
#' `disperseR::combine_monthly_links()` and sums them by emissions. Results 
#' can be aggregated to three source levels and two time scales.
#'
#' @param year.E Numeric. Emissions year.
#' @param year.D Numeric. HYSPLIT dispersion year.
#' @param link.to Character. Spatial scale: 'zips', 'counties', or 'grids'.
#'   Must match the original input to `link_all_units()`.
#' @param pollutant Character. Column name in `units.mo` for emissions weighting.
#'   Default: 'SO2.tons'. (For backward compatibility, the legacy column name
#'   'SO2..tons.' is accepted when present.)
#' @param units.mo Data.frame or data.table with monthly unit emissions data.
#'   Must contain columns: uID, year, month, and the pollutant column.
#'   If NULL, attempts to use `PP.units.monthly1995_2017` from the disperseR
#'   cache (set by get_data()).
#' @param monthly_maps Named list of monthly MAP data.tables from 
#'   `combine_monthly_links()`. Names should be in format "MAP\{month\}.\{year\}".
#'   If NULL, attempts to load from `rda_file`.
#' @param rda_file Character. Path to RData file from `combine_monthly_links()`.
#'   Only used if `monthly_maps` is NULL. Default: NULL.
#' @param exp_dir Character. Directory to save exposure output. If NULL, uses
#'   the cached exp_dir from create_dirs() when available, otherwise the
#'   current working directory.
#' @param source.agg Character. Source aggregation: 'total', 'facility', or 'unit'.
#' @param time.agg Character. Time aggregation: 'year' or 'month'.
#' @param return.monthly.data Logical. Return monthly data when time.agg='month'?
#' @param allow.partial Logical. If FALSE (default), errors when any monthly maps
#'   are missing. If TRUE, warns and continues with available data.
#'
#' @return A data.table with exposure values aggregated by the specified levels.
#'   If \code{allow.partial = TRUE} and some months were missing, the result
#'   has an attribute "missing_maps" listing the skipped months.


#' @export calculate_exposure

calculate_exposure <- function(year.E,
                               year.D,
                               link.to = 'zips',
                               pollutant = 'SO2.tons',
                               units.mo = NULL,
                               monthly_maps = NULL,
                               rda_file = NULL,
                               exp_dir = NULL,
                               source.agg = c('total', 'facility', 'unit'),
                               time.agg = c('year', 'month'),
                               return.monthly.data = FALSE,
                               allow.partial = FALSE) {
  source.agg <- match.arg(source.agg)
  time.agg   <- match.arg(time.agg)

  # Validate units.mo
  if (is.null(units.mo)) {
    units.mo <- .disperseR_cache_get("PP.units.monthly1995_2017")
    if (is.null(units.mo)) {
      stop("units.mo must be provided. This should be a data.table with monthly unit data.\n",
           "Use disperseR::PP.units.monthly1995_2017 or provide your own dataset.",
           call. = FALSE)
    }
    message("Using PP.units.monthly1995_2017 from cache for units.mo.")
  }
  if (!inherits(units.mo, "data.frame")) {
    stop("units.mo must be a data.frame or data.table.", call. = FALSE)
  }
  units.mo <- data.table::as.data.table(units.mo)

  pollutant <- as.character(pollutant)
  if (length(pollutant) != 1 || is.na(pollutant) || !nzchar(pollutant)) {
    stop("pollutant must be a single, non-empty string.", call. = FALSE)
  }
  if (!pollutant %in% names(units.mo)) {
    legacy_map <- list("SO2..tons." = "SO2.tons", "SO2.tons" = "SO2..tons.")
    alt <- legacy_map[[pollutant]]
    if (!is.null(alt) && alt %in% names(units.mo)) {
      warning(
        "pollutant column '", pollutant, "' not found in units.mo; using '", alt, "' instead.",
        call. = FALSE
      )
      pollutant <- alt
    }
  }
  required_cols <- c("uID", "year", "month", pollutant)
  missing_cols <- setdiff(required_cols, names(units.mo))
  if (length(missing_cols) > 0) {
    stop(
      "units.mo is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Load or validate monthly_maps
  if (is.null(monthly_maps)) {
    if (is.null(rda_file)) {
      stop("Either monthly_maps (from combine_monthly_links()) or rda_file must be provided.",
           call. = FALSE)
    }
    if (!file.exists(rda_file)) {
      stop("rda_file does not exist: ", rda_file, call. = FALSE)
    }
    # Load into a temporary environment to avoid global pollution.
    # Handles both new format (single 'monthly_maps' list) and legacy
    # format (many MAP*.YYYY objects spread across the environment).
    load_env <- new.env(parent = emptyenv())
    load(rda_file, envir = load_env)
    if (exists("monthly_maps", envir = load_env, inherits = FALSE) &&
        is.list(get("monthly_maps", envir = load_env))) {
      monthly_maps <- get("monthly_maps", envir = load_env)
    } else {
      monthly_maps <- as.list(load_env)
    }
    message("Loaded ", length(monthly_maps), " monthly maps from ", basename(rda_file))
  }
  if (!is.list(monthly_maps)) {
    stop("monthly_maps must be a named list of data.tables.", call. = FALSE)
  }
  if (length(monthly_maps) == 0) {
    stop("monthly_maps is empty. No data to process.", call. = FALSE)
  }
  
  # Pre-check which maps are available for the requested year
  expected_maps <- paste0("MAP", 1:12, ".", year.D)
  available_maps <- intersect(expected_maps, names(monthly_maps))
  if (length(available_maps) == 0) {
    stop("No monthly maps found for year ", year.D, " in monthly_maps.\n",
         "Expected names like 'MAP1.", year.D, "', 'MAP2.", year.D, "', etc.\n",
         "Available names: ", paste(utils::head(names(monthly_maps), 5), collapse = ", "),
         if (length(names(monthly_maps)) > 5) "..." else "",
         call. = FALSE)
  }
  missing_maps <- setdiff(expected_maps, names(monthly_maps))
  if (length(missing_maps) > 0) {
    msg <- paste0("Missing ", length(missing_maps), " of 12 monthly maps for year ", year.D, ": ",
                  paste(missing_maps, collapse = ", "))
    if (!allow.partial) {
      stop(msg, "\nSet allow.partial = TRUE to proceed with available data.", call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  # Create directory to store output files if it does not exist
  if (is.null(exp_dir)) {
    exp_dir <- .disperseR_cache_get("exp_dir")
    if (!is.null(exp_dir) && nzchar(exp_dir)) {
      message("Using exp_dir from cache: ", exp_dir)
    } else {
      exp_dir <- file.path(getwd(), "rdata_hyspdisp")
      message("No exp_dir provided. Defaulting to ", exp_dir)
    }
  }
  exp_dir <- path.expand(exp_dir)
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)

  #initiate list for collecting monthly results (avoids O(n^2) rbind accumulation)
  monthly_results <- vector("list", 12)

  #initiate list of monthly files
  monthly.filelist <- c()

  #Iterate over months of the year
  message(
    paste0(
      "Calculating ", link.to, " exposures for HYSPLIT year ",
      year.D,
      " and emissions year ",
      year.E,
      "!"
    )
  )
  for (i in seq_len(12)) {
    PP.units_monthly <- units.mo[month == i & year == year.E]
    data.table::setnames(PP.units_monthly, pollutant, "pollutant", skip_absent = TRUE)

    #Aggregate unit power plant emissions to unit level
    PP_monthly <- PP.units_monthly[!duplicated(uID)]
    PP_monthly <- PP_monthly[is.na(pollutant), pollutant := 0]

    # Get HYSPLIT mappings from monthly_maps list (no global env probing)
    map.name <- paste0("MAP", i, ".", year.D)
    if (!map.name %in% names(monthly_maps)) {
      next
    }
    month_mapping <- data.table::copy(monthly_maps[[map.name]])

    #melt them to long format
    if( link.to == 'zips'){
      id.v <- 'ZIP'
      month_mapping <- month_mapping[ZIP != 'ZIP']
    } else if( link.to == 'counties'){
      id.v <- c("statefp", "countyfp", "state_name", "name", "geoid")
    } else if( link.to == 'grids')
      id.v <- c('x', 'y')

    month_mapping[is.na(month_mapping)] <- 0

    month_mapping_long <- data.table::melt(
      month_mapping,
      id.vars = id.v,
      variable.factor = FALSE,
      variable.name = "uID",
      value.name = "N"
    )
    if (is.character(month_mapping_long[["N"]]))
      month_mapping_long[, `:=`(N = as.double(N))]

    # Wide map columns use ID format (hyphenated, e.g. "7-1") from
    # combine_monthly_links(); emissions data uses uID format (dotted,
    # e.g. "7.1"). Convert so the merge matches.
    month_mapping_long[, uID := gsub("-", ".", uID, fixed = TRUE)]

    #This is what I want - pollutant-weighted emissions trajectories
    PP.linkage <-
      merge(month_mapping_long,
            PP_monthly,
            by = 'uID',
            all.y = TRUE)
    PP.linkage[is.na(N), N := 0]
    PP.linkage[is.na(pollutant), pollutant := 0]

    # Warn if no spatial linkages matched any units (silent merge failure)
    if (nrow(PP.linkage) > 0 && all(PP.linkage$N == 0)) {
      warning("Month ", i, ": no spatial linkages matched any emission units. ",
              "Check that uID formats are consistent between monthly_maps and units.mo.",
              call. = FALSE)
    }

    #  clean house
    rm(list = c('month_mapping_long', 'PP_monthly', 'month_mapping'))

    # Sum by ZIP and uID if calculating annual
    if (time.agg == 'year') {
      # define aggregation strings
      if (source.agg == 'total'){
        sum.by <- id.v
        file.by <- '_exposures_total_'
      }
      if (source.agg == 'facility'){
        sum.by <- c(id.v, 'FacID')
        file.by <- '_exposures_byfacility_'
      }
      if (source.agg == 'unit'){
        sum.by <- c(id.v, 'uID')
        file.by <- '_exposures_byunit_'
      }

      # calculate exposure, label year/month
      PP.linkage[, `:=` (Exposure  = pollutant * N)]

      # collect monthly aggregate (final merge done after loop)
      monthly_results[[i]] <- PP.linkage[, list(Exposure = sum(Exposure)),
                                         by = sum.by]
    } else {
      # define aggregation strings
      if (source.agg == 'total'){
        sum.by <- c(id.v, 'yearmonth')
        file.by <- '_exposures_total_'
      }
      if (source.agg == 'facility'){
        sum.by <- c(id.v, 'FacID', 'yearmonth')
        file.by <- '_exposures_byfacility_'
      }
      if (source.agg == 'unit'){
        sum.by <- c(id.v, 'uID', 'yearmonth')
        file.by <- '_exposures_byunit_'
      }

      # add month
      PP.linkage[, `:=` (
        Exposure  = pollutant * N,
        yearmonth = paste0(year.E, formatC(i, width = 2, flag = "0"))
      )]

      # Aggregate this month's exposure
      month_exposures <- PP.linkage[, list(hyads = sum(Exposure)),
                                    by = sum.by]
      month_exposures <- month_exposures[hyads > 0]

      # write to file, add monthly file to list if not empty data.table
      file.mo <- file.path(exp_dir,
                           paste0(
                             link.to,
                             file.by,
                             paste0(year.E, '_', formatC(
                               i, width = 2, flag = '0'
                             )),
                             '.fst'
                           ))

      if( link.to == 'zips')
        month_exposures <- month_exposures[ZIP != '   NA']

      if (nrow(month_exposures) != 0) {
        write.fst(month_exposures,
                  path = file.mo)
        monthly.filelist[i] <- file.mo
      }
    }

  }

  if (time.agg == 'year') {
    # Aggregate all monthly results in one pass (replaces O(n^2) accumulation)
    exposures <- data.table::rbindlist(
      Filter(function(x) !is.null(x) && nrow(x) > 0, monthly_results)
    )
    if (nrow(exposures) > 0) {
      exposures <- exposures[, list(Exposure = sum(Exposure)), by = sum.by]
    }
    setnames(exposures,
             c('Exposure'),
             c('hyads'))
    exposures[,  `:=` (
      year.E = year.E,
      year.D = year.D
    )]
    #convert 3-digit zip code to 5, add emissions and hysplit years
    if( link.to == 'zips'){
      exposures[,  `:=` (
        ZIP = formatC(
          as.integer(ZIP),
          width = 5,
          flag = "0",
          format = "d"
        ))]
      exposures <- exposures[ZIP != '   NA']
    }
    # write to file, add monthly file to list if not empty data.table
    file.yr <- file.path(exp_dir,
                         paste0(
                           link.to,
                           file.by,
                           year.E,
                           '.fst'
                         ))
    if (nrow(exposures) != 0) {
      write.fst(exposures,
                path = file.yr)
    }

    # Attach missing maps info if partial processing occurred
    if (length(missing_maps) > 0) {
      attr(exposures, "missing_maps") <- missing_maps
    }
    return(exposures)
  } else {
    if (return.monthly.data) {
      out <- rbindlist(lapply(stats::na.omit(monthly.filelist),
                              read.fst))

      if( link.to == 'zips')
        out <- out[ZIP != '   NA']

      if (length(missing_maps) > 0) {
        attr(out, "missing_maps") <- missing_maps
      }
      return(out)
    } else {
      if (length(missing_maps) > 0) {
        attr(monthly.filelist, "missing_maps") <- missing_maps
      }
      return(monthly.filelist)
    }
  }
}
