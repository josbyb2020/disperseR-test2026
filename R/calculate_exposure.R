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
#'   Default: 'SO2..tons.'.
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
                               pollutant = 'SO2..tons.',
                               units.mo = NULL,
                               monthly_maps = NULL,
                               rda_file = NULL,
                               exp_dir = NULL,
                               source.agg = c('total', 'facility', 'unit'),
                               time.agg = c('year', 'month'),
                               return.monthly.data = FALSE,
                               allow.partial = FALSE) {
  `%ni%` <- Negate(`%in%`)

  # Validate source.agg
  if (length(source.agg) > 1) {
    message('Multiple source.agg provided, defaulting to "total".')
    source.agg <- 'total'
  }
  if (source.agg %ni% c('total', 'facility', 'unit')) {
    stop('source.agg not recognized, please provide one of c("total", "facility", "unit").')
  }
  
  # Validate time.agg
  if (length(time.agg) > 1) {
    message('Multiple time.agg provided, defaulting to "year".')
    time.agg <- 'year'
  }
  if (time.agg %ni% c('year', 'month')) {
    stop('time.agg not recognized, please provide one of c("year", "month").')
  }

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
    # Load into a temporary environment to avoid global pollution
    load_env <- new.env(parent = emptyenv())
    load(rda_file, envir = load_env)
    monthly_maps <- as.list(load_env)
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

  #initiate exposure data.table
  exposures <-  data.table()

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
  for (i in 1:12) {
    PP.units_monthly <- subset(units.mo, month == i & year == year.E)
    setnames(PP.units_monthly, pollutant, 'pollutant')

    #Aggregate unit power plant emissions to unit level
    PP_monthly <- PP.units_monthly[!duplicated(uID)]
    PP_monthly <- PP_monthly[is.na(pollutant), pollutant := 0]

    # Get HYSPLIT mappings from monthly_maps list (no global env probing)
    map.name <- paste0("MAP", i, ".", year.D)
    if (!map.name %in% names(monthly_maps)) {
      message("  ", map.name, " not found in monthly_maps. Skipping.")
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
    names(month_mapping)[names(month_mapping) %ni% 'state_name'] <-
      gsub('_|-|\\*', '.', names(month_mapping)[names(month_mapping) %ni% 'state_name'])

    month_mapping_long <- data.table::melt(
      month_mapping,
      id.vars = id.v,
      variable.factor = FALSE,
      variable.name = "uID",
      value.name = "N"
    )
    if (sapply(month_mapping_long, class)['N'] == 'character')
      month_mapping_long[, `:=`(N = as.double(N))]

    #This is what I want - pollutant-weighted emissions trajectories
    PP.linkage <-
      merge(month_mapping_long,
            PP_monthly,
            by = 'uID',
            all.y = TRUE)
    PP.linkage[is.na(N), N := 0]
    PP.linkage[is.na(pollutant), pollutant := 0]

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

      # Append running data frame
      exposures <- data.table(rbind(exposures,
                                    PP.linkage[, list(Exposure = sum(Exposure)),
                                               by = sum.by]))

      # sum over the year so far
      exposures <- exposures[, list(Exposure = sum(Exposure)),
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

      # Append running data frame
      exposures <- data.table(rbind(exposures,
                                    PP.linkage[, list(hyads = sum(Exposure)),
                                               by = sum.by]))[hyads > 0]

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
        exposures <- exposures[ZIP != '   NA']

      if (nrow(exposures) != 0) {
        write.fst(exposures,
                  path = file.mo)
        monthly.filelist[i] <- file.mo
      }
      #re-initiate ZIP exposure data.table
      exposures <- data.table::data.table()
    }

  }

  if (time.agg == 'year') {
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
