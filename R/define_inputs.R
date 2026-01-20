#' Define input runs for HYSPLIT simulations
#'
#' @description Creates a run configuration table by combining unit metadata with
#' emission dates, hours, and durations. The output is ready for
#' `run_disperser_parallel()`.
#'
#' @param units Data.frame or data.table containing power plant units. Must have
#'   columns: `ID` (character, unit identifier) and `year` (numeric, emission year).
#'   Typically from `data("units", package = "disperseR")`.
#' @param startday Character string: start date in "YYYY-MM-DD" format (e.g., "2005-01-15").
#' @param endday Character string: end date in "YYYY-MM-DD" format (e.g., "2005-01-31").
#' @param start.hours Integer vector: hours of day (0-23) when emissions start.
#'   Default is `c(0, 6, 12, 18)` for 6-hourly runs.
#' @param duration Integer: forward tracking duration in hours. Default 240 (10 days).
#'   Common values: 120 (5 days), 240 (10 days).
#'
#' @return Data.table with one row per run, containing all unit metadata plus:
#'   - `start_day`: Date of emission start
#'   - `start_hour`: Hour of emission start (0-23)
#'   - `duration_emiss_hours`: Emission duration (always 1)
#'   - `duration_run_hours`: Forward tracking duration
#'
#' @examples
#' \dontrun{
#' # Load power plant units
#' data("units", package = "disperseR")
#'
#' # Select top 2 emitters in 2005
#' top_units <- units[year == 2005][order(-SOx)][1:2]
#'
#' # Create run configurations for January 2005
#' inputs <- define_inputs(
#'   units = top_units,
#'   startday = "2005-01-01",
#'   endday = "2005-01-31",
#'   start.hours = c(0, 12), # Runs at midnight and noon
#'   duration = 120 # 5-day forward tracking
#' )
#' }
#'
#' @export
define_inputs <- function(units,
                          startday,
                          endday,
                          start.hours = c(0, 6, 12, 18),
                          duration = 240) {
  # ===== Input Validation =====

  # Check units is provided and is a data.frame

  if (missing(units) || is.null(units)) {
    stop(
      "ERROR: 'units' argument is required.\n",
      "  Provide a data.frame with power plant information.\n",
      "  Example: data('units', package = 'disperseR'); top_units <- units[year == 2005][1:2]",
      call. = FALSE
    )
  }

  if (!is.data.frame(units)) {
    stop(
      "ERROR: 'units' must be a data.frame or data.table.\n",
      "  You provided: ", class(units)[1],
      call. = FALSE
    )
  }

  if (nrow(units) == 0) {
    stop(
      "ERROR: 'units' has no rows.\n",
      "  Provide at least one power plant unit.",
      call. = FALSE
    )
  }

  # Check required columns
  required_cols <- c("ID", "year")
  missing_cols <- setdiff(required_cols, names(units))
  if (length(missing_cols) > 0) {
    stop(
      "ERROR: 'units' is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
      "  Available columns: ", paste(names(units), collapse = ", "), "\n",
      "  Hint: Load the built-in units dataset with: data('units', package = 'disperseR')",
      call. = FALSE
    )
  }

  # Check startday
  if (missing(startday) || is.null(startday)) {
    stop(
      "ERROR: 'startday' is required.\n",
      "  Provide a date string like: startday = '2005-01-15'",
      call. = FALSE
    )
  }

  startday.date <- tryCatch(
    as.Date(startday),
    error = function(e) NULL
  )
  if (is.null(startday.date) || is.na(startday.date)) {
    stop(
      "ERROR: Cannot parse 'startday': '", startday, "'\n",
      "  Use format YYYY-MM-DD, e.g., startday = '2005-01-15'",
      call. = FALSE
    )
  }

  # Check endday
  if (missing(endday) || is.null(endday)) {
    stop(
      "ERROR: 'endday' is required.\n",
      "  Provide a date string like: endday = '2005-01-31'",
      call. = FALSE
    )
  }

  endday.date <- tryCatch(
    as.Date(endday),
    error = function(e) NULL
  )
  if (is.null(endday.date) || is.na(endday.date)) {
    stop(
      "ERROR: Cannot parse 'endday': '", endday, "'\n",
      "  Use format YYYY-MM-DD, e.g., endday = '2005-01-31'",
      call. = FALSE
    )
  }

  # Check date order
  if (endday.date < startday.date) {
    stop(
      "ERROR: 'endday' (", endday, ") is before 'startday' (", startday, ").\n",
      "  endday must be >= startday.",
      call. = FALSE
    )
  }

  # Check start.hours
  if (!is.numeric(start.hours) || length(start.hours) == 0) {
    stop(
      "ERROR: 'start.hours' must be a numeric vector of hours (0-23).\n",
      "  Example: start.hours = c(0, 6, 12, 18)",
      call. = FALSE
    )
  }

  if (any(start.hours < 0 | start.hours > 23)) {
    stop(
      "ERROR: 'start.hours' values must be between 0 and 23.\n",
      "  You provided: ", paste(start.hours, collapse = ", "),
      call. = FALSE
    )
  }

  # Check duration
  if (!is.numeric(duration) || length(duration) != 1 || duration <= 0) {
    stop(
      "ERROR: 'duration' must be a positive number (hours).\n",
      "  Common values: 120 (5 days), 240 (10 days).\n",
      "  You provided: ", duration,
      call. = FALSE
    )
  }

  # ===== Build Run Table =====

  out <- data.table::data.table(
    expand.grid(
      ID = unique(units$ID),
      year = unique(units$year),
      start_hour = start.hours,
      start_day = seq.Date(
        from = startday.date,
        to = endday.date,
        by = "1 day"
      ),
      duration_emiss_hours = 1,
      duration_run_hours = duration
    )
  )

  # Filter to matching years only
  out <- out[year == data.table::year(start_day)]
  out <- unique(merge(out, units, by = c("ID", "year")))

  if (nrow(out) == 0) {
    warning(
      "No matching runs found. This usually means:\n",
      "  - The 'year' in units doesn't match the year in startday/endday\n",
      "  - Units year: ", paste(unique(units$year), collapse = ", "), "\n",
      "  - Date range: ", startday, " to ", endday,
      call. = FALSE
    )
  } else {
    message(
      "Created ", nrow(out), " run configurations for ",
      length(unique(out$ID)), " units"
    )
  }

  return(out)
}
