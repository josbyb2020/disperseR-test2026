

#' Define input runs for HYSPLIT simulations
#'
#' @description Builds a run table by combining unit metadata with start
#' dates/hours and run durations. The result is suitable for
#' `run_disperser_parallel()`.
#'
#' @param units Data.frame or data.table of units with `ID` and `year` columns.
#' @param startday Start day in "YYYY-MM-DD" format.
#' @param endday End day in "YYYY-MM-DD" format.
#' @param start.hours Integer vector of start hours (0-23). Defaults to
#'   `c(0, 6, 12, 18)`.
#' @param duration Duration in hours for each run (default 240).
#' @return A data.table of run parameters for `run_disperser_parallel()`.
#'
#' @export define_inputs
define_inputs <-
  function(units,
    startday,
    endday,
    start.hours =  c(0, 6, 12, 18),
    duration = 240) {

    startday.date <- as.Date(startday)
    endday.date   <- as.Date(endday)

    out <- data.table(
      expand.grid(
        ID = unique( units$ID),
        year = unique( units$year),
        start_hour = start.hours,
        start_day = seq.Date(
          from = as.Date(startday.date),
          to =   as.Date(endday.date),
          by = '1 day'
        ),
        duration_emiss_hours = 1,
        duration_run_hours = duration
      )
    )

    # get only the input year for which we get units data
    out <- out[year==year( start_day)]
    out <- unique(merge(out, units, by = c('ID', 'year')))

    return(out)
  }
