

#' Generate a year-month sequence
#'
#' @description `get_yearmon()` outputs a character vector of year-month
#'   strings (format \code{YYYYMM}) that you can loop over. For January
#'   through March 2005, use `start.year = "2005"`, `start.month = "01"`,
#'   `end.year = "2005"`, and `end.month = "03"`.
#'
#' @param start.year what year do you want your vector to start with? eg. start.year = "2005"
#' @param start.month what month of `start.year` eg. start.month = "01"
#' @param end.year what year do you want your vector to end with? This has to be a character eg. end.year = "2005"
#' @param end.month what month of that year? eg. end.month = "03"
#'
#' @return A character vector of year-month strings in \code{YYYYMM} format.


#' @export get_yearmon

get_yearmon <- function(start.year = NULL,
  start.month = NULL,
  end.year = NULL,
  end.month = NULL) {

  # Check for NULL or NA inputs
  if (is.null(start.year) || is.null(start.month) ||
      is.null(end.year) || is.null(end.month)) {
    stop("All arguments (start.year, start.month, end.year, end.month) must be provided and cannot be NULL.",
         call. = FALSE)
  }
  if (anyNA(c(start.year, start.month, end.year, end.month))) {
    stop("All arguments (start.year, start.month, end.year, end.month) must be provided and cannot be NA.",
         call. = FALSE)
  }

  # Auto-coerce numeric inputs to character
  coerce_year <- function(x, name) {
    if (is.numeric(x)) return(as.character(as.integer(x)))
    if (is.character(x)) return(x)
    stop(name, " must be character or numeric.", call. = FALSE)
  }
  coerce_month <- function(x, name) {
    if (is.numeric(x)) return(formatC(as.integer(x), width = 2, flag = "0"))
    if (is.character(x)) return(x)
    stop(name, " must be character or numeric.", call. = FALSE)
  }
  start.year  <- coerce_year(start.year, "start.year")
  end.year    <- coerce_year(end.year, "end.year")
  start.month <- coerce_month(start.month, "start.month")
  end.month   <- coerce_month(end.month, "end.month")

  # Validate 4-digit years
  if (nchar(start.year) != 4 || nchar(end.year) != 4) {
    stop("start.year and end.year must be 4-digit years (e.g., '2005').", call. = FALSE)
  }

  startdate <- paste0(start.year, "/", start.month, "/01")
  enddate <- paste0(end.year, "/", end.month, "/01")

  start_parsed <- as.Date(startdate)
  end_parsed <- as.Date(enddate)
  if (start_parsed > end_parsed) {
    stop("start date (", startdate, ") must be on or before end date (", enddate, ").",
         call. = FALSE)
  }
  vector <- as.character(seq(as.Date(startdate), as.Date(enddate), "months"))

  getstring <- function(date) {
    year <- substr(date, 1, 4)
    month <- substr(date, 6, 7)
    out <- paste0(year, month)
    return(out)
  }

  return(unlist(lapply(FUN = getstring, vector)))
}
