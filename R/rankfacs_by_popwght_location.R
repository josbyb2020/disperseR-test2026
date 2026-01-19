#' Rank facilities by population-weighted exposure
#'
#' @description Ranks emission sources by their population-weighted exposure
#' contribution for a specific geographic subset.
#'
#' @param link.files Path to linked files (provide EITHER this OR data.linked)
#' @param data.linked Pre-loaded linked data (provide EITHER this OR link.files)
#' @param crosswalk. Crosswalk data with population info
#' @param rank.by Metric(s) to rank by (default: 'hyads')
#' @param zip.value ZIP code filter pattern (default: '*' for all)
#' @param state.value State filter pattern (default: '*' for all)
#' @param city.value City filter pattern (default: '*' for all)
#' @param year Year to analyze. If NULL, the function expects data.linked and
#'   crosswalk. to already contain a 'year' column.
#'
#' @return data.table with ranked facilities
#' @export
#' @importFrom data.table data.table copy setnames frankv fread
rankfacs_by_popwgt_location <- function(link.files = NULL,
                                         data.linked = NULL,
                                         crosswalk.,
                                         rank.by = c('hyads'),
                                         zip.value = '*',
                                         state.value = '*',
                                         city.value = '*',
                                         year = NULL) {

  # Validate inputs FIRST (before any conversion)
  if ((is.null(link.files) && is.null(data.linked)) || 
      (!is.null(link.files) && !is.null(data.linked))) {
    stop("Please provide EITHER link.files OR data.linked, not both or neither.")
  }

  `%ni%` <- Negate(`%in%`)

  # Convert crosswalk to data.table
  crosswalk_dt <- data.table::data.table(crosswalk.)

  # Read from file if link.files provided
  if (!is.null(link.files)) {
    data.linked <- data.table::fread(link.files)
    # Remove V1 if it exists (row numbers from CSV)
    if ("V1" %in% names(data.linked)) {
      data.linked[, V1 := NULL]
    }
    data.linked[, `:=`(
      ZIP = formatC(ZIP, width = 5, format = "d", flag = "0"),
      uID = gsub('_|-|\\*', '.', uID)
    )]
    # Extract year from yearmonth if present
    if ("yearmonth" %in% names(data.linked)) {
      data.linked[, year := as.integer(substr(as.character(yearmonth), 1, 4))]
    }
  } else {
    # Convert provided data.linked to data.table
    data.linked <- data.table::data.table(data.linked)
  }

  if (!is.null(year)) {
    year <- as.integer(year)
    if (anyNA(year)) {
      stop("year must be numeric.", call. = FALSE)
    }
    if ("year" %ni% names(crosswalk_dt)) {
      if (length(year) != 1) {
        stop(
          "year must be length 1 when crosswalk. lacks a year column.",
          call. = FALSE
        )
      }
      crosswalk_dt[, year := year]
    } else {
      crosswalk_dt <- crosswalk_dt[year %in% ..year]
    }
    if ("year" %ni% names(data.linked)) {
      if (length(year) != 1) {
        stop(
          "year must be length 1 when data.linked lacks a year column.",
          call. = FALSE
        )
      }
      data.linked[, year := year]
    } else {
      data.linked <- data.linked[year %in% ..year]
    }
  }

  # Validate year column exists in both datasets
  if ('year' %ni% names(data.linked)) {
    stop(
      "data.linked must include a 'year' column, or provide 'year' argument.",
      call. = FALSE
    )
  }
  if ('year' %ni% names(crosswalk_dt)) {
    stop(
      "crosswalk. must include a 'year' column, or provide 'year' argument.",
      call. = FALSE
    )
  }

  # Create working copy of crosswalk
  crosswalk.use <- data.table::copy(crosswalk_dt)

  # Merge ZIP code and census info with data.linked
  data.linked <- merge(data.linked, crosswalk.use, by = c('ZIP', 'year'))

  # Apply geographic filters
  zip.search <- paste0(zip.value, collapse = '|')
  state.search <- paste0(state.value, collapse = '|')
  city.search <- paste0(city.value, collapse = '|')

  data.linked.trim <- data.linked[Reduce(intersect, list(
    grep(zip.search, ZIP),
    grep(state.search, STATE),
    grep(city.search, CITY)
  ))]

  # Weight metric by population
  names.py <- paste0(rank.by, '.py')
  data.linked.trim[, (names.py) := lapply(rank.by, function(x) {
    TOTALESTIMATE * get(x)
  })]

  # Sum population-weighted rank.by by uID
  names.py.sum <- paste0(rank.by, '.py.sum')
  uID.pw <- data.linked.trim[, lapply(names.py, function(x) {
    sum(get(x))
  }), by = c("uID", "year")]
  data.table::setnames(uID.pw, paste0('V', seq_along(names.py.sum)), names.py.sum)

  # Rank facilities by metric in each year
  names.py.rank <- paste0(rank.by, '.rank')
  uID.pw[, (names.py.rank) := lapply(names.py.sum, function(x) {
    data.table::frankv(get(x), order = -1)
  }), by = year]

  return(uID.pw)
}
