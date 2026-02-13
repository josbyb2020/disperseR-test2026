#' Read linked ZIP data from an .fst file
#'
#' @param i Index into `files`.
#' @param files Character vector of .fst paths.
#' @return A data.table of ZIP links for the selected file.
#' @keywords internal
read_ziplinks_subfun <- function(i, files) {
  d <- tryCatch(
    read.fst(files[i], as.data.table = TRUE),
    error = function(e) {
      warning("Failed to read fst file '", files[i], "': ", conditionMessage(e),
              call. = FALSE)
      return(NULL)
    }
  )
  if (is.null(d)) return(NULL)
  d[, `:=` (ZIP = as.character(ZIP),
            month = as.character(month))]
  d <- d[N > 0]
  return(d)
}

#' Read linked grid data from an .fst file
#'
#' @param i Index into `files`.
#' @param files Character vector of .fst paths.
#' @return A data.table of grid links for the selected file.
#' @keywords internal
read_gridlinks_subfun <- function(i, files) {
  d <- tryCatch(
    read.fst(files[i], as.data.table = TRUE),
    error = function(e) {
      warning("Failed to read fst file '", files[i], "': ", conditionMessage(e),
              call. = FALSE)
      return(NULL)
    }
  )
  if (is.null(d)) return(NULL)
  d[, month := as.character(month)]
  d <- d[N > 0]
  return(d)
}

#' Read linked county data from an .fst file
#'
#' @param i Index into `files`.
#' @param files Character vector of .fst paths.
#' @return A data.table of county links for the selected file.
#' @keywords internal
read_countylinks_subfun <- function(i, files) {
  d <- tryCatch(
    read.fst(files[i], as.data.table = TRUE),
    error = function(e) {
      warning("Failed to read fst file '", files[i], "': ", conditionMessage(e),
              call. = FALSE)
      return(NULL)
    }
  )
  if (is.null(d)) return(NULL)
  d[, month := as.character(month)]
  d <- d[ N > 0]
  return(d)
}
