#' Read linked ZIP data from an .fst file
#'
#' @param i Index into `files`.
#' @param files Character vector of .fst paths.
#' @return A data.table of ZIP links for the selected file.
#' @export
read_ziplinks_subfun <- function(i, files) {
  d <- read.fst(files[i], as.data.table = TRUE)
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
#' @export
read_gridlinks_subfun <- function(i, files) {
  d <- read.fst(files[i], as.data.table = TRUE)
  d[, month := as.character(month)]
  d <- d[N > 0]
  return(d)
}

#' Read linked county data from an .fst file
#'
#' @param i Index into `files`.
#' @param files Character vector of .fst paths.
#' @return A data.table of county links for the selected file.
#' @export
read_countylinks_subfun <- function(i, files) {
  d <- read.fst(files[i], as.data.table = TRUE)
  d[, month := as.character(month)]
  d <- d[ N > 0]
  return(d)
}
