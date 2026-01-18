.disperseR_cache <- new.env(parent = emptyenv())

.disperseR_cache_get <- function(name, default = NULL) {
  get0(name, envir = .disperseR_cache, ifnotfound = default)
}

.disperseR_cache_set <- function(name, value) {
  assign(name, value, envir = .disperseR_cache)
  invisible(value)
}

.disperseR_require_splitr <- function(feature = NULL, fn = NULL) {
  splitr_path <- suppressWarnings(system.file(package = "SplitR"))
  if (!nzchar(splitr_path)) {
    if (is.null(feature)) {
      stop("This feature requires the 'SplitR' package.", call. = FALSE)
    }
    stop(feature, " requires the 'SplitR' package.", call. = FALSE)
  }
  if (is.null(fn)) {
    return(invisible(TRUE))
  }
  splitr_fun <- tryCatch(
    getExportedValue("SplitR", fn),
    error = function(e) e
  )
  if (inherits(splitr_fun, "error")) {
    stop("SplitR does not export '", fn, "'. Please update SplitR.", call. = FALSE)
  }
  splitr_fun
}
