.disperseR_cache <- new.env(parent = emptyenv())

.disperseR_cache_get <- function(name, default = NULL) {
  get0(name, envir = .disperseR_cache, ifnotfound = default)
}

.disperseR_cache_set <- function(name, value) {
  assign(name, value, envir = .disperseR_cache)
  invisible(value)
}

.disperseR_splitr_package <- function() {
  if (requireNamespace("splitr", quietly = TRUE)) {
    return("splitr")
  }
  if (requireNamespace("SplitR", quietly = TRUE)) {
    return("SplitR")
  }
  NULL
}

.disperseR_require_splitr <- function(feature = NULL, fn = NULL) {
  splitr_pkg <- .disperseR_splitr_package()
  if (is.null(splitr_pkg)) {
    if (is.null(feature)) {
      stop("This feature requires the 'splitr' package (aka 'SplitR').",
           call. = FALSE)
    }
    stop(feature, " requires the 'splitr' package (aka 'SplitR').",
         call. = FALSE)
  }
  if (is.null(fn)) {
    return(invisible(splitr_pkg))
  }
  splitr_fun <- tryCatch(
    getExportedValue(splitr_pkg, fn),
    error = function(e) e
  )
  if (inherits(splitr_fun, "error")) {
    stop("SplitR/splitr does not export '", fn,
         "'. Please update splitr.", call. = FALSE)
  }
  splitr_fun
}
