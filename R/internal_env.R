.disperseR_cache <- new.env(parent = emptyenv())

.disperseR_cache_get <- function(name, default = NULL) {

  get0(name, envir = .disperseR_cache, ifnotfound = default)
}

# Detect if running on ARM macOS (Apple Silicon)
.disperseR_is_arm_mac <- function() {
  Sys.info()[["sysname"]] == "Darwin" && 
    Sys.info()[["machine"]] %in% c("arm64", "aarch64")
}

# Wrap x86_64 binary command with arch -x86_64 on ARM Macs
.disperseR_rosetta_wrap <- function(binary_path) {

  if (.disperseR_is_arm_mac()) {
    # Check if binary is x86_64
    file_info <- tryCatch(
      system2("file", shQuote(binary_path), stdout = TRUE, stderr = TRUE),
      error = function(e) ""
    )
    if (any(grepl("x86_64", file_info, fixed = TRUE))) {
      return(paste("arch -x86_64", shQuote(binary_path)))
    }
  }
  shQuote(binary_path)
}

.disperseR_cache_set <- function(name, value) {
  assign(name, value, envir = .disperseR_cache)
  invisible(value)
}

#' List all cached values from create_dirs() and get_data()
#'
#' @description Shows directory paths and datasets currently stored in the
#'   internal disperseR session cache. Useful for debugging "Run create_dirs()
#'   first" errors.
#'
#' @return A named list of cached values (directory paths shown as strings,
#'   large objects summarized by class and size).
#' @export
disperseR_cache_list <- function() {
  nms <- ls(envir = .disperseR_cache)
  if (length(nms) == 0) {
    message("disperseR cache is empty. Run create_dirs() to populate it.")
    return(invisible(list()))
  }
  vals <- lapply(nms, function(nm) {
    val <- get(nm, envir = .disperseR_cache)
    if (is.character(val) && length(val) == 1) {
      val  # directory paths — show as-is
    } else {
      paste0("<", class(val)[1], " ", format(utils::object.size(val), units = "auto"), ">")
    }
  })
  names(vals) <- nms
  vals
}

#' Clear the disperseR session cache
#'
#' @description Removes all cached directory paths and datasets. You will need
#'   to call `create_dirs()` again after clearing.
#'
#' @return Invisibly returns NULL.
#' @export
disperseR_cache_clear <- function() {
  rm(list = ls(envir = .disperseR_cache), envir = .disperseR_cache)
  message("disperseR cache cleared.")
  invisible(NULL)
}

# Escape special regex characters so IDs are matched literally in list.files(pattern=...)
.disperseR_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

# Validate ID values used as file/path components across platforms.
# Reject path separators and characters invalid on Windows to keep outputs portable.
.disperseR_validate_id_component <- function(id, arg_name = "ID") {
  if (length(id) != 1 || is.na(id) || !nzchar(id)) {
    stop(arg_name, " must be a single, non-empty string.", call. = FALSE)
  }
  id <- as.character(id)

  if (grepl("[/\\\\]", id)) {
    stop(arg_name, " cannot contain path separators ('/' or '\\\\').", call. = FALSE)
  }
  if (grepl("[:*?\"<>|]", id)) {
    stop(
      arg_name,
      " contains characters that are not portable in filenames: : * ? \" < > |",
      call. = FALSE
    )
  }
  if (grepl("[[:cntrl:]]", id)) {
    stop(arg_name, " contains control characters and is not valid.", call. = FALSE)
  }

  id
}

.disperseR_splitr_package <- function() {
  if (nzchar(system.file(package = "splitr"))) {
    return("splitr")
  }
  NULL
}

.disperseR_require_splitr <- function(feature = NULL, fn = NULL) {
  splitr_pkg <- .disperseR_splitr_package()
  if (is.null(splitr_pkg)) {
    if (is.null(feature)) {
      stop("This feature requires the 'splitr' package. ",
           "Install it with remotes::install_github('rich-iannone/splitr').",
           call. = FALSE)
    }
    stop(feature, " requires the 'splitr' package. ",
         "Install it with remotes::install_github('rich-iannone/splitr').",
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
    stop("splitr does not export '", fn,
         "'. Please update splitr.", call. = FALSE)
  }
  splitr_fun
}
