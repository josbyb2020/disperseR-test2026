verify_state <- new.env(parent = emptyenv())
verify_state$results <- data.frame(
  step = character(),
  status = character(),
  detail = character(),
  stringsAsFactors = FALSE
)

verify_add_result <- function(step, status, detail) {
  verify_state$results <- rbind(
    verify_state$results,
    data.frame(step = step, status = status, detail = detail, stringsAsFactors = FALSE)
  )
}

verify_skip <- function(message) {
  structure(list(message = message), class = "verify_skip")
}

verify_step <- function(name, expr) {
  message("==> ", name)
  start_time <- Sys.time()
  result <- tryCatch(
    {
      out <- force(expr)
      if (inherits(out, "verify_skip")) {
        verify_add_result(name, "SKIP", out$message)
      } else {
        verify_add_result(name, "OK", "")
      }
      out
    },
    error = function(e) {
      verify_add_result(name, "FAIL", e$message)
      e
    }
  )
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  if (inherits(result, "error")) {
    message("    ERROR (", round(elapsed, 2), "s)")
  } else if (inherits(result, "verify_skip")) {
    message("    SKIP (", round(elapsed, 2), "s): ", result$message)
  } else {
    message("    OK (", round(elapsed, 2), "s)")
  }
  invisible(result)
}

verify_expect <- function(cond, message) {
  if (!isTRUE(cond)) {
    stop(message, call. = FALSE)
  }
  TRUE
}

verify_find_pkg_root <- function(start = getwd()) {
  if (file.exists(file.path(start, "DESCRIPTION"))) {
    return(normalizePath(start))
  }
  parent <- dirname(start)
  if (file.exists(file.path(parent, "DESCRIPTION"))) {
    return(normalizePath(parent))
  }
  stop("Could not find package root (DESCRIPTION). Open the repo or verification project.",
       call. = FALSE)
}

verify_read_config <- function() {
  cfg <- list(
    base_dir = file.path(tempdir(), "disperseR_verify"),
    data_download = FALSE,
    download_met = FALSE,
    run_hysplit = FALSE,
    binary_path = NULL,
    parhplot_path = NULL,
    met_dir = NULL,
    keep_artifacts = FALSE
  )

  config_path <- file.path(getwd(), "config.R")
  if (file.exists(config_path)) {
    env <- new.env(parent = emptyenv())
    sys.source(config_path, envir = env)
    if (exists("VERIFY_BASE_DIR", envir = env, inherits = FALSE)) {
      cfg$base_dir <- env$VERIFY_BASE_DIR
    }
    if (exists("VERIFY_DATA_DOWNLOAD", envir = env, inherits = FALSE)) {
      cfg$data_download <- isTRUE(env$VERIFY_DATA_DOWNLOAD)
    }
    if (exists("VERIFY_DOWNLOAD_MET", envir = env, inherits = FALSE)) {
      cfg$download_met <- isTRUE(env$VERIFY_DOWNLOAD_MET)
    }
    if (exists("VERIFY_RUN_HYSPLIT", envir = env, inherits = FALSE)) {
      cfg$run_hysplit <- isTRUE(env$VERIFY_RUN_HYSPLIT)
    }
    if (exists("VERIFY_BINARY_PATH", envir = env, inherits = FALSE)) {
      cfg$binary_path <- env$VERIFY_BINARY_PATH
    }
    if (exists("VERIFY_PARHPLOT_PATH", envir = env, inherits = FALSE)) {
      cfg$parhplot_path <- env$VERIFY_PARHPLOT_PATH
    }
    if (exists("VERIFY_MET_DIR", envir = env, inherits = FALSE)) {
      cfg$met_dir <- env$VERIFY_MET_DIR
    }
    if (exists("VERIFY_KEEP_ARTIFACTS", envir = env, inherits = FALSE)) {
      cfg$keep_artifacts <- isTRUE(env$VERIFY_KEEP_ARTIFACTS)
    }
  }

  cfg$base_dir <- path.expand(cfg$base_dir)
  if (!is.null(cfg$binary_path)) {
    cfg$binary_path <- path.expand(cfg$binary_path)
  }
  if (!is.null(cfg$parhplot_path)) {
    cfg$parhplot_path <- path.expand(cfg$parhplot_path)
  }
  if (!is.null(cfg$met_dir)) {
    cfg$met_dir <- path.expand(cfg$met_dir)
  }

  cfg
}

verify_load_package <- function(pkg_root) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(pkg_root, quiet = TRUE)
    return(invisible("pkgload"))
  }
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(pkg_root, quiet = TRUE)
    return(invisible("devtools"))
  }
  if (requireNamespace("disperseR", quietly = TRUE)) {
    library(disperseR)
    warning("Loaded installed disperseR package; may not match local repo.", call. = FALSE)
    return(invisible("library"))
  }
  stop("Install pkgload or devtools, or install disperseR, then retry.", call. = FALSE)
}

