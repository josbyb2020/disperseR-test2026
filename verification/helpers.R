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

verify_stop_on_failures <- function(context = NULL) {
  failures <- verify_state$results[verify_state$results$status == "FAIL", , drop = FALSE]
  if (nrow(failures) == 0) {
    return(invisible(TRUE))
  }
  if (is.null(context) || !nzchar(context)) {
    context <- "verification"
  }
  detail <- paste0(
    failures$step,
    ": ",
    ifelse(nzchar(failures$detail), failures$detail, "unknown error")
  )
  stop(
    "One or more ", context, " step(s) failed:\n",
    paste(" -", detail, collapse = "\n"),
    call. = FALSE
  )
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
    env <- new.env(parent = baseenv())
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

verify_compare_grouped <- function(legacy_dt,
                                   fast_dt,
                                   key_cols,
                                   value_col = "N",
                                   tol_abs = 1e-8,
                                   tol_rel = 1e-8) {
  legacy <- data.table::as.data.table(legacy_dt)
  fast <- data.table::as.data.table(fast_dt)

  need_cols <- unique(c(key_cols, value_col))
  if (!all(need_cols %in% names(legacy))) {
    stop("legacy_dt is missing required columns: ",
         paste(setdiff(need_cols, names(legacy)), collapse = ", "),
         call. = FALSE)
  }
  if (!all(need_cols %in% names(fast))) {
    stop("fast_dt is missing required columns: ",
         paste(setdiff(need_cols, names(fast)), collapse = ", "),
         call. = FALSE)
  }

  legacy <- legacy[, ..need_cols]
  fast <- fast[, ..need_cols]
  data.table::setnames(legacy, value_col, "N_legacy")
  data.table::setnames(fast, value_col, "N_fast")
  legacy[, in_legacy := TRUE]
  fast[, in_fast := TRUE]

  cmp <- merge(
    legacy,
    fast,
    by = key_cols,
    all = TRUE,
    sort = TRUE
  )

  key_rows_missing <- is.na(cmp$in_legacy) | is.na(cmp$in_fast)
  keys_equal <- !any(key_rows_missing)

  cmp[is.na(N_legacy), N_legacy := 0]
  cmp[is.na(N_fast), N_fast := 0]
  cmp[, abs_diff := abs(N_legacy - N_fast)]
  cmp[, rel_diff := abs_diff / pmax(abs(N_legacy), abs(N_fast), 1)]
  cmp[, within_tol := abs_diff <= tol_abs | rel_diff <= tol_rel]

  values_equal <- all(cmp$within_tol)
  bad <- cmp[within_tol == FALSE]

  max_abs <- if (nrow(cmp) == 0) 0 else max(cmp$abs_diff, na.rm = TRUE)
  max_rel <- if (nrow(cmp) == 0) 0 else max(cmp$rel_diff, na.rm = TRUE)

  list(
    keys_equal = isTRUE(keys_equal),
    values_equal = isTRUE(values_equal),
    parity_ok = isTRUE(keys_equal && values_equal),
    compared_rows = nrow(cmp),
    n_key_only_rows = sum(key_rows_missing),
    n_diff_rows = nrow(bad),
    max_abs_diff = as.numeric(max_abs),
    max_rel_diff = as.numeric(max_rel),
    worst_diff = if (nrow(bad) > 0) bad[which.max(abs_diff)] else NULL
  )
}
