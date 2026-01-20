#' Validate disperseR pipeline outputs
#'
#' @description Scans disperseR output directories and summarizes the number of
#' dispersion runs, trajectory points, linked outputs, and exposure tables.
#' Intended for quick sanity checks after a pipeline run.
#'
#' @param dirs Optional list returned by `create_dirs()`.
#' @param hysp_dir Directory containing `hyspdisp_*.fst` outputs.
#' @param ziplink_dir Directory containing `ziplinks_*.fst` outputs.
#' @param exp_dir Directory containing exposure outputs (e.g., `zips_exposures_*`).
#' @param compute_unique_zips Logical. If TRUE, reads ZIP link files and returns
#'   a unique ZIP count. Set FALSE to avoid reading large files.
#' @param verbose Logical. Print a short summary to the console.
#'
#' @return A named list with counts and detected issues.
#' @export
validate_pipeline <- function(dirs = NULL,
                              hysp_dir = NULL,
                              ziplink_dir = NULL,
                              exp_dir = NULL,
                              compute_unique_zips = TRUE,
                              verbose = TRUE) {
  resolve_dir <- function(key, explicit) {
    if (!is.null(explicit)) {
      return(path.expand(explicit))
    }
    if (!is.null(dirs) && !is.null(dirs[[key]])) {
      return(path.expand(dirs[[key]]))
    }
    cached <- .disperseR_cache_get(key)
    if (!is.null(cached)) {
      return(path.expand(cached))
    }
    NULL
  }

  safe_fst_nrows <- function(path) {
    meta <- tryCatch(fst::metadata_fst(path), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$nrOfRows)) {
      return(as.integer(meta$nrOfRows))
    }
    data <- tryCatch(fst::read.fst(path, as.data.table = FALSE), error = function(e) NULL)
    if (is.null(data)) {
      return(NA_integer_)
    }
    nrow(data)
  }

  hysp_dir <- resolve_dir("hysp_dir", hysp_dir)
  ziplink_dir <- resolve_dir("ziplink_dir", ziplink_dir)
  exp_dir <- resolve_dir("exp_dir", exp_dir)

  issues <- character(0)

  hysp_files <- character(0)
  if (is.null(hysp_dir) || !dir.exists(hysp_dir)) {
    issues <- c(issues, "hysp_dir missing or does not exist")
  } else {
    hysp_files <- list.files(
      hysp_dir,
      pattern = "^hyspdisp_.*\\.fst$",
      recursive = TRUE,
      full.names = TRUE
    )
  }

  hysp_rows <- if (length(hysp_files) > 0) {
    sum(vapply(hysp_files, safe_fst_nrows, integer(1)), na.rm = TRUE)
  } else {
    0L
  }

  ziplink_files <- character(0)
  if (is.null(ziplink_dir) || !dir.exists(ziplink_dir)) {
    issues <- c(issues, "ziplink_dir missing or does not exist")
  } else {
    ziplink_files <- list.files(
      ziplink_dir,
      pattern = "^ziplinks_.*\\.fst$",
      recursive = FALSE,
      full.names = TRUE
    )
  }

  ziplink_rows <- if (length(ziplink_files) > 0) {
    sum(vapply(ziplink_files, safe_fst_nrows, integer(1)), na.rm = TRUE)
  } else {
    0L
  }

  unique_zip_count <- NA_integer_
  if (compute_unique_zips) {
    if (length(ziplink_files) == 0) {
      unique_zip_count <- 0L
    } else {
      unique_zips <- character(0)
      for (file in ziplink_files) {
        zip_data <- tryCatch(
          fst::read.fst(file, as.data.table = TRUE),
          error = function(e) NULL
        )
        if (!is.null(zip_data) && "ZIP" %in% names(zip_data)) {
          unique_zips <- unique(c(unique_zips, as.character(zip_data$ZIP)))
        }
      }
      unique_zip_count <- length(unique_zips)
    }
  }

  exp_files <- character(0)
  if (is.null(exp_dir) || !dir.exists(exp_dir)) {
    issues <- c(issues, "exp_dir missing or does not exist")
  } else {
    exp_files <- list.files(
      exp_dir,
      pattern = "_exposures_.*\\.fst$",
      recursive = FALSE,
      full.names = TRUE
    )
  }

  exp_rows <- if (length(exp_files) > 0) {
    sum(vapply(exp_files, safe_fst_nrows, integer(1)), na.rm = TRUE)
  } else {
    0L
  }

  result <- list(
    runs = list(files = length(hysp_files), trajectory_points = hysp_rows),
    ziplinks = list(
      files = length(ziplink_files),
      rows = ziplink_rows,
      unique_zips = unique_zip_count
    ),
    exposures = list(files = length(exp_files), rows = exp_rows),
    issues = issues
  )

  if (isTRUE(verbose)) {
    message("Runs: ", result$runs$files,
            " files, ", result$runs$trajectory_points, " points")
    message("ZIP links: ", result$ziplinks$files,
            " files, ", result$ziplinks$rows, " rows")
    if (!is.na(result$ziplinks$unique_zips)) {
      message("Unique ZIPs: ", result$ziplinks$unique_zips)
    }
    message("Exposure outputs: ", result$exposures$files,
            " files, ", result$exposures$rows, " rows")
    if (length(result$issues) > 0) {
      message("Issues: ", paste(result$issues, collapse = "; "))
    }
  }

  result
}
