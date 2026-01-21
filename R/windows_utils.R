#' Windows-specific utilities for HYSPLIT execution
#'
#' @description Internal utilities for handling Windows-specific issues with
#' HYSPLIT execution, including zombie process cleanup and file locking.
#'
#' @name windows_utils
#' @keywords internal
NULL

#' Kill orphaned HYSPLIT processes
#'
#' @description Finds and terminates any running HYSPLIT processes (hyts_std,
#' hycs_std, parhplot) that may be left over from crashed runs. On Windows,
#' these can hold file locks and prevent subsequent runs.
#'
#' @param verbose Logical. Print information about killed processes. Default TRUE.
#'
#' @return Invisibly returns the number of processes killed.
#'
#' @details
#' On Windows, if an R worker crashes while HYSPLIT is running, the HYSPLIT

#' process may become orphaned and continue running, holding locks on output
#' files. This function identifies and terminates such processes.
#'
#' On Unix/macOS, orphaned processes are less common due to process group
#' handling, but this function works cross-platform.
#'
#' @examples
#' \dontrun{
#' # Clean up before starting a batch run
#' cleanup_hysplit_zombies()
#' }
#'
#' @export
cleanup_hysplit_zombies <- function(verbose = TRUE) {
  killed <- 0L
  
  if (.Platform$OS.type == "windows") {
    # Windows: use tasklist/taskkill
    hysplit_names <- c("hyts_std.exe", "hycs_std.exe", "parhplot.exe")
    
    for (proc_name in hysplit_names) {
      # Check if process exists
      check_cmd <- sprintf('tasklist /FI "IMAGENAME eq %s" 2>NUL', proc_name)
      result <- tryCatch(
        system(check_cmd, intern = TRUE, ignore.stderr = TRUE),
        error = function(e) character(0)
      )
      
      # tasklist returns header + processes; if only header, no match
      if (length(result) > 2 || any(grepl(proc_name, result, ignore.case = TRUE))) {
        if (verbose) message("Found running ", proc_name, " - terminating...")
        kill_cmd <- sprintf('taskkill /F /IM %s 2>NUL', proc_name)
        tryCatch(
          system(kill_cmd, intern = TRUE, ignore.stderr = TRUE),
          error = function(e) NULL
        )
        killed <- killed + 1L
      }
    }
  } else {
    # Unix/macOS: use pkill
    hysplit_names <- c("hyts_std", "hycs_std", "parhplot")
    
    for (proc_name in hysplit_names) {
      # Check if process exists
      check_cmd <- sprintf("pgrep -x %s 2>/dev/null", proc_name)
      result <- tryCatch(
        system(check_cmd, intern = TRUE, ignore.stderr = TRUE),
        error = function(e) character(0)
      )
      
      if (length(result) > 0 && nzchar(result[1])) {
        if (verbose) message("Found running ", proc_name, " (PID: ", result[1], ") - terminating...")
        kill_cmd <- sprintf("pkill -9 -x %s 2>/dev/null", proc_name)
        tryCatch(
          system(kill_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE),
          error = function(e) NULL
        )
        killed <- killed + 1L
      }
    }
  }
  

  if (verbose && killed > 0) {
    message("Cleaned up ", killed, " orphaned HYSPLIT process(es)")
  }
  
  invisible(killed)
}


#' Read file with retry logic for Windows file locking
#'
#' @description Attempts to read a file with retries to handle Windows file
#' locking delays. Useful when reading HYSPLIT output immediately after
#' the process completes.
#'
#' @param path Character. Path to file.
#' @param read_fn Function. The read function to use (e.g., readLines, fst::read_fst).
#' @param max_retries Integer. Maximum retry attempts. Default 5.
#' @param delay_ms Integer. Delay between retries in milliseconds. Default 200.
#' @param ... Additional arguments passed to read_fn.
#'
#' @return Result of read_fn(path, ...) on success, or stops with error.
#'
#' @keywords internal
safe_file_read <- function(path, read_fn = readLines, max_retries = 5L,
                           delay_ms = 200L, ...) {
  last_error <- NULL
  

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch(
      read_fn(path, ...),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
    
    # Check if it's a permission/locking error worth retrying
    if (!is.null(last_error)) {
      err_msg <- tolower(conditionMessage(last_error))
      is_lock_error <- grepl("permission|denied|locked|access|sharing", err_msg)
      
      if (!is_lock_error && attempt == 1) {
        # Not a locking error, fail immediately
        stop(last_error)
      }
    }
    
    if (attempt < max_retries) {
      Sys.sleep(delay_ms / 1000)
    }
  }
  
  stop("Failed to read '", path, "' after ", max_retries, " attempts: ",
       conditionMessage(last_error), call. = FALSE)
}


#' Check for potential antivirus interference
#'
#' @description Warns users about potential antivirus/EDR interference when
#' running many HYSPLIT processes rapidly. Called before large batch runs.
#'
#' @param n_runs Integer. Number of HYSPLIT runs planned.
#' @param threshold Integer. Warn if n_runs exceeds this. Default 100.
#'
#' @return Invisibly returns TRUE if warning was issued.
#'
#' @keywords internal
warn_av_interference <- function(n_runs, threshold = 100L) {
  if (.Platform$OS.type != "windows") {
    return(invisible(FALSE))
  }
  
  if (n_runs > threshold) {
    warning(
      "Planning ", n_runs, " HYSPLIT runs. ",
      "Windows Defender or corporate antivirus may flag rapid executable launches. ",
      "If runs fail unexpectedly, try: (1) adding the HYSPLIT binary folder to AV exclusions, ",
      "(2) reducing mc.cores, or (3) adding delays between batches.",
      call. = FALSE
    )
    return(invisible(TRUE))
  }
  
  invisible(FALSE)
}
