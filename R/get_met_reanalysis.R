#' Adapted from splitr's get_met_reanalysis
#' Get reanalysis meteorology data files
#' @description This function downloads reanalysis
#' meteorology data files from the NOAA server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of \code{'RP[YYYY][MM].gbl'}) or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for
#' the reanalysis files.
#' @param years a vector list of years for which
#' reanalysis files are to be obtained.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @param base_url Base URL for reanalysis archive. Defaults to NOAA HTTPS.
#'   Can be overridden for mirror sites or local servers.
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{downloaded}: character vector of successfully downloaded file paths
#'     \item \code{failed}: character vector of filenames that failed to download
#'   }
#' @export get_met_reanalysis
get_met_reanalysis <- function(files = NULL,
  years = NULL,
  path_met_files,
  base_url = NULL) {

  # default to HTTPS — way more reliable than FTP on most networks
  if (is.null(base_url)) {
    base_url <- "https://www.ready.noaa.gov/data/archives/reanalysis/"
  }
  # make sure the URL ends with /
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  if (missing(path_met_files) || is.null(path_met_files) || !nzchar(path_met_files)) {
    stop("path_met_files must be a valid directory path.", call. = FALSE)
  }
  path_met_files <- path.expand(path_met_files)
  if (!dir.exists(path_met_files)) {
    dir.create(path_met_files, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path_met_files)) {
    stop("path_met_files does not exist and could not be created: ",
         path_met_files, call. = FALSE)
  }
  
  # bump timeout — met files are ~120MB each
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))

  # download a single met file with up to 3 retries
  .download_met_file <- function(url, destfile) {
    max_attempts <- 3L

    for (attempt in seq_len(max_attempts)) {
      # nuke leftover partial file before retrying
      if (attempt > 1L && file.exists(destfile)) {
        unlink(destfile, force = TRUE)
      }
      if (attempt > 1L) {
        wait <- 2^(attempt - 1L)
        message("  Retry ", attempt, "/", max_attempts, " for ",
                basename(destfile), " after ", wait, "s...")
        Sys.sleep(wait)
      }

      download_ok <- FALSE
      last_error <- NULL

      if (.Platform$OS.type == "windows" &&
          requireNamespace("downloader", quietly = TRUE)) {
        download_ok <- tryCatch({
          downloader::download(
            url = url,
            destfile = destfile,
            method = "auto",
            quiet = FALSE,
            mode = "wb",
            cacheOK = FALSE
          )
          TRUE
        }, error = function(e) {
          last_error <<- e
          FALSE
        })
      }

      if (!download_ok) {
        download_methods <- "auto"
        if (.Platform$OS.type == "windows") {
          download_methods <- character(0)
          if (capabilities("libcurl")) {
            download_methods <- c(download_methods, "libcurl")
          }
          download_methods <- c(download_methods, "wininet", "auto")
          download_methods <- unique(download_methods)
        }

        for (method in download_methods) {
          status <- tryCatch(
            utils::download.file(
              url = url,
              destfile = destfile,
              method = method,
              quiet = FALSE,
              mode = "wb",
              cacheOK = FALSE
            ),
            error = function(e) e
          )
          if (inherits(status, "error")) {
            last_error <- status
            next
          }
          if (is.numeric(status) && status == 0) {
            download_ok <- TRUE
            break
          }
          last_error <- status
        }
      }

      # make sure we got a real file, not an empty stub
      if (download_ok && file.exists(destfile)) {
        fsize <- file.info(destfile)$size
        if (is.na(fsize) || fsize == 0) {
          if (attempt < max_attempts) {
            message("  Downloaded file is empty: ", basename(destfile), ", retrying...")
            unlink(destfile, force = TRUE)
            next
          }
          warning("Downloaded file is empty: ", basename(destfile), call. = FALSE)
          unlink(destfile, force = TRUE)
          return(FALSE)
        }
        return(TRUE)
      }

      # didn't work — retry or give up
      if (attempt < max_attempts) {
        msg <- if (!is.null(last_error)) last_error$message else "unknown error"
        message("  Download attempt ", attempt, " failed for ",
                basename(destfile), ": ", msg)
        next
      }

      if (!is.null(last_error)) {
        warning("Download failed for ", basename(destfile), ": ", last_error$message,
                call. = FALSE)
      }
    }
    return(FALSE)
  }
  
  downloaded <- character(0)
  failed <- character(0)

  # grab specific files by name
  if (!is.null(files)) {
    for (i in seq_along(files)) {
      url <- paste0(base_url, files[i])
      destfile <- file.path(path_met_files, files[i])
      
      if (.download_met_file(url, destfile)) {
        downloaded <- c(downloaded, destfile)
      } else {
        failed <- c(failed, files[i])
      }
    }
  }

  # bulk download by year (all 12 months)
  if (!is.null(years)) {
    for (i in seq_along(years)) {
      for (j in seq_len(12)) {
        filename <- paste0("RP", years[i], formatC(j, width = 2, format = "d", flag = "0"), ".gbl")
        url <- paste0(base_url, filename)
        destfile <- file.path(path_met_files, filename)
        
        if (.download_met_file(url, destfile)) {
          downloaded <- c(downloaded, destfile)
        } else {
          failed <- c(failed, filename)
        }
      }
    }
  }
  
  # let the user know if anything failed
  if (length(failed) > 0) {
    warning(length(failed), " file(s) failed to download: ",
            paste(failed, collapse = ", "), call. = FALSE)
  }
  
  list(downloaded = downloaded, failed = failed)
}
