#' Adapted from SplitR's get_met_reanalysis
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

  # Use HTTPS by default (more reliable than FTP on corporate/academic networks)
  if (is.null(base_url)) {
    base_url <- "https://www.ready.noaa.gov/data/archives/reanalysis/"
  }
  # Ensure trailing slash
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }
  
  # Internal helper for validated downloads
  .download_met_file <- function(url, destfile) {
    download_ok <- FALSE
    
    result <- tryCatch({
      if (.Platform$OS.type == "windows" &&
          requireNamespace("downloader", quietly = TRUE)) {
        downloader::download(
          url = url,
          destfile = destfile,
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        )
      } else {
        utils::download.file(
          url = url,
          destfile = destfile,
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        )
      }
      download_ok <- TRUE
    }, error = function(e) {
      warning("Download failed for ", basename(destfile), ": ", e$message,
              call. = FALSE)
    })
    
    # Validate file exists and has content
    if (download_ok && file.exists(destfile)) {
      fsize <- file.info(destfile)$size
      if (is.na(fsize) || fsize == 0) {
        warning("Downloaded file is empty: ", basename(destfile), call. = FALSE)
        unlink(destfile, force = TRUE)
        return(FALSE)
      }
      return(TRUE)
    }
    return(FALSE)
  }
  
  downloaded <- character(0)
  failed <- character(0)

  # Download list of reanalysis met files by name
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

  # Download one or more years of reanalysis met files
  if (!is.null(years)) {
    for (i in seq_along(years)) {
      for (j in 1:12) {
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
  
  # Report summary
  if (length(failed) > 0) {
    warning(length(failed), " file(s) failed to download: ",
            paste(failed, collapse = ", "), call. = FALSE)
  }
  
  list(downloaded = downloaded, failed = failed)
}
