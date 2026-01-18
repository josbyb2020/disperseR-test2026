#' Adapted from SplitR's get_met_reanalysis
#' Get reanalysis meteorology data files
#' @description This function downloads reanalysis
#' meteorology data files from the NOAA FTP server and
#' saves them to a specified folder. Files can be
#' downloaded either by specifying a list of filenames
#' (in the form of \code{'RP[YYYY][MM].gbl'}) or
#' through bulk download of a year of files.
#' @param files a vector list of exact filenames for
#' the reanalysis files.
#' @param years a vector list of years for which
#' reanalysis files are to be obtained via FTP.
#' @param path_met_files a full path should be provided
#' for the location of the meteorological data files;
#' downloaded files will be saved in this location.
#' @return Invisibly returns a character vector of successfully downloaded files.
#' @export get_met_reanalysis
get_met_reanalysis <- function(files = NULL,
  years = NULL,
  path_met_files) {

  reanalysis_dir <- "ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/"
  
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

  # Download list of reanalysis met files by name
  if (!is.null(files)) {
    for (i in seq_along(files)) {
      url <- paste0(reanalysis_dir, files[i])
      destfile <- file.path(path_met_files, files[i])
      
      if (.download_met_file(url, destfile)) {
        downloaded <- c(downloaded, destfile)
      }
    }
  }

  # Download one or more years of reanalysis met files
  if (!is.null(years)) {
    for (i in seq_along(years)) {
      for (j in 1:12) {
        filename <- paste0("RP", years[i], formatC(j, width = 2, format = "d", flag = "0"), ".gbl")
        url <- paste0(reanalysis_dir, filename)
        destfile <- file.path(path_met_files, filename)
        
        if (.download_met_file(url, destfile)) {
          downloaded <- c(downloaded, destfile)
        }
      }
    }
  }
  
  invisible(downloaded)
}
