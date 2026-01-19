#' Download a file with validation
#'
#' @description Downloads a file and checks that it exists and is non-empty.
#' Zip files are unzipped to `dir`.
#'
#' @param url URL to download.
#' @param file Destination file path.
#' @param dir Directory for unzip output (if applicable).
#' @return Invisibly returns TRUE on success.
#' @export
#'
# Helper function for downloading files with error handling
download_file <- function(url, file, dir) {
  # Set a longer timeout for large downloads (CRAN recommendation)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(300, old_timeout))  # At least 5 minutes

  # Capture warnings but continue execution
  download_warnings <- NULL
  result <- tryCatch(
    withCallingHandlers({
      download_status <- utils::download.file(url = url, destfile = file, mode = "wb")
      if (download_status != 0) {
        stop("download.file() returned non-zero exit status: ", download_status)
      }
    },
    warning = function(w) {
      download_warnings <<- c(download_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(cond) {
      if (file.exists(file)) {
        unlink(file, force = TRUE)
      }
      stop("Failed to download '", basename(file), "' from ", url, ".\n",
           "Error: ", cond$message, "\n",
           "Please check your network connection and try again.",
           call. = FALSE)
    }
  )
  
  # Report warnings if any occurred
  if (length(download_warnings) > 0) {
    warning("Download warnings: ", paste(download_warnings, collapse = "; "),
            call. = FALSE)
  }
  
  # Post-download validation (always runs, even after warnings)
  if (!file.exists(file)) {
    stop("Download completed but file '", file, "' does not exist.\n",
         "URL: ", url,
         call. = FALSE)
  }
  if (file.info(file)$size == 0) {
    unlink(file, force = TRUE)
    stop("Downloaded file '", file, "' is empty (0 bytes).\n",
         "URL: ", url,
         call. = FALSE)
  }
  
  # Unzip if applicable
  if (substr(file, nchar(file) - 3 + 1, nchar(file)) == "zip") {
    utils::unzip(file, exdir = dir)
  }
  
  return(invisible(TRUE))
}

#' Download and prepare data for disperseR analysis
#'
#' @description Downloads and prepares data required for disperseR analysis.
#' This includes crosswalk data, ZCTA shapefiles, planetary boundary layer
#' height data, and meteorological files.
#'
#' @param data Character. Type of data to retrieve. Options: "all", "crosswalk",
#'   "zctashapefile", "zctashapefileSF", "pblheight", "metfiles", "zcta_dataset".
#' @param start.year Character. Starting year for metfiles download (format: "YYYY").
#' @param end.year Character. Ending year for metfiles download (format: "YYYY").
#' @param start.month Character. Starting month for metfiles download (format: "MM").
#' @param end.month Character. Ending month for metfiles download (format: "MM").
#'
#' @return Depends on data type (results are also cached for use by other
#'   disperseR functions):
#'   \itemize{
#'     \item crosswalk: Returns crosswalk data.table
#'     \item zctashapefile/zctashapefileSF: Returns sf object with ZCTA polygons
#'     \item pblheight: Returns SpatRaster with planetary boundary layer heights
#'     \item metfiles: Downloads files, returns NULL invisibly
#'     \item all: Downloads all data, caches results, and returns a named list
#'   }
#'
#' @details
#' This function uses modern spatial packages (sf, terra) instead of the
#' retired rgdal/rgeos packages. Return types are sf objects for vector
#' data and terra SpatRaster for raster data.
#'
#' @examples
#' \dontrun{
#' # Create directories first
#' create_dirs(location = tempdir())
#'
#' # Get all data for a specific time period
#' get_data(
#'   data = "all",
#'   start.year = "2005",
#'   start.month = "01",
#'   end.year = "2005",
#'   end.month = "03"
#' )
#'
#' # Get only crosswalk data
#' crosswalk <- get_data(data = "crosswalk")
#' }
#'
#' @export get_data
#' @importFrom sf st_read st_transform st_crs
#' @importFrom terra rast crs
get_data <- function(data,
                     start.year = NULL,
                     start.month = NULL,
                     end.year = NULL,
                     end.month = NULL) {

  # Validate data parameter

  valid_data <- c("all", "crosswalk", "zctashapefile", "zctashapefileSF",
                  "pblheight", "metfiles", "zcta_dataset")
  if (!data %in% valid_data) {
    stop("Unknown data type: '", data, "'. Valid options are: ",
         paste(valid_data, collapse = ", "), call. = FALSE)
  }

  # Store parameters
  startyear <- start.year
  startmonth <- start.month
  endyear <- end.year
  endmonth <- end.month

  # Standard projection for disperseR (North American Albers Equal Area Conic)
  p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

  # Validate cached directory variables for data types that require them
  dirs_needed <- c("all", "zctashapefile", "zctashapefileSF", "pblheight", "metfiles", "zcta_dataset")
  if (data %in% dirs_needed) {
    zcta_dir <- .disperseR_cache_get("zcta_dir")
    hpbl_dir <- .disperseR_cache_get("hpbl_dir")
    meteo_dir <- .disperseR_cache_get("meteo_dir")
    
    if (data %in% c("all", "zctashapefile", "zctashapefileSF", "zcta_dataset")) {
      if (is.null(zcta_dir) || !nzchar(zcta_dir)) {
        stop("zcta_dir not found. Run create_dirs() first.", call. = FALSE)
      }
    }
    if (data %in% c("all", "pblheight")) {
      if (is.null(hpbl_dir) || !nzchar(hpbl_dir)) {
        stop("hpbl_dir not found. Run create_dirs() first.", call. = FALSE)
      }
    }
    if (data %in% c("all", "metfiles")) {
      if (is.null(meteo_dir) || !nzchar(meteo_dir)) {
        stop("meteo_dir not found. Run create_dirs() first.", call. = FALSE)
      }
    }
  }

  # ==========================================================================
  # ALL DATA
  # ==========================================================================
  if (data == "all") {

    # --- Crosswalk ---
    message("Loading crosswalk data from disperseR...")
    crosswalk <- disperseR::crosswalk
    .disperseR_cache_set("crosswalk", crosswalk)
    message("  Cached as 'crosswalk'")

    # --- PP.units.monthly1995_2017 ---
    message("Loading PP.units.monthly1995_2017 data from disperseR...")
    PP.units.monthly1995_2017 <- disperseR::PP.units.monthly1995_2017
    .disperseR_cache_set("PP.units.monthly1995_2017", PP.units.monthly1995_2017)
    message("  Cached as 'PP.units.monthly1995_2017'")

    # --- Zip code coordinates ---
    message("Loading zipcodecoordinate data from disperseR...")
    zipcodecoordinate <- disperseR::zipcodecoordinate
    .disperseR_cache_set("zipcodecoordinate", zipcodecoordinate)
    message("  Cached as 'zipcodecoordinate'")

    # --- ZCTA shapefile ---
    message("Downloading ZCTA shapefile...")
    directory <- zcta_dir
    file <- file.path(directory, 'cb_2017_us_zcta510_500k.zip')
    url <- 'https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip'

    if (!file.exists(file)) {
      message("  Downloading ZCTA shapefile (this may take a moment)...")
      download_file(url, file, directory)
    } else {
      message("  File already exists, skipping download.")
    }

    if (!file.exists(file)) {
      stop("ZCTA zip file '", file, "' does not exist after download attempt. ",
           "Please check your network connection and try again.",
           call. = FALSE)
    }

    zcta_shp <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_shp)) {
      message("  Unzipping ZCTA file...")
      utils::unzip(file, exdir = zcta_dir)
    }

    if (!file.exists(zcta_shp)) {
      stop("ZCTA shapefile '", zcta_shp, "' does not exist after unzip. ",
           "The zip file may be corrupted. Please delete '", file, "' and try again.",
           call. = FALSE)
    }

    # Read and transform using sf (replaces raster::shapefile + sp::spTransform)
    message("  Processing shapefile with sf...")
    zcta <- sf::st_read(zcta_shp, quiet = TRUE)
    zcta_trans <- sf::st_transform(zcta, crs = p4s)
    message("  Preprocessing complete")
    .disperseR_cache_set("zcta", zcta_trans)
    message("  Cached as 'zcta'")

    # --- Planetary boundary layer height ---
    message("Downloading planetary boundary layer data...")
    directory <- hpbl_dir
    file <- file.path(directory, 'hpbl.mon.mean.nc')
    url <- 'https://psl.noaa.gov/repository/entry/get/hpbl.mon.mean.nc?entryid=synth%3Ae570c8f9-ec09-4e89-93b4-babd5651e7a9%3AL05BUlIvTW9udGhsaWVzL21vbm9sZXZlbC9ocGJsLm1vbi5tZWFuLm5j'

    if (!file.exists(file)) {
      message("  Downloading PBL data (this may take a moment)...")
      download_file(url, file, directory)
    } else {
      message("  File already exists, skipping download.")
    }

    if (!file.exists(file)) {
      stop("PBL data file '", file, "' does not exist after download attempt. ",
           "Please check your network connection and try again.",
           call. = FALSE)
    }

    # Read using terra (replaces raster::brick)
    old_tz <- Sys.getenv("TZ")
    Sys.setenv(TZ = "UTC")
    on.exit(Sys.setenv(TZ = old_tz), add = TRUE)
    message("  Processing PBL data with terra...")
    hpbl_rasterin <- terra::rast(file, subds = "hpbl")
    
    # Set CRS to fix dataset error
    terra::crs(hpbl_rasterin) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    message("  Preprocessing complete")
    .disperseR_cache_set("pblheight", hpbl_rasterin)
    message("  Cached as 'pblheight'")

    # --- Meteorological files ---
    message("Downloading meteorological files...")
    if (is.null(startyear) || is.null(startmonth) || 
        is.null(endyear) || is.null(endmonth)) {
      stop("Please specify start.year, start.month, end.year, and end.month for metfiles.")
    }
    
    inputdates <- c(
      paste(startyear, startmonth, "01", sep = "/"),
      paste(endyear, endmonth, "01", sep = "/")
    )
    start <- as.Date(inputdates[1])
    end <- as.Date(inputdates[2])

    vectorfiles <- NULL
    i <- 1
    while (start <= end) {
      string <- paste0("RP", format(start, "%Y%m"), ".gbl")
      vectorfiles[i] <- string
      lubridate::month(start) <- lubridate::month(start) + 1
      i <- i + 1
    }
    metfiles <- vectorfiles[!(vectorfiles %in% list.files(meteo_dir))]

    if (length(metfiles) > 0) {
      message("  Downloading: ", paste(metfiles, collapse = ", "))
      disperseR::get_met_reanalysis(files = metfiles, path_met_files = meteo_dir)
      
      # Validate downloads completed
      still_missing <- metfiles[!metfiles %in% list.files(meteo_dir)]
      if (length(still_missing) > 0) {
        stop("Meteorology download failed. Missing files: ",
             paste(still_missing, collapse = ", "),
             call. = FALSE)
      }
      message("  All meteorology files downloaded successfully.")
    } else {
      message("  All requested files already available.")
    }

    # --- ZCTA dataset (merged with crosswalk) ---
    message("Creating ZCTA dataset for graphs...")
    directory <- zcta_dir
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
    zcta <- merge(
      zcta,
      as.data.frame(disperseR::crosswalk),
      by = "ZCTA",
      all = FALSE
    )
    message("  Preprocessing complete")
    .disperseR_cache_set("zcta_dataset", zcta)
    message("  Cached as 'zcta_dataset'")

    return(invisible(list(
      crosswalk = crosswalk,
      PP.units.monthly1995_2017 = PP.units.monthly1995_2017,
      zipcodecoordinate = zipcodecoordinate,
      zcta = zcta_trans,
      pblheight = hpbl_rasterin,
      zcta_dataset = zcta
    )))
  }

  # ==========================================================================
  # INDIVIDUAL DATA TYPES
  # ==========================================================================

  if (data == "crosswalk") {
    message("Loading crosswalk data from disperseR...")
    crosswalk <- disperseR::crosswalk
    message("  Crosswalk data loaded")
    .disperseR_cache_set("crosswalk", crosswalk)
    return(crosswalk)
  }

  # --- ZCTA shapefile setup ---
  if (data == "zctashapefile" || data == "zcta_dataset" || data == "zctashapefileSF") {
    directory <- zcta_dir
    file <- file.path(directory, 'cb_2017_us_zcta510_500k.zip')
    url <- 'https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip'
  }

  # --- PBL height setup ---
  if (data == "pblheight") {
    directory <- hpbl_dir
    file <- file.path(directory, 'hpbl.mon.mean.nc')
    url <- 'https://psl.noaa.gov/repository/entry/get/hpbl.mon.mean.nc?entryid=synth%3Ae570c8f9-ec09-4e89-93b4-babd5651e7a9%3AL05BUlIvTW9udGhsaWVzL21vbm9sZXZlbC9ocGJsLm1vbi5tZWFuLm5j'
  }

  # --- Meteorological files ---
  if (data == "metfiles") {
    if (is.null(startyear) || is.null(startmonth) || 
        is.null(endyear) || is.null(endmonth)) {
      stop("Please specify start.year, start.month, end.year, and end.month.")
    }
    inputdates <- c(
      paste(startyear, startmonth, "01", sep = "/"),
      paste(endyear, endmonth, "01", sep = "/")
    )
    start <- as.Date(inputdates[1])
    end <- as.Date(inputdates[2])

    vectorfiles <- NULL
    i <- 1
    while (start <= end) {
      string <- paste0("RP", format(start, "%Y%m"), ".gbl")
      vectorfiles[i] <- string
      lubridate::month(start) <- lubridate::month(start) + 1
      i <- i + 1
    }
    metfiles <- vectorfiles[!(vectorfiles %in% list.files(meteo_dir))]

    if (length(metfiles) > 0) {
      message("Downloading: ", paste(metfiles, collapse = ", "))
      disperseR::get_met_reanalysis(files = metfiles, path_met_files = meteo_dir)
      
      # Validate downloads completed
      still_missing <- metfiles[!metfiles %in% list.files(meteo_dir)]
      if (length(still_missing) > 0) {
        stop("Meteorology download failed. Missing files: ",
             paste(still_missing, collapse = ", "),
             call. = FALSE)
      }
      message("All meteorology files downloaded successfully.")
    } else {
      message("All requested files already available.")
    }
    return(invisible(NULL))
  }

  # --- Download files if needed ---
  if (data != "metfiles" && data != "all") {
    if (!file.exists(file)) {
      message("Downloading data (this may take a moment)...")
      download_file(url, file, directory)
    } else {
      message("File already exists, skipping download.")
    }
    if (!file.exists(file)) {
      stop("Required file '", basename(file), "' does not exist after download attempt. ",
           "Please check your network connection and try again.",
           call. = FALSE)
    }
  }

  # --- Process and return ZCTA dataset ---
  if (data == "zcta_dataset") {
    message("Processing ZCTA dataset...")
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_path)) {
      if (!file.exists(file)) {
        stop("ZCTA zip file '", file, "' does not exist. Cannot extract shapefile.",
             call. = FALSE)
      }
      utils::unzip(file, exdir = directory)
    }
    if (!file.exists(zcta_path)) {
      stop("ZCTA shapefile '", zcta_path, "' does not exist after unzip. ",
           "The zip file may be corrupted. Please delete '", file, "' and try again.",
           call. = FALSE)
    }
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
    zcta <- merge(
      zcta,
      as.data.frame(disperseR::crosswalk),
      by = "ZCTA",
      all = FALSE
    )
    message("Preprocessing complete")
    .disperseR_cache_set("zcta_dataset", zcta)
    return(zcta)
  }

  # --- Process and return ZCTA shapefile (as sf object) ---
  if (data == "zctashapefile" || data == "zctashapefileSF") {
    message("Processing ZCTA shapefile...")
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_path)) {
      if (!file.exists(file)) {
        stop("ZCTA zip file '", file, "' does not exist. Cannot extract shapefile.",
             call. = FALSE)
      }
      utils::unzip(file, exdir = directory)
    }
    if (!file.exists(zcta_path)) {
      stop("ZCTA shapefile '", zcta_path, "' does not exist after unzip. ",
           "The zip file may be corrupted. Please delete '", file, "' and try again.",
           call. = FALSE)
    }
    # Use sf instead of raster::shapefile + sp::spTransform
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    zcta_trans <- sf::st_transform(zcta, crs = p4s)
    message("Preprocessing complete")
    .disperseR_cache_set("zcta", zcta_trans)
    return(zcta_trans)
  }

  # --- Process and return PBL height raster ---
  if (data == "pblheight") {
    if (!file.exists(file)) {
      stop("PBL data file '", file, "' does not exist. ",
           "Please download it first using get_data(data = 'pblheight').",
           call. = FALSE)
    }
    old_tz <- Sys.getenv("TZ")
    Sys.setenv(TZ = "UTC")
    on.exit(Sys.setenv(TZ = old_tz), add = TRUE)
    message("Processing PBL height data...")
    # Use terra instead of raster::brick
    hpbl_rasterin <- terra::rast(file, subds = "hpbl")
    terra::crs(hpbl_rasterin) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    message("Preprocessing complete")
    .disperseR_cache_set("pblheight", hpbl_rasterin)
    return(hpbl_rasterin)
  }
}
