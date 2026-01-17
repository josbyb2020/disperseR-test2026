#' Download and prepare data for disperseR analysis
#'
#' @description Downloads and prepares data required for disperseR analysis.
#' This includes crosswalk data, ZCTA shapefiles, planetary boundary layer 
#' height data, and meteorological files.
#'
#' @param data Character. Type of data to retrieve. Options: "all", "crosswalk",
#'   "zctashapefile", "zctashapefileSF", "pblheight", "metfiles", "zcta_dataset"
#' @param start.year Character. Starting year for metfiles download (format: "YYYY").
#' @param end.year Character. Ending year for metfiles download (format: "YYYY").
#' @param start.month Character. Starting month for metfiles download (format: "MM").
#' @param end.month Character. Ending month for metfiles download (format: "MM").
#'
#' @return Depends on data type:
#'   \itemize{
#'     \item crosswalk: Returns crosswalk data.table
#'     \item zctashapefile/zctashapefileSF: Returns sf object with ZCTA polygons
#'     \item pblheight: Returns SpatRaster with planetary boundary layer heights
#'     \item metfiles: Downloads files, returns NULL invisibly
#'     \item all: Downloads all data, assigns to global environment
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
#' @export download_file
#' @importFrom sf st_read st_transform st_crs
#' @importFrom terra rast crs


# Helper function for downloading files with error handling
download_file <- function(url, file, dir) {
  out <- tryCatch({
    utils::download.file(url = url, destfile = file, mode = "wb")
    if (substr(file, nchar(file) - 3 + 1, nchar(file)) == "zip") {
      utils::unzip(file, exdir = dir)
    }
  },
  error = function(cond) {
    message("URL connection failed. You may need to try again later.")
    message("Original error: ", cond$message)
    return(NA)
  },
  warning = function(cond) {
    message("Warning: ", cond$message)
    return(NULL)
  })
  return(out)
}


get_data <- function(data,
                     start.year = NULL,
                     start.month = NULL,
                     end.year = NULL,
                     end.month = NULL) {

  # Store parameters to avoid unused argument warnings

startyear <- start.year
  startmonth <- start.month
  endyear <- end.year
  endmonth <- end.month

  # Standard projection for disperseR (North American Albers Equal Area Conic)
  p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

  # ==========================================================================
  # ALL DATA
  # ==========================================================================
  if (data == "all") {

    # --- Crosswalk ---
    message("Loading crosswalk data from disperseR...")
    crosswalk <- disperseR::crosswalk
    assign("crosswalk", crosswalk, envir = .GlobalEnv)
    message("  Assigned to 'crosswalk' variable")

    # --- PP.units.monthly1995_2017 ---
    message("Loading PP.units.monthly1995_2017 data from disperseR...")
    PP.units.monthly1995_2017 <- disperseR::PP.units.monthly1995_2017
    assign("PP.units.monthly1995_2017", PP.units.monthly1995_2017, envir = .GlobalEnv)
    message("  Assigned to 'PP.units.monthly1995_2017' variable")

    # --- Zip code coordinates ---
    message("Loading zipcodecoordinate data from disperseR...")
    zipcodecoordinate <- disperseR::zipcodecoordinate
    assign("zipcodecoordinate", zipcodecoordinate, envir = .GlobalEnv)
    message("  Assigned to 'zipcodecoordinate' variable")

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

    zcta_shp <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_shp)) {
      message("  Unzipping ZCTA file...")
      utils::unzip(file, exdir = zcta_dir)
    }

    # Read and transform using sf (replaces raster::shapefile + sp::spTransform)
    message("  Processing shapefile with sf...")
    zcta <- sf::st_read(zcta_shp, quiet = TRUE)
    zcta_trans <- sf::st_transform(zcta, crs = p4s)
    message("  Preprocessing complete")
    assign("zcta", zcta_trans, envir = .GlobalEnv)
    message("  Assigned to 'zcta' variable")

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

    # Read using terra (replaces raster::brick)
    Sys.setenv(TZ = 'UTC')
    message("  Processing PBL data with terra...")
    hpbl_rasterin <- terra::rast(file, subds = "hpbl")
    
    # Set CRS to fix dataset error
    terra::crs(hpbl_rasterin) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    message("  Preprocessing complete")
    assign("pblheight", hpbl_rasterin, envir = .GlobalEnv)
    message("  Assigned to 'pblheight' variable")

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
    } else {
      message("  All requested files already available.")
    }

    # --- ZCTA dataset (merged with crosswalk) ---
    message("Creating ZCTA dataset for graphs...")
    directory <- zcta_dir
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
    zcta <- merge(zcta, disperseR::crosswalk, by = "ZCTA", 
                  all = FALSE, allow.cartesian = TRUE)
    message("  Preprocessing complete")
    assign("zcta_dataset", zcta, envir = .GlobalEnv)
    message("  Assigned to 'zcta_dataset' variable")

    return(invisible(NULL))
  }

  # ==========================================================================
  # INDIVIDUAL DATA TYPES
  # ==========================================================================

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
  }

  # --- Process and return ZCTA dataset ---
  if (data == "zcta_dataset") {
    message("Processing ZCTA dataset...")
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_path)) {
      utils::unzip(file, exdir = directory)
    }
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    data.table::setnames(zcta, 'ZCTA5CE10', 'ZCTA')
    zcta <- merge(zcta, disperseR::crosswalk, by = "ZCTA", 
                  all = FALSE, allow.cartesian = TRUE)
    message("Preprocessing complete")
    return(zcta)
  }

  # --- Process and return ZCTA shapefile (as sf object) ---
  if (data == "zctashapefile" || data == "zctashapefileSF") {
    message("Processing ZCTA shapefile...")
    zcta_path <- file.path(directory, 'cb_2017_us_zcta510_500k.shp')
    if (!file.exists(zcta_path)) {
      utils::unzip(file, exdir = directory)
    }
    # Use sf instead of raster::shapefile + sp::spTransform
    zcta <- sf::st_read(zcta_path, quiet = TRUE)
    zcta_trans <- sf::st_transform(zcta, crs = p4s)
    message("Preprocessing complete")
    return(zcta_trans)
  }

  # --- Process and return PBL height raster ---
  if (data == "pblheight") {
    Sys.setenv(TZ = 'UTC')
    message("Processing PBL height data...")
    # Use terra instead of raster::brick
    hpbl_rasterin <- terra::rast(file, subds = "hpbl")
    terra::crs(hpbl_rasterin) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    message("Preprocessing complete")
    return(hpbl_rasterin)
  }
}
