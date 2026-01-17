#' Create impact table for single unit/month
#'
#' @description Creates a table of spatial impacts from linked data for 
#' a single unit and month, suitable for plotting.
#'
#' @param data.linked Data from disperseR::link_all_units()
#' @param data.units Unit location information from disperseR::units()
#' @param link.to Spatial scale: 'zips', 'counties', or 'grids'
#' @param zcta.dataset ZCTA spatial dataset from disperseR::get_data()
#' @param counties. US counties sf object
#' @param metric Metric column name. Defaults to 'N'
#' @param map.month Month in YYYYMM format
#' @param map.unitID Unit ID string matching 'ID' column in data.units
#'
#' @return sf data.table of impacts for plotting
#'
#' @export
#' @importFrom sf st_as_sf st_crs st_transform
#' @importFrom terra rast as.polygons
#' @importFrom data.table data.table setnames
create_impact_table_single <- function(data.linked,
                                       data.units,
                                       link.to = 'zips',
                                       zcta.dataset = NULL,
                                       counties. = NULL,
                                       map.month,
                                       map.unitID,
                                       metric = 'N') {

  year.use <- as.integer(substr(map.month, 1, 4))
  datareduced <- data.linked[month == map.month & ID == map.unitID]
  dataunits <- data.units[ID == map.unitID & year == year.use]

  if (link.to == 'zips') {
    dataset_sf <- data.table::data.table(
      dataunits,
      merge(
        zcta.dataset,
        datareduced,
        by = 'ZIP',
        all.y = TRUE
      )
    )
    data.table::setnames(dataset_sf, metric, 'metric')
    myVector <- c(
      "ID", "month", "Latitude", "Longitude", "metric",
      "ZIP", "ZCTA", "CITY", "STATE", "TOTALESTIMATE",
      "MARGINOFERROR", "geometry"
    )

  } else if (link.to == 'counties') {
    dataset_sf <- data.table::data.table(
      dataunits,
      merge(
        counties.[, c("statefp", "countyfp", "state_name", 
                      "name", "geoid", "geometry")],
        datareduced,
        by = c("statefp", "countyfp", "state_name", "name", "geoid"),
        all.y = TRUE
      )
    )
    data.table::setnames(dataset_sf, metric, 'metric')
    myVector <- c(
      "ID", "month", "Latitude", "Longitude", "metric",
      "statefp", "countyfp", "state_name", "name", "geometry"
    )

  } else if (link.to == 'grids') {
    # Use terra instead of raster for grid conversion
    # Create raster from xyz data
    crs_albers <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
    
    # Prepare data for raster creation
    xyz_data <- datareduced[, .(x, y, N)]
    data.table::setnames(xyz_data, "N", metric)
    
    # Create SpatRaster from xyz using terra
    dataset_r <- terra::rast(xyz_data, type = "xyz", crs = crs_albers)
    
    # Convert to polygons using terra, then to sf
    dataset_poly <- terra::as.polygons(dataset_r, dissolve = FALSE, na.rm = TRUE)
    dataset_sf <- sf::st_as_sf(dataset_poly)
    
    # Ensure CRS is set
    sf::st_crs(dataset_sf) <- crs_albers
    
    data.table::setnames(dataset_sf, metric, 'metric', skip_absent = TRUE)
    
    # Add additional columns
    if (nrow(datareduced) > 0) {
      dataset_sf$ID <- datareduced$ID[1]
      if ("comb" %in% names(datareduced)) {
        dataset_sf$comb <- datareduced$comb[1]
      }
    }
    
    myVector <- names(dataset_sf)
  }

  out <- data.table::data.table(dataset_sf[, myVector, with = FALSE])
  return(out)
}
