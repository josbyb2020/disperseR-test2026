#' Migration Guide: From rgdal/rgeos/sp to sf/terra
#'
#' @description This file documents the migration of disperseR from retired 
#' packages (rgdal, rgeos, maptools, sp, raster) to modern alternatives 
#' (sf, terra). It provides function mappings and usage examples.
#'
#' @name migration_guide
#' @section Background:
#' In October 2023, the CRAN packages rgdal, rgeos, and maptools were officially
#' retired. These packages provided interfaces to external geospatial libraries
#' (GDAL, GEOS, PROJ) but have been superseded by:
#' \itemize{
#'   \item sf: Simple Features for R (vector data)
#'   \item terra: Spatial Data Analysis (raster and vector data)
#' }
#'
#' @section Function Mappings:
#'
#' \subsection{Reading/Writing Spatial Data}{
#' \tabular{ll}{
#'   Old (rgdal/sp) \tab New (sf/terra) \cr
#'   rgdal::readOGR() \tab sf::st_read() \cr
#'   rgdal::writeOGR() \tab sf::st_write() \cr
#'   raster::shapefile() \tab sf::st_read() \cr
#'   raster::brick() \tab terra::rast() \cr
#'   raster::stack() \tab terra::rast() \cr
#' }
#' }
#'
#' \subsection{Coordinate Operations}{
#' \tabular{ll}{
#'   Old (sp/rgdal) \tab New (sf/terra) \cr
#'   sp::spTransform() \tab sf::st_transform() or terra::project() \cr
#'   sp::proj4string() \tab sf::st_crs() or terra::crs() \cr
#'   sp::coordinates() \tab sf::st_coordinates() or terra::crds() \cr
#'   sp::CRS() \tab sf::st_crs() or terra::crs() \cr
#' }
#' }
#'
#' \subsection{Geometric Operations}{
#' \tabular{ll}{
#'   Old (rgeos) \tab New (sf/terra) \cr
#'   rgeos::gBuffer() \tab sf::st_buffer() or terra::buffer() \cr
#'   rgeos::gIntersection() \tab sf::st_intersection() or terra::intersect() \cr
#'   rgeos::gUnion() \tab sf::st_union() or terra::aggregate() \cr
#'   rgeos::gArea() \tab sf::st_area() or terra::expanse() \cr
#'   rgeos::gCentroid() \tab sf::st_centroid() or terra::centroids() \cr
#'   rgeos::gContains() \tab sf::st_contains() \cr
#'   rgeos::gWithin() \tab sf::st_within() \cr
#'   rgeos::gIntersects() \tab sf::st_intersects() \cr
#'   rgeos::gDistance() \tab sf::st_distance() or terra::distance() \cr
#' }
#' }
#'
#' \subsection{Spatial Overlay}{
#' \tabular{ll}{
#'   Old (sp) \tab New (sf) \cr
#'   sp::over() \tab sf::st_join() or sf::st_intersection() \cr
#'   sp::merge() \tab base::merge() or dplyr::left_join() \cr
#' }
#' }
#'
#' @section Data Classes:
#'
#' \subsection{Old sp classes to New sf/terra classes}{
#' \itemize{
#'   \item SpatialPoints -> sf with POINT geometry
#'   \item SpatialPointsDataFrame -> sf with POINT geometry
#'   \item SpatialPolygons -> sf with POLYGON/MULTIPOLYGON geometry
#'   \item SpatialPolygonsDataFrame -> sf with POLYGON/MULTIPOLYGON geometry
#'   \item SpatialLines -> sf with LINESTRING/MULTILINESTRING geometry
#'   \item SpatialLinesDataFrame -> sf with LINESTRING/MULTILINESTRING geometry
#'   \item RasterLayer -> terra::SpatRaster
#'   \item RasterStack -> terra::SpatRaster (multi-layer)
#'   \item RasterBrick -> terra::SpatRaster (multi-layer)
#' }
#' }
#'
#' @section Converting Between Formats:
#'
#' \subsection{sp to sf}{
#' \preformatted{
#' # Convert sp object to sf
#' sf_obj <- sf::st_as_sf(sp_obj)
#' }
#' }
#'
#' \subsection{sf to sp (if needed for legacy code)}{
#' \preformatted{
#' # Convert sf to sp (requires sp package)
#' sp_obj <- as(sf_obj, "Spatial")
#' }
#' }
#'
#' \subsection{raster to terra}{
#' \preformatted{
#' # Convert raster to terra
#' terra_obj <- terra::rast(raster_obj)
#' }
#' }
#'
#' @section CRS Handling:
#'
#' The new packages use WKT2 format for CRS by default, which is more
#' accurate than the old PROJ.4 strings. However, PROJ.4 strings are
#' still accepted for backwards compatibility.
#'
#' \preformatted{
#' # Old way (PROJ.4 string)
#' crs <- CRS("+proj=longlat +datum=WGS84")
#'
#' # New way (WKT2 or EPSG code)
#' crs <- sf::st_crs(4326)  # EPSG code
#' crs <- sf::st_crs("EPSG:4326")  # Same as above
#' crs <- sf::st_crs("+proj=longlat +datum=WGS84")  # PROJ.4 still works
#' }
#'
#' @section disperseR-Specific Changes:
#'
#' \enumerate{
#'   \item DESCRIPTION: Removed rgdal, rgeos, sp, raster from Depends. 
#'         Added sf, terra to Imports.
#'   \item get_data.R: raster::shapefile() -> sf::st_read(), 
#'         sp::spTransform() -> sf::st_transform(), 
#'         raster::brick() -> terra::rast()
#'   \item link_all_units_subfun.R: Complete rewrite using sf for vector 
#'         operations and terra for raster operations.
#'   \item Return types: Functions now return sf objects instead of sp 
#'         objects for vector data, and SpatRaster instead of RasterLayer/Brick.
#' }
#'
#' @section Additional Resources:
#'
#' \itemize{
#'   \item sf package: https://r-spatial.github.io/sf/
#'   \item terra package: https://rspatial.github.io/terra/
#'   \item Migration guide: https://r-spatial.org/r/2022/04/12/evolution.html
#'   \item r-spatial evolution: https://r-spatial.github.io/evolution/
#' }
#'
#' @keywords internal
NULL
