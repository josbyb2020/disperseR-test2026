#' Spatial Utility Functions for disperseR
#'
#' @description Provides modern spatial utility functions using sf and terra
#' to replace deprecated rgdal/rgeos/sp functionality.
#'
#' @name spatial_utils
#' @importFrom sf st_read st_write st_transform st_crs st_as_sf 
#'   st_coordinates st_centroid st_buffer st_intersection st_union 
#'   st_area st_within st_join st_intersects st_drop_geometry
#' @importFrom terra rast crs project vect buffer expanse centroids
NULL


#' Transform coordinates between CRS systems
#'
#' @description Modern replacement for sp::spTransform.
#'
#' @param x sf object or terra SpatVector/SpatRaster
#' @param crs Target CRS (character string, EPSG code, or crs object)
#' @return Transformed spatial object
#' @export
transform_crs <- function(x, crs) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    return(sf::st_transform(x, crs = crs))
  } else if (inherits(x, "SpatVector") || inherits(x, "SpatRaster")) {
    return(terra::project(x, crs))
  } else {
    stop("Input must be sf, sfc, SpatVector, or SpatRaster object")
  }
}


#' Get CRS from spatial object
#'
#' @description Modern replacement for sp::proj4string.
#'
#' @param x sf object or terra Spat* object
#' @return CRS as character string
#' @export
get_crs <- function(x) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    crs_obj <- sf::st_crs(x)
    return(crs_obj$proj4string)
  } else if (inherits(x, c("SpatVector", "SpatRaster"))) {
    return(terra::crs(x, proj = TRUE))
  } else {
    stop("Input must be sf, sfc, or terra Spat* object")
  }
}


#' Buffer spatial object
#'
#' @description Modern replacement for rgeos::gBuffer.
#'
#' @param x sf object or terra SpatVector
#' @param dist Buffer distance in CRS units
#' @param ... Additional arguments passed to st_buffer or buffer
#' @return Buffered spatial object
#' @export
spatial_buffer <- function(x, dist, ...) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    return(sf::st_buffer(x, dist = dist, ...))
  } else if (inherits(x, "SpatVector")) {
    return(terra::buffer(x, width = dist, ...))
  } else {
    stop("Input must be sf, sfc, or SpatVector object")
  }
}


#' Intersect spatial objects
#'
#' @description Modern replacement for rgeos::gIntersection.
#'
#' @param x First sf object or terra SpatVector
#' @param y Second sf object or terra SpatVector
#' @return Intersection of x and y
#' @export
spatial_intersect <- function(x, y) {
  if ((inherits(x, "sf") || inherits(x, "sfc")) &&
      (inherits(y, "sf") || inherits(y, "sfc"))) {
    return(sf::st_intersection(x, y))
  } else if (inherits(x, "SpatVector") && inherits(y, "SpatVector")) {
    return(terra::intersect(x, y))
  } else {
    stop("Both inputs must be sf/sfc objects or both must be SpatVector objects")
  }
}


#' Union spatial objects
#'
#' @description Modern replacement for rgeos::gUnion.
#'
#' @param x sf object or terra SpatVector
#' @return Unified spatial object
#' @export
spatial_union <- function(x) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    return(sf::st_union(x))
  } else if (inherits(x, "SpatVector")) {
    return(terra::aggregate(x))
  } else {
    stop("Input must be sf, sfc, or SpatVector object")
  }
}


#' Calculate area of spatial features
#'
#' @description Modern replacement for rgeos::gArea.
#'
#' @param x sf object or terra SpatVector with polygon geometry
#' @return Numeric vector of areas
#' @export
spatial_area <- function(x) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    areas <- sf::st_area(x)
    return(as.numeric(areas))
  } else if (inherits(x, "SpatVector")) {
    return(terra::expanse(x))
  } else {
    stop("Input must be sf, sfc, or SpatVector object")
  }
}


#' Get centroids of spatial features
#'
#' @description Modern replacement for rgeos::gCentroid.
#'
#' @param x sf object or terra SpatVector
#' @return Centroids as sf or SpatVector
#' @export
spatial_centroid <- function(x) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    suppressWarnings(return(sf::st_centroid(x)))
  } else if (inherits(x, "SpatVector")) {
    return(terra::centroids(x))
  } else {
    stop("Input must be sf, sfc, or SpatVector object")
  }
}


#' Extract coordinates from spatial object
#'
#' @description Modern replacement for sp::coordinates.
#'
#' @param x sf object or terra SpatVector
#' @return Matrix of coordinates
#' @export
get_coordinates <- function(x) {
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    return(sf::st_coordinates(x))
  } else if (inherits(x, "SpatVector")) {
    return(terra::crds(x))
  } else {
    stop("Input must be sf, sfc, or SpatVector object")
  }
}


#' Point in polygon test
#'
#' @description Modern replacement for sp::over or rgeos::gContains.
#'
#' @param points sf object with point geometry
#' @param polygons sf object with polygon geometry
#' @param returnList If TRUE, return list of polygon indices for each point
#' @return If returnList=FALSE, vector of polygon indices. If TRUE, list.
#' @export
point_in_polygon <- function(points, polygons, returnList = FALSE) {
  if (!inherits(points, c("sf", "sfc")) || 
      !inherits(polygons, c("sf", "sfc"))) {
    stop("Both inputs must be sf or sfc objects")
  }

  result <- sf::st_within(points, polygons)

  if (returnList) {
    return(result)
  } else {
    return(sapply(result, function(x) if (length(x) > 0) x[1] else NA_integer_))
  }
}


#' Spatial join
#'
#' @description Modern replacement for sp::over.
#'
#' @param x sf object
#' @param y sf object
#' @param join Function defining spatial relationship (default: st_intersects)
#' @param ... Additional arguments
#' @return sf object with joined attributes
#' @export
spatial_join <- function(x, y, join = sf::st_intersects, ...) {
  sf::st_join(x, y, join = join, ...)
}


#' Read shapefile
#'
#' @description Modern replacement for rgdal::readOGR and raster::shapefile.
#'
#' @param path Path to shapefile (.shp)
#' @param ... Additional arguments passed to st_read
#' @return sf object
#' @export
read_shapefile <- function(path, ...) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  sf::st_read(path, quiet = TRUE, ...)
}


#' Write shapefile
#'
#' @description Modern replacement for rgdal::writeOGR.
#'
#' @param x sf object
#' @param path Output path (without extension)
#' @param ... Additional arguments passed to st_write
#' @return Path to written file (invisibly)
#' @export
write_shapefile <- function(x, path, ...) {
  if (!inherits(x, "sf")) {
    stop("Input must be an sf object")
  }

  if (!grepl("\\.shp$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".shp")
  }

  sf::st_write(x, path, delete_layer = TRUE, quiet = TRUE, ...)
  invisible(path)
}


#' Common CRS definitions
#'
#' @description Standard CRS definitions used in disperseR.
#'
#' @return Named list of CRS strings
#' @export
get_crs_definitions <- function() {
  list(
    # North American Albers Equal Area Conic (main projection for disperseR)
    albers_na = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m",

    # WGS84 (standard lat/lon)
    wgs84 = "+proj=longlat +datum=WGS84 +no_defs",
    wgs84_epsg = "EPSG:4326",

    # NAD83 (North American Datum 1983)
    nad83 = "+proj=longlat +datum=NAD83 +no_defs",
    nad83_epsg = "EPSG:4269",

    # Web Mercator (for web mapping)
    web_mercator = "EPSG:3857"
  )
}


#' Check spatial package status
#'
#' @description Utility function to verify spatial package installation
#' and provide migration guidance.
#'
#' @return Invisibly returns list of package status
#' @export
check_spatial_packages <- function() {
  deprecated <- c("rgdal", "rgeos", "maptools")
  modern <- c("sf", "terra")

  installed_deprecated <- sapply(deprecated, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })

  installed_modern <- sapply(modern, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })

  message("=== Spatial Package Status ===\n")

  if (any(installed_deprecated)) {
    message("WARNING: Deprecated packages detected:")
    for (pkg in deprecated[installed_deprecated]) {
      message("  - ", pkg, " (retired from CRAN October 2023)")
    }
    message("\nThese packages are no longer maintained.")
    message("disperseR 0.2.0+ does not require these packages.\n")
  }

  message("Modern spatial packages:")
  for (pkg in modern) {
    if (installed_modern[pkg]) {
      version <- utils::packageVersion(pkg)
      message("  - ", pkg, " v", version, " [INSTALLED]")
    } else {
      message("  - ", pkg, " [NOT INSTALLED - REQUIRED]")
    }
  }

  if (!all(installed_modern)) {
    message("\nTo install missing packages:")
    message('  install.packages(c("sf", "terra"))')
  }

  invisible(list(
    deprecated_installed = installed_deprecated,
    modern_installed = installed_modern
  ))
}
