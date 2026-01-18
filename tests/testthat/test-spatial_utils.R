# Tests for spatial_utils.R functions

test_that("transform_crs works with sf objects", {
  skip_if_not_installed("sf")
  
  # Create simple sf point
  pt <- sf::st_sfc(sf::st_point(c(-75.1, 39.9)), crs = 4326)
  pt_sf <- sf::st_sf(id = 1, geometry = pt)
  
  # Transform to a projected CRS
  result <- disperseR::transform_crs(pt_sf, crs = 3857)
  
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 3857)
})

test_that("get_crs extracts CRS from sf objects", {
  skip_if_not_installed("sf")
  
  pt <- sf::st_sfc(sf::st_point(c(-75.1, 39.9)), crs = 4326)
  pt_sf <- sf::st_sf(id = 1, geometry = pt)
  
  result <- disperseR::get_crs(pt_sf)
  
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("spatial_buffer buffers sf objects", {
  skip_if_not_installed("sf")
  
  # Create point in projected CRS (buffer needs units)
  pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  pt_sf <- sf::st_sf(id = 1, geometry = pt)
  
  result <- disperseR::spatial_buffer(pt_sf, dist = 1000)
  
  expect_s3_class(result, "sf")
  # Buffered point should be a polygon
  expect_true(sf::st_geometry_type(result, by_geometry = FALSE) %in% 
                c("POLYGON", "MULTIPOLYGON"))
})

test_that("spatial_union unions sf objects", {
  skip_if_not_installed("sf")
  
  # Create two overlapping polygons
  p1 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  p2 <- sf::st_polygon(list(rbind(c(0.5,0.5), c(1.5,0.5), c(1.5,1.5), c(0.5,1.5), c(0.5,0.5))))
  polys <- sf::st_sfc(list(p1, p2), crs = 4326)
  polys_sf <- sf::st_sf(id = 1:2, geometry = polys)
  
  result <- disperseR::spatial_union(polys_sf)
  
  # Union should return single geometry
  expect_true(inherits(result, "sfc") || inherits(result, "sf"))
})

test_that("get_coordinates extracts coordinates", {
  skip_if_not_installed("sf")
  
  pt <- sf::st_sfc(sf::st_point(c(-75.1, 39.9)), crs = 4326)
  pt_sf <- sf::st_sf(id = 1, geometry = pt)
  
  result <- disperseR::get_coordinates(pt_sf)
  
  expect_true(is.matrix(result) || is.data.frame(result))
  expect_equal(ncol(result), 2)
})

test_that("spatial_centroid computes centroids", {
  skip_if_not_installed("sf")
  
  # Create polygon
  p <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(p, crs = 4326))
  
  result <- disperseR::spatial_centroid(poly_sf)
  
  expect_true(inherits(result, "sf") || inherits(result, "sfc"))
})

test_that("check_spatial_packages runs without error", {
  # Should not error, just print messages
  expect_no_error(disperseR::check_spatial_packages())
})
