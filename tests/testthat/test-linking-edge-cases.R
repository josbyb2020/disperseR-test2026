# Edge-case tests for linking helpers

test_that("trim_zero trims from earliest zero-height hour", {
  skip_if_not_installed("data.table")
  
  d <- data.table::data.table(
    particle_no = c(1, 1, 1, 1, 2, 2, 2),
    lon = 0,
    lat = 0,
    height = c(10, 5, 0, 5, 10, 0, 3),
    Pdate = as.Date("2005-01-01"),
    hour = c(1, 2, 3, 4, 1, 2, 3)
  )
  
  out <- disperseR::trim_zero(d)
  
  # Particle 1 zeros at hour 3 -> keep hours 1-2 only
  expect_equal(out[particle_no == 1, max(hour)], 2)
  # Particle 2 zeros at hour 2 -> keep hour 1 only
  expect_equal(out[particle_no == 2, max(hour)], 1)
})

test_that("link_to returns correctly shaped empty outputs when PBL normalization yields all NA", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  p4 <- "EPSG:4326"
  
  d <- data.table::data.table(
    lon = c(-75.1, -75.2),
    lat = c(39.9, 40.0),
    height = c(10, 12),
    Pdate = as.Date("2005-01-01"),
    hour = c(1, 2)
  )
  
  # Single-layer PBL raster with all NA values and a matching monthly layer name.
  pbl <- terra::rast(
    xmin = -76, xmax = -74,
    ymin = 39, ymax = 41,
    resolution = 1,
    crs = p4
  )
  terra::values(pbl) <- NA_real_
  names(pbl) <- "X2005.01.01"
  
  out_grids <- disperseR::link_to(
    d = d,
    link.to = "grids",
    p4string = p4,
    rasterin = pbl,
    pbl. = TRUE
  )
  expect_true(data.table::is.data.table(out_grids))
  expect_equal(names(out_grids), c("x", "y", "N"))
  expect_equal(nrow(out_grids), 0)
  
  out_counties <- disperseR::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    rasterin = pbl,
    pbl. = TRUE
  )
  expect_true(data.table::is.data.table(out_counties))
  expect_equal(
    names(out_counties),
    c("statefp", "countyfp", "state_name", "name", "geoid", "N")
  )
  expect_equal(nrow(out_counties), 0)
  
  out_zips <- disperseR::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    rasterin = pbl,
    pbl. = TRUE
  )
  expect_true(data.table::is.data.table(out_zips))
  expect_equal(names(out_zips), c("ZIP", "N"))
  expect_equal(nrow(out_zips), 0)
})
