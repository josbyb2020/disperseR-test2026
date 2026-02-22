# Fast engine parity tests for linking internals

test_that("link_to fast engine matches legacy for ZIP linking", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.7, -75.2, -75.1),
    lat = c(39.1, 39.4, 39.2, 39.5),
    height = c(10, 12, 8, 9),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 3, 2, 4)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine normalizes numeric crosswalk ZIP/ZCTA codes", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.2),
    lat = c(39.2, 39.2),
    height = c(10, 12),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 2)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("01234", "12345"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw_char <- data.table::data.table(
    ZCTA = c("01234", "12345"),
    ZIP = c("01234", "12345")
  )

  cw_numeric <- data.table::data.table(
    ZCTA = c(1234, 12345),
    ZIP = c(1234, 12345)
  )

  out_char <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw_char,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  out_numeric <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw_numeric,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  s_char <- data.table::as.data.table(out_char)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_numeric <- data.table::as.data.table(out_numeric)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_numeric$ZIP, s_char$ZIP)
  expect_equal(s_numeric$N, s_char$N, tolerance = 1e-12)
  expect_equal(s_numeric$ZIP, c("01234", "12345"))
})

test_that("link_to ZIP linking ignores NA in non-key crosswalk metadata columns", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.2),
    lat = c(39.2, 39.2),
    height = c(10, 12),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 2)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw_base <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )
  cw_with_na_meta <- data.table::copy(cw_base)
  cw_with_na_meta[, comment := NA_character_]

  out_base <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw_base,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  out_with_na_meta <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw_with_na_meta,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  s_base <- data.table::as.data.table(out_base)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_with_na_meta <- data.table::as.data.table(out_with_na_meta)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_with_na_meta$ZIP, s_base$ZIP)
  expect_equal(s_with_na_meta$N, s_base$N, tolerance = 1e-12)
  expect_gt(nrow(s_with_na_meta), 0L)
})

test_that("link_to fast engine fallback-to-legacy path preserves ZIP outputs", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.7, -75.2, -75.1),
    lat = c(39.1, 39.4, 39.2, 39.5),
    height = c(10, 12, 8, 9),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 3, 2, 4)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  old_opt <- getOption("disperseR.fast.extract.min.cells")
  options(disperseR.fast.extract.min.cells = .Machine$integer.max)
  on.exit(options(disperseR.fast.extract.min.cells = old_opt), add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine ratio gate can force legacy-equivalent fallback", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.7, -75.2, -75.1),
    lat = c(39.1, 39.4, 39.2, 39.5),
    height = c(10, 12, 8, 9),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 3, 2, 4)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = .Machine$double.xmax)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine aggregates duplicated ZCTA rows like legacy", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.85, -75.8, -75.2, -75.2, -75.2, -75.2),
    lat = c(39.1, 39.2, 39.3, 39.1, 39.2, 39.3, 39.4),
    height = rep(10, 7),
    Pdate = as.Date("2005-01-01"),
    hour = rep(2, 7)
  )

  # Two polygons share the same ZCTA code to emulate split geometries.
  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "11111"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = "11111",
    ZIP = "11111"
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = 0)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine preserves cell-weighted duplicate ZCTA aggregation", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(rep(-100.8, 20), rep(-80.2, 2), rep(-80.25, 2)),
    lat = c(seq(35.05, 35.45, length.out = 20), c(40.1, 40.2), c(40.25, 40.3)),
    height = rep(10, 24),
    Pdate = as.Date("2005-01-01"),
    hour = rep(2, 24)
  )

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "11111"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-101.0, 35.0), c(-100.5, 35.0), c(-100.5, 35.5), c(-101.0, 35.5), c(-101.0, 35.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-80.5, 40.0), c(-80.0, 40.0), c(-80.0, 40.5), c(-80.5, 40.5), c(-80.5, 40.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = "11111",
    ZIP = "11111"
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = 0)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine preserves parity on lonlat ZIP geometries", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  # Seeded scenario that previously diverged under fast extract on lon/lat CRS.
  set.seed(1)
  n <- sample(200:800, 1)
  d <- data.table::data.table(
    lon = runif(n, -102, -78),
    lat = runif(n, 33, 42),
    height = runif(n, 5, 20),
    Pdate = as.Date("2005-01-01"),
    hour = sample(2:10, n, replace = TRUE)
  )
  x1 <- -102 + runif(1, 0, 5)
  x2 <- -98 + runif(1, 0, 5)
  x3 <- -84 + runif(1, 0, 4)
  x4 <- -80 + runif(1, 0, 2)
  y1 <- 33 + runif(1, 0, 2)
  y2 <- 40 + runif(1, 0, 2)

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(x1, y1), c(x2, y1), c(x2, y2), c(x1, y2), c(x1, y1)
      ))),
      sf::st_polygon(list(rbind(
        c(x3, y1), c(x4, y1), c(x4, y2), c(x3, y2), c(x3, y1)
      ))),
      sf::st_polygon(list(rbind(
        c(-90.0, 34.0), c(-86.0, 34.0), c(-86.0, 38.0), c(-90.0, 38.0), c(-90.0, 34.0)
      )))
    ),
    crs = 4326
  )

  cw <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = 0)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.2,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "zips",
    p4string = p4,
    zc = zcta_sf,
    cw = cw,
    pbl. = FALSE,
    res.link. = 0.2,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]

  expect_equal(s_fast$ZIP, s_legacy$ZIP)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})


test_that("link_to fast engine matches legacy for county linking", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(-75.9, -75.7, -75.2, -75.1),
    lat = c(39.1, 39.4, 39.2, 39.5),
    height = c(10, 12, 8, 9),
    Pdate = as.Date("2005-01-01"),
    hour = c(2, 3, 2, 4)
  )

  counties_sf <- sf::st_sf(
    statefp = c("01", "01"),
    countyfp = c("001", "003"),
    state_name = c("A", "A"),
    name = c("Left", "Right"),
    geoid = c("01001", "01003"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  out_legacy <- disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "legacy"
  )

  out_fast <- disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.25,
    engine = "fast"
  )

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]

  expect_equal(s_fast$geoid, s_legacy$geoid)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine preserves parity on lonlat county geometries", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  # Seeded scenario that previously diverged under fast extract on lon/lat CRS.
  set.seed(1002)
  n <- sample(200:800, 1)
  d <- data.table::data.table(
    lon = runif(n, -102, -78),
    lat = runif(n, 33, 42),
    height = runif(n, 5, 20),
    Pdate = as.Date("2005-01-01"),
    hour = sample(2:10, n, replace = TRUE)
  )
  x1 <- -102 + runif(1, 0, 5)
  x2 <- -98 + runif(1, 0, 5)
  x3 <- -84 + runif(1, 0, 4)
  x4 <- -80 + runif(1, 0, 2)
  y1 <- 33 + runif(1, 0, 2)
  y2 <- 40 + runif(1, 0, 2)

  counties_sf <- sf::st_sf(
    statefp = c("01", "01", "02"),
    countyfp = c("001", "001", "003"),
    state_name = c("A", "A", "B"),
    name = c("X", "X", "Y"),
    geoid = c("01001", "01001", "02003"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(x1, y1), c(x2, y1), c(x2, y2), c(x1, y2), c(x1, y1)
      ))),
      sf::st_polygon(list(rbind(
        c(x3, y1), c(x4, y1), c(x4, y2), c(x3, y2), c(x3, y1)
      ))),
      sf::st_polygon(list(rbind(
        c(-90.0, 34.0), c(-86.0, 34.0), c(-86.0, 38.0), c(-90.0, 38.0), c(-90.0, 34.0)
      )))
    ),
    crs = 4326
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = 0)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.2,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.2,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]

  expect_equal(s_fast$geoid, s_legacy$geoid)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})

test_that("link_to fast engine aggregates duplicated county rows like legacy", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  p4 <- "EPSG:4326"

  d <- data.table::data.table(
    lon = c(rep(-100.8, 20), rep(-80.2, 2), rep(-80.25, 2)),
    lat = c(seq(35.05, 35.45, length.out = 20), c(40.1, 40.2), c(40.25, 40.3)),
    height = rep(10, 24),
    Pdate = as.Date("2005-01-01"),
    hour = rep(2, 24)
  )

  # Two polygons share the same county identifiers to emulate split geometries.
  counties_sf <- sf::st_sf(
    statefp = c("01", "01"),
    countyfp = c("001", "001"),
    state_name = c("A", "A"),
    name = c("X", "X"),
    geoid = c("01001", "01001"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-101.0, 35.0), c(-100.5, 35.0), c(-100.5, 35.5), c(-101.0, 35.5), c(-101.0, 35.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-80.5, 40.0), c(-80.0, 40.0), c(-80.0, 40.5), c(-80.5, 40.5), c(-80.5, 40.0)
      )))
    ),
    crs = 4326
  )

  old_min_cells <- getOption("disperseR.fast.extract.min.cells")
  old_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio")
  options(disperseR.fast.extract.min.cells = 0L)
  options(disperseR.fast.extract.min.cell_poly_ratio = 0)
  on.exit({
    options(disperseR.fast.extract.min.cells = old_min_cells)
    options(disperseR.fast.extract.min.cell_poly_ratio = old_ratio)
  }, add = TRUE)

  out_legacy <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "legacy"
  ))

  out_fast <- suppressWarnings(disperseR:::link_to(
    d = d,
    link.to = "counties",
    p4string = p4,
    county.sf = counties_sf,
    pbl. = FALSE,
    res.link. = 0.1,
    engine = "fast"
  ))

  s_legacy <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]
  s_fast <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = geoid][order(geoid)]

  expect_equal(s_fast$geoid, s_legacy$geoid)
  expect_equal(s_fast$N, s_legacy$N, tolerance = 1e-12)
})


test_that("window reader filters deterministically during load", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")

  tmp <- tempfile("disperser_fast_reader_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  f1 <- file.path(tmp, "a.fst")
  f2 <- file.path(tmp, "b.fst")

  dt1 <- data.table::data.table(
    lon = c(1, 2),
    lat = c(1, 2),
    height = c(10, 20),
    Pdate = as.Date(c("2005-01-01", "2005-01-02")),
    hour = c(2, 0)
  )
  dt2 <- data.table::data.table(
    lon = c(3, 4),
    lat = c(3, 4),
    height = c(30, 40),
    Pdate = as.Date(c("2005-01-01", "2005-01-03")),
    hour = c(5, 2)
  )

  fst::write.fst(dt1, f1)
  fst::write.fst(dt2, f2)

  vec_dates <- as.character(as.Date(c("2005-01-01", "2005-01-03")))

  read_a <- disperseR:::.read_hysp_files_for_window(
    files.read = c(f1, f2),
    vec_dates = vec_dates
  )

  read_b <- disperseR:::.read_hysp_files_for_window(
    files.read = c(f1, f2),
    vec_dates = vec_dates
  )

  read_a <- data.table::as.data.table(read_a)[order(lon, lat)]
  read_b <- data.table::as.data.table(read_b)[order(lon, lat)]

  expect_equal(nrow(read_a), 3L)
  expect_equal(nrow(read_b), nrow(read_a))
  expect_equal(read_b$lon, read_a$lon)
  expect_equal(read_b$lat, read_a$lat)
  expect_equal(read_b$height, read_a$height)
  expect_equal(as.character(read_b$Pdate), as.character(read_a$Pdate))
  expect_equal(read_b$hour, read_a$hour)
})


test_that("link_all_units fast engine preserves legacy outputs on synthetic files", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("fst")
  skip_if_not_installed("terra")

  tmp <- tempfile("disperser_fast_link_all_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(tmp)

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )
  disperseR:::.disperseR_cache_set("zcta", zcta_sf)

  crosswalk <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  units_run <- data.table::data.table(
    uID = c("U1", "U2"),
    ID = c("U1", "U2")
  )

  for (unit_id in units_run$ID) {
    ymdir <- file.path(dirs$hysp_dir, "2005", "01")
    dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(ymdir, paste0("hyspdisp_", unit_id, "_2005-01-15_00.fst"))
    if (unit_id == "U1") {
      dt <- data.table::data.table(
        lon = c(-75.9, -75.8),
        lat = c(39.2, 39.3),
        height = c(10, 12),
        Pdate = as.Date(c("2005-01-15", "2005-01-15")),
        hour = c(2, 4)
      )
    } else {
      dt <- data.table::data.table(
        lon = c(-75.2, -75.1),
        lat = c(39.2, 39.4),
        height = c(11, 9),
        Pdate = as.Date(c("2005-01-15", "2005-01-15")),
        hour = c(3, 5)
      )
    }
    fst::write.fst(dt, out_file)
  }

  legacy <- suppressWarnings(disperseR::link_all_units(
    units.run = units_run,
    link.to = "zips",
    mc.cores = 1,
    year.mons = "200501",
    pbl_trim = FALSE,
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    duration.run.hours = 24,
    overwrite = TRUE,
    return.linked.data = TRUE,
    engine = "legacy"
  ))

  fast <- suppressWarnings(disperseR::link_all_units(
    units.run = units_run,
    link.to = "zips",
    mc.cores = 1,
    year.mons = "200501",
    pbl_trim = FALSE,
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    duration.run.hours = 24,
    overwrite = TRUE,
    return.linked.data = TRUE,
    engine = "fast"
  ))

  expect_true(nrow(legacy) > 0)
  expect_true(nrow(fast) > 0)

  legacy_s <- data.table::as.data.table(legacy)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
  fast_s <- data.table::as.data.table(fast)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]

  expect_equal(fast_s$ZIP, legacy_s$ZIP)
  expect_equal(fast_s$ID, legacy_s$ID)
  expect_equal(fast_s$month, legacy_s$month)
  expect_equal(fast_s$N, legacy_s$N, tolerance = 1e-12)
})


test_that("disperser_link_zips accepts explicit zcta and bypasses cache lookup", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("fst")
  skip_if_not_installed("terra")

  tmp <- tempfile("disperser_fast_link_direct_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(tmp)

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )

  crosswalk <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  unit <- data.table::data.table(uID = "U1", ID = "U1")
  ymdir <- file.path(dirs$hysp_dir, "2005", "01")
  dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)
  fst::write.fst(
    data.table::data.table(
      lon = c(-75.9, -75.2),
      lat = c(39.2, 39.4),
      height = c(10, 11),
      Pdate = as.Date(c("2005-01-15", "2005-01-15")),
      hour = c(2, 3)
    ),
    file.path(ymdir, "hyspdisp_U1_2005-01-15_00.fst")
  )

  old_zcta <- disperseR:::.disperseR_cache_get("zcta")
  on.exit(disperseR:::.disperseR_cache_set("zcta", old_zcta), add = TRUE)
  disperseR:::.disperseR_cache_set("zcta", NULL)

  out <- suppressWarnings(disperseR:::disperser_link_zips(
    month_YYYYMM = "200501",
    unit = unit,
    duration.run.hours = 24,
    pbl.height = NULL,
    crosswalk. = crosswalk,
    zcta = zcta_sf,
    res.link. = 12000,
    overwrite = TRUE,
    pbl. = FALSE,
    return.linked.data. = TRUE,
    engine = "fast"
  ))

  p4 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
  zcta_vect <- terra::vect(sf::st_transform(zcta_sf, crs = p4))
  out_vect <- suppressWarnings(disperseR:::disperser_link_zips(
    month_YYYYMM = "200501",
    unit = unit,
    duration.run.hours = 24,
    pbl.height = NULL,
    crosswalk. = crosswalk,
    zcta = NULL,
    zcta.vect = zcta_vect,
    res.link. = 12000,
    overwrite = TRUE,
    pbl. = FALSE,
    return.linked.data. = TRUE,
    engine = "fast"
  ))
  out_vect_fn <- suppressWarnings(disperseR:::disperser_link_zips(
    month_YYYYMM = "200501",
    unit = unit,
    duration.run.hours = 24,
    pbl.height = NULL,
    crosswalk. = crosswalk,
    zcta = zcta_sf,
    zcta.vect = function() zcta_vect,
    res.link. = 12000,
    overwrite = TRUE,
    pbl. = FALSE,
    return.linked.data. = TRUE,
    engine = "fast"
  ))

  expect_true(nrow(out) > 0)
  expect_true("ZIP" %in% names(out))
  out_s <- data.table::as.data.table(out)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
  out_vect_s <- data.table::as.data.table(out_vect)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
  out_vect_fn_s <- data.table::as.data.table(out_vect_fn)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
  expect_equal(out_vect_s$ZIP, out_s$ZIP)
  expect_equal(out_vect_s$ID, out_s$ID)
  expect_equal(out_vect_s$month, out_s$month)
  expect_equal(out_vect_s$N, out_s$N, tolerance = 1e-12)
  expect_equal(out_vect_fn_s$ZIP, out_s$ZIP)
  expect_equal(out_vect_fn_s$ID, out_s$ID)
  expect_equal(out_vect_fn_s$month, out_s$month)
  expect_equal(out_vect_fn_s$N, out_s$N, tolerance = 1e-12)
})


test_that("link_all_units handles packed pbl.height consistently", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("fst")
  skip_if_not_installed("terra")

  tmp <- tempfile("disperser_fast_link_pbl_packed_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(tmp)

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )
  disperseR:::.disperseR_cache_set("zcta", zcta_sf)

  crosswalk <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  units_run <- data.table::data.table(uID = "U1", ID = "U1")
  ymdir <- file.path(dirs$hysp_dir, "2005", "01")
  dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)
  fst::write.fst(
    data.table::data.table(
      lon = c(-75.9, -75.2),
      lat = c(39.2, 39.4),
      height = c(10, 11),
      Pdate = as.Date(c("2005-01-15", "2005-01-15")),
      hour = c(2, 3)
    ),
    file.path(ymdir, "hyspdisp_U1_2005-01-15_00.fst")
  )

  pbl <- terra::rast(
    nrows = 20, ncols = 20,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    crs = "EPSG:4326"
  )
  terra::values(pbl) <- 10000
  names(pbl) <- "X2005.01.01"

  out_plain <- suppressWarnings(disperseR::link_all_units(
    units.run = units_run,
    link.to = "zips",
    mc.cores = 1,
    year.mons = "200501",
    pbl_trim = TRUE,
    pbl.height = pbl,
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    duration.run.hours = 24,
    overwrite = TRUE,
    return.linked.data = TRUE,
    engine = "fast"
  ))

  out_packed <- suppressWarnings(disperseR::link_all_units(
    units.run = units_run,
    link.to = "zips",
    mc.cores = 1,
    year.mons = "200501",
    pbl_trim = TRUE,
    pbl.height = terra::wrap(pbl, proxy = TRUE),
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    duration.run.hours = 24,
    overwrite = TRUE,
    return.linked.data = TRUE,
    engine = "fast"
  ))

  plain_s <- data.table::as.data.table(out_plain)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
  packed_s <- data.table::as.data.table(out_packed)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]

  expect_equal(packed_s$ZIP, plain_s$ZIP)
  expect_equal(packed_s$ID, plain_s$ID)
  expect_equal(packed_s$month, plain_s$month)
  expect_equal(packed_s$N, plain_s$N, tolerance = 1e-12)
})

test_that("link_all_units errors when all linking tasks fail", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("fst")

  tmp <- tempfile("disperser_link_all_fail_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(tmp)

  units_run <- data.table::data.table(uID = "U1", ID = "U1")
  ymdir <- file.path(dirs$hysp_dir, "2005", "01")
  dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)
  fst::write.fst(
    data.table::data.table(
      lon = c(-75.9, -75.2),
      lat = c(39.2, 39.4),
      height = c(10, 11),
      Pdate = as.Date(c("2005-01-15", "2005-01-15")),
      hour = c(2, 3)
    ),
    file.path(ymdir, "hyspdisp_U1_2005-01-15_00.fst")
  )

  # Missing required county identifier columns to force task failure.
  bad_counties <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      )))
    ),
    crs = 4326
  )

  expect_error(
    suppressWarnings(disperseR::link_all_units(
      units.run = units_run,
      link.to = "counties",
      mc.cores = 1,
      year.mons = "200501",
      pbl_trim = FALSE,
      counties. = bad_counties,
      hysp_dir = dirs$hysp_dir,
      ziplink_dir = dirs$ziplink_dir,
      duration.run.hours = 24,
      overwrite = TRUE,
      return.linked.data = TRUE,
      engine = "fast"
    )),
    "All linking tasks failed"
  )
})

test_that("link_all_units warns and returns partial results when some tasks fail", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("sf")
  skip_if_not_installed("fst")

  tmp <- tempfile("disperser_link_all_partial_fail_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(tmp)

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = c("11111", "22222"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-76.0, 39.0), c(-75.5, 39.0), c(-75.5, 39.6), c(-76.0, 39.6), c(-76.0, 39.0)
      ))),
      sf::st_polygon(list(rbind(
        c(-75.5, 39.0), c(-75.0, 39.0), c(-75.0, 39.6), c(-75.5, 39.6), c(-75.5, 39.0)
      )))
    ),
    crs = 4326
  )
  disperseR:::.disperseR_cache_set("zcta", zcta_sf)

  crosswalk <- data.table::data.table(
    ZCTA = c("11111", "22222"),
    ZIP = c("11111", "22222")
  )

  units_run <- data.table::data.table(uID = c("U1", "U2"), ID = c("U1", "U2"))
  day <- "2005-01-15"
  ymdir <- file.path(dirs$hysp_dir, "2005", "01")
  dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)
  fst::write.fst(
    data.table::data.table(
      lon = c(-75.9, -75.2),
      lat = c(39.2, 39.4),
      height = c(10, 11),
      Pdate = as.Date(c(day, day)),
      hour = c(2, 3)
    ),
    file.path(ymdir, "hyspdisp_U1_2005-01-15_00.fst")
  )
  # Missing required `hour` column forces one task-level failure.
  fst::write.fst(
    data.table::data.table(
      lon = c(-75.9, -75.2),
      lat = c(39.2, 39.4),
      height = c(10, 11),
      Pdate = as.Date(c(day, day))
    ),
    file.path(ymdir, "hyspdisp_U2_2005-01-15_00.fst")
  )

  expect_warning(
    out <- disperseR::link_all_units(
      units.run = units_run,
      link.to = "zips",
      mc.cores = 1,
      year.mons = "200501",
      pbl_trim = FALSE,
      crosswalk. = crosswalk,
      hysp_dir = dirs$hysp_dir,
      ziplink_dir = dirs$ziplink_dir,
      duration.run.hours = 24,
      overwrite = TRUE,
      return.linked.data = TRUE,
      engine = "fast"
    ),
    "linking task\\(s\\) failed"
  )

  expect_true(nrow(out) > 0)
  expect_true(all(out$month == "200501"))
  expect_true(all(out$ID == "U1"))
  expect_false(any(out$ID == "U2"))
})
