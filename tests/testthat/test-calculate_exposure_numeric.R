# tests/testthat/test-calculate_exposure_numeric.R
# Numeric correctness and regression test for calculate_exposure()
#
# Wide map columns use hyphenated ID format (e.g., "3136-1") from
# combine_monthly_links(), while units.mo$uID uses dotted format
# (e.g., "3136.1") from PP.units.monthly.  calculate_exposure() must
# bridge the two formats via gsub("-", ".").

test_that("calculate_exposure produces correct numeric values with known inputs", {
  # Use a tempdir for output so the test is self-contained
  tmp_exp <- file.path(tempdir(), "test_calc_exp_numeric")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # --- Build a mock monthly_maps list for January (month 1) of year 2005 ---
  # Wide columns are hyphenated (real pipeline output from combine_monthly_links)
  mock_wide <- data.table::data.table(
    ZIP  = c("02101", "10001"),
    `3136-1` = c(5.0, 3.0)   # N values: particle counts reaching each ZIP
  )

  monthly_maps <- list(
    "MAP1.2005" = mock_wide
  )

  # --- Build a mock units.mo data.table ---
  # uID uses dotted format (real PP.units.monthly data)
  units_mo <- data.table::data.table(
    uID       = "3136.1",
    year      = 2005L,
    month     = 1L,
    SO2.tons  = 10.0
  )

  # --- Call calculate_exposure ---
  expect_warning(
    result <- calculate_exposure(
      year.E        = 2005,
      year.D        = 2005,
      link.to       = "zips",
      pollutant     = "SO2.tons",
      units.mo      = units_mo,
      monthly_maps  = monthly_maps,
      exp_dir       = tmp_exp,
      source.agg    = "total",
      time.agg      = "year",
      allow.partial = TRUE
    ),
    "Missing.*of 12 monthly maps"
  )

  expect_s3_class(result, "data.table")

  # Expected calculation:
  #   ZIP 02101: hyads = N * SO2.tons = 5.0 * 10.0 = 50.0
  #   ZIP 10001: hyads = N * SO2.tons = 3.0 * 10.0 = 30.0
  expect_true("hyads" %in% names(result))
  expect_true("ZIP" %in% names(result))

  data.table::setorder(result, ZIP)

  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(50.0, 30.0), tolerance = 1e-10)
  expect_equal(result$ZIP, c("02101", "10001"))
})

test_that("calculate_exposure works with multiple units and sums correctly", {
  tmp_exp <- file.path(tempdir(), "test_calc_exp_multi")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # Wide map columns: hyphenated (pipeline output)
  mock_jan <- data.table::data.table(
    ZIP      = c("02101", "10001"),
    `3136-1` = c(5.0, 3.0),
    `3136-2` = c(2.0, 1.0)
  )
  mock_feb <- data.table::data.table(
    ZIP      = c("02101", "10001"),
    `3136-1` = c(4.0, 6.0),
    `3136-2` = c(1.0, 0.0)
  )

  monthly_maps <- list(
    "MAP1.2005" = mock_jan,
    "MAP2.2005" = mock_feb
  )

  # units.mo$uID: dotted (real emissions data)
  units_mo <- data.table::data.table(
    uID      = c("3136.1", "3136.1", "3136.2", "3136.2"),
    year     = c(2005L, 2005L, 2005L, 2005L),
    month    = c(1L, 2L, 1L, 2L),
    SO2.tons = c(10.0, 20.0, 5.0, 15.0)
  )

  expect_warning(
    result <- calculate_exposure(
      year.E        = 2005,
      year.D        = 2005,
      link.to       = "zips",
      pollutant     = "SO2.tons",
      units.mo      = units_mo,
      monthly_maps  = monthly_maps,
      exp_dir       = tmp_exp,
      source.agg    = "total",
      time.agg      = "year",
      allow.partial = TRUE
    ),
    "Missing.*of 12 monthly maps"
  )

  data.table::setorder(result, ZIP)

  # ZIP 02101:
  #   Jan: (5.0 * 10.0) + (2.0 * 5.0) = 50 + 10 = 60
  #   Feb: (4.0 * 20.0) + (1.0 * 15.0) = 80 + 15 = 95
  #   Total: 60 + 95 = 155

  # ZIP 10001:
  #   Jan: (3.0 * 10.0) + (1.0 * 5.0) = 30 + 5 = 35
  #   Feb: (6.0 * 20.0) + (0.0 * 15.0) = 120 + 0 = 120
  #   Total: 35 + 120 = 155

  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(155.0, 155.0), tolerance = 1e-10)
})

test_that("calculate_exposure regression: cross-format ID matching works", {
  # Specifically tests that hyphenated wide-map IDs (e.g., "99-42") are
  # correctly matched to dotted units.mo IDs (e.g., "99.42") after the
  # gsub("-", ".") conversion in calculate_exposure().

  tmp_exp <- file.path(tempdir(), "test_calc_exp_crossfmt")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # Wide map: hyphenated ID column
  mock_wide <- data.table::data.table(
    ZIP      = "90210",
    `99-42`  = 7.0
  )

  monthly_maps <- list("MAP6.2005" = mock_wide)

  # Emissions: dotted uID
  units_mo <- data.table::data.table(
    uID      = "99.42",
    year     = 2005L,
    month    = 6L,
    SO2.tons = 3.0
  )

  expect_warning(
    result <- calculate_exposure(
      year.E        = 2005,
      year.D        = 2005,
      link.to       = "zips",
      pollutant     = "SO2.tons",
      units.mo      = units_mo,
      monthly_maps  = monthly_maps,
      exp_dir       = tmp_exp,
      source.agg    = "total",
      time.agg      = "year",
      allow.partial = TRUE
    ),
    "Missing.*of 12 monthly maps"
  )

  expect_equal(nrow(result), 1L)
  # Expected: 7.0 * 3.0 = 21.0
  expect_equal(result$hyads, 21.0, tolerance = 1e-10)
  expect_equal(result$ZIP, "90210")
})

test_that("calculate_exposure monthly time.agg returns correct per-month values", {
  tmp_exp <- file.path(tempdir(), "test_calc_exp_monthly")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # Wide maps: hyphenated
  mock_jan <- data.table::data.table(
    ZIP      = "02101",
    `3136-1` = 5.0
  )
  mock_feb <- data.table::data.table(
    ZIP      = "02101",
    `3136-1` = 8.0
  )

  monthly_maps <- list(
    "MAP1.2005" = mock_jan,
    "MAP2.2005" = mock_feb
  )

  # units.mo: dotted
  units_mo <- data.table::data.table(
    uID      = c("3136.1", "3136.1"),
    year     = c(2005L, 2005L),
    month    = c(1L, 2L),
    SO2.tons = c(10.0, 20.0)
  )

  expect_warning(
    result <- calculate_exposure(
      year.E              = 2005,
      year.D              = 2005,
      link.to             = "zips",
      pollutant           = "SO2.tons",
      units.mo            = units_mo,
      monthly_maps        = monthly_maps,
      exp_dir             = tmp_exp,
      source.agg          = "total",
      time.agg            = "month",
      return.monthly.data = TRUE,
      allow.partial       = TRUE
    ),
    "Missing.*of 12 monthly maps"
  )

  data.table::setorder(result, yearmonth)

  # Jan: 5.0 * 10.0 = 50.0
  # Feb: 8.0 * 20.0 = 160.0
  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(50.0, 160.0), tolerance = 1e-10)
})

test_that("calculate_exposure bridges hyphenated map IDs to dotted emission IDs", {
  # Explicit test: wide map has "7-1", units.mo has "7.1" — should still merge
  tmp_exp <- file.path(tempdir(), "test_calc_exp_bridge")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  mock_wide <- data.table::data.table(
    ZIP    = c("11201", "60601"),
    `7-1`  = c(10.0, 20.0),
    `7-2`  = c(3.0,  0.0)
  )

  monthly_maps <- list("MAP3.2010" = mock_wide)

  units_mo <- data.table::data.table(
    uID      = c("7.1", "7.2"),
    year     = c(2010L, 2010L),
    month    = c(3L, 3L),
    SO2.tons = c(2.0, 5.0)
  )

  expect_warning(
    result <- calculate_exposure(
      year.E        = 2010,
      year.D        = 2010,
      link.to       = "zips",
      pollutant     = "SO2.tons",
      units.mo      = units_mo,
      monthly_maps  = monthly_maps,
      exp_dir       = tmp_exp,
      source.agg    = "total",
      time.agg      = "year",
      allow.partial = TRUE
    ),
    "Missing.*of 12 monthly maps"
  )

  data.table::setorder(result, ZIP)

  # ZIP 11201: (10*2) + (3*5) = 20 + 15 = 35
  # ZIP 60601: (20*2) + (0*5) = 40 + 0  = 40
  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(35.0, 40.0), tolerance = 1e-10)
})
