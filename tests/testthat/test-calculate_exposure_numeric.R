# tests/testthat/test-calculate_exposure_numeric.R
# Numeric correctness and regression test for calculate_exposure()
#
# This is a REGRESSION TEST for the ID-mangling bug: unit IDs containing
# hyphens (e.g., "3136-1") must survive the melt/merge pipeline.

test_that("calculate_exposure produces correct numeric values with known inputs", {
  # Use a tempdir for output so the test is self-contained
  tmp_exp <- file.path(tempdir(), "test_calc_exp_numeric")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # --- Build a mock monthly_maps list for January (month 1) of year 2005 ---
  # The wide data.table has ZIP as the ID column, and unit IDs as value columns.
  # Here we use a hyphenated unit ID "3136-1" to exercise the ID-mangling path.

  # Two ZIPs, one unit
  mock_wide <- data.table::data.table(
    ZIP  = c("02101", "10001"),
    `3136-1` = c(5.0, 3.0)   # N values: particle counts reaching each ZIP
  )

  # The monthly_maps list is keyed as "MAP{month}.{year}"
  monthly_maps <- list(
    "MAP1.2005" = mock_wide
  )

  # --- Build a mock units.mo data.table ---
  # Must contain: uID, year, month, SO2.tons
  # The uID should match the column name in monthly_maps exactly: "3136-1"
  units_mo <- data.table::data.table(
    uID       = "3136-1",
    year      = 2005L,
    month     = 1L,
    SO2.tons  = 10.0
  )

  # --- Call calculate_exposure ---
  # time.agg = "year", source.agg = "total", allow.partial = TRUE (only 1 of 12 months)
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
  )

  expect_s3_class(result, "data.table")

  # Expected calculation:
  #   ZIP 02101: hyads = N * SO2.tons = 5.0 * 10.0 = 50.0
  #   ZIP 10001: hyads = N * SO2.tons = 3.0 * 10.0 = 30.0
  expect_true("hyads" %in% names(result))
  expect_true("ZIP" %in% names(result))

  # Sort by ZIP for deterministic comparison
  data.table::setorder(result, ZIP)

  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(50.0, 30.0), tolerance = 1e-10)
  expect_equal(result$ZIP, c("02101", "10001"))
})

test_that("calculate_exposure works with multiple units and sums correctly", {
  tmp_exp <- file.path(tempdir(), "test_calc_exp_multi")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # Two units, two ZIPs, two months (January, February)
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

  units_mo <- data.table::data.table(
    uID      = c("3136-1", "3136-1", "3136-2", "3136-2"),
    year     = c(2005L, 2005L, 2005L, 2005L),
    month    = c(1L, 2L, 1L, 2L),
    SO2.tons = c(10.0, 20.0, 5.0, 15.0)
  )

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

test_that("calculate_exposure regression: hyphenated ID survives melt/merge", {
  # Specifically tests that unit IDs with hyphens (e.g., "3136-1")
  # are not mangled by data.table::melt or column name handling.

  tmp_exp <- file.path(tempdir(), "test_calc_exp_hyphen")
  dir.create(tmp_exp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_exp, recursive = TRUE), add = TRUE)

  # Use a deliberately awkward hyphenated ID
  mock_wide <- data.table::data.table(
    ZIP      = "90210",
    `99-42`  = 7.0
  )

  monthly_maps <- list("MAP6.2005" = mock_wide)

  units_mo <- data.table::data.table(
    uID      = "99-42",
    year     = 2005L,
    month    = 6L,
    SO2.tons = 3.0
  )

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

  units_mo <- data.table::data.table(
    uID      = c("3136-1", "3136-1"),
    year     = c(2005L, 2005L),
    month    = c(1L, 2L),
    SO2.tons = c(10.0, 20.0)
  )

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
  )

  data.table::setorder(result, yearmonth)

  # Jan: 5.0 * 10.0 = 50.0
  # Feb: 8.0 * 20.0 = 160.0
  expect_equal(nrow(result), 2L)
  expect_equal(result$hyads, c(50.0, 160.0), tolerance = 1e-10)
})
