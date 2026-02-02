# Tests for calculate_exposure()
# Focus on parameter validation and logic, not full workflow

# Create minimal mock units.mo data for tests
mock_units_mo <- data.table::data.table(
  uID = "unit1",
  year = 2005,
  month = 1,
  `SO2.tons` = 100
)

test_that("calculate_exposure validates source.agg", {
  skip_on_cran()
  
  mock_maps <- list("MAP1.2005" = data.table::data.table(ZIP = "12345", uID = 1))
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "invalid",
      time.agg = "year",
      monthly_maps = mock_maps,
      units.mo = mock_units_mo
    ),
    "source.agg not recognized"
  )
})

test_that("calculate_exposure validates time.agg", {
  skip_on_cran()
  
  mock_maps <- list("MAP1.2005" = data.table::data.table(ZIP = "12345", uID = 1))
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "total",
      time.agg = "invalid",
      monthly_maps = mock_maps,
      units.mo = mock_units_mo
    ),
    "time.agg not recognized"
  )
})

test_that("calculate_exposure requires monthly_maps or rda_file", {
  skip_on_cran()
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "total",
      time.agg = "year",
      units.mo = mock_units_mo
    ),
    "Either monthly_maps.*or rda_file must be provided"
  )
})

test_that("calculate_exposure validates rda_file existence", {
  skip_on_cran()
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "total",
      time.agg = "year",
      units.mo = mock_units_mo,
      rda_file = "/nonexistent/path/file.RData"
    ),
    "rda_file does not exist"
  )
})

test_that("calculate_exposure accepts valid source.agg values without validation error", {
  skip_on_cran()
  
  mock_maps <- list("MAP1.2005" = data.table::data.table(ZIP = "12345", uID = 1))
  
  for (agg in c("total", "facility", "unit")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = agg,
        time.agg = "year",
        monthly_maps = mock_maps,
        units.mo = mock_units_mo
      ),
      error = function(e) e
    )
    # Should NOT be a source.agg validation error
    if (inherits(result, "error")) {
      expect_false(
        grepl("source.agg not recognized", result$message),
        info = paste("source.agg =", agg, "should be valid")
      )
    }
  }
})

test_that("calculate_exposure accepts valid time.agg values without validation error", {
  skip_on_cran()
  
  mock_maps <- list("MAP1.2005" = data.table::data.table(ZIP = "12345", uID = 1))
  
  for (agg in c("year", "month")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = "total",
        time.agg = agg,
        monthly_maps = mock_maps,
        units.mo = mock_units_mo
      ),
      error = function(e) e
    )
    # Should NOT be a time.agg validation error
    if (inherits(result, "error")) {
      expect_false(
        grepl("time.agg not recognized", result$message),
        info = paste("time.agg =", agg, "should be valid")
      )
    }
  }
})

test_that("calculate_exposure maps legacy SO2..tons. to SO2.tons when needed", {
  skip_on_cran()

  mock_units <- data.table::data.table(
    uID = "unit1",
    year = 2005,
    month = 1,
    `SO2.tons` = 100
  )

  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      pollutant = "SO2..tons.",
      monthly_maps = list(),
      units.mo = mock_units
    ),
    "monthly_maps is empty"
  )
})
