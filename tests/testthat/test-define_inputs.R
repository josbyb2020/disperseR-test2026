# tests/testthat/test-define_inputs.R
# Critical test coverage for define_inputs()

# Use disperseR::units directly to avoid name collision with units package
# (sf imports units package, whose units() function shadows the dataset)

test_that("happy path: 1 unit, 1-day range produces correct data.table structure", {

  unit_data <- disperseR::units
  # Filter to 1 unit in year 2005 to keep test fast

  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  result <- define_inputs(
    units      = one_unit,
    startday   = "2005-06-01",
    endday     = "2005-06-01",
    start.hours = c(0, 6, 12, 18),
    duration   = 240
  )

  expect_s3_class(result, "data.table")

  expected_cols <- c("ID", "Latitude", "Longitude", "Height",
                     "start_day", "start_hour",
                     "duration_emiss_hours", "duration_run_hours",
                     "year", "uID")
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                info = paste("Missing column:", col))
  }
})

test_that("happy path: row count matches days * length(start.hours)", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  # 3 days, 4 start hours => 12 rows for 1 unit

  result <- define_inputs(
    units      = one_unit,
    startday   = "2005-06-01",
    endday     = "2005-06-03",
    start.hours = c(0, 6, 12, 18),
    duration   = 240
  )

  n_days <- as.integer(as.Date("2005-06-03") - as.Date("2005-06-01")) + 1L
  n_hours <- 4L
  expected_rows <- n_days * n_hours * 1L  # 1 unit


  expect_equal(nrow(result), expected_rows)
})

test_that("edge case: single day (startday == endday), single start.hour", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]


  result <- define_inputs(
    units      = one_unit,
    startday   = "2005-06-15",
    endday     = "2005-06-15",
    start.hours = 12,
    duration   = 120
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$start_hour, 12)
  expect_equal(result$duration_run_hours, 120)
  expect_equal(result$duration_emiss_hours, 1)
  expect_equal(as.character(result$start_day), "2005-06-15")
})

test_that("error: missing units argument", {
  expect_error(
    define_inputs(startday = "2005-01-01", endday = "2005-01-31"),
    "units.*required"
  )
})

test_that("error: empty units (0 rows)", {
  unit_data <- disperseR::units
  empty <- unit_data[0, ]

  expect_error(
    define_inputs(
      units    = empty,
      startday = "2005-01-01",
      endday   = "2005-01-31"
    ),
    "no rows"
  )
})

test_that("error: missing required columns in units (ID, year)", {
  bad_df <- data.frame(name = "test", lat = 40, lon = -90)

  expect_error(
    define_inputs(
      units    = bad_df,
      startday = "2005-01-01",
      endday   = "2005-01-31"
    ),
    "missing required columns"
  )
})

test_that("error: unparseable date string for startday", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units    = one_unit,
      startday = "not-a-date",
      endday   = "2005-01-31"
    ),
    "Cannot parse.*startday"
  )
})

test_that("error: endday before startday", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units    = one_unit,
      startday = "2005-06-15",
      endday   = "2005-06-01"
    ),
    "endday.*before.*startday"
  )
})

test_that("error: invalid start.hours (negative)", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units       = one_unit,
      startday    = "2005-06-01",
      endday      = "2005-06-01",
      start.hours = c(-1, 6)
    ),
    "start.hours.*between 0 and 23"
  )
})

test_that("error: invalid start.hours (> 23)", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units       = one_unit,
      startday    = "2005-06-01",
      endday      = "2005-06-01",
      start.hours = c(0, 25)
    ),
    "start.hours.*between 0 and 23"
  )
})

test_that("error: invalid duration (0)", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units    = one_unit,
      startday = "2005-06-01",
      endday   = "2005-06-01",
      duration = 0
    ),
    "duration.*positive"
  )
})

test_that("error: invalid duration (negative)", {
  unit_data <- disperseR::units
  one_unit <- data.table::as.data.table(unit_data)[unit_data$year == 2005, ][1]

  expect_error(
    define_inputs(
      units    = one_unit,
      startday = "2005-06-01",
      endday   = "2005-06-01",
      duration = -10
    ),
    "duration.*positive"
  )
})
