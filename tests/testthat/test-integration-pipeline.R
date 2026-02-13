# tests/testthat/test-integration-pipeline.R
# Minimal integration test exercising multiple pipeline steps end-to-end.

test_that("integration: create_dirs + define_inputs produces correct pipeline output", {
  # Use a unique tempdir for this test
  tmp_base <- file.path(tempdir(), paste0("disperseR_integ_", Sys.getpid()))
  on.exit(unlink(tmp_base, recursive = TRUE), add = TRUE)

  # Step 1: create_dirs() with a tempdir location
  dirs <- create_dirs(location = tmp_base)

  expect_type(dirs, "list")
  expect_true("main_dir" %in% names(dirs))
  expect_true("hysp_dir" %in% names(dirs))
  expect_true("ziplink_dir" %in% names(dirs))
  expect_true("exp_dir" %in% names(dirs))
  expect_true(dir.exists(dirs$main_dir))
  expect_true(dir.exists(dirs$ziplink_dir))

  # Step 2: define_inputs() with a small units subset
  data("units", package = "disperseR")
  small_units <- units[units$year == 2005, ][1:2]  # 2 units

  inputs <- define_inputs(
    units       = small_units,
    startday    = "2005-01-01",
    endday      = "2005-01-03",
    start.hours = c(0, 12),
    duration    = 120
  )

  # Verify shape: 2 units * 3 days * 2 hours = 12 rows
  expect_s3_class(inputs, "data.table")
  expect_equal(nrow(inputs), 2 * 3 * 2)

  # Verify all expected columns exist
  expected_cols <- c("ID", "Latitude", "Longitude", "Height",
                     "start_day", "start_hour",
                     "duration_emiss_hours", "duration_run_hours",
                     "year", "uID")
  for (col in expected_cols) {
    expect_true(col %in% names(inputs),
                info = paste("Missing column:", col))
  }

  # Verify the units match
  expect_setequal(unique(inputs$ID), small_units$ID)
  expect_true(all(inputs$year == 2005))
  expect_true(all(inputs$start_hour %in% c(0, 12)))
})

test_that("integration: mock .fst files + combine_monthly_links round-trips", {
  tmp_base <- file.path(tempdir(), paste0("disperseR_integ_cml_", Sys.getpid()))
  on.exit(unlink(tmp_base, recursive = TRUE), add = TRUE)

  # Create directory structure
  dirs <- create_dirs(location = tmp_base)
  ziplink_dir <- dirs$ziplink_dir
  rdata_dir   <- dirs$rdata_dir

  # Create mock .fst files mimicking the output of link_all_units()
  # File naming convention: ziplinks_{unitID}_{YYYY}-{MM}.fst
  # Each .fst contains columns: ZIP, N, month, ID, Pdate
  #
  # Unit "3136-1" for January 2005
  mock_unit1_jan <- data.table::data.table(
    ZIP   = c("02101", "10001", "90210"),
    N     = c(5.0, 3.0, 1.0),
    month = c("01", "01", "01"),
    ID    = c("3136-1", "3136-1", "3136-1"),
    Pdate = as.Date(c("2005-01-15", "2005-01-15", "2005-01-15"))
  )

  # Unit "3136-2" for January 2005
  mock_unit2_jan <- data.table::data.table(
    ZIP   = c("02101", "10001"),
    N     = c(2.0, 4.0),
    month = c("01", "01"),
    ID    = c("3136-2", "3136-2"),
    Pdate = as.Date(c("2005-01-15", "2005-01-15"))
  )

  # Write the mock .fst files
  fst::write.fst(mock_unit1_jan,
                 file.path(ziplink_dir, "ziplinks_3136-1_2005-01.fst"))
  fst::write.fst(mock_unit2_jan,
                 file.path(ziplink_dir, "ziplinks_3136-2_2005-01.fst"))

  # Call combine_monthly_links with the mock data
  month_YYYYMMs <- "200501"

  result <- combine_monthly_links(
    month_YYYYMMs = month_YYYYMMs,
    link.to       = "zips",
    ziplink_dir   = ziplink_dir,
    rdata_dir     = rdata_dir
  )

  # Verify the returned list structure

  expect_type(result, "list")
  expect_true("MAP1.2005" %in% names(result))

  map_dt <- result[["MAP1.2005"]]
  expect_s3_class(map_dt, "data.table")

  # The wide table should have ZIP as a column plus one column per unit
  expect_true("ZIP" %in% names(map_dt))

  # Unit IDs should appear as column names (after dcast)
  unit_cols <- setdiff(names(map_dt), "ZIP")
  expect_true(length(unit_cols) >= 2)

  # Verify the RData file was written
  rda_path <- file.path(rdata_dir, "hyads_unwgted_zips.RData")
  expect_true(file.exists(rda_path))
})

test_that("integration: full pipeline from define_inputs through calculate_exposure", {
  tmp_base <- file.path(tempdir(), paste0("disperseR_integ_full_", Sys.getpid()))
  on.exit(unlink(tmp_base, recursive = TRUE), add = TRUE)

  # Step 1: create_dirs
  dirs <- create_dirs(location = tmp_base)

  # Step 2: define_inputs
  data("units", package = "disperseR")
  one_unit <- units[units$year == 2005, ][1]

  inputs <- define_inputs(
    units       = one_unit,
    startday    = "2005-03-01",
    endday      = "2005-03-01",
    start.hours = 0,
    duration    = 120
  )
  expect_equal(nrow(inputs), 1L)

  # Step 3: Build mock monthly_maps (simulating combine_monthly_links output)
  unit_id <- one_unit$uID  # e.g., "7.1"
  mock_wide <- data.table::data.table(
    ZIP = c("02101", "10001")
  )
  mock_wide[, (unit_id) := c(4.0, 6.0)]

  monthly_maps <- stats::setNames(
    list(mock_wide),
    paste0("MAP3.2005")
  )

  # Step 4: Build mock units.mo
  units_mo <- data.table::data.table(
    uID      = unit_id,
    year     = 2005L,
    month    = 3L,
    SO2.tons = 100.0
  )

  # Step 5: calculate_exposure
  result <- calculate_exposure(
    year.E        = 2005,
    year.D        = 2005,
    link.to       = "zips",
    pollutant     = "SO2.tons",
    units.mo      = units_mo,
    monthly_maps  = monthly_maps,
    exp_dir       = dirs$exp_dir,
    source.agg    = "total",
    time.agg      = "year",
    allow.partial = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_true("hyads" %in% names(result))
  expect_true("ZIP" %in% names(result))
  expect_equal(nrow(result), 2L)

  data.table::setorder(result, ZIP)
  # ZIP 02101: 4.0 * 100.0 = 400
  # ZIP 10001: 6.0 * 100.0 = 600
  expect_equal(result$hyads, c(400.0, 600.0), tolerance = 1e-10)
})
