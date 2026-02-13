# Tests for validate_pipeline()

test_that("validate_pipeline reports zip/county/grid link counts", {
  skip_if_not_installed("fst")
  skip_if_not_installed("data.table")

  tmp_loc <- file.path(tempdir(), paste0("disperseR_test_validate_", Sys.getpid()))
  on.exit(unlink(tmp_loc, recursive = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(location = tmp_loc)
  dir.create(file.path(dirs$hysp_dir, "2005", "01"), recursive = TRUE, showWarnings = FALSE)

  fst::write.fst(
    data.table::data.table(
      lon = -75,
      lat = 40,
      height = 100,
      Pdate = as.Date("2005-01-01"),
      hour = 2
    ),
    file.path(dirs$hysp_dir, "2005", "01", "hyspdisp_7-1_2005-01-01_00.fst")
  )

  fst::write.fst(
    data.table::data.table(ZIP = "12345", N = 1, month = "200501", ID = "7-1"),
    file.path(dirs$ziplink_dir, "ziplinks_7-1_2005-01-01_2005-01-31.fst")
  )
  fst::write.fst(
    data.table::data.table(
      statefp = "01", countyfp = "001", state_name = "X", name = "Y", geoid = "01001", N = 1,
      month = "200501", ID = "7-1"
    ),
    file.path(dirs$ziplink_dir, "countylinks_7-1_2005-01-01_2005-01-31.fst")
  )
  fst::write.fst(
    data.table::data.table(x = 1000, y = 2000, N = 1, month = "200501", ID = "7-1"),
    file.path(dirs$ziplink_dir, "gridlinks_7-1_2005-01-01_2005-01-31.fst")
  )

  fst::write.fst(
    data.table::data.table(ZIP = "12345", hyads = 10),
    file.path(dirs$exp_dir, "zips_exposures_total_2005.fst")
  )

  out <- disperseR::validate_pipeline(dirs = dirs, compute_unique_zips = TRUE, verbose = FALSE)

  expect_equal(out$runs$files, 1)
  expect_equal(out$links$zips$files, 1)
  expect_equal(out$links$counties$files, 1)
  expect_equal(out$links$grids$files, 1)
  expect_equal(out$ziplinks$unique_zips, 1)
  expect_equal(out$exposures$files, 1)
})
