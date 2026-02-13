# Tests for cross-platform path/filename portability

test_that("ID path component validator enforces portability rules", {
  expect_equal(
    disperseR:::.disperseR_validate_id_component("UNIT_100", "unit$ID"),
    "UNIT_100"
  )

  expect_error(
    disperseR:::.disperseR_validate_id_component("bad/name", "unit$ID"),
    "path separators"
  )
  expect_error(
    disperseR:::.disperseR_validate_id_component("bad:name", "unit$ID"),
    "not portable in filenames"
  )
})

test_that("regex escaping matches literal IDs with metacharacters", {
  id <- "A+B(1)"
  escaped <- disperseR:::.disperseR_escape_regex(id)
  files <- c("hyspdisp_A+B(1)_2005-01-01_00.fst", "hyspdisp_AB1_2005-01-01_00.fst")

  matched <- grep(paste0("hyspdisp_", escaped, "_2005-01-01_00\\.fst$"), files, value = TRUE)
  expect_equal(matched, "hyspdisp_A+B(1)_2005-01-01_00.fst")
})

test_that("run_disperser_parallel fails fast on non-portable IDs", {
  skip_if_not_installed("data.table")

  input_refs <- data.table::data.table(
    ID = "bad/name",
    Latitude = 39.9,
    Longitude = -75.1,
    Height = 100,
    start_day = as.Date("2005-06-15"),
    start_hour = 0L,
    duration_emiss_hours = 1,
    duration_run_hours = 1
  )

  expect_error(
    disperseR::run_disperser_parallel(
      input.refs = input_refs,
      proc_dir = tempdir(),
      hysp_dir = tempdir(),
      meteo_dir = tempdir(),
      mc.cores = 1
    ),
    "path separators"
  )
})

test_that("linking helpers reject non-portable unit IDs before IO", {
  skip_if_not_installed("data.table")

  unit_bad <- data.table::data.table(ID = "bad/name")

  expect_error(
    disperseR::disperser_link_grids(
      month_YYYYMM = "200506",
      unit = unit_bad,
      pbl. = FALSE
    ),
    "path separators"
  )
})
