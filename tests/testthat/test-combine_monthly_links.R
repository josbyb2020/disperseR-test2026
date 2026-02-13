# Tests for combine_monthly_links() robustness with sparse grid links

test_that("combine_monthly_links handles sparse/single-cell grid links", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")

  tmp_loc <- file.path(tempdir(), paste0("disperseR_test_combine_", Sys.getpid()))
  on.exit(unlink(tmp_loc, recursive = TRUE), add = TRUE)

  dirs <- disperseR::create_dirs(location = tmp_loc)

  # Single-cell grid link file (previously triggered terra::rast failures).
  sparse_grid <- data.table::data.table(
    x = 1000,
    y = 2000,
    N = 1,
    month = "200501",
    ID = "7-1"
  )

  fst::write.fst(
    sparse_grid,
    file.path(dirs$ziplink_dir, "gridlinks_7-1_2005-01-01_2005-01-31.fst")
  )

  expect_no_error({
    maps <- disperseR::combine_monthly_links(
      month_YYYYMMs = "200501",
      link.to = "grids",
      ziplink_dir = dirs$ziplink_dir,
      rdata_dir = dirs$rdata_dir
    )
  })

  expect_true(is.list(maps))
  expect_true("MAP1.2005" %in% names(maps))
  expect_true(data.table::is.data.table(maps[["MAP1.2005"]]))
  expect_true(all(c("x", "y") %in% names(maps[["MAP1.2005"]])))
})

test_that("disperser_link_grids returns empty data.table when no source files exist", {
  skip_if_not_installed("data.table")

  tmp_loc <- file.path(tempdir(), paste0("disperseR_test_empty_grid_", Sys.getpid()))
  on.exit(unlink(tmp_loc, recursive = TRUE), add = TRUE)

  disperseR::create_dirs(location = tmp_loc)

  out <- disperseR::disperser_link_grids(
    month_YYYYMM = "200501",
    unit = data.table::data.table(ID = "7-1"),
    duration.run.hours = 24,
    overwrite = TRUE,
    pbl. = FALSE,
    return.linked.data. = TRUE
  )

  expect_true(data.table::is.data.table(out))
  expect_equal(names(out), c("x", "y", "N", "month", "ID"))
  expect_equal(nrow(out), 0)
})
