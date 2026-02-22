test_that("plot_impact_unit returns a plot object and saves output", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")

  tmp <- tempfile("disperser_plot_impact_unit_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  data.linked <- data.table::data.table(
    ZIP = c("19104", "19104", "19104", "19104"),
    uID = c("u1", "u1", "u2", "u2"),
    hyads = c(0.1, 0.15, 0.2, 0.22),
    yearmonth = c("200501", "200502", "200501", "200502")
  )

  out <- disperseR::plot_impact_unit(
    data.linked = data.linked,
    zip.codes = "19104",
    graph.dir = tmp
  )

  expect_true(inherits(out, "gtable") || inherits(out, "grob") || is.list(out))
  expect_true(file.exists(file.path(tmp, "plot_impact_unit.pdf")))
})

test_that("plot_impact_unit validates required inputs", {
  skip_if_not_installed("data.table")

  data.linked <- data.table::data.table(
    ZIP = c("19104", "19104"),
    uID = c("u1", "u1"),
    hyads = c(0.1, 0.2),
    yearmonth = c("200501", "200502")
  )

  expect_error(
    disperseR::plot_impact_unit(data.linked = NULL, zip.codes = "19104"),
    "data"
  )
  expect_error(
    disperseR::plot_impact_unit(data.linked = data.linked, zip.codes = NULL),
    "zip"
  )
})
