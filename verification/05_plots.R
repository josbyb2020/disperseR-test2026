if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Plots: smoke render", {
  dirs <- verify_state$dirs

  data.ranked <- data.table::data.table(
    uID = c("u1", "u2"),
    hyads.py.sum = c(10, 5),
    hyads.rank = c(1, 2),
    year = c(2005, 2005)
  )
  data.units <- data.table::data.table(
    uID = c("u1", "u2"),
    Longitude = c(-75.1, -75.2),
    Latitude = c(39.9, 40.0),
    SOx = c(1, 2),
    year = c(2005, 2005)
  )

  plots_ranked <- disperseR::plot_units_ranked(
    data.ranked = data.ranked,
    data.units = data.units,
    year = 2005,
    graph.dir = dirs$graph_dir
  )
  verify_expect(is.list(plots_ranked), "plot_units_ranked() did not return list")

  data.linked <- data.table::data.table(
    ZIP = c("19104", "19104"),
    uID = c("u1", "u2"),
    hyads = c(0.1, 0.2),
    yearmonth = c("200501", "200501")
  )
  plots_unit <- disperseR::plot_impact_unit(
    data.linked = data.linked,
    zip.codes = "19104",
    link.to = "zips",
    plot.type = "unit"
  )
  verify_expect(is.list(plots_unit), "plot_impact_unit() did not return list")

  invisible(TRUE)
})

