if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Smoke: core utils", {
  os <- disperseR::get_os()
  verify_expect(os %in% c("win", "mac", "unix"), "get_os() returned unexpected value")

  ym <- disperseR::get_yearmon(start.year = "2005", start.month = "01",
                               end.year = "2005", end.month = "03")
  verify_expect(identical(ym, c("200501", "200502", "200503")), "get_yearmon() mismatch")

  td <- file.path(tempdir(), "disperseR_smoke")
  dir.create(td, showWarnings = FALSE)
  utils::write.table(data.frame(1, 0, 0, 10), file.path(td, "GIS_part_001_ps.csv"),
                     row.names = FALSE, col.names = FALSE, sep = ",")
  utils::write.table(data.frame(1, 0, 0, 10), file.path(td, "GIS_part_042_ps.csv"),
                     row.names = FALSE, col.names = FALSE, sep = ",")
  disp <- disperseR::dispersion_read(td)
  verify_expect(all(disp$hour %in% c(1L, 42L)), "dispersion_read() hour parse failed")

  model <- disperseR::create_disp_model()
  model <- disperseR::add_params(
    model,
    lat = 39.9,
    lon = -75.1,
    height = 100,
    duration = 1,
    start_day = "2005-01-01",
    start_hour = 0,
    met_type = "reanalysis",
    met_dir = tempdir()
  )
  model <- disperseR::add_emissions(
    model,
    rate = 1,
    duration = 1,
    start_day = "2005-01-01",
    start_hour = 0
  )
  model <- disperseR::add_species(
    model,
    name = "so2",
    pdiam = 0,
    density = 0,
    shape_factor = 0,
    ddep_vel = 0.002
  )
  model <- disperseR::add_grid(model, range = c(0.5, 0.5), division = c(0.1, 0.1))
  verify_expect(inherits(model, "disp_model"), "create_disp_model() did not return disp_model")

  mock_maps <- list("MAP1.2005" = data.table::data.table(ZIP = "12345", u1 = 1))
  mock_units <- data.table::data.table(uID = "u1", year = 2005, month = 1, `SO2..tons.` = 10)
  exp_dir <- file.path(verify_state$config$base_dir, "exp_smoke")
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  out <- disperseR::calculate_exposure(2005, 2005, monthly_maps = mock_maps,
                                       units.mo = mock_units, time.agg = "year",
                                       exp_dir = exp_dir,
                                       allow.partial = TRUE)
  verify_expect("hyads" %in% names(out), "calculate_exposure() missing hyads")

  out_mo <- disperseR::calculate_exposure(2005, 2005, monthly_maps = mock_maps,
                                          units.mo = mock_units, time.agg = "month",
                                          exp_dir = exp_dir,
                                          return.monthly.data = TRUE,
                                          allow.partial = TRUE)
  verify_expect(all(nchar(as.character(out_mo$yearmonth)) == 6), "yearmonth not formatted as YYYYMM")

  pts <- data.table::data.table(lon = c(-75.1, -75.2), lat = c(39.9, 40.0),
                                height = c(10, 12), Pdate = as.Date("2005-01-01"), hour = c(1, 2))
  grid <- disperseR::link_to(d = pts, link.to = "grids",
                             p4string = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m",
                             rasterin = NULL, pbl. = FALSE)
  verify_expect(all(c("x", "y", "N") %in% names(grid)), "link_to(grids) missing columns")

  invisible(TRUE)
})
