if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Smoke: core utils", {
  ym <- disperseR::get_yearmon(start.year = "2005", start.month = "01",
                               end.year = "2005", end.month = "03")
  verify_expect(identical(ym, c("200501", "200502", "200503")), "get_yearmon() mismatch")

  dirs <- disperseR::create_dirs(file.path(tempdir(), "disperseR_smoke"))
  verify_expect(dir.exists(dirs$main_dir), "create_dirs() did not create main_dir")

  units_dt <- data.table::as.data.table(disperseR::units)
  year_pick <- if (2005L %in% units_dt$year) 2005L else as.integer(units_dt$year[[1]])
  units_one <- units_dt[year == year_pick][1]
  smoke_day <- sprintf("%04d-01-15", year_pick)
  refs <- disperseR::define_inputs(
    units = units_one,
    startday = smoke_day,
    endday = smoke_day,
    start.hours = c(0, 12),
    duration = 24
  )
  verify_expect(nrow(refs) == 2, "define_inputs() smoke run did not return expected rows")

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

  map_names <- sprintf("MAP%d.%d", 1:12, year_pick)
  mock_maps <- stats::setNames(
    lapply(1:12, function(m) data.table::data.table(ZIP = "12345", u1 = as.numeric(m))),
    map_names
  )
  mock_units <- data.table::data.table(
    uID = "u1",
    year = year_pick,
    month = 1:12,
    `SO2.tons` = rep(10, 12)
  )
  exp_dir <- file.path(verify_state$config$base_dir, "exp_smoke")
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  out <- disperseR::calculate_exposure(year_pick, year_pick, monthly_maps = mock_maps,
                                       units.mo = mock_units, time.agg = "year",
                                       exp_dir = exp_dir,
                                       allow.partial = TRUE)
  verify_expect("hyads" %in% names(out), "calculate_exposure() missing hyads")

  out_mo <- disperseR::calculate_exposure(year_pick, year_pick, monthly_maps = mock_maps,
                                          units.mo = mock_units, time.agg = "month",
                                          exp_dir = exp_dir,
                                          return.monthly.data = TRUE,
                                          allow.partial = TRUE)
  verify_expect(all(nchar(as.character(out_mo$yearmonth)) == 6), "yearmonth not formatted as YYYYMM")

  invisible(TRUE)
})
