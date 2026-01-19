if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("HYSPLIT: smoke run", {
  cfg <- verify_state$config
  if (!isTRUE(cfg$run_hysplit)) {
    return(verify_skip("VERIFY_RUN_HYSPLIT is FALSE"))
  }

  dirs <- verify_state$dirs
  met_dir <- cfg$met_dir
  if (is.null(met_dir) || !nzchar(met_dir)) {
    met_dir <- dirs$meteo_dir
  }
  if (!dir.exists(met_dir)) {
    return(verify_skip("met_dir does not exist; set VERIFY_MET_DIR"))
  }

  splitr_ok <- requireNamespace("splitr", quietly = TRUE) ||
    requireNamespace("SplitR", quietly = TRUE)
  if (is.null(cfg$binary_path) && !splitr_ok) {
    return(verify_skip("No HYSPLIT binaries and splitr not installed"))
  }

  run_dir <- file.path(cfg$base_dir, "hysplit_smoke")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  disp <- disperseR::hysplit_dispersion(
    lat = 39.9,
    lon = -75.1,
    height = 100,
    duration = 1,
    start_day = "2005-01-01",
    start_hour = 0,
    direction = "forward",
    met_type = "reanalysis",
    met_dir = met_dir,
    vert_motion = 0,
    model_height = 20000,
    particle_num = 10,
    particle_max = 1000,
    return_disp_df = TRUE,
    write_disp_CSV = FALSE,
    run_dir = run_dir,
    binary_path = cfg$binary_path,
    parhplot_path = cfg$parhplot_path
  )
  verify_expect(is.data.frame(disp) && nrow(disp) > 0, "hysplit_dispersion() returned empty")

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
    met_dir = met_dir,
    binary_path = cfg$binary_path,
    parhplot_path = cfg$parhplot_path
  )
  model <- disperseR::add_emissions(model, rate = 1, duration = 1, start_day = "2005-01-01", start_hour = 0)
  model <- disperseR::add_species(model, name = "so2", pdiam = 0, density = 0,
                                 shape_factor = 0, ddep_vel = 0.002)
  model <- disperseR::add_grid(model, range = c(0.5, 0.5), division = c(0.1, 0.1))

  run_dir2 <- file.path(cfg$base_dir, "hysplit_smoke_model")
  dir.create(run_dir2, recursive = TRUE, showWarnings = FALSE)
  model <- disperseR::run_model(model, npart = 10, run.dir = run_dir2)
  verify_expect(is.data.frame(model$disp_df) && nrow(model$disp_df) > 0, "run_model() returned empty")

  invisible(TRUE)
})

