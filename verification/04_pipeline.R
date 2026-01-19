if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Pipeline: run -> link -> combine -> exposure", {
  cfg <- verify_state$config
  if (!isTRUE(cfg$run_hysplit)) {
    return(verify_skip("VERIFY_RUN_HYSPLIT is FALSE"))
  }
  if (!isTRUE(cfg$data_download)) {
    return(verify_skip("VERIFY_DATA_DOWNLOAD is FALSE (needs zcta/crosswalk)"))
  }

  dirs <- verify_state$dirs
  met_dir <- cfg$met_dir
  if (is.null(met_dir) || !nzchar(met_dir)) {
    met_dir <- dirs$meteo_dir
  }
  if (!dir.exists(met_dir)) {
    return(verify_skip("met_dir does not exist; set VERIFY_MET_DIR"))
  }

  # Ensure required spatial data is cached
  zcta <- disperseR::get_data("zctashapefile")
  crosswalk <- disperseR::crosswalk
  verify_expect(inherits(zcta, "sf"), "zcta shapefile missing")

  # Minimal input.refs
  input.refs <- data.table::data.table(
    ID = "unit_test",
    uID = "unit_test",
    Latitude = 39.9,
    Longitude = -75.1,
    Height = 100,
    start_day = as.Date("2005-01-01"),
    start_hour = 0,
    duration_emiss_hours = 1,
    duration_run_hours = 1,
    year = "2005"
  )

  disperseR::run_disperser_parallel(
    input.refs = input.refs,
    pbl.height = NULL,
    species = "so2",
    proc_dir = dirs$proc_dir,
    hysp_dir = dirs$hysp_dir,
    meteo_dir = met_dir,
    overwrite = TRUE,
    npart = 10,
    mc.cores = 1,
    keep.hysplit.files = FALSE
  )

  year.mons <- disperseR::get_yearmon("2005", "01", "2005", "01")
  links <- disperseR::link_all_units(
    units.run = input.refs[, .(uID, ID, Latitude, Longitude, year)],
    link.to = "zips",
    year.mons = year.mons,
    pbl_trim = FALSE,
    pbl.height = NULL,
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    overwrite = TRUE,
    return.linked.data = TRUE
  )
  verify_expect(nrow(links) > 0, "link_all_units() returned no data")
  verify_state$pipeline_links <- links
  verify_state$pipeline_units <- input.refs
  verify_state$pipeline_year <- 2005

  monthly_maps <- disperseR::combine_monthly_links(
    month_YYYYMMs = year.mons,
    link.to = "zips",
    ziplink_dir = dirs$ziplink_dir,
    rdata_dir = dirs$rdata_dir
  )
  verify_expect(length(monthly_maps) > 0, "combine_monthly_links() returned empty")

  units.mo <- data.table::data.table(
    uID = "unit_test",
    year = 2005,
    month = 1,
    `SO2..tons.` = 1
  )
  exp <- disperseR::calculate_exposure(
    year.E = 2005,
    year.D = 2005,
    monthly_maps = monthly_maps,
    units.mo = units.mo,
    time.agg = "year",
    allow.partial = TRUE
  )
  verify_expect(nrow(exp) > 0, "calculate_exposure() returned no data")
  verify_state$pipeline_exp <- exp

  invisible(TRUE)
})
