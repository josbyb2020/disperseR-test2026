if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Data: downloads and caching", {
  cfg <- verify_state$config
  if (!isTRUE(cfg$data_download)) {
    return(verify_skip("VERIFY_DATA_DOWNLOAD is FALSE"))
  }

  dirs <- verify_state$dirs

  crosswalk <- disperseR::get_data("crosswalk")
  verify_expect(nrow(crosswalk) > 0, "crosswalk data not loaded")

  zcta <- disperseR::get_data("zctashapefile")
  verify_expect(inherits(zcta, "sf"), "zcta shapefile did not load")

  pbl <- disperseR::get_data("pblheight")
  verify_expect(inherits(pbl, "SpatRaster"), "pblheight did not load")

  if (isTRUE(cfg$download_met)) {
    files <- c("RP200501.gbl")
    res <- disperseR::get_met_reanalysis(files = files, path_met_files = dirs$meteo_dir)
    verify_expect(length(res$failed) == 0, "get_met_reanalysis() failed")
  }

  invisible(TRUE)
})

