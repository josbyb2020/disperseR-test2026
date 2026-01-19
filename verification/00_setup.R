if (!exists("verify_state")) {
  source("helpers.R")
}

verify_step("Setup", {
  cfg <- verify_read_config()
  verify_state$config <- cfg

  pkg_root <- verify_find_pkg_root()
  verify_state$pkg_root <- pkg_root

  verify_load_package(pkg_root)

  dirs <- disperseR::create_dirs(location = cfg$base_dir)
  verify_state$dirs <- dirs

  options(disperseR.mc.cores = 1L)

  verify_expect(dir.exists(dirs$hysp_dir), "hysp_dir was not created")
  verify_expect(dir.exists(dirs$ziplink_dir), "ziplink_dir was not created")

  invisible(TRUE)
})

