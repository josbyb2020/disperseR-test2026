if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

cfg <- verify_read_config()
pkg_root <- verify_find_pkg_root()
verify_load_package(pkg_root)

perf_dir <- file.path(cfg$base_dir, "perf")
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

run_with_env <- function(expr, env = list()) {
  keys <- names(env)
  old <- setNames(vector("list", length(keys)), keys)
  for (k in keys) {
    old[[k]] <- Sys.getenv(k, unset = NA_character_)
    Sys.setenv(structure(as.character(env[[k]]), names = k))
  }
  on.exit({
    for (k in keys) {
      if (is.na(old[[k]])) {
        Sys.unsetenv(k)
      } else {
        Sys.setenv(structure(old[[k]], names = k))
      }
    }
  }, add = TRUE)
  force(expr)
}

run_platform_suite <- function() {
  os_name <- Sys.info()[["sysname"]]
  r_ver <- paste(R.version$major, R.version$minor, sep = ".")
  message("Running cross-platform perf suite on ", os_name, " / R ", r_ver)

  dt_threads <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_PARALLEL_DT_THREADS", "1")))
  if (!is.finite(dt_threads) || is.na(dt_threads) || dt_threads < 1) {
    dt_threads <- 1L
  }
  options(disperseR.parallel.dt.threads = dt_threads)

  project_threshold <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_FAST_PROJECT_MIN_ROWS", "50000")))
  if (!is.finite(project_threshold) || is.na(project_threshold) || project_threshold < 1) {
    project_threshold <- 50000L
  }
  project_enable <- tolower(Sys.getenv("DISPERSER_FAST_PROJECT_ENABLE", "false")) %in% c("1", "true", "yes")
  options(disperseR.fast.project.min_rows = project_threshold)
  options(disperseR.fast.project.enable = project_enable)

  if (!nzchar(Sys.getenv("DISPERSER_BENCH_PARTICLES"))) Sys.setenv(DISPERSER_BENCH_PARTICLES = "200000")
  if (!nzchar(Sys.getenv("DISPERSER_BENCH_GRID_X"))) Sys.setenv(DISPERSER_BENCH_GRID_X = "40")
  if (!nzchar(Sys.getenv("DISPERSER_BENCH_GRID_Y"))) Sys.setenv(DISPERSER_BENCH_GRID_Y = "20")
  source("06_perf_linking_engine.R")

  if (!nzchar(Sys.getenv("DISPERSER_USERFLOW_PROFILE"))) Sys.setenv(DISPERSER_USERFLOW_PROFILE = "full")
  source("07_perf_user_flow_linking.R")

  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_UNITS"))) Sys.setenv(DISPERSER_HEAVY_UNITS = "8")
  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_MONTHS"))) Sys.setenv(DISPERSER_HEAVY_MONTHS = "2")
  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_ROWS"))) Sys.setenv(DISPERSER_HEAVY_ROWS = "200000")
  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_GRID_X"))) Sys.setenv(DISPERSER_HEAVY_GRID_X = "120")
  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_GRID_Y"))) Sys.setenv(DISPERSER_HEAVY_GRID_Y = "100")
  if (!nzchar(Sys.getenv("DISPERSER_HEAVY_CORES"))) {
    cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (!is.finite(cores) || is.na(cores) || cores < 1) cores <- 1L
    Sys.setenv(DISPERSER_HEAVY_CORES = as.character(max(1L, min(4L, as.integer(cores)))))
  }
  source("08_perf_user_heavy_linking.R")

  files <- c(
    file.path(perf_dir, "linking_engine_benchmark.csv"),
    file.path(perf_dir, "userflow_link_all_units_smoke.csv"),
    file.path(perf_dir, "userflow_link_all_units_full.csv"),
    file.path(perf_dir, "heavy_user_flow_link_all_units.csv")
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) {
    stop("No benchmark outputs found in ", perf_dir, call. = FALSE)
  }

  rows <- lapply(files, function(path) {
    dt <- data.table::fread(path)
    dt[, source_file := basename(path)]
    dt
  })
  summary_dt <- data.table::rbindlist(rows, fill = TRUE)
  summary_dt[, os := os_name]
  summary_dt[, r_version := r_ver]
  summary_dt[, generated_at_utc := as.character(format(Sys.time(), tz = "UTC", usetz = TRUE))]

  out <- file.path(perf_dir, "crossplatform_ci_summary.csv")
  data.table::fwrite(summary_dt, out)
  message("Cross-platform summary written to: ", out)
  print(summary_dt)
  invisible(summary_dt)
}

verify_step("crossplatform_perf_suite", {
  run_platform_suite()
})

verify_stop_on_failures("09_perf_crossplatform_ci")
