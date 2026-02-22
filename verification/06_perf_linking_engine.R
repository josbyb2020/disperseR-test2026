if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

cfg <- verify_read_config()
pkg_root <- verify_find_pkg_root()
verify_load_package(pkg_root)

perf_dir <- file.path(cfg$base_dir, "perf")
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

bench_particles <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_BENCH_PARTICLES", "200000")))
if (!is.finite(bench_particles) || is.na(bench_particles) || bench_particles < 1000) {
  bench_particles <- 200000L
}

bench_cells_x <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_BENCH_GRID_X", "40")))
bench_cells_y <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_BENCH_GRID_Y", "20")))
if (!is.finite(bench_cells_x) || is.na(bench_cells_x) || bench_cells_x < 2) {
  bench_cells_x <- 40L
}
if (!is.finite(bench_cells_y) || is.na(bench_cells_y) || bench_cells_y < 2) {
  bench_cells_y <- 20L
}

verify_step("build_synthetic_inputs", {
  set.seed(42)

  particles <- data.table::data.table(
    lon = runif(bench_particles, -124, -67),
    lat = runif(bench_particles, 25, 49),
    height = runif(bench_particles, 10, 1500),
    Pdate = as.Date("2005-01-01"),
    hour = sample.int(239, bench_particles, replace = TRUE) + 1L
  )

  x_breaks <- seq(-124, -67, length.out = bench_cells_x + 1L)
  y_breaks <- seq(25, 49, length.out = bench_cells_y + 1L)

  polygon_list <- vector("list", bench_cells_x * bench_cells_y)
  zcta_vals <- character(length(polygon_list))
  idx <- 1L
  for (ix in seq_len(bench_cells_x)) {
    for (iy in seq_len(bench_cells_y)) {
      x0 <- x_breaks[ix]
      x1 <- x_breaks[ix + 1L]
      y0 <- y_breaks[iy]
      y1 <- y_breaks[iy + 1L]
      polygon_list[[idx]] <- sf::st_polygon(list(rbind(
        c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0)
      )))
      zcta_vals[idx] <- sprintf("%05d", idx)
      idx <- idx + 1L
    }
  }

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = zcta_vals,
    geometry = sf::st_sfc(polygon_list, crs = 4326)
  )

  crosswalk <- data.table::data.table(
    ZCTA = zcta_vals,
    ZIP = zcta_vals
  )

  assign("bench_particles_dt", particles, envir = verify_state)
  assign("bench_zcta_sf", zcta_sf, envir = verify_state)
  assign("bench_crosswalk", crosswalk, envir = verify_state)
  invisible(TRUE)
})

verify_step("benchmark_link_to_legacy_vs_fast", {
  p4 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
  d <- get("bench_particles_dt", envir = verify_state)
  zcta <- get("bench_zcta_sf", envir = verify_state)
  cw <- get("bench_crosswalk", envir = verify_state)

  t_legacy <- system.time({
    out_legacy <- suppressWarnings(disperseR:::link_to(
      d = d,
      link.to = "zips",
      p4string = p4,
      zc = zcta,
      cw = cw,
      pbl. = FALSE,
      res.link. = 12000,
      engine = "legacy"
    ))
  })["elapsed"]

  t_fast <- system.time({
    out_fast <- suppressWarnings(disperseR:::link_to(
      d = d,
      link.to = "zips",
      p4string = p4,
      zc = zcta,
      cw = cw,
      pbl. = FALSE,
      res.link. = 12000,
      engine = "fast"
    ))
  })["elapsed"]

  legacy_summary <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  fast_summary <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = ZIP][order(ZIP)]
  parity <- verify_compare_grouped(
    legacy_dt = legacy_summary,
    fast_dt = fast_summary,
    key_cols = "ZIP",
    value_col = "N",
    tol_abs = 1e-8,
    tol_rel = 1e-8
  )

  verify_expect(
    parity$keys_equal,
    paste0(
      "ZIP keys diverged between legacy and fast engine. ",
      "Missing-key rows: ", parity$n_key_only_rows, "."
    )
  )
  verify_expect(
    parity$values_equal,
    paste0(
      "ZIP exposure values diverged between legacy and fast engine. ",
      "Max abs diff=", signif(parity$max_abs_diff, 6),
      ", max rel diff=", signif(parity$max_rel_diff, 6),
      ", diff rows=", parity$n_diff_rows, "."
    )
  )

  speedup <- as.numeric(t_legacy) / as.numeric(t_fast)
  summary_dt <- data.table::data.table(
    benchmark = "link_to_zips_synthetic",
    particles = nrow(d),
    zcta_polygons = nrow(zcta),
    legacy_elapsed_sec = as.numeric(t_legacy),
    fast_elapsed_sec = as.numeric(t_fast),
    speedup_x = speedup,
    parity_keys = parity$keys_equal,
    parity_values = parity$values_equal,
    parity_ok = parity$parity_ok,
    max_abs_diff = parity$max_abs_diff,
    max_rel_diff = parity$max_rel_diff
  )

  outfile <- file.path(perf_dir, "linking_engine_benchmark.csv")
  data.table::fwrite(summary_dt, outfile)
  message("Benchmark summary written to: ", outfile)
  print(summary_dt)
  invisible(summary_dt)
})

verify_stop_on_failures("06_perf_linking_engine")
