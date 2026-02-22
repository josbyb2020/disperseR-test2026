if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

cfg <- verify_read_config()
pkg_root <- verify_find_pkg_root()
verify_load_package(pkg_root)

perf_dir <- file.path(cfg$base_dir, "perf")
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

heavy_units <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_UNITS", "8")))
heavy_months <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_MONTHS", "2")))
heavy_rows <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_ROWS", "200000")))
heavy_grid_x <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_GRID_X", "120")))
heavy_grid_y <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_GRID_Y", "100")))
heavy_cores <- suppressWarnings(as.integer(Sys.getenv("DISPERSER_HEAVY_CORES", "4")))

if (!is.finite(heavy_units) || is.na(heavy_units) || heavy_units < 1) heavy_units <- 8L
if (!is.finite(heavy_months) || is.na(heavy_months) || heavy_months < 1) heavy_months <- 2L
if (!is.finite(heavy_rows) || is.na(heavy_rows) || heavy_rows < 1000) heavy_rows <- 200000L
if (!is.finite(heavy_grid_x) || is.na(heavy_grid_x) || heavy_grid_x < 2) heavy_grid_x <- 120L
if (!is.finite(heavy_grid_y) || is.na(heavy_grid_y) || heavy_grid_y < 2) heavy_grid_y <- 100L
if (!is.finite(heavy_cores) || is.na(heavy_cores) || heavy_cores < 1) heavy_cores <- 4L

build_polygon_inputs <- function(nx, ny) {
  x_breaks <- seq(-124, -67, length.out = nx + 1L)
  y_breaks <- seq(25, 49, length.out = ny + 1L)
  n_poly <- nx * ny

  polygon_list <- vector("list", n_poly)
  zcta_vals <- character(n_poly)
  statefp <- character(n_poly)
  countyfp <- character(n_poly)
  state_name <- character(n_poly)
  county_name <- character(n_poly)
  geoid <- character(n_poly)

  idx <- 1L
  for (ix in seq_len(nx)) {
    for (iy in seq_len(ny)) {
      x0 <- x_breaks[ix]
      x1 <- x_breaks[ix + 1L]
      y0 <- y_breaks[iy]
      y1 <- y_breaks[iy + 1L]
      polygon_list[[idx]] <- sf::st_polygon(list(rbind(
        c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0)
      )))
      zcta_vals[idx] <- sprintf("%05d", idx)

      st_idx <- (idx - 1L) %/% 1000L + 1L
      cty_idx <- (idx - 1L) %% 1000L
      statefp[idx] <- sprintf("%02d", st_idx)
      countyfp[idx] <- sprintf("%03d", cty_idx)
      state_name[idx] <- paste0("S", statefp[idx])
      county_name[idx] <- paste0("C", idx)
      geoid[idx] <- paste0(statefp[idx], countyfp[idx])
      idx <- idx + 1L
    }
  }

  list(
    zcta = sf::st_sf(ZCTA5CE10 = zcta_vals, geometry = sf::st_sfc(polygon_list, crs = 4326)),
    counties = sf::st_sf(
      statefp = statefp,
      countyfp = countyfp,
      state_name = state_name,
      name = county_name,
      geoid = geoid,
      geometry = sf::st_sfc(polygon_list, crs = 4326)
    ),
    crosswalk = data.table::data.table(ZCTA = zcta_vals, ZIP = zcta_vals)
  )
}

generate_particles <- function(n, seed = 1L, run_date = as.Date("2005-01-15")) {
  set.seed(seed)
  data.table::data.table(
    lon = stats::runif(n, -124, -67),
    lat = stats::runif(n, 25, 49),
    height = stats::runif(n, 10, 1500),
    Pdate = run_date,
    hour = sample.int(23, n, replace = TRUE) + 1L
  )
}

run_heavy_case <- function(link_to = c("zips", "counties")) {
  link_to <- match.arg(link_to)

  case_name <- sprintf("heavy_%s_%du_%dm_%dr", link_to, heavy_units, heavy_months, heavy_rows)
  scenario_root <- file.path(cfg$base_dir, "perf_userflow", case_name)
  if (dir.exists(scenario_root)) {
    unlink(scenario_root, recursive = TRUE, force = TRUE)
  }
  dir.create(scenario_root, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    if (!isTRUE(cfg$keep_artifacts)) {
      unlink(scenario_root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  dirs <- disperseR::create_dirs(scenario_root)
  poly_inputs <- build_polygon_inputs(heavy_grid_x, heavy_grid_y)
  disperseR:::.disperseR_cache_set("zcta", poly_inputs$zcta)

  units.run <- data.table::data.table(
    uID = sprintf("U%03d", seq_len(heavy_units)),
    ID = sprintf("U%03d", seq_len(heavy_units))
  )

  month_seq <- seq.Date(as.Date("2005-01-01"), by = "1 month", length.out = heavy_months)
  year_mons <- format(month_seq, "%Y%m")

  file_seed <- 1L
  for (month in year_mons) {
    year_str <- substr(month, 1, 4)
    mon_str <- substr(month, 5, 6)
    run_date <- as.Date(sprintf("%s-%s-15", year_str, mon_str))
    ymdir <- file.path(dirs$hysp_dir, year_str, mon_str)
    dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)

    for (unit_id in units.run$ID) {
      dt <- generate_particles(heavy_rows, seed = file_seed, run_date = run_date)
      file_seed <- file_seed + 1L
      outfile <- file.path(ymdir, sprintf("hyspdisp_%s_%s_00.fst", unit_id, run_date))
      fst::write.fst(dt, outfile)
    }
  }

  run_engine <- function(engine_name) {
    suppressMessages(suppressWarnings(
      disperseR::link_all_units(
        units.run = units.run,
        link.to = link_to,
        mc.cores = heavy_cores,
        year.mons = year_mons,
        pbl_trim = FALSE,
        crosswalk. = poly_inputs$crosswalk,
        counties. = poly_inputs$counties,
        hysp_dir = dirs$hysp_dir,
        ziplink_dir = dirs$ziplink_dir,
        duration.run.hours = 24,
        overwrite = TRUE,
        return.linked.data = TRUE,
        engine = engine_name
      )
    ))
  }

  t_legacy <- system.time({
    out_legacy <- run_engine("legacy")
  })["elapsed"]

  t_fast <- system.time({
    out_fast <- run_engine("fast")
  })["elapsed"]

  if (identical(link_to, "zips")) {
    legacy_s <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
    fast_s <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
    parity <- verify_compare_grouped(
      legacy_dt = legacy_s,
      fast_dt = fast_s,
      key_cols = c("ZIP", "ID", "month"),
      value_col = "N",
      tol_abs = 1e-8,
      tol_rel = 1e-8
    )
  } else {
    legacy_s <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = .(geoid, ID, month)][order(geoid, ID, month)]
    fast_s <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = .(geoid, ID, month)][order(geoid, ID, month)]
    parity <- verify_compare_grouped(
      legacy_dt = legacy_s,
      fast_dt = fast_s,
      key_cols = c("geoid", "ID", "month"),
      value_col = "N",
      tol_abs = 1e-8,
      tol_rel = 1e-8
    )
  }

  data.table::data.table(
    case = case_name,
    link_to = link_to,
    units = heavy_units,
    months = heavy_months,
    rows_per_file = heavy_rows,
    polygons = heavy_grid_x * heavy_grid_y,
    cores = heavy_cores,
    legacy_elapsed_sec = as.numeric(t_legacy),
    fast_elapsed_sec = as.numeric(t_fast),
    speedup_x = as.numeric(t_legacy) / as.numeric(t_fast),
    parity_keys = parity$keys_equal,
    parity_values = parity$values_equal,
    parity_ok = parity$parity_ok,
    parity_n_key_only_rows = parity$n_key_only_rows,
    parity_n_diff_rows = parity$n_diff_rows,
    parity_max_abs_diff = parity$max_abs_diff,
    parity_max_rel_diff = parity$max_rel_diff,
    output_rows_legacy = nrow(out_legacy),
    output_rows_fast = nrow(out_fast)
  )
}

verify_step("benchmark_user_heavy_link_all_units", {
  summary_dt <- data.table::rbindlist(list(
    run_heavy_case("zips"),
    run_heavy_case("counties")
  ), fill = TRUE)

  bad_parity <- summary_dt[is.na(parity_ok) | !parity_ok]
  if (nrow(bad_parity) > 0) {
    detail <- paste(
      apply(
        bad_parity[, .(
          case,
          parity_n_key_only_rows,
          parity_n_diff_rows,
          parity_max_abs_diff,
          parity_max_rel_diff
        )],
        1L,
        function(x) {
          paste0(
            x[["case"]],
            " (key-only=", x[["parity_n_key_only_rows"]],
            ", diff-rows=", x[["parity_n_diff_rows"]],
            ", max-abs=", signif(as.numeric(x[["parity_max_abs_diff"]]), 6),
            ", max-rel=", signif(as.numeric(x[["parity_max_rel_diff"]]), 6),
            ")"
          )
        }
      ),
      collapse = "; "
    )
    verify_expect(
      FALSE,
      paste0("Parity check failed for one or more heavy user-flow cases. ", detail)
    )
  }

  outfile <- file.path(perf_dir, "heavy_user_flow_link_all_units.csv")
  data.table::fwrite(summary_dt, outfile)
  message("Heavy user-flow benchmark summary written to: ", outfile)
  print(summary_dt)
  invisible(summary_dt)
})

verify_stop_on_failures("08_perf_user_heavy_linking")
