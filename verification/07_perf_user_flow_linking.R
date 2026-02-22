if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

cfg <- verify_read_config()
pkg_root <- verify_find_pkg_root()
verify_load_package(pkg_root)

perf_dir <- file.path(cfg$base_dir, "perf")
dir.create(perf_dir, recursive = TRUE, showWarnings = FALSE)

profile <- tolower(Sys.getenv("DISPERSER_USERFLOW_PROFILE", "smoke"))
if (!(profile %in% c("smoke", "full"))) {
  profile <- "smoke"
}

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

  zcta_sf <- sf::st_sf(
    ZCTA5CE10 = zcta_vals,
    geometry = sf::st_sfc(polygon_list, crs = 4326)
  )

  counties_sf <- sf::st_sf(
    statefp = statefp,
    countyfp = countyfp,
    state_name = state_name,
    name = county_name,
    geoid = geoid,
    geometry = sf::st_sfc(polygon_list, crs = 4326)
  )

  crosswalk <- data.table::data.table(
    ZCTA = zcta_vals,
    ZIP = zcta_vals
  )

  list(
    zcta = zcta_sf,
    counties = counties_sf,
    crosswalk = crosswalk
  )
}

generate_particles <- function(n, distribution = "uniform", seed = 1L) {
  set.seed(seed)
  if (distribution == "clustered") {
    lon <- stats::rnorm(n, mean = -90, sd = 1.2)
    lat <- stats::rnorm(n, mean = 40, sd = 1.0)
    lon <- pmin(pmax(lon, -124), -67)
    lat <- pmin(pmax(lat, 25), 49)
  } else {
    lon <- stats::runif(n, -124, -67)
    lat <- stats::runif(n, 25, 49)
  }
  data.table::data.table(
    lon = lon,
    lat = lat,
    height = stats::runif(n, 10, 1500),
    hour = sample.int(23, n, replace = TRUE) + 1L
  )
}

estimate_unique_cells <- function(d, res.link = 12000) {
  p4 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
  pts <- sf::st_transform(sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326), crs = p4)
  bb <- sf::st_bbox(pts)
  r <- terra::rast(
    xmin = floor(bb["xmin"] / res.link) * res.link,
    xmax = ceiling(bb["xmax"] / res.link) * res.link,
    ymin = floor(bb["ymin"] / res.link) * res.link,
    ymax = ceiling(bb["ymax"] / res.link) * res.link,
    resolution = res.link,
    crs = p4
  )
  cells <- terra::cellFromXY(r, sf::st_coordinates(pts))
  cells <- as.integer(cells[!is.na(cells)])
  length(unique(cells))
}

build_scenarios <- function(profile_name) {
  smoke <- list(
    list(name = "zips_small_uniform", link.to = "zips", units = 6L, year.mons = c("200501"), rows = 3000L, nx = 40L, ny = 20L, distribution = "uniform"),
    list(name = "zips_medium_uniform", link.to = "zips", units = 12L, year.mons = c("200501", "200502"), rows = 12000L, nx = 40L, ny = 20L, distribution = "uniform"),
    list(name = "counties_medium_uniform", link.to = "counties", units = 12L, year.mons = c("200501", "200502"), rows = 12000L, nx = 40L, ny = 20L, distribution = "uniform")
  )

  full_extra <- list(
    list(name = "zips_dense_uniform", link.to = "zips", units = 6L, year.mons = c("200501", "200502"), rows = 12000L, nx = 120L, ny = 100L, distribution = "uniform"),
    list(name = "zips_dense_clustered", link.to = "zips", units = 6L, year.mons = c("200501", "200502"), rows = 12000L, nx = 120L, ny = 100L, distribution = "clustered"),
    list(name = "zips_extract_uniform", link.to = "zips", units = 2L, year.mons = c("200501"), rows = 200000L, nx = 120L, ny = 100L, distribution = "uniform"),
    list(name = "counties_extract_uniform", link.to = "counties", units = 2L, year.mons = c("200501"), rows = 200000L, nx = 120L, ny = 100L, distribution = "uniform")
  )

  if (identical(profile_name, "full")) {
    return(c(smoke, full_extra))
  }
  smoke
}

run_scenario <- function(sc) {
  scenario_root <- file.path(cfg$base_dir, "perf_userflow", sc$name)
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
  poly_inputs <- build_polygon_inputs(sc$nx, sc$ny)

  if (identical(sc$link.to, "zips")) {
    disperseR:::.disperseR_cache_set("zcta", poly_inputs$zcta)
  }

  units.run <- data.table::data.table(
    uID = sprintf("U%03d", seq_len(sc$units)),
    ID = sprintf("U%03d", seq_len(sc$units))
  )

  first_dt <- NULL
  file_seed <- 1L
  for (month in sc$year.mons) {
    year_str <- substr(month, 1, 4)
    mon_str <- substr(month, 5, 6)
    run_date <- as.Date(sprintf("%s-%s-15", year_str, mon_str))
    ymdir <- file.path(dirs$hysp_dir, year_str, mon_str)
    dir.create(ymdir, recursive = TRUE, showWarnings = FALSE)

    for (unit_id in units.run$ID) {
      dt <- generate_particles(sc$rows, distribution = sc$distribution, seed = file_seed)
      file_seed <- file_seed + 1L
      dt[, Pdate := run_date]
      outfile <- file.path(ymdir, sprintf("hyspdisp_%s_%s_00.fst", unit_id, run_date))
      fst::write.fst(dt, outfile)
      if (is.null(first_dt)) {
        first_dt <- dt
      }
    }
  }

  fast_threshold <- getOption("disperseR.fast.extract.min.cells", 5000L)
  if (!is.numeric(fast_threshold) || length(fast_threshold) != 1 || !is.finite(fast_threshold)) {
    fast_threshold <- 5000L
  }
  fast_ratio <- getOption("disperseR.fast.extract.min.cell_poly_ratio", 2)
  if (!is.numeric(fast_ratio) || length(fast_ratio) != 1 || !is.finite(fast_ratio) || fast_ratio <= 0) {
    fast_ratio <- 2
  }
  sample_unique_cells <- estimate_unique_cells(first_dt[, .(lon, lat)], res.link = 12000)
  sample_cell_poly_ratio <- sample_unique_cells / (sc$nx * sc$ny)
  expected_fast_extract <- sample_unique_cells >= as.integer(fast_threshold) &&
    sample_cell_poly_ratio >= as.numeric(fast_ratio)

  run_engine <- function(engine_name) {
    suppressMessages(suppressWarnings(
      disperseR::link_all_units(
        units.run = units.run,
        link.to = sc$link.to,
        mc.cores = 4,
        year.mons = sc$year.mons,
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

  if (identical(sc$link.to, "zips")) {
    legacy_s <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
    fast_s <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = .(ZIP, ID, month)][order(ZIP, ID, month)]
    keys_equal <- identical(legacy_s[, .(ZIP, ID, month)], fast_s[, .(ZIP, ID, month)])
    values_equal <- isTRUE(all.equal(legacy_s$N, fast_s$N, tolerance = 1e-10))
  } else {
    legacy_s <- data.table::as.data.table(out_legacy)[, .(N = sum(N, na.rm = TRUE)), by = .(geoid, ID, month)][order(geoid, ID, month)]
    fast_s <- data.table::as.data.table(out_fast)[, .(N = sum(N, na.rm = TRUE)), by = .(geoid, ID, month)][order(geoid, ID, month)]
    keys_equal <- identical(legacy_s[, .(geoid, ID, month)], fast_s[, .(geoid, ID, month)])
    values_equal <- isTRUE(all.equal(legacy_s$N, fast_s$N, tolerance = 1e-10))
  }

  data.table::data.table(
    scenario = sc$name,
    profile = profile,
    link_to = sc$link.to,
    units = sc$units,
    months = length(sc$year.mons),
    rows_per_file = sc$rows,
    polygons = sc$nx * sc$ny,
    distribution = sc$distribution,
    fast_extract_min_cells = as.integer(fast_threshold),
    fast_extract_min_cell_poly_ratio = as.numeric(fast_ratio),
    sample_unique_cells = as.integer(sample_unique_cells),
    sample_cell_poly_ratio = as.numeric(sample_cell_poly_ratio),
    expected_fast_extract = isTRUE(expected_fast_extract),
    legacy_elapsed_sec = as.numeric(t_legacy),
    fast_elapsed_sec = as.numeric(t_fast),
    speedup_x = as.numeric(t_legacy) / as.numeric(t_fast),
    parity_keys = keys_equal,
    parity_values = values_equal,
    parity_ok = isTRUE(keys_equal && values_equal),
    output_rows_legacy = nrow(out_legacy),
    output_rows_fast = nrow(out_fast)
  )
}

verify_step("benchmark_user_flow_link_all_units", {
  scenarios <- build_scenarios(profile)
  summaries <- lapply(scenarios, run_scenario)
  summary_dt <- data.table::rbindlist(summaries, fill = TRUE)

  verify_expect(all(summary_dt$parity_ok), "Parity check failed in one or more user-flow benchmark scenarios.")

  outfile <- file.path(perf_dir, sprintf("userflow_link_all_units_%s.csv", profile))
  data.table::fwrite(summary_dt, outfile)
  message("User-flow benchmark summary written to: ", outfile)
  print(summary_dt)
  invisible(summary_dt)
})

verify_stop_on_failures("07_perf_user_flow_linking")
