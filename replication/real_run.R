# real_run.R - End-to-end pipeline execution (vignette-based)
# Runs full disperseR workflow for top 2 SOx emitters, January 2005
# See docs/REFERENCE.md for API details and troubleshooting

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
})
source("helpers.R")

message("disperseR real pipeline execution")

# Step 1: Directories
dirs <- replication_init_dirs(require_existing = FALSE)
project_path <- dirname(dirs$main_dir)
message("Project: ", project_path)

# Step 2: Download data for Jan 2005
message("\nDownloading required data (ZCTA, crosswalk, PBL, met)...")
get_data(
  data = "all",
  start.year = "2005", start.month = "01",
  end.year = "2005", end.month = "01"
)

met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$")
message("  Met files: ", paste(met_files, collapse = ", "))

zcta_dataset <- tryCatch(get_data("zctashapefile"), error = function(e) NULL)
crosswalk <- tryCatch(get_data("crosswalk"), error = function(e) NULL)
pblheight <- tryCatch(get_data("pblheight"), error = function(e) NULL)

if (!is.null(zcta_dataset)) message("  ZCTA: ", nrow(zcta_dataset), " features")
if (!is.null(crosswalk)) message("  Crosswalk: ", nrow(crosswalk), " records")

# Step 3: Select units (top 2 SOx emitters)
units_dt <- data.table(disperseR::units)
unitsrun <- units_dt[year == 2005][order(-SOx)][1:2]
message("\nSelected units: ", paste(unitsrun$ID, collapse = ", "))

# Step 4: Define inputs (3 days, 4 starts/day, 120-hour duration)
input_refs <- define_inputs(
  units = unitsrun,
  startday = "2005-01-15",
  endday = "2005-01-17",
  start.hours = c(0, 6, 12, 18),
  duration = 120
)
message("Input refs: ", nrow(input_refs), " emission events")
saveRDS(input_refs, "outputs/logs/input_refs_real.rds")

# Step 5: Run HYSPLIT dispersion
message("\nPreflight checks...")

# Check splitr availability
splitr_ok <- requireNamespace("splitr", quietly = TRUE)
if (!splitr_ok) {

  message("  splitr: NOT INSTALLED")
  message("  Install with: remotes::install_github('rich-iannone/splitr')")
} else {
  message("  splitr: OK")
}

# Check met files
met_ok <- length(met_files) > 0
if (!met_ok) {
  message("  Met files: MISSING")
} else {
  message("  Met files: OK (", length(met_files), ")")
}

# Check PBL
pbl_ok <- !is.null(pblheight)
if (!pbl_ok) {
  message("  PBL data: MISSING")
} else {
  message("  PBL data: OK")
}

# Run HYSPLIT only if all prerequisites are met
if (!splitr_ok || !met_ok || !pbl_ok) {
  message("\nSkipping HYSPLIT: prerequisites not met.")
  message("See docs/REFERENCE.md troubleshooting section.")
  hysp_raw <- NULL
  runtime <- NA
  fst_files <- character(0)
} else {
  message("\nRunning HYSPLIT dispersion (may take 5-15 minutes)...")
  start_time <- Sys.time()
  
  hysp_raw <- tryCatch({
    run_disperser_parallel(
      input.refs = input_refs,
      pbl.height = pblheight,
      species = "so2",
      proc_dir = dirs$proc_dir,
      hysp_dir = dirs$hysp_dir,
      meteo_dir = dirs$meteo_dir,
      overwrite = FALSE,
      npart = 100,
      keep.hysplit.files = FALSE,
      mc.cores = 1
    )
  }, error = function(e) {
    message("HYSPLIT error: ", e$message)
    message("See docs/REFERENCE.md for troubleshooting (e.g., Rosetta on Apple Silicon).")
    NULL
  })
  
  runtime <- difftime(Sys.time(), start_time, units = "mins")
}

# Check outputs (whether from this run or previous runs)
fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
if (!is.na(runtime)) {
  message("  Runtime: ", round(as.numeric(runtime), 1), " min")
}
message("  Output files: ", length(fst_files))

write.csv(data.frame(
  step = "HYSPLIT dispersion",
  n_runs = nrow(input_refs),
  n_outputs = length(fst_files),
  runtime_mins = if (is.na(runtime)) NA else round(as.numeric(runtime), 2),
  status = if (length(fst_files) > 0) "SUCCESS" else if (is.na(runtime)) "SKIPPED" else "FAILED"
), "outputs/tables/real_runtime.csv", row.names = FALSE)

if (length(fst_files) > 0) {
  saveRDS(
    list(
      source = "real_run.R",
      duration_run_hours = input_refs$duration_run_hours[1],
      created = Sys.time()
    ),
    "outputs/logs/last_run_config.rds"
  )
}

# Step 6: Link to ZIP codes
linked_zips <- NULL
if (length(fst_files) > 0 && !is.null(crosswalk) && !is.null(pblheight)) {
  message("\nLinking parcels to ZIP codes...")
  yearmons <- get_yearmon("2005", "01", "2005", "01")
  
  link_start <- Sys.time()
  linked_zips <- link_all_units(
    units.run = unitsrun,
    link.to = "zips",
    mc.cores = 1,
    year.mons = yearmons,
    pbl.height = pblheight,
    crosswalk. = crosswalk,
    hysp_dir = dirs$hysp_dir,
    ziplink_dir = dirs$ziplink_dir,
    duration.run.hours = 120,
    res.link = 12000,
    overwrite = TRUE
  )
  link_runtime <- difftime(Sys.time(), link_start, units = "mins")
  
  message("  Linked records: ", nrow(linked_zips))
  message("  Runtime: ", round(as.numeric(link_runtime), 1), " min")
  fst::write_fst(linked_zips, "outputs/logs/linked_zips.fst")
} else {
  message("Skipping linking: missing outputs or spatial data")
}

# Step 7: Calculate exposure
if (!is.null(linked_zips) && nrow(linked_zips) > 0) {
  message("\nCombining links and calculating exposure...")
  yearmons <- get_yearmon("2005", "01", "2005", "01")
  
  combined_ziplinks <- combine_monthly_links(
    month_YYYYMMs = yearmons,
    link.to = "zips",
    filename = "hyads_real_unwgted_zips.RData"
  )
  
  exp_ann_unit_zip <- tryCatch({
    calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      link.to = "zips",
      pollutant = "SO2.tons",
      rda_file = file.path(dirs$rdata_dir, "hyads_real_unwgted_zips.RData"),
      exp_dir = dirs$exp_dir,
      units.mo = disperseR::PP.units.monthly1995_2017,
      source.agg = "unit",
      time.agg = "month",
      return.monthly.data = TRUE
    )
  }, error = function(e) {
    message("  Exposure calculation error: ", e$message)
    NULL
  })
  
  if (!is.null(exp_ann_unit_zip) && nrow(exp_ann_unit_zip) > 0) {
    message("  Exposure records: ", nrow(exp_ann_unit_zip))
    write.csv(exp_ann_unit_zip, "outputs/tables/exposure_real.csv", row.names = FALSE)
  }
}

# Step 8: Generate figures
message("\nGenerating figures...")
library(ggplot2)

if (length(fst_files) > 0) {
  sample_file <- file.path(dirs$hysp_dir, fst_files[1])
  parcels <- fst::read_fst(sample_file)
  hour_col <- intersect(names(parcels), c("hour", "hour.inc"))[1]
  
  if ("height" %in% names(parcels) && !is.na(hour_col)) {
    parcels$hour_var <- parcels[[hour_col]]
    agg <- data.table(parcels)[, .(
      mean_height = mean(height, na.rm = TRUE),
      sd_height = sd(height, na.rm = TRUE),
      n = .N
    ), by = .(hour_var)]
    
    p1 <- ggplot(agg, aes(x = hour_var, y = mean_height)) +
      geom_ribbon(aes(
        ymin = pmax(0, mean_height - sd_height),
        ymax = mean_height + sd_height
      ), fill = "#fee0d2", alpha = 0.5) +
      geom_line(color = "#de2d26", linewidth = 1) +
      labs(
        title = "Parcel Height Evolution",
        subtitle = paste("Unit", unitsrun$ID[1], "- January 2005"),
        x = "Hours After Release", y = "Height (m AGL)"
      ) +
      theme_minimal(base_size = 12)
    
    ggsave("outputs/figures/fig1_parcel_heights.png", p1, width = 8, height = 5, dpi = 300)
    message("  Saved: fig1_parcel_heights.png")
  }
  
  if ("lat" %in% names(parcels) && "lon" %in% names(parcels) && !is.na(hour_col)) {
    sample_parcels <- parcels[sample(nrow(parcels), min(5000, nrow(parcels))), ]
    
    p2 <- ggplot(sample_parcels, aes(x = lon, y = lat, color = hour_var)) +
      geom_point(alpha = 0.3, size = 0.5) +
      scale_color_viridis_c(name = "Hour") +
      labs(
        title = "Parcel Dispersion Pattern",
        subtitle = paste("Unit", unitsrun$ID[1]),
        x = "Longitude", y = "Latitude"
      ) +
      theme_minimal() +
      coord_fixed(1.3)
    
    ggsave("outputs/figures/fig2_parcel_dispersion.png", p2, width = 10, height = 8, dpi = 300)
    message("  Saved: fig2_parcel_dispersion.png")
  }
}

if (!is.null(linked_zips) && nrow(linked_zips) > 0 && "N" %in% names(linked_zips)) {
  zip_agg <- linked_zips[, .(total_N = sum(N, na.rm = TRUE)), by = .(ZIP)]
  top_zips <- head(zip_agg[order(-total_N)], 20)
  
  p3 <- ggplot(top_zips, aes(x = reorder(ZIP, total_N), y = total_N)) +
    geom_bar(stat = "identity", fill = "#41ab5d") +
    coord_flip() +
    labs(
      title = "Top 20 ZIP Codes by HyADS Exposure",
      subtitle = "Unweighted parcel contributions",
      x = "ZIP Code", y = "HyADS Exposure (N)"
    ) +
    theme_minimal(base_size = 11)
  
  ggsave("outputs/figures/fig3_top_zips.png", p3, width = 8, height = 6, dpi = 300)
  message("  Saved: fig3_top_zips.png")
  write.csv(zip_agg, "outputs/tables/zip_exposure.csv", row.names = FALSE)
}

# Summary
message("\n--- Summary ---")
message("Met files: ", length(met_files))
message("Parcel files: ", length(fst_files))
message("Figures: ", length(list.files("outputs/figures", pattern = "\\.png$")))
