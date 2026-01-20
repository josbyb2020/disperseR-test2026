# =============================================================================
# real_run.R — ACTUAL PIPELINE EXECUTION (Vignette-Based)
# Following the official disperseR workflow from Vignette_DisperseR.Rmd
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
})
source("helpers.R")

cat("=== REAL PIPELINE EXECUTION ===\n")
cat("Following official disperseR vignette workflow\n\n")

# -----------------------------------------------------------------------------
# STEP 1: Setup directories (must call fresh to populate internal cache)
# -----------------------------------------------------------------------------
cat("=== Step 1: Setting Up Directories ===\n")

# Must call create_dirs() in the session to populate disperseR's internal cache
# The RDS was saved for reference but get_data() needs the cache
dirs <- replication_init_dirs(require_existing = FALSE)
project_path <- dirname(dirs$main_dir)

cat("Directories initialized at:", project_path, "\n")

# -----------------------------------------------------------------------------
# STEP 2: Download all required data using get_data()
# -----------------------------------------------------------------------------
cat("\n=== Step 2: Downloading Required Data ===\n")
cat("Running: get_data(data='all', start.year='2005', start.month='01', end.year='2005', end.month='01')\n")
cat("This downloads ZCTA shapefile, crosswalk, PBL height, and met data (~150MB)\n\n")

# Download all data for January 2005
get_data(
    data = "all",
    start.year = "2005",
    start.month = "01",
    end.year = "2005",
    end.month = "01"
)

cat("\nData download complete.\n")

# Verify met files
met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$")
cat("Met files available:", paste(met_files, collapse = ", "), "\n")

# Load the zcta and crosswalk objects that get_data() should have loaded
cat("\nLoading spatial data objects...\n")

# These should be available after get_data("all")
zcta_dataset <- tryCatch(get_data("zctashapefile"), error = function(e) NULL)
crosswalk <- tryCatch(get_data("crosswalk"), error = function(e) NULL)
pblheight <- tryCatch(get_data("pblheight"), error = function(e) NULL)

if (!is.null(zcta_dataset)) cat("ZCTA shapefile loaded:", nrow(zcta_dataset), "features\n")
if (!is.null(crosswalk)) cat("Crosswalk loaded:", nrow(crosswalk), "records\n")
if (!is.null(pblheight)) cat("PBL height loaded\n")

# -----------------------------------------------------------------------------
# STEP 3: Select units to run
# -----------------------------------------------------------------------------
cat("\n=== Step 3: Selecting Units ===\n")

# Load built-in units data (as shown in vignette)
units_dt <- data.table(disperseR::units)

# Select top 2 SOx emitters in 2005 (as vignette does)
unitsrun <- units_dt[year == 2005][order(-SOx)][1:2]

cat("Selected units:\n")
print(unitsrun[, .(ID, Latitude, Longitude, SOx, Height)])

# -----------------------------------------------------------------------------
# STEP 4: Define input references using define_inputs()
# -----------------------------------------------------------------------------
cat("\n=== Step 4: Defining Input References ===\n")

# Use define_inputs() as shown in vignette
# For proof of concept: just 3 days, 4 hours per day, 120-hour duration
input_refs <- define_inputs(
    units = unitsrun,
    startday = "2005-01-15",
    endday = "2005-01-17",
    start.hours = c(0, 6, 12, 18),
    duration = 120 # 5 days (vignette example uses this)
)

cat("Input refs created:", nrow(input_refs), "emissions events\n")
print(head(input_refs))

# Save for reference
saveRDS(input_refs, "outputs/logs/input_refs_real.rds")

# -----------------------------------------------------------------------------
# STEP 5: Run HYSPLIT dispersion
# -----------------------------------------------------------------------------
cat("\n=== Step 5: Running HYSPLIT Dispersion ===\n")
cat("This may take 5-15 minutes...\n\n")

start_time <- Sys.time()

# Run disperser parallel as shown in vignette
hysp_raw <- run_disperser_parallel(
    input.refs = input_refs,
    pbl.height = pblheight,
    species = "so2",
    proc_dir = dirs$proc_dir,
    hysp_dir = dirs$hysp_dir,
    meteo_dir = dirs$meteo_dir,
    overwrite = FALSE,
    npart = 100,
    keep.hysplit.files = FALSE,
    mc.cores = 1 # Sequential for reliability
)

end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "mins")

cat("\nDispersion complete!\n")
cat("Runtime:", round(as.numeric(runtime), 2), "minutes\n")

# Check outputs
fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
cat("Generated .fst files:", length(fst_files), "\n")

# Show sample if available
if (length(fst_files) > 0) {
    sample_file <- file.path(dirs$hysp_dir, fst_files[1])
    sample_data <- fst::read_fst(sample_file)
    cat("\nSample output structure:\n")
    str(sample_data)
}

# Save runtime stats
write.csv(data.frame(
    step = "HYSPLIT dispersion",
    n_runs = nrow(input_refs),
    n_outputs = length(fst_files),
    runtime_mins = round(as.numeric(runtime), 2),
    status = ifelse(length(fst_files) > 0, "SUCCESS", "FAILED")
), "outputs/tables/real_runtime.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# STEP 6: Link to ZIP codes
# -----------------------------------------------------------------------------
cat("\n=== Step 6: Linking to ZIP Codes ===\n")

if (length(fst_files) > 0 && !is.null(zcta_dataset) && !is.null(crosswalk)) {
    # Get yearmons as shown in vignette
    yearmons <- get_yearmon(
        start.year = "2005",
        start.month = "01",
        end.year = "2005",
        end.month = "01"
    )
    cat("Year-months to link:", yearmons, "\n")

    link_start <- Sys.time()

    # Note: ZCTA data is automatically retrieved from internal cache
    # (set by get_data("zctashapefile") earlier)
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

    link_end <- Sys.time()
    link_runtime <- difftime(link_end, link_start, units = "mins")

    cat("\nLinking complete!\n")
    cat("Linked records:", nrow(linked_zips), "\n")
    cat("Runtime:", round(as.numeric(link_runtime), 2), "minutes\n")

    # Save linked data
    fst::write_fst(linked_zips, "outputs/logs/linked_zips.fst")
    cat("\nSample linked data:\n")
    print(head(linked_zips))
} else {
    cat("Cannot link - missing outputs or spatial data\n")
    linked_zips <- NULL
}

# -----------------------------------------------------------------------------
# STEP 7: Combine monthly links and calculate exposure
# -----------------------------------------------------------------------------
cat("\n=== Step 7: Combine Links and Calculate Exposure ===\n")

if (!is.null(linked_zips) && nrow(linked_zips) > 0) {
    # Combine monthly links as shown in vignette
    yearmons <- get_yearmon("2005", "01", "2005", "01")

    combined_ziplinks <- combine_monthly_links(
        month_YYYYMMs = yearmons,
        link.to = "zips",
        filename = "hyads_real_unwgted_zips.RData"
    )

    cat("Combined links created\n")

    # Calculate exposure (as shown in vignette)
    # Note: need PP.units.monthly1995_2017 for this
    cat("\nCalculating weighted exposure...\n")

    # Try to calculate exposure
    exp_ann_unit_zip <- tryCatch(
        {
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
        },
        error = function(e) {
            cat("Exposure calculation error:", e$message, "\n")
            NULL
        }
    )

    if (!is.null(exp_ann_unit_zip) && nrow(exp_ann_unit_zip) > 0) {
        cat("Exposure calculated:", nrow(exp_ann_unit_zip), "records\n")
        write.csv(exp_ann_unit_zip, "outputs/tables/exposure_real.csv", row.names = FALSE)
        print(head(exp_ann_unit_zip))
    }
}

# -----------------------------------------------------------------------------
# STEP 8: Generate figures
# -----------------------------------------------------------------------------
cat("\n=== Step 8: Generating Figures ===\n")

library(ggplot2)

# Figure from dispersion output
if (length(fst_files) > 0) {
    sample_file <- file.path(dirs$hysp_dir, fst_files[1])
    parcels <- fst::read_fst(sample_file)

    # Detect hour column (may be 'hour' or 'hour.inc' depending on version)
    hour_col <- intersect(names(parcels), c("hour", "hour.inc"))[1]
    
    if ("height" %in% names(parcels) && !is.na(hour_col)) {
        # Rename to consistent 'hour_var' for plotting
        parcels$hour_var <- parcels[[hour_col]]
        
        agg <- data.table(parcels)[, .(
            mean_height = mean(height, na.rm = TRUE),
            sd_height = sd(height, na.rm = TRUE),
            n = .N
        ), by = .(hour_var)]

        p1 <- ggplot(agg, aes(x = hour_var, y = mean_height)) +
            geom_ribbon(
                aes(
                    ymin = pmax(0, mean_height - sd_height),
                    ymax = mean_height + sd_height
                ),
                fill = "#fee0d2", alpha = 0.5
            ) +
            geom_line(color = "#de2d26", linewidth = 1) +
            labs(
                title = "Figure 1: Parcel Height Evolution",
                subtitle = paste("Unit", unitsrun$ID[1], "- January 2005"),
                x = "Hours After Release",
                y = "Height (m AGL)"
            ) +
            theme_minimal(base_size = 12)

        ggsave("outputs/figures/fig1_parcel_heights_real.png", p1,
            width = 8, height = 5, dpi = 300
        )
        cat("Saved: fig1_parcel_heights_real.png\n")
    }

    # Figure 2: Parcel scatter (lat/lon)
    if ("lat" %in% names(parcels) && "lon" %in% names(parcels) && !is.na(hour_col)) {
        sample_parcels <- parcels[sample(nrow(parcels), min(5000, nrow(parcels))), ]

        p2 <- ggplot(sample_parcels, aes(x = lon, y = lat, color = hour_var)) +
            geom_point(alpha = 0.3, size = 0.5) +
            scale_color_viridis_c(name = "Hour") +
            labs(
                title = "Figure 2: Parcel Dispersion Pattern",
                subtitle = paste("Unit", unitsrun$ID[1]),
                x = "Longitude",
                y = "Latitude"
            ) +
            theme_minimal() +
            coord_fixed(1.3)

        ggsave("outputs/figures/fig2_parcel_dispersion_real.png", p2,
            width = 10, height = 8, dpi = 300
        )
        cat("Saved: fig2_parcel_dispersion_real.png\n")
    }
}

# Figure 3: Linked ZIP exposure if available
if (!is.null(linked_zips) && nrow(linked_zips) > 0 && "N" %in% names(linked_zips)) {
    zip_agg <- linked_zips[, .(total_N = sum(N, na.rm = TRUE)), by = .(ZIP)]
    top_zips <- head(zip_agg[order(-total_N)], 20)

    p3 <- ggplot(top_zips, aes(x = reorder(ZIP, total_N), y = total_N)) +
        geom_bar(stat = "identity", fill = "#41ab5d") +
        coord_flip() +
        labs(
            title = "Figure 3: Top 20 ZIP Codes by Raw HyADS Exposure",
            subtitle = "Unweighted parcel contributions",
            x = "ZIP Code",
            y = "HyADS Raw Exposure (N)"
        ) +
        theme_minimal(base_size = 11)

    ggsave("outputs/figures/fig3_top_zips_real.png", p3,
        width = 8, height = 6, dpi = 300
    )
    cat("Saved: fig3_top_zips_real.png\n")

    # Save exposure table
    write.csv(zip_agg, "outputs/tables/zip_exposure_real.csv", row.names = FALSE)
    cat("Saved: zip_exposure_real.csv with", nrow(zip_agg), "ZIPs\n")
}

# -----------------------------------------------------------------------------
# SUMMARY
# -----------------------------------------------------------------------------
cat("\n")
cat("================================================================\n")
cat("=== REAL PIPELINE EXECUTION COMPLETE ===\n")
cat("================================================================\n\n")

cat("Generated outputs:\n")
cat("- Met files:", length(list.files(dirs$meteo_dir, pattern = "\\.gbl$")), "\n")
cat("- Parcel files:", length(fst_files), "\n")

if (file.exists("outputs/tables/zip_exposure_real.csv")) {
    exp_df <- read.csv("outputs/tables/zip_exposure_real.csv")
    cat("- Exposure records:", nrow(exp_df), "ZIPs\n")
}

cat("- Figures:", length(list.files("outputs/figures", pattern = "\\.png$")), "\n")

cat("\nThis is REAL DATA from actual HYSPLIT runs!\n")
