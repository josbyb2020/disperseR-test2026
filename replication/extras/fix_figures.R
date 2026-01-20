# =============================================================================
# fix_figures.R — Fix and Generate All Required Figures
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
    library(ggplot2)
})
source("helpers.R")

cat("=== GENERATING ALL REQUIRED FIGURES ===\n\n")

# Setup
dirs <- replication_init_dirs(require_existing = TRUE)

# -----------------------------------------------------------------------------
# Figure 1: Parcel Height Evolution
# -----------------------------------------------------------------------------
cat("=== Figure 1: Parcel Height Evolution ===\n")

fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE, full.names = TRUE)
cat("Found", length(fst_files), "parcel files\n")

if (length(fst_files) > 0) {
    # Load multiple files for richer data
    all_parcels <- rbindlist(lapply(fst_files[1:min(5, length(fst_files))], fst::read_fst))
    cat("Loaded", nrow(all_parcels), "parcel records\n")

    # Check column names
    cat("Columns:", paste(names(all_parcels), collapse = ", "), "\n\n")

    # Find the hour column (could be hour.inc, hour, or traj_hour)
    hour_col <- intersect(names(all_parcels), c("hour.inc", "hour", "traj_hour"))[1]
    if (!is.na(hour_col)) {
        setnames(all_parcels, hour_col, "hour_var", skip_absent = TRUE)

        agg <- all_parcels[, .(
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
            geom_line(color = "#de2d26", linewidth = 1.2) +
            geom_point(color = "#de2d26", size = 2) +
            labs(
                title = "Figure 1: Air Parcel Height Evolution Over Time",
                subtitle = paste("Based on", format(nrow(all_parcels), big.mark = ","), "trajectory points"),
                x = "Hours After Emission",
                y = "Mean Height (m AGL)",
                caption = "Shaded area shows ± 1 standard deviation"
            ) +
            theme_minimal(base_size = 12) +
            theme(
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(color = "gray40")
            )

        ggsave("outputs/figures/fig1_parcel_heights.png", p1,
            width = 10, height = 6, dpi = 300
        )
        cat("Saved: fig1_parcel_heights.png\n")
    }
}

# -----------------------------------------------------------------------------
# Figure 2: Parcel Dispersion Pattern (Spatial)
# -----------------------------------------------------------------------------
cat("\n=== Figure 2: Parcel Dispersion Pattern ===\n")

if (exists("all_parcels") && "lat" %in% names(all_parcels) && "lon" %in% names(all_parcels)) {
    # Sample for plotting
    plot_parcels <- all_parcels[sample(.N, min(10000, .N))]

    p2 <- ggplot(plot_parcels, aes(x = lon, y = lat, color = hour_var)) +
        geom_point(alpha = 0.4, size = 0.8) +
        scale_color_viridis_c(name = "Hours", option = "plasma") +
        labs(
            title = "Figure 2: Air Parcel Dispersion Pattern",
            subtitle = "Trajectory points colored by time since emission",
            x = "Longitude (°W)",
            y = "Latitude (°N)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "right"
        ) +
        coord_fixed(1.3)

    ggsave("outputs/figures/fig2_dispersion_pattern.png", p2,
        width = 12, height = 9, dpi = 300
    )
    cat("Saved: fig2_dispersion_pattern.png\n")
}

# -----------------------------------------------------------------------------
# Figure 3: Top ZIP Codes by Exposure
# -----------------------------------------------------------------------------
cat("\n=== Figure 3: Top ZIP Codes by Exposure ===\n")

if (file.exists("outputs/logs/linked_zips.fst")) {
    linked_zips <- fst::read_fst("outputs/logs/linked_zips.fst")
    setDT(linked_zips)
    cat("Linked data:", nrow(linked_zips), "records\n")
    cat("Columns:", paste(names(linked_zips), collapse = ", "), "\n")

    # Find the exposure column (could be N, hyads, or exposure)
    exp_col <- intersect(names(linked_zips), c("N", "hyads", "exposure", "n"))[1]

    if (!is.na(exp_col)) {
        setnames(linked_zips, exp_col, "exposure_val", skip_absent = TRUE)

        # Find ZIP column
        zip_col <- intersect(names(linked_zips), c("ZIP", "zip", "ZCTA", "zcta", "GEOID"))[1]
        if (!is.na(zip_col)) {
            setnames(linked_zips, zip_col, "zip_id", skip_absent = TRUE)
        }

        if ("zip_id" %in% names(linked_zips)) {
            zip_agg <- linked_zips[, .(total_exposure = sum(exposure_val, na.rm = TRUE)), by = .(zip_id)]
            top_zips <- head(zip_agg[order(-total_exposure)], 20)

            p3 <- ggplot(top_zips, aes(x = reorder(zip_id, total_exposure), y = total_exposure)) +
                geom_bar(stat = "identity", fill = "#41ab5d", width = 0.7) +
                geom_text(aes(label = round(total_exposure, 1)), hjust = -0.1, size = 3) +
                coord_flip() +
                labs(
                    title = "Figure 3: Top 20 ZIP Codes by HyADS Exposure",
                    subtitle = "Unweighted parcel contributions",
                    x = "ZIP Code",
                    y = "Raw HyADS Exposure (N)",
                    caption = "Higher values indicate more air parcels traced to location"
                ) +
                theme_minimal(base_size = 11) +
                theme(
                    plot.title = element_text(face = "bold"),
                    axis.text.y = element_text(size = 9)
                ) +
                expand_limits(x = 0, y = max(top_zips$total_exposure) * 1.1)

            ggsave("outputs/figures/fig3_top_zips.png", p3,
                width = 9, height = 7, dpi = 300
            )
            cat("Saved: fig3_top_zips.png\n")

            # Save table
            write.csv(zip_agg, "outputs/tables/zip_exposure_aggregated.csv", row.names = FALSE)
            cat("Saved: zip_exposure_aggregated.csv with", nrow(zip_agg), "ZIPs\n")
        }
    }
} else {
    cat("No linked ZIP data available\n")
}

# -----------------------------------------------------------------------------
# Figure 4: Units Ranked by Emissions
# -----------------------------------------------------------------------------
cat("\n=== Figure 4: Units Ranked by Emissions ===\n")

units_dt <- data.table(disperseR::units)
top_units_2005 <- units_dt[year == 2005][order(-SOx)][1:10]

p4 <- ggplot(top_units_2005, aes(x = reorder(ID, SOx), y = SOx / 1000)) +
    geom_bar(stat = "identity", fill = "#2171b5", width = 0.7) +
    geom_text(aes(label = paste0(round(SOx / 1000, 1), "K")), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
        title = "Figure 4: Top 10 Power Plant Units by SO₂ Emissions (2005)",
        subtitle = "Annual emissions from EPA CEMS data",
        x = "Unit ID",
        y = "SOx Emissions (thousands of tons)",
        caption = "Data source: disperseR built-in units dataset"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9)
    ) +
    expand_limits(x = 0, y = max(top_units_2005$SOx / 1000) * 1.15)

ggsave("outputs/figures/fig4_units_ranked.png", p4,
    width = 9, height = 6, dpi = 300
)
cat("Saved: fig4_units_ranked.png\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n=== FIGURE GENERATION COMPLETE ===\n")
figures <- list.files("outputs/figures", pattern = "\\.png$")
cat("Total figures generated:", length(figures), "\n")
cat("Files:", paste(figures, collapse = ", "), "\n")
