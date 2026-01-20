# =============================================================================
# enhanced_maps.R — Professional Spatial Maps with State Boundaries
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
    library(ggplot2)
    library(sf)
})
source("helpers.R")

cat("=== GENERATING ENHANCED SPATIAL MAPS ===\n\n")

# Setup
dirs <- replication_init_dirs(require_existing = TRUE)

# Load US state boundaries
cat("Loading state boundaries...\n")
states <- tryCatch(
    {
        # Try rnaturalearth first
        if (requireNamespace("rnaturalearth", quietly = TRUE)) {
            ne_states <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
            st_transform(ne_states, 4326)
        } else {
            # Fall back to maps package
            usa_map <- maps::map("state", plot = FALSE, fill = TRUE)
            st_as_sf(usa_map)
        }
    },
    error = function(e) {
        cat("Could not load state boundaries:", e$message, "\n")
        NULL
    }
)

# Get unit location
units_dt <- data.table(disperseR::units)
top_unit <- units_dt[year == 2005][order(-SOx)][1]
unit_location <- data.frame(
    lon = top_unit$Longitude,
    lat = top_unit$Latitude,
    ID = top_unit$ID
)
cat("Source unit:", top_unit$ID, "at", top_unit$Latitude, ",", top_unit$Longitude, "\n")

# -----------------------------------------------------------------------------
# Enhanced Dispersion Map with State Boundaries
# -----------------------------------------------------------------------------
cat("\n=== Creating Enhanced Dispersion Map ===\n")

fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE, full.names = TRUE)

if (length(fst_files) > 0) {
    # Load parcel data
    all_parcels <- rbindlist(lapply(fst_files[1:min(10, length(fst_files))], fst::read_fst))
    cat("Loaded", nrow(all_parcels), "parcel records\n")

    # Find hour column
    hour_col <- intersect(names(all_parcels), c("hour.inc", "hour", "traj_hour"))[1]
    if (!is.na(hour_col)) setnames(all_parcels, hour_col, "hour_var", skip_absent = TRUE)

    # Sample for plotting
    plot_parcels <- all_parcels[sample(.N, min(15000, .N))]

    # Calculate map bounds
    lon_range <- range(plot_parcels$lon, na.rm = TRUE)
    lat_range <- range(plot_parcels$lat, na.rm = TRUE)

    # Add margin
    lon_margin <- diff(lon_range) * 0.1
    lat_margin <- diff(lat_range) * 0.1

    p_map <- ggplot() +
        # State boundaries
        {
            if (!is.null(states)) geom_sf(data = states, fill = "gray95", color = "gray60", linewidth = 0.3)
        } +
        # Parcel trajectories
        geom_point(
            data = plot_parcels, aes(x = lon, y = lat, color = hour_var),
            alpha = 0.4, size = 0.6
        ) +
        scale_color_viridis_c(name = "Hours After\nEmission", option = "plasma") +
        # Source location marker
        geom_point(
            data = unit_location, aes(x = lon, y = lat),
            shape = 18, size = 6, color = "red"
        ) +
        geom_point(
            data = unit_location, aes(x = lon, y = lat),
            shape = 18, size = 4, color = "yellow"
        ) +
        # Labels
        annotate("text",
            x = unit_location$lon, y = unit_location$lat - 0.5,
            label = paste("Source:", unit_location$ID), fontface = "bold", size = 3
        ) +
        # Bounds
        coord_sf(
            xlim = c(lon_range[1] - lon_margin, lon_range[2] + lon_margin),
            ylim = c(lat_range[1] - lat_margin, lat_range[2] + lat_margin)
        ) +
        labs(
            title = "Air Parcel Dispersion from Power Plant Unit 3149-1",
            subtitle = "HYSPLIT trajectories over 120 hours, January 2005",
            x = "Longitude",
            y = "Latitude",
            caption = "Red diamond = emission source. Color shows time since release."
        ) +
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 14),
            legend.position = "right",
            panel.grid = element_line(color = "gray90")
        )

    ggsave("outputs/figures/fig5_dispersion_map_enhanced.png", p_map,
        width = 12, height = 9, dpi = 300
    )
    cat("Saved: fig5_dispersion_map_enhanced.png\n")
}

# -----------------------------------------------------------------------------
# ZIP Exposure Choropleth Map
# -----------------------------------------------------------------------------
cat("\n=== Creating ZIP Exposure Map ===\n")

if (file.exists("outputs/logs/linked_zips.fst")) {
    linked_zips <- fst::read_fst("outputs/logs/linked_zips.fst")
    setDT(linked_zips)

    # Find columns
    exp_col <- intersect(names(linked_zips), c("N", "hyads", "exposure", "n"))[1]
    zip_col <- intersect(names(linked_zips), c("ZIP", "zip", "ZCTA", "zcta", "GEOID"))[1]

    if (!is.na(exp_col) && !is.na(zip_col)) {
        setnames(linked_zips, c(exp_col, zip_col), c("exposure_val", "zip_id"), skip_absent = TRUE)

        zip_agg <- linked_zips[, .(total_exposure = sum(exposure_val, na.rm = TRUE)), by = .(zip_id)]
        cat("Aggregated", nrow(zip_agg), "ZIPs\n")

        # Get ZIP centroids (approximate with lat/lon if available in linked data)
        lat_col <- intersect(names(linked_zips), c("lat", "Latitude", "latitude"))[1]
        lon_col <- intersect(names(linked_zips), c("lon", "Longitude", "longitude"))[1]

        if (!is.na(lat_col) && !is.na(lon_col)) {
            setnames(linked_zips, c(lat_col, lon_col), c("lat", "lon"), skip_absent = TRUE)

            zip_coords <- linked_zips[, .(
                lat = mean(lat, na.rm = TRUE),
                lon = mean(lon, na.rm = TRUE),
                exposure = sum(exposure_val, na.rm = TRUE)
            ), by = .(zip_id)]

            # Filter to valid coords and top exposures
            zip_coords <- zip_coords[!is.na(lat) & !is.na(lon) & lat > 25 & lat < 50 & lon > -100 & lon < -65]

            if (nrow(zip_coords) > 0) {
                p_exposure <- ggplot() +
                    # State boundaries
                    {
                        if (!is.null(states)) geom_sf(data = states, fill = "gray98", color = "gray60", linewidth = 0.3)
                    } +
                    # Exposure points
                    geom_point(
                        data = zip_coords, aes(x = lon, y = lat, color = log10(exposure + 1), size = exposure),
                        alpha = 0.6
                    ) +
                    scale_color_gradient2(
                        low = "#edf8e9", mid = "#74c476", high = "#00441b",
                        midpoint = median(log10(zip_coords$exposure + 1)),
                        name = "log10(Exposure)"
                    ) +
                    scale_size_continuous(range = c(0.5, 4), guide = "none") +
                    # Source
                    geom_point(
                        data = unit_location, aes(x = lon, y = lat),
                        shape = 17, size = 5, color = "red"
                    ) +
                    # Bounds
                    coord_sf(xlim = c(-85, -70), ylim = c(37, 45)) +
                    labs(
                        title = "ZIP Code Exposure from Unit 3149-1",
                        subtitle = "Darker = higher HyADS exposure",
                        x = "Longitude",
                        y = "Latitude",
                        caption = "Red triangle = source location"
                    ) +
                    theme_minimal(base_size = 11) +
                    theme(
                        plot.title = element_text(face = "bold"),
                        legend.position = "right"
                    )

                ggsave("outputs/figures/fig6_zip_exposure_map.png", p_exposure,
                    width = 11, height = 8, dpi = 300
                )
                cat("Saved: fig6_zip_exposure_map.png\n")
            }
        }
    }
}

# -----------------------------------------------------------------------------
# Multi-Unit Comparison Map
# -----------------------------------------------------------------------------
cat("\n=== Creating Multi-Unit Comparison ===\n")

# Show top 5 units on map
top5_units <- units_dt[year == 2005][order(-SOx)][1:5]

p_units <- ggplot() +
    {
        if (!is.null(states)) geom_sf(data = states, fill = "gray95", color = "gray70", linewidth = 0.3)
    } +
    geom_point(
        data = top5_units, aes(x = Longitude, y = Latitude, size = SOx / 1000),
        color = "#2171b5", alpha = 0.8
    ) +
    geom_text(
        data = top5_units, aes(x = Longitude, y = Latitude - 0.5, label = ID),
        size = 3, fontface = "bold"
    ) +
    scale_size_continuous(name = "SOx (K tons)", range = c(4, 15)) +
    coord_sf(xlim = c(-95, -70), ylim = c(30, 45)) +
    labs(
        title = "Top 5 Power Plant Units by SO₂ Emissions (2005)",
        subtitle = "Circle size proportional to annual emissions",
        x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fig7_units_map.png", p_units,
    width = 11, height = 8, dpi = 300
)
cat("Saved: fig7_units_map.png\n")

cat("\n=== Enhanced Maps Complete ===\n")
print(list.files("outputs/figures", pattern = "fig[567]"))
