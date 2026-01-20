# fix_figures.R - Regenerate all figures from existing outputs
# Use after pipeline completion to fix or update visualizations

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
  library(ggplot2)
})
source("../helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE, full.names = TRUE)
message("Found ", length(fst_files), " parcel files")

if (length(fst_files) == 0) {
  stop("No parcel files found. Run real_run.R first.")
}

# Load parcel data
all_parcels <- rbindlist(lapply(fst_files[1:min(5, length(fst_files))], fst::read_fst))
message("Loaded ", nrow(all_parcels), " records")

# Detect hour column
hour_col <- intersect(names(all_parcels), c("hour.inc", "hour", "traj_hour"))[1]
if (is.na(hour_col)) {
  stop("No hour column found in parcel data")
}
setnames(all_parcels, hour_col, "hour_var", skip_absent = TRUE)

# Figure 1: Height evolution
agg <- all_parcels[, .(
  mean_height = mean(height, na.rm = TRUE),
  sd_height = sd(height, na.rm = TRUE),
  n = .N
), by = .(hour_var)]

p1 <- ggplot(agg, aes(x = hour_var, y = mean_height)) +
  geom_ribbon(aes(ymin = pmax(0, mean_height - sd_height),
                  ymax = mean_height + sd_height),
              fill = "#fee0d2", alpha = 0.5) +
  geom_line(color = "#de2d26", linewidth = 1) +
  labs(title = "Parcel Height Evolution",
       x = "Hours After Release", y = "Height (m AGL)") +
  theme_minimal(base_size = 12)

ggsave("../outputs/figures/fig1_height_evolution.png", p1, width = 8, height = 5, dpi = 300)
message("Saved: fig1_height_evolution.png")

# Figure 2: Parcel scatter
sample_parcels <- all_parcels[sample(nrow(all_parcels), min(10000, nrow(all_parcels))), ]

p2 <- ggplot(sample_parcels, aes(x = lon, y = lat, color = hour_var)) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_color_viridis_c(name = "Hour") +
  labs(title = "Parcel Dispersion Pattern", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_fixed(1.3)

ggsave("../outputs/figures/fig2_dispersion_scatter.png", p2, width = 10, height = 8, dpi = 300)
message("Saved: fig2_dispersion_scatter.png")

# Figure 3: Height distribution
p3 <- ggplot(all_parcels, aes(x = height)) +
  geom_histogram(bins = 50, fill = "#3182bd", color = "white", alpha = 0.8) +
  labs(title = "Parcel Height Distribution", x = "Height (m AGL)", y = "Count") +
  theme_minimal()

ggsave("../outputs/figures/fig3_height_distribution.png", p3, width = 8, height = 5, dpi = 300)
message("Saved: fig3_height_distribution.png")

# Load linked data if available
if (file.exists("../outputs/logs/linked_zips.fst")) {
  linked <- fst::read_fst("../outputs/logs/linked_zips.fst")
  
  if ("N" %in% names(linked)) {
    zip_agg <- linked[, .(total_N = sum(N, na.rm = TRUE)), by = .(ZIP)]
    top_zips <- head(zip_agg[order(-total_N)], 20)
    
    p4 <- ggplot(top_zips, aes(x = reorder(ZIP, total_N), y = total_N)) +
      geom_bar(stat = "identity", fill = "#41ab5d") +
      coord_flip() +
      labs(title = "Top 20 ZIP Codes by Exposure",
           x = "ZIP Code", y = "HyADS Exposure") +
      theme_minimal(base_size = 11)
    
    ggsave("../outputs/figures/fig4_top_zips.png", p4, width = 8, height = 6, dpi = 300)
    message("Saved: fig4_top_zips.png")
  }
}

message("\nAll figures regenerated.")
