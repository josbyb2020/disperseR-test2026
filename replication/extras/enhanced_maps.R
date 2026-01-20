# enhanced_maps.R - Publication-quality spatial maps
# Adds state boundaries and professional styling
# Requires: linked data or parcel outputs

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
  library(ggplot2)
  library(sf)
})
source("../helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

# Load US state boundaries
states <- tryCatch({
  if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    ne <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
    st_transform(ne, 4326)
  } else {
    usa_map <- maps::map("state", plot = FALSE, fill = TRUE)
    st_as_sf(usa_map)
  }
}, error = function(e) {
  message("Could not load state boundaries: ", e$message)
  NULL
})

# Get unit location
units_dt <- data.table(disperseR::units)
test_unit <- units_dt[year == 2005][order(-SOx)][1]

# Load parcel data
fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE, full.names = TRUE)
if (length(fst_files) == 0) {
  stop("No parcel files found. Run real_run.R first.")
}

parcels <- fst::read_fst(fst_files[1])
message("Loaded ", nrow(parcels), " parcel records")

# Sample for plotting
sample_parcels <- parcels[sample(nrow(parcels), min(5000, nrow(parcels))), ]

# Base map with states
p <- ggplot()

if (!is.null(states)) {
  p <- p + geom_sf(data = states, fill = "gray95", color = "gray70", linewidth = 0.3)
}

# Add parcels
hour_col <- intersect(names(sample_parcels), c("hour", "hour.inc"))[1]
if (!is.na(hour_col)) {
  sample_parcels$hour_var <- sample_parcels[[hour_col]]
  
  p <- p +
    geom_point(data = sample_parcels, aes(x = lon, y = lat, color = hour_var),
               alpha = 0.4, size = 0.5) +
    scale_color_viridis_c(name = "Hour")
}

# Add source location
p <- p +
  geom_point(data = test_unit, aes(x = Longitude, y = Latitude),
             shape = 17, size = 4, color = "red") +
  coord_sf(xlim = c(-100, -70), ylim = c(30, 50)) +
  labs(title = paste("Dispersion from", test_unit$ID),
       subtitle = "Parcel trajectories over 5 days",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave("../outputs/figures/enhanced_dispersion_map.png", p, width = 12, height = 8, dpi = 300)
message("Saved: outputs/figures/enhanced_dispersion_map.png")
