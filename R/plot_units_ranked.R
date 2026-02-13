#' Plot ranked units on map
#'
#' @description Creates map and bar plot visualizations of facility rankings
#' based on population-weighted exposure metrics.
#'
#' @param data.ranked Ranking data from rankfacs_by_popwgt_location()
#' @param data.units Unit/facility data with coordinates
#' @param year Year to plot
#' @param graph.dir Output directory for plots (NULL to skip saving)
#'
#' @return List with ggmap and ggbar plot objects
#' @export
plot_units_ranked <- function(data.ranked, data.units, year, graph.dir = NULL) {

  # Coerce to data.table so := works regardless of what callers pass
  data.units <- data.table::as.data.table(data.units)
  data.ranked <- data.table::as.data.table(data.ranked)

  # Filter BOTH datasets by year for consistency
  # Use ..year to reference the function argument, not the column
  year_filter <- year
  data.units <- data.units[year == year_filter, ]
  data.ranked <- data.ranked[year == year_filter, ]

  data.units[, uID := as.character(uID)]
  data.ranked[, uID := as.character(uID)]
  unitRanks <- merge(data.ranked, data.units, by = 'uID')

  if (nrow(unitRanks) == 0) {
    stop("No data after filtering to year ", year, " and merging. ",
         "Check that data.ranked and data.units share matching uID values.",
         call. = FALSE)
  }

  ## coordinates
  long <- unitRanks$Longitude
  minlong <-min(long) - 8
  maxlong <-max(long) + 8
  lat <- unitRanks$Latitude
  minlat <-min(lat) - 8
  maxlat <-max(lat) + 8

  uID <- unitRanks$uID
  hyads.py.sum<-unitRanks$hyads.py.sum

  rank<-unitRanks$hyads.rank

  facility_loc <- data.table(x = long, y = lat, hyads.py.sum = hyads.py.sum, rank = rank, uID = uID) %>%
    dplyr::mutate(label = paste("UNIT:", uID, "ranked", rank))

  title <- paste("Ranking (The biggest polluters) for year: ", year)

  ggmap <- ggplot2::ggplot(data = ggplot2::map_data("state")) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      linewidth = 0.25
    ) +
    ggplot2::geom_point(
      data = facility_loc,
      ggplot2::aes(x = x, y = y),
      shape = 1,
      colour = "forestgreen",
      inherit.aes = FALSE,
      size = 2,
      stroke = 2
    )

  # Add labels: use ggrepel if available, otherwise fall back to geom_text
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggmap <- ggmap +
      ggrepel::geom_label_repel(
        data = facility_loc,
        ggplot2::aes(x = x, y = y, label = label),
        nudge_x = 10,
        nudge_y = 10,
        segment.size = 0.7,
        na.rm = TRUE
      )
  } else {
    warning("Package 'ggrepel' not installed; labels may overlap. ",
            "Install with: install.packages('ggrepel')", call. = FALSE)
    ggmap <- ggmap +
      ggplot2::geom_text(
        data = facility_loc,
        ggplot2::aes(x = x, y = y, label = label),
        nudge_x = 2,
        nudge_y = 2,
        size = 3,
        na.rm = TRUE
      )
  }

  ggmap <- ggmap +
    ggplot2::scale_shape_discrete(solid = TRUE) +
    ggplot2::coord_sf(xlim = c(minlong, maxlong),
      ylim = c(minlat, maxlat))+
    ggplot2::theme(legend.position = "bottom")

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_ranking_map.pdf")
    ggplot2::ggsave(path, plot = ggmap, width = 20, height = 20, units = "cm")
  }

  # Pivot exposure + emission columns for faceted bar chart.
  # SOx may not always be present; fall back to hyads.py.sum only.
  pivot_cols <- intersect(c("hyads.py.sum", "SOx"), names(unitRanks))
  if (length(pivot_cols) == 0) {
    stop("unitRanks must contain at least 'hyads.py.sum' for the bar chart.", call. = FALSE)
  }

  label_map <- c(hyads.py.sum = "Hyads Exposure", SOx = "SOx emission")

  unitRanks <- unitRanks %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pivot_cols),
      names_to = "type",
      values_to = "Measurement"
    ) %>%
    dplyr::mutate(type = label_map[type])

  ggbar <- ggplot2::ggplot(data=unitRanks, ggplot2::aes(x = as.character(uID), y = Measurement))+
    ggplot2::geom_bar(stat = 'identity',
      color = "navyblue",
      fill = "grey",
      width = 0.3)+
    ggplot2::facet_wrap(.~type, scales="free")+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(labels = scales::comma)+
    ggplot2::xlab("Unit ID")

  return(list(ggbar = ggbar, ggmap = ggmap))

}
