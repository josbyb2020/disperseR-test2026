#' Plot exposure impacts from multiple units
#'
#' @description `plot_impact_unit()` produces a two-panel figure. The first
#'   panel is a time-series line plot of HyADS exposure by unit for each
#'   selected ZIP code. The second panel is a map showing the locations of the
#'   selected ZIP codes overlaid on US state boundaries.
#'
#' @param data.linked data for plotting as output from disperseR::calculate_exposure()
#'
#' @param zip.codes list of ZIP codes (as character strings) for ranking
#'
#' @param y.lim vertical limits for the spatial plot
#'
#' @param x.lim horizontal limits for the spatial plot
#'
#' @param plot.title plot title character string
#'
#' @param legend.lims legend limits. as c( lower,upper)
#'
#' @param legend.title legend title as string
#'
#' @param legend.text.angle angle of legend text (helpful for large numbers)
#'
#' @param graph.dir location to save output.
#'
#' @return A \code{gtable} object (from \code{gridExtra::grid.arrange})
#'   containing a time-series panel and a map panel. If \code{graph.dir} is
#'   supplied, the figure is also saved as a PDF.


#' @export plot_impact_unit

plot_impact_unit <- function(data.linked = NULL,
  zip.codes = NULL,
  y.lim = c(24, 50),
  x.lim = c(-123,-69),
  plot.title = NULL,
  legend.lims = NULL,
  legend.title = NULL,
  legend.text.angle = 0,
  graph.dir = NULL) {
  #################################################################################

  if (is.null(plot.title)){
    plot.title <- paste("Zip Code Locations", paste(zip.codes, collapse = ", "))
  }

  if (is.null(data.linked)) {
    stop("Please provide data set to the datalink argument")
  }
  if (is.null(zip.codes)) {
    stop("Please provide zipcodes")
  }

  dataplot <- data.linked[ZIP %in% zip.codes]
  dataplot <- dataplot[, uID := as.character(uID)]
  dataplot <- dataplot[, year := as.numeric(substr(yearmonth, start = 1, stop = 4))]
  dataplot <- dataplot[, month := as.numeric(substr(yearmonth, start = 5, stop = 6))]
  dataplot$date <- with(dataplot, lubridate::ymd(sprintf('%04d%02d%02d', year, month, 1)))

  #################################################################################

  plot1 <-
    ggplot2::ggplot(data = dataplot, ggplot2::aes(
      x = date,
      y = hyads,
      colour = as.factor(uID)
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Exposure", x = "Month") +
    ggplot2::labs(colour = "Unit ID") +
    ggplot2::facet_grid(ZIP ~ ., scales = "free") +
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::scale_x_date(labels = scales::date_format("%m-%Y"))

  #################################################################################

  colorscale <- viridis::scale_color_viridis(
    name = legend.title,
    discrete = FALSE,
    option = 'magma',
    limits = legend.lims,
    oob = scales::squish,
    direction = 1,
    na.value = NA,
    guide = ggplot2::guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      title.vjust = 0 ,
      label.vjust = 1
    )
  )

  fillscale <- viridis::scale_fill_viridis(
    name = legend.title,
    discrete = FALSE,
    option = 'magma',
    limits = legend.lims,
    oob = scales::squish,
    direction = 1,
    na.value = NA,
    guide = ggplot2::guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      title.vjust = 0,
      label.vjust = 1
    )
  )

  coordsf <- ggplot2::coord_sf(xlim = x.lim,
    ylim = y.lim)

  zip_coords <- NULL
  if (exists("zipcodecoordinate", inherits = TRUE)) {
    zip_coords <- get("zipcodecoordinate", inherits = TRUE)
  }
  if (is.null(zip_coords)) {
    utils::data("zipcodecoordinate", package = "disperseR", envir = environment())
    if (exists("zipcodecoordinate", inherits = FALSE)) {
      zip_coords <- get("zipcodecoordinate", inherits = FALSE)
    }
  }
  if (is.null(zip_coords)) {
    stop("Could not load zipcodecoordinate data required for plot_impact_unit().",
      call. = FALSE)
  }
  zip_coords_dt <- data.table::as.data.table(zip_coords)

  plot2 <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(title = plot.title) +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("state"),
      ggplot2::aes(x = long, y = lat, group = group),
      fill = NA,
      colour = "grey50",
      linewidth = 0.25
    ) +
    ggplot2::geom_point(
      data = zip_coords_dt[ZIP %in% zip.codes],
      ggplot2::aes(x = Longitude, y = Latitude),
      shape = 7,
      colour = "blue",
      inherit.aes = FALSE,
      size = 3
    ) +
    ggplot2::scale_shape_discrete(solid = TRUE) +
    coordsf +
    colorscale +
    fillscale +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = c(.20, .15),
      legend.text = ggplot2::element_text(size = 8, angle = legend.text.angle),
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.key.size = grid::unit(.05, 'npc'),
      legend.direction = 'horizontal'
    )

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required for plot_impact_unit(). ",
         "Install it with: install.packages('gridExtra')", call. = FALSE)
  }

  gg <-
    gridExtra::grid.arrange(plot1,
      plot2,
      layout_matrix = rbind(c(1, 2),
        c(1, NA)),
      widths = c(2, 1))

  if (!(is.null(graph.dir))) {
    path <- file.path(graph.dir, "plot_impact_unit.pdf")
    ggplot2::ggsave(path, plot = gg, width = 20, height = 20, units = "cm")
  }

  invisible(gg)
}
