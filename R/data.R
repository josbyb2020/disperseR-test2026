#' Crosswalk data for ZIP and census geographies
#'
#' @description Crosswalk table used to link ZIP codes to counties and other
#' census geographies.
#' @format data.table with ZIP identifiers and associated geography fields.
"crosswalk"

#' Power plant unit metadata
#'
#' @description Unit-level metadata used in dispersion runs, including unit IDs
#' and coordinates.
#' @format data.table with unit identifiers, location, and year fields.
"units"

#' ZIP code coordinate reference
#'
#' @description Coordinates for ZIP code centroids used in mapping.
#' @format data.table with ZIP and coordinate columns.
"zipcodecoordinate"

#' Monthly unit emissions (1995-2017)
#'
#' @description Historical monthly emissions data for power plant units.
#' @format data.table with unit identifiers, year, month, and emissions fields.
"PP.units.monthly1995_2017"
