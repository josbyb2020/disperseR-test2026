#' ZCTA-to-ZIP crosswalk with population estimates
#'
#' @description Crosswalk table used to link ZIP codes to ZIP Code Tabulation
#'   Areas (ZCTAs) and other census geographies. Includes city, state, and
#'   population estimates.
#'
#' @format A \code{data.table} with 41,151 rows and 6 columns:
#' \describe{
#'   \item{ZIP}{\code{character}. Five-digit ZIP code.}
#'   \item{CITY}{\code{character}. City name associated with the ZIP code.}
#'   \item{ZCTA}{\code{character}. Five-digit ZIP Code Tabulation Area
#'     identifier that the ZIP code maps to.}
#'   \item{STATE}{\code{character}. Two-letter US state or territory
#'     abbreviation (e.g., \code{"NY"}, \code{"PR"}).}
#'   \item{TOTALESTIMATE}{\code{integer}. Total population estimate for the
#'     ZCTA from the American Community Survey.}
#'   \item{MARGINOFERROR}{\code{factor}. Margin of error for the population
#'     estimate. Values of \code{"*****"} indicate that the estimate is not
#'     applicable or suppressed.}
#' }
"crosswalk"

#' Power plant unit metadata (1995--2018)
#'
#' @description Unit-level metadata for coal-fired power plant units used in
#'   HyADS dispersion runs. Contains annual records of unit identifiers,
#'   geographic coordinates, stack height, and annual emissions of SOx, CO2,
#'   and NOx.
#'
#' @format A \code{data.table} with 25,089 rows and 10 columns:
#' \describe{
#'   \item{ID}{\code{character}. Unique unit identifier in
#'     \code{"FacilityID-UnitID"} format (e.g., \code{"7-1"}).}
#'   \item{Latitude}{\code{numeric}. Latitude of the unit in decimal degrees
#'     (NAD83).}
#'   \item{Longitude}{\code{numeric}. Longitude of the unit in decimal degrees
#'     (NAD83).}
#'   \item{SOx}{\code{numeric}. Annual sulfur oxide emissions in tons.}
#'   \item{CO2}{\code{numeric}. Annual carbon dioxide emissions in tons.}
#'   \item{NOx}{\code{numeric}. Annual nitrogen oxide emissions in tons.}
#'   \item{Height}{\code{numeric}. Stack height in meters.}
#'   \item{inputed}{\code{numeric}. Flag indicating whether emissions were
#'     imputed (\code{1}) or observed (\code{0}).}
#'   \item{year}{\code{integer}. Calendar year of the record (1995--2018).}
#'   \item{uID}{\code{character}. Unique unit identifier in dotted format
#'     (e.g., \code{"7.1"}), used as a key in disperseR internals.}
#' }
"units"

#' ZIP code centroid coordinates
#'
#' @description Geographic coordinates for US ZIP code centroids, used for
#'   spatial mapping and linking exposure results to ZIP code locations.
#'
#' @format A \code{data.frame} with 43,191 rows and 4 columns:
#' \describe{
#'   \item{ZIP}{\code{integer}. Five-digit ZIP code (stored as integer).}
#'   \item{City}{\code{factor}. City name associated with the ZIP code.}
#'   \item{Latitude}{\code{numeric}. Latitude of the ZIP code centroid in
#'     decimal degrees.}
#'   \item{Longitude}{\code{numeric}. Longitude of the ZIP code centroid in
#'     decimal degrees.}
#' }
"zipcodecoordinate"

#' Monthly unit emissions (1995--2017)
#'
#' @description Historical monthly emissions data for power plant units
#'   reported to the US EPA Clean Air Markets Division. Covers 1995 through
#'   2017 at monthly resolution.
#'
#' @format A \code{data.table} with 966,912 rows and 11 columns:
#' \describe{
#'   \item{FacID}{\code{integer}. EPA facility (plant) identifier.}
#'   \item{Unit.ID}{\code{character}. Unit identifier within the facility.}
#'   \item{Latitude}{\code{numeric}. Latitude of the facility in decimal
#'     degrees.}
#'   \item{Longitude}{\code{numeric}. Longitude of the facility in decimal
#'     degrees.}
#'   \item{year}{\code{integer}. Calendar year of the record.}
#'   \item{month}{\code{integer}. Month of the record (1--12).}
#'   \item{SO2.tons}{\code{numeric}. Monthly sulfur dioxide emissions in
#'     tons.}
#'   \item{NOx.tons}{\code{numeric}. Monthly nitrogen oxide emissions in
#'     tons.}
#'   \item{HeatIn.MMBtu}{\code{numeric}. Monthly heat input in million
#'     BTU.}
#'   \item{GrossLoad.MWh}{\code{numeric}. Monthly gross electrical load in
#'     megawatt-hours.}
#'   \item{uID}{\code{character}. Unique unit identifier in dotted format
#'     (e.g., \code{"3.1"}), matching the \code{uID} column in
#'     \code{\link{units}}.}
#' }
"PP.units.monthly1995_2017"
