#' disperseR: Run HYSPLIT Many Times in Parallel
#'
#' @description
#' disperseR runs HYSPLIT dispersion models and calculates the HyADS
#' exposure metric, aggregating results to ZIP code level.
#'
#' @importFrom magrittr %>%
#' @import maps
#' @name disperseR-package
#' @aliases disperseR
"_PACKAGE"

# Suppress R CMD check NOTEs for data.table NSE symbols and column names
utils::globalVariables(c(
  # data.table special symbols
  ".N", ".SD", ".I", ".GRP", ":=",
  
  # Column names used in data.table/dplyr operations
  "ID", "uID", "ZIP", "ZCTA", "ZCTA5CE10", "year", "month", "yearmonth", "year.E",
  "Longitude", "Latitude", "lon", "x", "y", "long", "lat", "group",
  "hyads", "hyads.py.sum", "hyads.rank", "N", "SOx",
  "Height", "Height.unit", "comb", "metric", "Measurement", "type", "label",
  "statefp", "countyfp", "state_name", "name", "geoid", "geometry",
  "start_day", "start_hour", "duration", "direction",
  "file", "available", "run_dir", "met_dir",
  "V1", "Pdate", "mo", "yr", "Exposure",
  "STATE", "CITY", "TOTALESTIMATE",
  
  # .GlobalEnv assignments from create_dirs
 "main_dir", "input_dir", "output_dir", "process_dir", "ziplink_dir",
  "hysp_dir", "meteo_dir", "zcta_dir", "raster_dir", "graph_dir",
  "exp_dir", "rank_dir", "crosswalk_dir", "census_dir", "proc_dir",
  "hpbl_dir", "rdata_dir"
))
