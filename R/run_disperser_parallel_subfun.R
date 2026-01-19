#' Extract output from a model object
#'
#' @param model A `traj_model` or `disp_model` list.
#' @return A data.frame containing model output, or `NA` if absent.
#' @export

#########################################################
################# get_output_df

get_output_df <- function(model) {
  if (inherits(model, "traj_model")) {
    if (is.null(model$traj_df)) {
      return(NA)
    }
    return(model$traj_df)
  }

  if (inherits(model, "disp_model")) {
    if (is.null(model$disp_df)) {
      return(NA)
    }
    return(model$disp_df)
  }
}

#########################################################
################# dispersion_read

#' Read dispersion CSV outputs from a run directory
#'
#' Reads `GIS_part_###_ps.csv` files produced by parhplot and combines them
#' into a single data.frame. The hour is extracted from each filename (the
#' numeric suffix), supporting any number of hours per run.
#'
#' @param archive_folder Directory containing `GIS_part_###_ps.csv` files.
#' @return A data.frame of particle positions with columns: particle_no, lon,
#'   lat, height, hour. Returns an empty data.frame with a warning if no
#'   matching files are found.
#' @export
dispersion_read <- function(archive_folder) {

  if (!dir.exists(archive_folder)) {
    warning("archive_folder does not exist: ", archive_folder, call. = FALSE)
    return(data.frame(
      particle_no = integer(0), lon = numeric(0), lat = numeric(0),
      height = numeric(0), hour = integer(0)
    ))
  }

  dispersion_file_list <- list.files(
    path = archive_folder,
    pattern = "^GIS_part_[0-9]+_ps\\.csv$",
    full.names = TRUE
  )

  if (length(dispersion_file_list) == 0) {
    warning("No GIS_part_*_ps.csv files found in: ", archive_folder, call. = FALSE)
    return(data.frame(
      particle_no = integer(0), lon = numeric(0), lat = numeric(0),
      height = numeric(0), hour = integer(0)
    ))
  }

  # Pre-allocate result list for efficiency
  result_list <- vector("list", length(dispersion_file_list))

  for (i in seq_along(dispersion_file_list)) {
    fpath <- dispersion_file_list[i]
    # Extract hour from filename (e.g., GIS_part_001_ps.csv -> 1)
    fname <- basename(fpath)
    hour_str <- sub("^GIS_part_([0-9]+)_ps\\.csv$", "\\1", fname)
    hour_val <- as.integer(hour_str)

    disp <- utils::read.csv(fpath, header = FALSE)
    colnames(disp) <- c("particle_no", "lon", "lat", "height")
    disp$hour <- hour_val
    result_list[[i]] <- disp
  }

  dispersion <- do.call(rbind, result_list)
  return(dispersion)
}

#########################################################
################# add_params

#' Add or update model parameters
#'
#' @param model A model object created by `create_disp_model()`.
#' @param lat,lon,height Coordinates and release height.
#' @param duration,run_period Run durations.
#' @param start_day,start_hour Start date and hour.
#' @param daily_hours Vector of daily start hours.
#' @param direction Direction of run ("forward" or "backward").
#' @param met_type Meteorology type (e.g., "reanalysis").
#' @param vert_motion Vertical motion setting.
#' @param model_height Maximum model height.
#' @param traj_name Optional trajectory name.
#' @param exec_dir Directory containing HYSPLIT executables.
#' @param met_dir Directory containing meteorological files.
#' @param binary_path Path to a custom HYSPLIT dispersion binary (hycs_std).
#' @param parhplot_path Path to a custom parhplot binary for particle output.
#' @return Updated model object.
#' @export
add_params <- function(model,
  lat = NULL,
  lon = NULL,
  height = NULL,
  duration = NULL,
  run_period = NULL,
  start_day = NULL,
  start_hour = NULL,
  daily_hours = NULL,
  direction = NULL,
  met_type = NULL,
  vert_motion = NULL,
  model_height = NULL,
  traj_name = NULL,
  exec_dir = NULL,
  met_dir = NULL,
  binary_path = NULL,
  parhplot_path = NULL) {
  if (!is.null(lat)) {
    model$lat <- lat
  }

  if (!is.null(lon)) {
    model$lon <- lon
  }

  if (!is.null(height)) {
    model$height <- height
  }

  if (!is.null(duration)) {
    model$duration <- duration
  }

  if (!is.null(run_period)) {
    model$run_period <- run_period
  }

  if (!is.null(start_day)) {
    model$start_day <- start_day
  }

  if (!is.null(start_hour)) {
    model$start_hour <- start_hour
  }

  if (!is.null(daily_hours)) {
    model$daily_hours <- daily_hours
  }

  if (!is.null(direction)) {
    model$direction <- direction
  }

  if (!is.null(met_type)) {
    model$met_type <- met_type
  }

  if (!is.null(vert_motion)) {
    model$vert_motion <- vert_motion
  }

  if (!is.null(model_height)) {
    model$model_height <- model_height
  }

  if (!is.null(traj_name)) {
    model$traj_name <- traj_name
  }

  if (!is.null(exec_dir)) {
    model$exec_dir <- exec_dir
  }

  if (!is.null(met_dir)) {
    model$met_dir <- met_dir
  }

  if (!is.null(binary_path)) {
    model$binary_path <- binary_path
  }

  if (!is.null(parhplot_path)) {
    model$parhplot_path <- parhplot_path
  }

  return(model)
}

#########################################################
################# create_disp_model
#' Create a dispersion model object
#'
#' @param name Optional model name.
#' @return A list with class `disp_model`.
#' @export
# Create the 'disp_model' list object
create_disp_model <- function(name = NULL) {
  # Create the 'disp_model' list object
  disp_model <-
    list(
      disp_name = NULL,
      lat = NULL,
      lon = NULL,
      height = NULL,
      duration = NULL,
      start_day = NULL,
      start_hour = NULL,
      direction = "forward",
      met_type = NULL,
      emissions = NULL,
      species = NULL,
      grids = NULL,
      vert_motion = 0,
      model_height = 20000,
      disp_df = NULL
    )

  attr(disp_model, "class") <- "disp_model"

  if (!is.null(name))
    disp_model$disp_name <- name

  return(disp_model)
}


#########################################################
################# add_species
#' Add a species definition to a model
#'
#' @param model A model object created by `create_disp_model()`.
#' @param name Species name (defaults to "species_1", "species_2", ...).
#' @param pdiam Particle diameter.
#' @param density Particle density.
#' @param shape_factor Shape factor.
#' @param ddep_vel Dry deposition velocity.
#' @param ddep_mw Dry deposition molecular weight.
#' @param ddep_a_ratio Dry deposition A ratio.
#' @param ddep_d_ratio Dry deposition D ratio.
#' @param ddep_hl_coeff Dry deposition Henry's law coefficient.
#' @param wdep_hl_coeff Wet deposition Henry's law coefficient.
#' @param wdep_in_cloud Wet deposition in-cloud coefficient.
#' @param wdep_below_cloud Wet deposition below-cloud coefficient.
#' @param rad_decay Radioactive decay.
#' @param resuspension Resuspension factor.
#' @return Updated model object.
#' @export
add_species <- function(model,
  name = NULL,
  pdiam = NULL,
  density = NULL,
  shape_factor = NULL,
  ddep_vel = NULL,
  ddep_mw = NULL,
  ddep_a_ratio = NULL,
  ddep_d_ratio = NULL,
  ddep_hl_coeff = NULL,
  wdep_hl_coeff = NULL,
  wdep_in_cloud = NULL,
  wdep_below_cloud = NULL,
  rad_decay = NULL,
  resuspension = NULL) {
  if (is.null(name)) {
    if (is.null(model$species)) {
      name <- "species_1"
    } else {
      name <- paste0("species_",
        nrow(model$species) + 1)
    }
  }

  if (is.null(pdiam)) {
    pdiam <- 15.0
  }

  if (is.null(density)) {
    density <- 1.0
  }

  if (is.null(shape_factor)) {
    shape_factor <- 1.0
  }

  if (is.null(ddep_vel)) {
    ddep_vel <- 0.0
  }

  if (is.null(ddep_mw)) {
    ddep_mw <- 0.0
  }

  if (is.null(ddep_a_ratio)) {
    ddep_a_ratio <- 0.0
  }

  if (is.null(ddep_d_ratio)) {
    ddep_d_ratio <- 0.0
  }

  if (is.null(ddep_hl_coeff)) {
    ddep_hl_coeff <- 0.0
  }

  if (is.null(wdep_hl_coeff)) {
    wdep_hl_coeff <- 0.0
  }

  if (is.null(wdep_in_cloud)) {
    wdep_in_cloud <- 0.0
  }

  if (is.null(wdep_below_cloud)) {
    wdep_below_cloud <- 0.0
  }

  if (is.null(rad_decay)) {
    rad_decay <- 0.0
  }

  if (is.null(resuspension)) {
    resuspension <- 0.0
  }

  # Write species parameters to a data frame
  species <-
    data.frame(
      name = name,
      pdiam = pdiam,
      density = density,
      shape_factor = shape_factor,
      ddep_vel = ddep_vel,
      ddep_mw = ddep_mw,
      ddep_a_ratio = ddep_a_ratio,
      ddep_d_ratio = ddep_d_ratio,
      ddep_hl_coeff = ddep_hl_coeff,
      wdep_hl_coeff = wdep_hl_coeff,
      wdep_in_cloud = wdep_in_cloud,
      wdep_below_cloud = wdep_below_cloud,
      rad_decay = rad_decay,
      resuspension = resuspension,
      stringsAsFactors = FALSE
    )

  # Write data frame to the `species` list
  # component of `model`
  if (is.null(model$species)) {
    model$species <- species
  } else {
    model$species <-
      rbind(model$species, species)
  }

  return(model)
}


#########################################################
################# add_emissions
#' Add emissions settings to a model
#'
#' @param model A model object created by `create_disp_model()`.
#' @param rate Emissions rate.
#' @param duration Emissions duration.
#' @param start_day Start day for emissions in "YYYY-MM-DD" format.
#' @param start_hour Start hour for emissions.
#' @param name Emissions name (defaults to "emissions_1", "emissions_2", ...).
#' @return Updated model object.
#' @export
add_emissions <- function(model,
  rate = NULL,
  duration = NULL,
  start_day = NULL,
  start_hour = NULL,
  name = NULL) {
  if (is.null(name)) {
    if (is.null(model$emissions)) {
      name <- "emissions_1"
    } else {
      name <- paste0("emissions_",
        nrow(model$emissions) + 1)
    }
  }

  if (is.null(rate)) {
    rate <- 1
  }

  if (is.null(duration)) {
    duration <- 1
  }

  if (is.null(start_day)) {
    # Use YYYY-MM-DD format consistent with hysplit_dispersion()
    start_day <- format(Sys.Date(), "%Y-%m-%d")
  }

  if (is.null(start_hour)) {
    start_hour <- 0
  }


  # Write emissions parameters to a data frame
  emissions <-
    data.frame(
      name = name,
      rate = rate,
      duration = duration,
      start_day = start_day,
      start_hour = start_hour,
      stringsAsFactors = FALSE
    )

  # Write data frame to the `emissions` list
  # component of `model`
  if (is.null(model$emissions)) {
    model$emissions <- emissions
  } else {
    model$emissions <-
      rbind(model$emissions, emissions)
  }

  return(model)
}


#########################################################
################# create_grid (local implementation)
#' Create a grid of lat/lon points
#' @description Creates a regular grid of latitude/longitude points.
#' This is a local implementation based on SplitR::create_grid.
#' @param lat Center latitude
#' @param lon Center longitude
#' @param range Range around center (lat, lon)
#' @param division Grid spacing (lat, lon)
#' @return List with lat and lon vectors
#' @keywords internal
create_grid <- function(lat, lon, range, division) {
  # Calculate grid boundaries
  lat_min <- lat - range[1] / 2
  lat_max <- lat + range[1] / 2
  lon_min <- lon - range[2] / 2
  lon_max <- lon + range[2] / 2
  
  # Create sequences
  lat_seq <- seq(lat_min, lat_max, by = division[1])
  lon_seq <- seq(lon_min, lon_max, by = division[2])
  
  # Create grid
  grid_expand <- expand.grid(lat = lat_seq, lon = lon_seq)
  
  list(lat = grid_expand$lat, lon = grid_expand$lon)
}

#########################################################
################# add_grid
#' Add a grid specification to a model
#'
#' @param model A `disp_model` or `traj_model` object.
#' @param lat,lon Center coordinates for the grid.
#' @param range Range around center (lat, lon).
#' @param division Grid spacing (lat, lon).
#' @param start_day,start_hour,end_day,end_hour Grid timing fields.
#' @param duration Duration for grid output.
#' @param heights Output heights.
#' @param samp_type Sampling type.
#' @param samp_interval Sampling interval in hours.
#' @param name Grid name (defaults to "grid_1", "grid_2", ...).
#' @return Updated model object.
#' @export
add_grid <- function(model,
  lat = NULL,
  lon = NULL,
  range = c(5, 5),
  division = c(0.5, 0.5),
  start_day = NULL,
  start_hour = NULL,
  end_day = NULL,
  end_hour = NULL,
  duration = NULL,
  heights = NULL,
  samp_type = "avg",
  samp_interval = 24,
  name = NULL) {
  if (inherits(model, "traj_model")) {
    # Obtain the grid of lat/lon points using local create_grid
    grid <- create_grid(
      lat = lat,
      lon = lon,
      range = range,
      division = division
    )

    # Add the grid points to the model object
    model$lat <- grid$lat
    model$lon <- grid$lon

    return(model)
  }

  if (inherits(model, "disp_model")) {
    if (is.null(name)) {
      if (is.null(model$grids)) {
        name <- "grid_1"
      } else {
        name <- paste0("grid_",
          nrow(model$grids) + 1)
      }
    }

    if (is.null(lat)) {
      if (is.null(model$lat)) {
        lat <- NA
      } else {
        lat <- model$lat
      }
    }

    if (is.null(lon)) {
      if (is.null(model$lon)) {
        lon <- NA
      } else {
        lon <- model$lon
      }
    }

    if (is.null(heights)) {
      heights <- 50
      layers <- 1
    } else {
      layers <- length(heights)
      heights <-
        paste(heights, collapse = " ")
    }

    if (is.null(start_day)) {
      if (!is.null(model$start_day)) {
        start_day <- model$start_day
      } else {
        start_day <- NA
      }
    }

    if (is.null(start_hour)) {
      if (!is.null(model$start_hour)) {
        start_hour <- model$start_hour
      } else {
        start_hour <- NA
      }
    }

    if (is.null(end_day) &
        is.null(end_hour)) {
      duration <- NA
      end_day <- NA
      end_hour <- NA
    }

    # Write grid parameters to a data frame
    grid <-
      data.frame(
        name = name,
        lat = lat,
        lon = lon,
        range_lat = range[1],
        range_lon = range[2],
        division_lat = division[1],
        division_lon = division[2],
        duration = duration,
        start_day = start_day,
        start_hour = start_hour,
        end_day = end_day,
        end_hour = end_hour,
        heights = heights,
        samp_type = samp_type,
        samp_interval = samp_interval,
        stringsAsFactors = FALSE
      )

    # Write data frame to the `grids` list
    # component of `model`
    if (is.null(model$grids)) {
      model$grids <- grid
    } else {
      model$grids <-
        rbind(model$grids, grid)
    }

    return(model)
  }
}


#########################################################
################# subset_nc_date
#' Subset NetCDF raster by date
#'
#' @description Extracts a single layer from a multi-layer PBL raster 
#' based on the specified date.
#'
#' @param hpbl_file Path to NetCDF file (optional)
#' @param hpbl_brick SpatRaster object (optional)
#' @param varname Variable name in NetCDF
#' @param vardate Date to extract
#' @return Single-layer SpatRaster
#' @export
#' @importFrom terra rast rotate subset nlyr names
#' @importFrom lubridate year month
subset_nc_date <- function(hpbl_file = NULL,
                           hpbl_brick = NULL,
                           varname = NULL,
                           vardate) {
  
  if ((is.null(hpbl_file) & is.null(hpbl_brick)) |
      (!is.null(hpbl_file) & !is.null(hpbl_brick))) {
    stop("Please define EITHER hpbl_file OR hpbl_brick, not both or neither")
  }

  old_tz <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_tz), add = TRUE)

  # Read raster using terra (replaces raster::brick)
  if (!is.null(hpbl_file)) {
    if (!is.null(varname)) {
      rasterin <- terra::rotate(terra::rast(hpbl_file, subds = varname))
    } else {
      rasterin <- terra::rotate(terra::rast(hpbl_file))
    }
  }
  if (!is.null(hpbl_brick)) {
    rasterin <- hpbl_brick
  }

  # Get time vector from terra (NetCDF time dimension)
  time_vals <- terra::time(rasterin)
  
  # Get first day of the month for vardate
  vardate_month <- as.Date(paste(
    lubridate::year(vardate),
    lubridate::month(vardate),
    '01',
    sep = '-'
  ))

  # Select layer by matching year-month
  if (!is.null(time_vals) && length(time_vals) > 0) {
    # Match by year-month (first of month)
    target_ym <- format(vardate_month, "%Y-%m")
    time_ym <- format(time_vals, "%Y-%m")
    layer <- which(time_ym == target_ym)
  } else {
    # Fallback: try layer names (legacy format)
    dates <- terra::names(rasterin)
    dates <- gsub('X', '', dates)
    dates <- gsub('\\.', '-', dates)
    dates <- gsub('_', '-', dates)
    
    layer <- tryCatch(
      which(as.Date(dates) == vardate_month),
      error = function(e) {
        # Try matching by year-month only  
        target_ym <- format(vardate_month, "%Y-%m")
        dates_ym <- substr(dates, 1, 7)
        which(dates_ym == target_ym)
      }
    )
  }
  
  if (length(layer) == 0) {
    stop("Cannot match dates of PBL raster file. Ensure TZ is set to UTC before reading.")
  }

  # Use terra::subset instead of raster::subset
  rastersub <- terra::subset(rasterin, subset = layer[1])

  return(rastersub)
}
