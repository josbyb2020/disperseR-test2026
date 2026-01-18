#' Run HYSPLIT trajectory or dispersion model
#'
#' @description Executes a HYSPLIT simulation based on model type. Trajectory
#' models use SplitR's hysplit_trajectory(); dispersion models use disperseR's
#' hysplit_dispersion().
#'
#' @param model A model object created by [create_disp_model()] or similar.
#'   Must inherit from either "traj_model" or "disp_model".
#' @param npart Numeric. Number of particles for dispersion runs. Default 2500.
#' @param run.dir Character. Working directory for HYSPLIT output files.
#'   Required for dispersion models.
#'
#' @return The input model object with results attached:
#'   - For traj_model: `model$traj_df` contains trajectory data frame
#'   - For disp_model: `model$disp_df` contains dispersion data frame
#'
#' @details
#' Trajectory models require the SplitR package for HYSPLIT trajectory support.
#'
#' Model parameters (lat, lon, height, duration, met_type, etc.) should be
#' set using [add_params()] before calling run_model().
#'
#' @examples
#' \dontrun{
#' # Create and run a dispersion model
#' model <- create_disp_model() %>%
#'   add_params(lat = 39.9, lon = -75.1, height = 100) %>%
#'   add_emissions(my_emissions) %>%
#'   add_species(my_species) %>%
#'   add_grid(my_grid)
#'
#' result <- run_model(model, run.dir = "~/disperseR_output")
#' }
#'
#' @seealso [create_disp_model()], [add_params()], [hysplit_dispersion()]
#' @export
run_model <- function(model, npart = 2500, run.dir) {

  if (inherits(model, "traj_model")) {
    # Check if SplitR is available for trajectory models
    splitr_traj <- .disperseR_require_splitr(
      feature = "Trajectory models",
      fn = "hysplit_trajectory"
    )

    # Use model$met_dir (set by add_params), not model$meteo which is undefined
    traj_df <- splitr_traj(
      lat = model$lat,
      lon = model$lon,
      height = model$height,
      duration = ifelse(is.null(model$duration), 24, model$duration),
      run_period = model$run_period,
      daily_hours = model$daily_hours,
      direction = ifelse(is.null(model$direction), "forward", model$direction),
      met_type = ifelse(is.null(model$met_type), "reanalysis", model$met_type),
      vert_motion = ifelse(is.null(model$vert_motion), 0, model$vert_motion),
      model_height = ifelse(is.null(model$model_height), 20000, model$model_height),
      extended_met = TRUE,
      return_traj_df = TRUE,
      traj_name = model$traj_name,
      exec_dir = model$exec_dir,
      met_dir = model$met_dir,
      binary_path = model$binary_path
    )

    model$traj_df <- traj_df
    return(model)
  }

  if (inherits(model, "disp_model")) {
    disp_df <- disperseR::hysplit_dispersion(
      lat = model$lat,
      lon = model$lon,
      height = ifelse(is.null(model$height), 50, model$height),
      duration = ifelse(is.null(model$duration), 24, model$duration),
      start_day = model$start_day,
      start_hour = model$start_hour,
      direction = ifelse(is.null(model$direction), "forward", model$direction),
      met_type = ifelse(is.null(model$met_type), "reanalysis", model$met_type),
      met_dir = model$met_dir,
      vert_motion = ifelse(is.null(model$vert_motion), 0, model$vert_motion),
      model_height = ifelse(is.null(model$model_height), 20000, model$model_height),
      particle_num = npart,
      particle_max = 10000,
      emissions = model$emissions,
      species = model$species,
      grids = model$grids,
      return_disp_df = TRUE,
      write_disp_CSV = FALSE,
      run_dir = run.dir
    )

    model$disp_df <- disp_df
    return(model)
  }
  
  stop("Model must be either 'traj_model' or 'disp_model'", call. = FALSE)
}
