# Helpers for replication scripts

replication_ensure_outputs <- function() {
  dirs <- c("outputs", "outputs/logs", "outputs/tables", "outputs/figures")
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

replication_default_project_path <- function() {
  if (.Platform$OS.type == "windows") {
    return("C:/disperseR_project")
  }
  file.path(getwd(), "disperser_project")
}

replication_resolve_project_path <- function() {
  if (file.exists("outputs/logs/directory_config.rds")) {
    existing_dirs <- readRDS("outputs/logs/directory_config.rds")
    if (!is.null(existing_dirs$main_dir)) {
      return(dirname(existing_dirs$main_dir))
    }
  }
  Sys.getenv("DISPERSER_PROJECT", unset = replication_default_project_path())
}

replication_init_dirs <- function(require_existing = FALSE) {
  replication_ensure_outputs()
  if (require_existing && !file.exists("outputs/logs/directory_config.rds")) {
    stop("Run 00_setup.R first to create directories.", call. = FALSE)
  }
  project_path <- replication_resolve_project_path()
  dirs <- disperseR::create_dirs(project_path)
  saveRDS(dirs, "outputs/logs/directory_config.rds")
  dirs
}
