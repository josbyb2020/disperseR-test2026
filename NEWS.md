# disperseR 0.2.1

Windows reliability and error clarity improvements.

## New Features

- Windows path handling: Normalize long paths to 8.3 format to avoid path length limits
- Download resilience: Method fallback (libcurl → wininet → auto) on Windows; proxy support
- Error clarity: Enhanced validation messages with actionable guidance (e.g., how to get required data)
- SpatRaster detection: Warn and auto-fallback to sequential runs when terra::SpatRaster objects 
  are passed to Windows parallel code (socket clusters cannot serialize them)
- Config tracking: `last_run_config.rds` stores `duration_run_hours` and source after each 
  HYSPLIT run; downstream phases auto-detect the correct duration for linking
- Zombie process cleanup: `cleanup_hysplit_zombies()` kills orphaned HYSPLIT processes that may 
  hold file locks after crashed runs (called automatically before batch runs on Windows)
- AV/EDR warning: Alerts users when batch size may trigger antivirus heuristics
- File read retry: Internal `safe_file_read()` handles Windows file locking delays

## Bug Fixes

- Fixed `proc_dir` auto-creation in `run_disperser_parallel()`
- Fixed Windows shell path escaping in `hysplit_dispersion()` for long paths
- Fixed GIS file pattern matching (now requires exact `GIS_part_*_ps.txt` format)
- Fixed `get_met_reanalysis()` error handling on Windows with multiple download methods

## Documentation

- Added comprehensive replication study in `replication/` folder with 8-phase pipeline
- Created `replication/docs/REFERENCE.md` with API reference, parameter guide, troubleshooting
- Enhanced README with Windows-specific notes (path limits, parallel warnings, proxy settings)
- Clarified `monthly_maps` parameter naming in `calculate_exposure()`

## Internal

- Code quality: Fixed quote style consistency in `link_all_units.R`
- Improved proc_dir validation to handle creation failures
- Added `shortPathName()` fallback for Windows paths exceeding limits

---

# disperseR 0.2.0

Major update migrating from retired spatial packages to modern alternatives.

## Breaking Changes

- Requires R >= 4.1.0
- Replaced `rgdal`, `rgeos`, `maptools`, `sp`, `raster` with `sf` and `terra`
- **`calculate_exposure()` API change**: Now requires `monthly_maps` (list from 
  `combine_monthly_links()`) or `rda_file` (path to RData). The old 
  `rda_file = 'loaded'` pattern that probed global environment is removed.
- `combine_monthly_links()` no longer assigns to caller environment; returns
  a named list of data.tables instead

## New Features
 
- Cross-platform parallelization: `mclapply` on Unix/macOS, `parLapply` on Windows
- `path.expand()` support for `~` paths in all directory arguments
- Robust download validation with file existence and size checks
- HYSPLIT binary exit status checks with clear error messages
- Optional `binary_path` and `parhplot_path` support in `hysplit_dispersion()`
  and `run_model()` for custom HYSPLIT installations without splitr
- splitr is optional: use custom binaries via `binary_path`/`parhplot_path`
  if splitr is not available
- New spatial utility functions in `spatial_utils.R`
- Package-level cache replaces `.GlobalEnv` side effects for directory paths
- Added `validate_pipeline()` for quick output summaries and sanity checks
- Windows parallel helpers now warn and fall back to sequential runs when
  disperseR is not installed (e.g., devtools::load_all sessions)

## Bug Fixes

- Fixed `download_file()` warning handler that skipped post-download validation
- Fixed meteorology download validation after `get_met_reanalysis()`
- Fixed `start_day` handling in `hysplit_dispersion()` for Date/POSIXt inputs
- Fixed `run_dir` validation and auto-creation
- Fixed config file checks to use `run_dir` instead of working directory
- Added `is.na()` guard in `start_day` validation
- Eliminated `eval(parse())` and global environment probing in `calculate_exposure()`
- Eliminated `assign()` side effects in `combine_monthly_links()`
- Fixed `get_yearmon()` to preserve leading zero in month (returns "200501" not "20051")
- Fixed PBL height download URL (NOAA PSL endpoint changed)
- Fixed `dispersion_read()` to handle empty folders gracefully and extract hour from filename
- Added input validation to `get_data()`, `run_disperser_parallel()`, `calculate_exposure()`
- Fixed `yearmonth` column formatting in `calculate_exposure()` (YYYYMM with zero-padded month)
- `calculate_exposure()` now uses cached `exp_dir` from `create_dirs()` when available

## Documentation

- Fixed vignette title typo ("DipserseR" -> "DisperseR")
- Removed outdated package references (ggsn, USAboundaries, tidyverse)
- Updated splitr install guidance (now optional with custom binary support)
- Tightened prose throughout vignettes

## Internal

- Added testthat infrastructure with unit tests for core functions
- Removed unused files (bibliography.bib, RESEARCH_FINDINGS.md)
- Cleaned up .Rbuildignore
- Package uses internal cache instead of `.GlobalEnv` for state

# disperseR 0.1.0

- Initial release
