# disperseR 0.2.1

Windows reliability, performance, correctness, and API cleanup improvements.

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
- Apple Silicon support: Automatic Rosetta wrapping for x86_64 HYSPLIT binaries on ARM Macs
- ID validation: Filesystem-safe ID enforcement prevents cross-platform filename failures
  (rejects `/`, `\`, `:*?"<>|`, and control characters in unit IDs)
- `run_disperser_parallel()`: Added `binary_path` and `parhplot_path` parameters, threaded
  through all execution paths (sequential, Windows `parLapply`, Unix `mclapply`)
- `run_disperser_parallel()`: Progress reporting with run counter and estimated completion
- `run_disperser_parallel()`: Default `verbose = TRUE` for better user feedback
- `calculate_exposure()`: New `allow.partial` parameter to proceed with incomplete monthly data
- `combine_monthly_links()`: Warns when months are skipped; attaches `"missing_months"` attribute
  to the result so users know which months were unavailable

## Bug Fixes

- **CRITICAL**: Fixed unit ID mangling in `calculate_exposure()` and
  `rankfacs_by_popwgt_location()` -- `gsub('_|-|\\*', '.', ...)` silently converted
  hyphenated unit IDs (e.g., "3136-1" to "3136.1"), causing merge failures with emissions
  data and producing zero exposure values
- Fixed `proc_dir` auto-creation in `run_disperser_parallel()`
- Fixed Windows shell path escaping in `hysplit_dispersion()` for long paths
- Fixed GIS file pattern matching (now requires exact `GIS_part_*_ps.txt` format)
- Fixed `get_met_reanalysis()` error handling on Windows with multiple download methods
- Fixed HYSPLIT grid control writing to avoid index leakage in grid date fields
- Fixed link file matching for unit IDs with regex metacharacters
- Fixed `combine_monthly_links()` grid alignment: replaced `terra::rast()` with pure data.table
  approach to avoid crashes on sparse or single-cell grids
- Fixed `get_output_df()` returning `NA` on empty results (now returns empty data.table with
  correct schema and issues a warning)
- Fixed `get_yearmon()`: replaced `|` with `||` for scalar validation, added `start <= end`
  check and `NULL`/`NA` input guards
- Fixed silent parallel worker failures in `link_all_units()`: now warns with count of
  dropped workers after `Filter(is.data.table, ...)`
- Fixed `combine_monthly_links_subfun.R`: wrapped `read.fst()` in `tryCatch` to handle
  corrupted files gracefully instead of crashing
- Fixed `plot_impact_weighted()`: added `time.agg` validation (now errors on invalid values
  instead of returning `NULL` silently)

## Performance

- Eliminated O(n^2) accumulate-and-re-aggregate pattern in `calculate_exposure()`: monthly
  results are now collected in a pre-allocated list and merged in a single `rbindlist()` pass
- Replaced `utils::read.csv()` with `data.table::fread()` in `dispersion_read()` (5-10x faster)
- Eliminated double `dispersion_read()` call in `hysplit_dispersion()` (read once, use for
  both file write and return)
- Replaced `utils::write.table()` with `data.table::fwrite()` in `hysplit_dispersion()`
- Consolidated 13+ `cat(append=TRUE)` calls into single `writeLines()` for HYSPLIT CONTROL file
- Moved `terra::rotate()` for PBL rasters from per-unit (inside loop) to once-before-loop
  in `link_all_units()`

## API Changes

- Reduced exported namespace from 55 to 23 functions. Internal helpers in `spatial_utils.R`
  (14 functions), `hysplit_dispersion_subfun.R`, `windows_utils.R`, and `get_data.R` are now
  `@keywords internal`. All remain accessible via `disperseR:::` for advanced users.

## Documentation

- Added comprehensive replication study in `replication/` folder with 8-phase pipeline
- Created `replication/docs/REFERENCE.md` with API reference, parameter guide, troubleshooting
- Enhanced README with Windows-specific notes (path limits, parallel warnings, proxy settings)
- Clarified `monthly_maps` parameter naming in `calculate_exposure()`
- Documented filesystem-safe ID requirements for cross-platform runs
- Expanded dataset documentation (`units`, `PP.units.monthly1995_2017`, `crosswalk`,
  `zipcodecoordinate`) with full column descriptions in `@format` blocks
- Fixed roxygen titles for `get_yearmon()`, `plot_impact_unit()`, `plot_impact_single()`
- Fixed main vignette: resolved undefined variables after `create_dirs()`/`get_data()`,
  corrected date ranges for 120h duration consistency, replaced retired `USAboundaries`
  with `sf::st_as_sf(maps::map(...))`, fixed month format ("20061" -> "200601")
- Fixed crosswalk preparation vignette: corrected package name "hyspdisp" -> "disperseR"
- Fixed units preparation vignette: swapped incorrect Velocity/Temp unit conversions
- Fixed README.Rmd: syntax error, missing `proc_dir` in quick example, added R version note

## Internal

- Code quality: Fixed quote style consistency in `link_all_units.R`
- Improved proc_dir validation to handle creation failures
- Added `shortPathName()` fallback for Windows paths exceeding limits
- Path traversal guard in `run_disperser_parallel()` prevents `unlink()` outside `proc_dir`
- Rewrote `trim_zero()` for better performance on large datasets (data.table merge)
- Changed intra-package calls from `disperseR::fn()` to bare `fn()` to avoid namespace issues
- Added unit tests: `test-define_inputs.R` (11 tests), `test-calculate_exposure_numeric.R`
  (4 tests including hyphenated ID regression), `test-integration-pipeline.R` (3 tests)
- Updated existing tests to use `disperseR:::` for newly-internal functions
- Test suite: 0 failures, 146 passes, 16 skips

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
