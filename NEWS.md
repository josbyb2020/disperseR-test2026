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
  and `run_model()` for custom HYSPLIT installations without SplitR
- SplitR is optional: use custom binaries via `binary_path`/`parhplot_path`
  if SplitR is not available
- New spatial utility functions in `spatial_utils.R`
- Package-level cache replaces `.GlobalEnv` side effects for directory paths
- Added `validate_pipeline()` for quick output summaries and sanity checks

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
- Updated SplitR install guidance (now optional with custom binary support)
- Tightened prose throughout vignettes

## Internal

- Added testthat infrastructure with unit tests for core functions
- Removed unused files (bibliography.bib, RESEARCH_FINDINGS.md)
- Cleaned up .Rbuildignore
- Package uses internal cache instead of `.GlobalEnv` for state

# disperseR 0.1.0

- Initial release
