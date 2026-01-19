# disperseR Verification Project

This folder is a self-contained RStudio workflow to validate disperseR
functionality end-to-end. It is designed for users who clone the repo
from GitHub and want a guided, reproducible verification path.

## Quick Start (RStudio)
1. Open `verification/disperseR-verify.Rproj` in RStudio.
2. Copy `config_template.R` to `config.R` and edit the settings.
3. Run:

```r
source("run_all.R")
```

## What Gets Tested
- Core utils: `create_dirs()`, `get_os()`, `get_yearmon()`
- CSV parsing: `dispersion_read()`
- Exposure math: `calculate_exposure()` (smoke + input validation)
- Data downloads: `get_data()`, `get_met_reanalysis()` (optional)
- HYSPLIT: `hysplit_dispersion()`, `run_model()` (optional)
- Pipeline: `run_disperser_parallel()` -> `link_all_units()` ->
  `combine_monthly_links()` -> `calculate_exposure()` (optional)
- Plotting: core map/time series outputs (optional)

## Configuration
Edit `config.R` (copy from template) to enable optional steps:
- `VERIFY_DATA_DOWNLOAD`: run network downloads (Census/NOAA)
- `VERIFY_DOWNLOAD_MET`: download a single reanalysis file
- `VERIFY_RUN_HYSPLIT`: run HYSPLIT binaries (requires binaries)
- `VERIFY_BINARY_PATH` and `VERIFY_PARHPLOT_PATH`: your local binaries
- `VERIFY_MET_DIR`: folder containing meteorology files

If a step is not enabled or a dependency is missing, it will be skipped
with a clear message.

## Outputs
All artifacts are written under `VERIFY_BASE_DIR`. Delete that folder
when you are done, or set `VERIFY_KEEP_ARTIFACTS = TRUE`.

## Troubleshooting
- If `pkgload` is missing, install it: `install.packages("pkgload")`
- If HYSPLIT is missing, set `VERIFY_RUN_HYSPLIT = FALSE`
- Network errors: set `VERIFY_DATA_DOWNLOAD = FALSE` and run offline tests

