# HyADS Exposure Mapping Replication Study

Reproducible replication of the HyADS (HYSPLIT-based Average Daily Snapshots) 
exposure methodology using disperseR.

## Structure

```
replication/
  helpers.R           Shared initialization helpers
  00_setup.R          Creates directories, documents system
  01_data.R           Downloads datasets (met, PBL, crosswalk)
  02_single_run.R     Single unit HYSPLIT proof-of-concept
  03_batch_run.R      Multi-unit parallel batch run
  04_linking.R        Links parcel outputs to ZIP codes
  05_exposure.R       Calculates HyADS exposure
  06_plots.R          Generates figures
  07_edge_cases.R     Tests error handling
  real_run.R          Complete pipeline (single script)
  report.Rmd          Final report template
  outputs/            Generated locally (not committed)
```

## Usage

Run scripts in order from the replication folder:

```r
setwd("path/to/disperseR-fork/replication")

source("00_setup.R")
source("01_data.R")
source("02_single_run.R")
# ... or run the complete pipeline:
source("real_run.R")
```

Outputs are generated in `outputs/` (figures, tables, logs).

## Requirements

| Requirement | Notes |
|-------------|-------|
| R >= 4.1.0 | Required by disperseR |
| disperseR | `remotes::install_github("josbyb2020/disperseR-test2026")` |
| splitr | `remotes::install_github("rich-iannone/splitr")` for HYSPLIT binaries |
| Rosetta (Apple Silicon) | `softwareupdate --install-rosetta` |

## Windows Notes

- Use a short project path (e.g., `C:/disperseR_project`) to avoid path-length limits
- Set `DISPERSER_PROJECT` environment variable to override the default path
- Parallel runs use socket clusters; ensure disperseR is installed or use `mc.cores = 1`

## Phase Reference

| Phase | Script | Key Functions |
|-------|--------|---------------|
| 0 | 00_setup.R | `create_dirs()` |
| 1 | 01_data.R | `get_data()` |
| 2 | 02_single_run.R | `define_inputs()`, `run_disperser_parallel()` |
| 3 | 03_batch_run.R | `run_disperser_parallel()` |
| 4 | 04_linking.R | `link_all_units()`, `combine_monthly_links()` |
| 5 | 05_exposure.R | `calculate_exposure()` |
| 6 | 06_plots.R | ggplot2 visualizations |
| 7 | 07_edge_cases.R | Error handling validation |

Scripts fail gracefully if prerequisites (met data, HYSPLIT binaries) are unavailable.

## Documentation

See `docs/REFERENCE.md` for:
- Full API parameter reference
- Recommended parameter values
- PBL trimming explanation
- Visualization function guide
- Troubleshooting tips
- Data source details
