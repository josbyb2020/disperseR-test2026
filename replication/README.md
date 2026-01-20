# HyADS Exposure Mapping Replication Study

## Overview

This folder contains a reproducible replication study using the disperseR R package 
to implement the HyADS (HYSPLIT-based Average Daily Snapshots) exposure methodology.

**Project Title:** HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study

## Learning Goals

- Reproduce a published-style HyADS exposure workflow
- Validate the full disperseR pipeline from raw data to results
- Stress test edge cases and document failure modes

## What This Study Produces

- Reproducible analysis scripts
- Figures: dispersion patterns, exposure maps, rankings
- Tables: runtime comparisons, exposure by ZIP code
- Edge-case testing report documenting error handling

## Folder Structure

```
replication/
├── README.md              # This file
├── findings_summary.md    # Results summary
├── helpers.R              # Shared helpers (paths + output dirs)
├── 00_setup.R             # Phase 0: Setup & Verification
├── 01_data.R              # Phase 1: Data Acquisition
├── 02_single_run.R        # Phase 2: Single HYSPLIT Run
├── 03_batch_run.R         # Phase 3: Batch Processing
├── 04_linking.R           # Phase 4: Geographic Linking
├── 05_exposure.R          # Phase 5: Exposure Calculation
├── 06_plots.R             # Phase 6: Visualization
├── 07_edge_cases.R        # Phase 7: Edge-Case Testing
├── report.Rmd             # Final Report Template
├── real_run.R             # Complete working pipeline example
├── extras/                # Optional analyses (see below)
└── outputs/
    ├── figures/           # Generated plots
    ├── tables/            # CSV outputs
    └── logs/              # RDS files and logs
```

## Execution Order

Run scripts in numerical order from the replication folder:

```r
setwd("path/to/disperseR-fork/replication")

source("00_setup.R")       # Creates directories, documents system
source("01_data.R")        # Downloads required datasets
source("02_single_run.R")  # Runs single unit dispersion
source("03_batch_run.R")   # Runs multiple units in parallel
source("04_linking.R")     # Links outputs to ZIP codes
source("05_exposure.R")    # Calculates HyADS exposure
source("06_plots.R")       # Creates figures
source("07_edge_cases.R")  # Tests error handling

# Alternative: Run complete pipeline with real_run.R
source("real_run.R")       # Full pipeline, vignette-based

# Finally, render the report:
rmarkdown::render("report.Rmd")
```

Outputs and downloaded data are generated locally and are not committed to Git.

## Phase Summary

| Phase | Script | Key Functions | Output |
|-------|--------|---------------|--------|
| 0 | 00_setup.R | `create_dirs()` | Directory structure, system info |
| 1 | 01_data.R | `get_data()` | Downloaded datasets (met, PBL, crosswalk) |
| 2 | 02_single_run.R | `define_inputs()`, `run_disperser_parallel()` | Single HYSPLIT run, parameter docs |
| 3 | 03_batch_run.R | `run_disperser_parallel()` | Multi-unit batch run |
| 4 | 04_linking.R | `link_all_units()`, `combine_monthly_links()` | ZIP-linked parcel data |
| 5 | 05_exposure.R | `calculate_exposure()` | HyADS exposure tables |
| 6 | 06_plots.R | `ggplot2` | Figures |
| 7 | 07_edge_cases.R | All (error testing) | Edge-case report |

## Requirements

### Software
- **R >= 4.1.0** (required by disperseR)
- **disperseR package** (from GitHub)
- **splitr package** (provides HYSPLIT binaries)

### Installation

```r
# Install dependencies
install.packages(c("ggplot2", "sf", "terra", "data.table", "fst", "knitr", "rmarkdown"))

# Install disperseR
remotes::install_github("josbyb2020/disperseR-test2026")

# Install splitr for HYSPLIT binaries
remotes::install_github("rich-iannone/splitr")
```

### Apple Silicon Macs

splitr bundles x86_64 HYSPLIT binaries. On ARM Macs, install Rosetta:

```bash
softwareupdate --install-rosetta
```

The disperseR package automatically wraps HYSPLIT calls with `arch -x86_64` when needed.

### Data (Downloaded Automatically)
- ZCTA-County Crosswalk (HUD)
- ZCTA Shapefile (Census TIGER)  
- PBL Height Data (NARR)
- Meteorological Data (NCEP Reanalysis, ~100MB per month)

## Important Notes

1. **Scripts fail gracefully** without met data or HYSPLIT binaries
2. **Met data downloads are large** (~500MB for a full year)
3. **Check outputs/tables/*.csv** for run status
4. **See outputs/logs/edge_case_appendix.md** for error handling documentation

## Windows notes

- Use a short project path (example: `C:/disperseR_project`) to avoid
  Windows path-length limits. You can set `DISPERSER_PROJECT` as an
  environment variable to override the default.
- Parallel runs on Windows use socket clusters. Make sure disperseR is
  installed (not just `devtools::load_all`) or set `mc.cores = 1`.
- If you see splitr-related errors, install `splitr` or provide
  `binary_path` and `parhplot_path` explicitly.

## Optional analyses

Additional scripts are available in `extras/` and assume you have run the
core pipeline:

- `extras/sensitivity_analysis.R`
- `extras/multi_month_run.R`
- `extras/enhanced_maps.R`
- `extras/fix_figures.R`

## Grading Rubric

| Component | Weight |
|-----------|--------|
| Correct pipeline output (runs + links + exposure) | 40% |
| Visualization quality + clarity | 20% |
| Sensitivity analysis | 20% |
| Edge-case documentation | 20% |

## Contact

For questions about disperseR, see the main package documentation.
