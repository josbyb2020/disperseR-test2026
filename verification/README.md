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

`run_all.R` is strict: it now exits with an error if any verification step fails.

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

## Performance Benchmark (Fast Linking Engine)
Run a targeted benchmark that compares `engine="legacy"` vs `engine="fast"`
for ZIP linking on synthetic data:

```r
source("06_perf_linking_engine.R")
```

Optional environment variables for benchmark scale:
- `DISPERSER_BENCH_PARTICLES` (default `200000`)
- `DISPERSER_BENCH_GRID_X` (default `40`)
- `DISPERSER_BENCH_GRID_Y` (default `20`)

Outputs are saved to `VERIFY_BASE_DIR/perf/linking_engine_benchmark.csv`.
Runtime tuning option for adaptive fast linking:
- `options(disperseR.fast.extract.min.cells = 5000L)`
- `options(disperseR.fast.extract.min.cell_poly_ratio = 2)`
- `options(disperseR.fast.project.enable = FALSE)` (parity-safe default)
- `options(disperseR.fast.project.min_rows = 50000L)`
- `options(disperseR.fast.crop.min.cover_ratio = 0.98)`
- `options(disperseR.parallel.dt.threads = 1L)`

## User-Flow Benchmark (End-to-End Linking)
Run end-user style benchmarks through the public `link_all_units()` API
for both `zips` and `counties`, with parity checks:

```r
source("07_perf_user_flow_linking.R")
```

Optional profile selector:
- `DISPERSER_USERFLOW_PROFILE=smoke` (default, quick matrix)
- `DISPERSER_USERFLOW_PROFILE=full` (adds dense ZIP polygon scenarios)

Outputs are saved to:
- `VERIFY_BASE_DIR/perf/userflow_link_all_units_smoke.csv`
- `VERIFY_BASE_DIR/perf/userflow_link_all_units_full.csv`

## Heavy User-Flow Benchmark (Large Workloads)
Run a heavy-load benchmark that simulates user-scale workloads through the
public `link_all_units()` API for both ZIPs and counties:

```r
source("08_perf_user_heavy_linking.R")
```

Optional environment variables:
- `DISPERSER_HEAVY_UNITS` (default `8`)
- `DISPERSER_HEAVY_MONTHS` (default `2`)
- `DISPERSER_HEAVY_ROWS` (default `200000`)
- `DISPERSER_HEAVY_GRID_X` (default `120`)
- `DISPERSER_HEAVY_GRID_Y` (default `100`)
- `DISPERSER_HEAVY_CORES` (default `4`)

Output is saved to:
- `VERIFY_BASE_DIR/perf/heavy_user_flow_link_all_units.csv`

## Cross-Platform CI Benchmark Runner
Run all three benchmark suites (engine, user-flow, heavy) and emit a combined
CSV summary that is suitable for Linux/Windows CI artifacts:

```r
source("09_perf_crossplatform_ci.R")
```

Combined output is saved to:
- `VERIFY_BASE_DIR/perf/crossplatform_ci_summary.csv`

Optional fast-projection environment controls for this runner:
- `DISPERSER_FAST_PROJECT_ENABLE` (default `false`; set `true` to enable `sf_project` path)
- `DISPERSER_FAST_PROJECT_MIN_ROWS` (default `50000`)

## CI Performance Assertions
Enforce parity and minimum speedup floors on the combined CI summary:

```r
source("10_perf_ci_assertions.R")
```

Optional threshold environment variables:
- `DISPERSER_MIN_SPEEDUP_ENGINE` (default: Linux/macOS `1.05`; Windows smoke/full `0.90/0.95`)
- `DISPERSER_MIN_ROWS_EXPECTED_FAST` (default: `50000`; only user-flow rows at or above this count are held to the expected-fast floor)
- `DISPERSER_MIN_SPEEDUP_USERFLOW_EXPECTED_FAST` (default: Linux/macOS `0.95`; Windows smoke/full `0.90/0.95`)
- `DISPERSER_MIN_SPEEDUP_USERFLOW_NON_EXTRACT` (default: Linux/macOS `0.70`; Windows smoke/full `0.65/0.70`)
- `DISPERSER_MIN_SPEEDUP_USERFLOW_NON_EXTRACT_NON_STRICT` (default: Linux/macOS `0.65`; Windows smoke/full `0.60/0.65`)
- `DISPERSER_MIN_USERFLOW_LEGACY_SEC_STRICT` (default: `1`; when a fallback user-flow scenario runs faster than this in legacy, the non-strict floor is used)
- `DISPERSER_MIN_SPEEDUP_USERFLOW` (legacy single-floor fallback when `expected_fast_extract` is unavailable)
- `DISPERSER_MIN_SPEEDUP_HEAVY` (default: Linux/macOS `1.05`; Windows smoke/full `0.90/1.00`)
- `DISPERSER_MIN_SPEEDUP_HEAVY_NON_STRICT` (default: Linux/macOS `0.95`; Windows smoke/full `0.85/0.90`)
- `DISPERSER_MIN_HEAVY_LEGACY_SEC_STRICT` (default: `5`; when a heavy case runs faster than this, the non-strict floor is used)
- `DISPERSER_MIN_SPEEDUP_MEDIAN` (default: Linux/macOS `1.00`; Windows smoke/full `0.90/0.95`)

For merge/branch-protection criteria, require the cross-platform test and
performance workflows in your repository settings.

## Troubleshooting
- If `pkgload` is missing, install it: `install.packages("pkgload")`
- If HYSPLIT is missing, set `VERIFY_RUN_HYSPLIT = FALSE`
- Network errors: set `VERIFY_DATA_DOWNLOAD = FALSE` and run offline tests
