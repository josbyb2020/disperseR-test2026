# Findings Summary

## Overview

This study replicates the HyADS exposure mapping methodology using disperseR. 
Results depend on successful HYSPLIT execution; outputs are generated locally 
and not committed to the repository.

## Pipeline Status

| Phase | Script | Purpose |
|-------|--------|---------|
| 0 | 00_setup.R | Directories, system documentation |
| 1 | 01_data.R | Data acquisition |
| 2 | 02_single_run.R | Single unit HYSPLIT test |
| 3 | 03_batch_run.R | Multi-unit parallel runs |
| 4 | 04_linking.R | ZIP code linkage |
| 5 | 05_exposure.R | HyADS exposure calculation |
| 6 | 06_plots.R | Visualization |
| 7 | 07_edge_cases.R | Error handling tests |

## Expected Behavior

**Dispersion patterns:**
- Initial height: stack height (~200-300m AGL)
- 5-day height: ~800-1000m AGL (mixing)
- Transport: 500-1000+ km over 120 hours

**Exposure distribution:**
- Highest within 100-200km downwind
- Decreases with distance

## Key Parameters

| Parameter | Recommended | Effect |
|-----------|-------------|--------|
| `npart` | >= 100 | Statistical stability |
| `res.link` | 12000 | Balance detail vs. file size |
| `duration` | 120-240 hours | Regional-scale transport |

## Prerequisites

1. `splitr` for HYSPLIT binaries
2. Rosetta on Apple Silicon Macs
3. Meteorological data (~100MB per month)
4. PBL height data

Scripts fail gracefully when prerequisites are not met.
