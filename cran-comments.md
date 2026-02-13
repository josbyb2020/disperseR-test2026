# CRAN Submission Comments for disperseR 0.2.1

## Test Environments

- macOS Tahoe 26.3 (aarch64-apple-darwin20), R 4.5.2
- Linux via Docker (rocker/r-ver:4.3.2) -- pending confirmation
- Windows -- pending confirmation via CI or real host

## R CMD check Results

0 ERRORs, 0 WARNINGs, 0 NOTEs.

On CRAN submission, the following additional NOTEs are expected:

1. **CRAN incoming feasibility NOTE**
   - "New submission" -- this is the first CRAN submission of the modernized fork.
   - "Suggests or Enhances not in mainstream repositories: splitr" -- splitr is a
     GitHub-only package that bundles HYSPLIT binaries. disperseR works without it
     when users supply their own `binary_path`/`parhplot_path`. All code paths that
     require splitr are guarded by `requireNamespace()` checks with informative
     error messages.

## Test Suite

146 tests pass, 0 failures, 16 skips (tests requiring HYSPLIT binaries or network).

## Package Size

Installed size is 10.8 MB, of which 10.1 MB is bundled data
(`PP.units.monthly1995_2017`, `crosswalk`, `units`, `zipcodecoordinate`).
These datasets are compressed with `xz` (`LazyDataCompression: xz` in DESCRIPTION)
and are required for the core workflow demonstrated in vignettes.

## Downstream Dependencies

None currently on CRAN.
