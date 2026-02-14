# CRAN Submission Comments for disperseR 0.2.2

## Test Environments

- macOS Sequoia 15.3 (aarch64-apple-darwin20), R 4.5.2
- Ubuntu 24.04 (GitHub Actions), R release and R devel
- Windows Server 2022 (GitHub Actions), R release
- macOS 14 (GitHub Actions), R release

## R CMD check Results

0 ERRORs, 0 WARNINGs, 0 NOTEs.

On CRAN submission, the following additional NOTE is expected:

1. **CRAN incoming feasibility NOTE**
   - "New submission" -- this is the first CRAN submission of the modernized fork.

## Test Suite

167 tests pass, 0 failures, 0 warnings, 2 skips (platform-specific OS detection tests).

## Package Size

Installed size is 10.8 MB, of which 10.1 MB is bundled data
(`PP.units.monthly1995_2017`, `crosswalk`, `units`, `zipcodecoordinate`).
These datasets are compressed with `xz` (`LazyDataCompression: xz` in DESCRIPTION)
and are required for the core workflow demonstrated in vignettes.

## Optional splitr dependency

disperseR can optionally use the 'splitr' package (GitHub-only:
<https://github.com/rich-iannone/splitr>) for HYSPLIT binary management. splitr
is NOT declared in Suggests (since it is not available from CRAN). All code paths
that use splitr are guarded by runtime availability checks with informative error
messages guiding users to install it. Users can alternatively supply their own
HYSPLIT binary paths via `binary_path`/`parhplot_path` arguments.

## Downstream Dependencies

None currently on CRAN.
