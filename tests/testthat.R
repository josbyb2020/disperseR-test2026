# Run all testthat tests for disperseR
# Guard against missing testthat (e.g., during R CMD check with Suggests unavailable)
if (requireNamespace("testthat", quietly = TRUE)) {
  library(disperseR)
  testthat::test_check("disperseR")
}
