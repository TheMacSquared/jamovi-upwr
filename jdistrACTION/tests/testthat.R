# Standard testthat runner (used when the compiled distrACTION package is installed).
# Without the compiled package the pure-formula tests can be run directly:
#   Rscript -e 'testthat::test_dir("tests/testthat")'
library(testthat)
if (requireNamespace("distrACTION", quietly = TRUE)) {
  library(distrACTION)
  test_check("distrACTION")
}
