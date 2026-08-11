# Standard testthat runner (used when the compiled jRISK package is installed).
# Without the compiled package the computational core can be tested directly:
#   Rscript -e 'source("R/utils.R"); testthat::test_dir("tests/testthat")'
library(testthat)
if (requireNamespace("jRISK", quietly = TRUE)) {
  library(jRISK)
  test_check("jRISK")
}
