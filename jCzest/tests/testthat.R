library(testthat)

if (requireNamespace("jCzest", quietly = TRUE)) {
    library(jCzest)
    test_check("jCzest")
}
