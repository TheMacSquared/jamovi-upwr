library(testthat)

if (requireNamespace("jCI", quietly = TRUE)) {
    library(jCI)
    test_check("jCI")
}
