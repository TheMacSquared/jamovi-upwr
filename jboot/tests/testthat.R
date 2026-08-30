library(testthat)

if (requireNamespace("jboot", quietly = TRUE)) {
    library(jboot)
    test_check("jboot")
}
