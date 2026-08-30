library(testthat)

if (requireNamespace("jperm", quietly = TRUE)) {
    library(jperm)
    test_check("jperm")
}
