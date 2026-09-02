library(testthat)

if (requireNamespace("jTestyT", quietly = TRUE)) {
    library(jTestyT)
    test_check("jTestyT")
}
