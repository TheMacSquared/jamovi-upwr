library(testthat)

if (requireNamespace("jANOVA", quietly = TRUE)) {
    library(jANOVA)
    test_check("jANOVA")
}
