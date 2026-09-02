library(testthat)

if (requireNamespace("jDosw", quietly = TRUE)) {
    library(jDosw)
    test_check("jDosw")
}
