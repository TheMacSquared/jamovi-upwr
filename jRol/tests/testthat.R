library(testthat)

if (requireNamespace("jRol", quietly = TRUE)) {
    library(jRol)
    test_check("jRol")
}
