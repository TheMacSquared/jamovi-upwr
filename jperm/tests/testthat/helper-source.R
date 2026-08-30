# Make the permutation engine testable before the jamovi module is compiled.
if (!exists("permPValue", mode = "function")) {
    source(testthat::test_path("..", "..", "R", "utils.R"), local = FALSE)
}
