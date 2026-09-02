# Make the computational helpers testable before the jamovi module is compiled.
if (!exists("fitDesign", mode = "function")) {
    source(testthat::test_path("..", "..", "R", "utils.R"), local = FALSE)
}
