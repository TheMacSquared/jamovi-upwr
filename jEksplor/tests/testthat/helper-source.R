# Make the engine testable before the jamovi module is compiled.
if (!exists("descStats", mode = "function")) {
    source(testthat::test_path("..", "..", "R", "utils.R"), local = FALSE)
}
