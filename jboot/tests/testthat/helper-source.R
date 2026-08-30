# Make shared helpers testable before the jamovi module is compiled.
if (!exists("extractBootCI", mode = "function")) {
    source(testthat::test_path("..", "..", "R", "utils.R"), local = FALSE)
}
