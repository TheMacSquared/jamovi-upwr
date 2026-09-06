# Run against freshly compiled modules, including their integration tests.
args <- commandArgs(trailingOnly = TRUE)
root <- args[1]; work <- args[2]
modules <- c("jperm", "jCI", "jRegr")
.libPaths(c(file.path(work, "library"), file.path(work, "modules", modules, "R"), .libPaths()))
library(testthat)
for (module in modules) {
    loadNamespace(module) # Missing packages are errors, never silently skipped.
    env <- new.env(parent = asNamespace(module))
    test_dir(file.path(root, module, "tests", "testthat"), env = env,
             reporter = "summary", stop_on_failure = TRUE)
}
