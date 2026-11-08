# Run against freshly compiled modules, including their integration tests.
args <- commandArgs(trailingOnly = TRUE)
root <- args[1]; work <- args[2]
modules <- c("jperm", "jCI", "jRegr", "distrACTION", "jEksplor", "jTestyT", "jCzest", "scatr")
.libPaths(c(file.path(work, "library"), file.path(work, "modules", modules, "R"), .libPaths()))
library(testthat)
test_file(file.path(root, "jmvcore", "tests", "testthat", "test-vector-fallback.R"),
          env = new.env(parent = asNamespace("jmvcore")), reporter = "summary", stop_on_failure = TRUE)
# Nemenyi's numerical helper has no dependency on the native ANOVA runtime.
nemenyiEnv <- new.env()
source(file.path(root, "jANOVA", "R", "utils.R"), local = nemenyiEnv)
test_file(file.path(root, "jANOVA", "tests", "testthat", "test-nemenyi.R"),
          env = nemenyiEnv, reporter = "summary", stop_on_failure = TRUE)
for (module in modules) {
    loadNamespace(module) # Missing packages are errors, never silently skipped.
    env <- new.env(parent = asNamespace(module))
    sourceDir <- switch(module, distrACTION = "jdistrACTION", scatr = "plots", module)
    # The older syntax-equivalence snapshots have separate, known palette
    # differences. Gate the new vector output and the numerical audit here.
    testPath <- file.path(root, sourceDir, "tests", "testthat")
    if (module == "scatr") {
        results <- lapply(c("testaudit.R", "test-vector.R"), function(file)
            test_file(file.path(testPath, file), env = env,
                      reporter = "summary", stop_on_failure = TRUE))
        counts <- do.call(rbind, lapply(results, as.data.frame))
    } else {
        results <- test_dir(testPath, env = env,
                            reporter = "summary", stop_on_failure = TRUE)
        counts <- as.data.frame(results)
    }
    cat(sprintf("%s: %d tests, %d passed expectations, %d skipped, %d warnings\n",
                module, nrow(counts), sum(counts$passed), sum(counts$skipped), sum(counts$warning)))
}
