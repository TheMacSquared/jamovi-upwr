# Run against freshly compiled modules, including their integration tests.
args <- commandArgs(trailingOnly = TRUE)
root <- args[1]; work <- args[2]
# Remaining arguments are module directories; two differ from the package name.
dirs <- args[-(1:2)]
packageNames <- c(jdistrACTION = "distrACTION", plots = "scatr")
modules <- ifelse(dirs %in% names(packageNames), packageNames[dirs], dirs)
.libPaths(c(file.path(work, "library"), file.path(work, "modules", modules, "R"), .libPaths()))
library(testthat)
test_file(file.path(root, "jmvcore", "tests", "testthat", "test-vector-fallback.R"),
          env = new.env(parent = asNamespace("jmvcore")), reporter = "summary", stop_on_failure = TRUE)
for (module in modules) {
    loadNamespace(module) # Missing packages are errors, never silently skipped.
    # jANOVA's wrapper anova() would mask stats::anova in the reference
    # computations; its tests reach internals through helper-source.R instead.
    env <- new.env(parent = if (module == "jANOVA") globalenv() else asNamespace(module))
    sourceDir <- dirs[match(module, modules)]
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
