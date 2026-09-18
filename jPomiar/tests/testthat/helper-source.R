# Also support test_check() in an installed package, where R sources are absent.
helpers <- testthat::test_path("..", "..", "R", "utils.R")
if (file.exists(helpers)) {
    source(helpers, local = TRUE)
} else {
    for (name in c("measurementSeries", "measurementBudget", "measurementPropagation",
                   "measurementVariancePlot"))
        assign(name, getFromNamespace(name, "jPomiar"))
}
ellipseHelpers <- testthat::test_path("..", "..", "R", "ellipse.R")
if (file.exists(ellipseHelpers)) {
    source(ellipseHelpers, local = TRUE)
} else {
    for (name in c("measurementEllipse", "measurementEllipsePlot"))
        assign(name, getFromNamespace(name, "jPomiar"))
}
