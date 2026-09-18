# Run against a jmc-built package to test the options/results contract.
skip_if_not_installed("jPomiar")

test_that("series results and missing-value handling reach the results tables", {
    r <- jPomiar:::powtorzenia(data = data.frame(v = c(9, NA, 10, 11)), dep = "v",
                               useReference = TRUE, reference = 9.5, showPlot = TRUE, metody = TRUE)
    expect_equal(r$summary$asDF$mean, 10)
    expect_equal(r$summary$asDF$missing, 1L)
    expect_equal(r$summary$asDF$se, 1 / sqrt(3))
    expect_equal(r$comparison$asDF$difference, 0.5)
    expect_equal(r$plot$state$index, c(1L, 3L, 4L))
    expect_match(r$metody$content, "SD/√n")
    expect_match(r$metody$content, "„v”")
    expect_match(r$comparison$notes$reference$note, "opisowa")
})

test_that("disabled budget options do not contribute", {
    empty <- jPomiar:::budzet()
    expect_equal(nrow(empty$summary$asDF), 0)
    expect_match(empty$summary$notes$empty$note, "Włącz")
    r <- jPomiar:::budzet(value = 10, includeA = TRUE, uA = 0.2,
                         correction = 100, calibrationU = 100, resolution = 100,
                         showPlot = TRUE)
    expect_equal(r$summary$asDF$estimate, 10)
    expect_equal(r$summary$asDF$uc, 0.2)
    expect_equal(nrow(r$components$asDF), 1)
    expect_equal(nrow(r$plot$state), 1)
    corrected <- jPomiar:::budzet(value = 10, useCorrection = TRUE, correction = -0.5,
        includeCalibration = TRUE, calibrationU = 0.4, calibrationK = 2,
        includeResolution = TRUE, resolution = 0.1, metody = TRUE)
    expect_equal(corrected$summary$asDF$estimate, 9.5)
    expect_equal(corrected$summary$asDF$uc, sqrt(0.04 + 0.01/12))
    expect_match(corrected$metody$content, "nie poziom ufności")
    expect_match(corrected$metody$content, "U/k")
    expect_false(grepl("Typ A", corrected$metody$content))
})

test_that("correlation toggle and negative covariance reach results and plot", {
    off <- jPomiar:::propagacja(model = "difference", x = 10, y = 8, ux = 2, uy = 2, rho = 0.75)
    on <- jPomiar:::propagacja(model = "difference", x = 10, y = 8, ux = 2, uy = 2,
                              useCorrelation = TRUE, rho = 0.75, showPlot = TRUE)
    expect_equal(off$summary$asDF$uc, sqrt(8))
    expect_equal(on$summary$asDF$uc, sqrt(2))
    expect_equal(on$components$asDF$variance, c(4, 4, -6))
    expect_equal(on$plot$state$variance, c(4, 4, -6))
    expect_match(on$metody$content, "ρ = 0.75")
    expect_match(off$metody$content, "korelacja wyłączona")
    ratio <- jPomiar:::propagacja(model = "ratio", x = 1, y = 0.1, ux = 0.1, uy = 0.02)
    expect_setequal(names(ratio$summary$notes), c("linear", "denominator"))
    expect_length(on$summary$notes, 0)
    expect_error(jPomiar:::propagacja(model = "ratio", y = 0), "mianownika")
})

test_that("both plot renderers work with computed analysis state", {
    out <- tempfile(fileext = ".pdf")
    skip_if_not(capabilities("cairo"))
    grDevices::cairo_pdf(out)
    on.exit({grDevices::dev.off(); unlink(out)}, add = TRUE)
    r <- jPomiar:::propagacja(model = "difference", ux = 2, uy = 2,
                              useCorrelation = TRUE, rho = 0.75, showPlot = TRUE)
    expect_true(measurementVariancePlot(r$plot$state, ggplot2::theme_minimal(), list()))
    options <- jPomiar:::powtorzeniaOptions$new(dep = "v", showPlot = TRUE,
                                                useReference = TRUE, reference = 2)
    analysis <- jPomiar:::powtorzeniaClass$new(options = options,
                                               data = data.frame(v = c(1, 2, 4)))
    analysis$run()
    render <- analysis$.__enclos_env__$private$.plot
    expect_true(render(analysis$results$plot, ggplot2::theme_minimal(), list()))
})
