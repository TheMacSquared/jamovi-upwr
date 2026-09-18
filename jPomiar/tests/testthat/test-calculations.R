test_that("repeatability differs from uncertainty of the mean", {
    r <- measurementSeries(c(9, 10, NA, 11))
    expect_equal(r[c("n", "missing", "mean", "sd")], list(n = 3L, missing = 1L, mean = 10, sd = 1))
    expect_equal(r$se, 1 / sqrt(3))
    expect_equal(r$index, c(1L, 2L, 4L))
    expect_equal(measurementSeries(rep(10, 4))$se, 0)
    expect_error(measurementSeries(c(NA_real_, NA_real_)), "co najmniej")
    expect_error(measurementSeries(c(1, NA)), "co najmniej")
    expect_error(measurementSeries(c(1, Inf)), "nieskończone")
    expect_error(measurementSeries(c("a", "b")), "liczbową")
})

test_that("budget converts certificate and rectangular rounding uncertainties", {
    r <- measurementBudget(100, correction = -0.1, uA = 0.02,
        calibrationU = 0.08, calibrationK = 2, resolution = 0.01, k = 2)
    expected <- sqrt(0.02^2 + 0.04^2 + 0.01^2 / 12)
    expect_equal(r$summary$estimate, 99.9)
    expect_equal(r$summary$uc, expected)
    expect_equal(r$summary$expanded, 2 * expected)
    expect_equal(r$summary$lower, 99.9 - 2 * expected)
    expect_equal(r$components$u, c(0.02, 0.04, 0.01 / sqrt(12)))
    expect_equal(sum(r$components$share), 100)
    expect_equal(measurementBudget(1, uA = 1)$summary$uc, 1)
    zero <- measurementBudget(1, uA = 0)
    expect_true(all(is.na(zero$components$share)))
    expect_equal(zero$summary$uc, 0)
    expect_error(measurementBudget(1), "co najmniej")
    expect_error(measurementBudget(1, uA = -1), "typu A")
    expect_error(measurementBudget(1, calibrationU = 1, calibrationK = 0), "wzorcowania")
    expect_error(measurementBudget(1, uA = 1, k = 0), "Mnożnik")
})

test_that("correlation changes sum and difference uncertainty with the right sign", {
    independent <- measurementPropagation("difference", 10, 8, 2, 2)
    correlated <- measurementPropagation("difference", 10, 8, 2, 2, rho = 0.75)
    expect_equal(independent$summary$uc, sqrt(8))
    expect_equal(correlated$summary$estimate, 2)
    expect_equal(correlated$summary$uc, sqrt(2))
    expect_equal(correlated$components$variance, c(4, 4, -6))
    expect_equal(measurementPropagation("sum", 10, 8, 2, 2, rho = 0.75)$summary$uc, sqrt(14))
    expect_equal(measurementPropagation("difference", 10, 8, 2, 2, rho = 1)$summary$uc, 0)
    expect_equal(measurementPropagation("sum", 10, 8, 2, 2, rho = -1)$summary$uc, 0)
    expect_error(measurementPropagation("sum", 1, 1, 1, 1, rho = 1.01), "Korelacja")
})

test_that("products and ratios use derivatives in the original units", {
    product <- measurementPropagation("product", 4, 3, 0.2, 0.1)
    expect_equal(product$summary$estimate, 12)
    expect_equal(product$inputs$c, c(3, 4))
    expect_equal(product$summary$uc, sqrt(0.52))
    ratio <- measurementPropagation("ratio", 100, 20, 0.2, 0.1)
    expect_equal(ratio$summary$estimate, 5)
    expect_equal(ratio$inputs$c, c(0.05, -0.25))
    expect_equal(ratio$summary$uc, sqrt(0.000725))
    # Numerical derivatives independently check every model, including negative inputs.
    for (model in c("sum", "difference", "product", "ratio")) {
        f <- switch(model, sum = function(x, y) x + y, difference = function(x, y) x - y,
                    product = function(x, y) x * y, ratio = function(x, y) x / y)
        h <- 1e-5
        cRef <- c((f(-3 + h, 2) - f(-3 - h, 2)) / (2 * h),
                  (f(-3, 2 + h) - f(-3, 2 - h)) / (2 * h))
        r <- measurementPropagation(model, -3, 2, 0.2, 0.1, rho = -0.6)
        sigma <- matrix(c(0.04, -0.012, -0.012, 0.01), 2)
        expect_equal(r$inputs$c, cRef, tolerance = 1e-8)
        expect_equal(r$summary$uc^2, as.numeric(t(cRef) %*% sigma %*% cRef), tolerance = 1e-8)
        expect_equal(r$summary$uc^2, sum(r$components$variance), tolerance = 1e-12)
    }
})

test_that("nonlinear degeneracy and invalid values cannot look like reliable results", {
    expect_error(measurementPropagation("ratio", 1, 0, 0.1, 0.1), "mianownika")
    expect_error(measurementPropagation("sum", Inf, 1, 1, 1), "skończoną")
    expect_error(measurementPropagation("sum", 1, 1, -1, 1), "u\\(x\\)")
    expect_error(measurementPropagation("sum", 1, 1, 1e300, 1), "zakres")
    expect_error(measurementPropagation("other", 1, 1, 1, 1), "Nieznany")
    expect_match(paste(measurementPropagation("ratio", 1, 0.1, 0.1, 0.02)$notes, collapse = " "), "mianownika")
    zero <- measurementPropagation("product", 0, 0, 1, 1)
    expect_equal(zero$summary$uc, 0)
    expect_match(paste(zero$notes, collapse = " "), "wyższego rzędu")
})
