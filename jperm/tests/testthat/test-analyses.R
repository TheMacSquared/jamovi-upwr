# Integration tests run after jmc has installed the compiled jperm package.
skip_if_not_installed("jperm")

test_that("one-sample exact analysis reports the enumerated result", {
    d <- data.frame(x = c(1, 2, 4, 8))
    res <- jperm:::permtestone(data = d, vars = "x", testValue = 0, exact = TRUE, plot = FALSE)$table$asDF
    dist <- permDistOneSample(d$x, 0, 10, 1, TRUE)
    expect_equal(res$stat, mean(d$x))
    expect_equal(res$p, permPValue(mean(d$x), dist, "different"))
    expect_equal(res$nPerm, 2^nrow(d))
})

test_that("two-sample exact analysis matches all label allocations; several variables at once", {
    d <- data.frame(
        y = c(1, 2, 3, 7, 8), z = c(2, 2, 3, 9, 9),
        g = factor(c("A", "A", "A", "B", "B")))
    res <- jperm:::permtesttwo(data = d, vars = c("y", "z"), group = "g", exact = TRUE, plot = FALSE)$table$asDF
    dist <- permDistTwoSample(d$y, d$g, 10, 1, TRUE)
    observed <- mean(d$y[d$g == "A"]) - mean(d$y[d$g == "B"])
    expect_equal(nrow(res), 2)
    expect_equal(res$stat[1], observed)
    expect_equal(res$p[1], permPValue(observed, dist, "different"))
    expect_equal(res$nPerm[1], choose(nrow(d), 3))
})

test_that("paired analysis removes missing pairs and respects direction", {
    d <- data.frame(
        before = c(5, 8, NA, 10, 12),
        after = c(4, 6, 7, NA, 9))
    res <- jperm:::permtestpaired(data = d, pairs = list(list(i1 = "before", i2 = "after")),
                                 hypothesis = "greater", exact = TRUE, plot = FALSE)$table$asDF
    keep <- complete.cases(d)
    diffs <- d$before[keep] - d$after[keep]
    dist <- permDistPaired(diffs, 10, 1, TRUE)
    expect_equal(res$stat, mean(diffs))
    expect_equal(res$p, permPValue(mean(diffs), dist, "greater"))
    expect_equal(res$nPerm, 2^length(diffs))
})

test_that("Monte Carlo with seed is reproducible; opis metod describes the scheme", {
    r1 <- jperm:::permtesttwo(data = sleep, vars = "extra", group = "group", seed = 7, nPerm = 500, plot = FALSE)$table$asDF
    r2 <- jperm:::permtesttwo(data = sleep, vars = "extra", group = "group", seed = 7, nPerm = 500, plot = FALSE)$table$asDF
    expect_equal(r1$p, r2$p)
    res <- jperm:::permtesttwo(data = sleep, vars = "extra", group = "group", seed = 7, nPerm = 500, metody = TRUE)
    h <- res$metody$content
    expect_true(res$metody$visible)
    expect_true(grepl("przetasowanie etykiet", h) && grepl("B = 500", h) && grepl("ziarno generatora 7", h))
    expect_true(grepl("„1” − „2”", h))
    expect_false(grepl(sprintf("%.3f", res$table$asDF$stat[1]), h))
    expect_false(jperm:::permtestone(data = sleep, vars = "extra")$metody$visible)
    one <- jperm:::permtestone(data = sleep, vars = "extra", testValue = 1, exact = TRUE, metody = TRUE)$metody$content
    expect_true(grepl("μ₀ = 1", one) && grepl("sign-flip", one) && grepl("Test dokładny", one))
})
