# Integration tests run after jmc has installed the compiled jperm package.
skip_if_not_installed("jperm")

test_that("one-sample exact analysis reports the enumerated result", {
    d <- data.frame(x = c(1, 2, 4, 8))
    res <- jperm:::permtestone(data = d, dep = "x", mu = 0,
                              exact = TRUE, showPlot = FALSE)$table$asDF
    dist <- permDistOneSample(d$x, 0, 10, 1, TRUE)
    expect_equal(res$stat, mean(d$x))
    expect_equal(res$p, permPValue(mean(d$x), dist, "twoSided"))
    expect_equal(res$nPerm, 2^nrow(d))
})

test_that("two-sample exact analysis matches all label allocations", {
    d <- data.frame(
        y = c(1, 2, 3, 7, 8),
        g = factor(c("A", "A", "A", "B", "B")))
    res <- jperm:::permtesttwo(data = d, dep = "y", group = "g",
                              exact = TRUE, showPlot = FALSE)$table$asDF
    dist <- permDistTwoSample(d$y, d$g, 10, 1, TRUE)
    observed <- mean(d$y[d$g == "A"]) - mean(d$y[d$g == "B"])
    expect_equal(res$stat, observed)
    expect_equal(res$p, permPValue(observed, dist, "twoSided"))
    expect_equal(res$nPerm, choose(nrow(d), 3))
})

test_that("paired analysis removes missing pairs and respects direction", {
    d <- data.frame(
        before = c(5, 8, NA, 10, 12),
        after = c(4, 6, 7, NA, 9))
    res <- jperm:::permtestpaired(data = d, var1 = "before", var2 = "after",
                                 hypothesis = "greater", exact = TRUE,
                                 showPlot = FALSE)$table$asDF
    keep <- complete.cases(d)
    diffs <- d$before[keep] - d$after[keep]
    dist <- permDistPaired(diffs, 10, 1, TRUE)
    expect_equal(res$stat, mean(diffs))
    expect_equal(res$p, permPValue(mean(diffs), dist, "greater"))
    expect_equal(res$nPerm, 2^length(diffs))
})
