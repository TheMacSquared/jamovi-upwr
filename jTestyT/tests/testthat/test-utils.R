test_that("t tests match stats::t.test and d intervals are sane", {
    x <- sleep$extra[sleep$group == 1]; y <- sleep$extra[sleep$group == 2]
    one <- oneSampleT(x, 0)
    ref <- t.test(x)
    expect_equal(one$stat, unname(ref$statistic)); expect_equal(one$p, ref$p.value)
    g <- factor(rep(c("a", "b"), each = 10)); yy <- c(x, y)
    two <- twoSampleT(yy, g, welch = TRUE)
    ref2 <- t.test(x, y)
    expect_equal(two$stat, unname(ref2$statistic)); expect_equal(two$df, unname(ref2$parameter))
    twoS <- twoSampleT(yy, g, welch = FALSE)
    expect_equal(twoS$df, 18)
    expect_equal(twoS$es, (mean(x) - mean(y)) / sqrt((var(x) + var(y)) / 2))
    mw <- mannWhitney(yy, g)
    expect_equal(mw$p, wilcox.test(x, y, exact = FALSE)$p.value)
    expect_equal(mw$es, 2 * unname(wilcox.test(x, y, exact = FALSE)$statistic) / 100 - 1)
})

test_that("rank effect sizes have the sign of the difference (audit 2026-11-05)", {
    hi <- c(13, 12, 11, 12.5, 11.8); lo <- c(10, 9.5, 10.2, 9.8, 10.5)
    g <- factor(rep(c("a", "b"), each = 5))
    mw <- mannWhitney(c(hi, lo), g)
    expect_gt(mw$es, 0)
    expect_equal(sign(mw$es), sign(twoSampleT(c(hi, lo), g)$es))
    expect_lt(mannWhitney(c(lo, hi), g)$es, 0)
    expect_gt(wilcoxOne(hi, 10)$es, 0)
})

test_that("the one-sample Wilcoxon estimate is a difference from the test value (audit 2026-11-05)", {
    x <- c(101, 104, 108, 110, 99, 107)
    w <- suppressWarnings(wilcox.test(x, mu = 100, conf.int = TRUE, exact = FALSE))
    expect_equal(wilcoxOne(x, 100)$est, unname(w$estimate) - 100)
    expect_lt(abs(wilcoxOne(x, 100)$est - (mean(x) - 100)), 2)
    # paired differences: the test value is 0, the estimate is the pseudomedian
    d <- c(1.2, -0.3, 2.1, 0.8, 1.5)
    expect_equal(wilcoxOne(d, 0)$est, unname(suppressWarnings(wilcox.test(d, conf.int = TRUE, exact = FALSE))$estimate))
})

test_that("one-sided hypotheses; d is a point value (intervals live in jCI)", {
    x <- c(1.2, 0.8, 2.1, 1.7, 0.3, 1.1, 2.4, 0.9)
    expect_equal(oneSampleT(x, 0, "greater")$p, t.test(x, alternative = "greater")$p.value)
    expect_equal(oneSampleT(x, 1)$es, (mean(x) - 1) / sd(x))
    expect_false(exists("dInterval", mode = "function"))
})
