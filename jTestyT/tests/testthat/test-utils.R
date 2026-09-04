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
    expect_equal(mw$es, 1 - 2 * unname(wilcox.test(x, y, exact = FALSE)$statistic) / 100)
})

test_that("one-sided hypotheses; d is a point value (intervals live in jCI)", {
    x <- c(1.2, 0.8, 2.1, 1.7, 0.3, 1.1, 2.4, 0.9)
    expect_equal(oneSampleT(x, 0, "greater")$p, t.test(x, alternative = "greater")$p.value)
    expect_equal(oneSampleT(x, 1)$es, (mean(x) - 1) / sd(x))
    expect_false(exists("dInterval", mode = "function"))
})
