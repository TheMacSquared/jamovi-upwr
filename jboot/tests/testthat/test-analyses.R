# Integration tests run after jmc has installed the compiled jboot package.
skip_if_not_installed("jboot")

test_that("bootCI reports the observed statistic and is reproducible", {
    d <- data.frame(x = c(1, 2, 3, 4, 8, 9, NA))
    args <- list(data = d, dep = "x", nBoot = 500, seed = 91,
                 ciMethod = "perc")
    a <- do.call(jboot:::bootCI, args)$ciTable$asDF
    b <- do.call(jboot:::bootCI, args)$ciTable$asDF

    expect_equal(a$mean, mean(d$x, na.rm = TRUE))
    expect_equal(a$n, sum(!is.na(d$x)))
    expect_equal(a, b)
    expect_true(a$ciLower < a$mean && a$mean < a$ciUpper)
})

test_that("bootPaired removes incomplete pairs before resampling", {
    d <- data.frame(
        before = c(10, 12, NA, 15, 9, 13),
        after = c(8, 11, 7, NA, 8, 10))
    res <- jboot:::bootPaired(data = d, pair1 = "before", pair2 = "after",
                             nBoot = 500, seed = 22,
                             ciMethod = "perc")$testTable$asDF
    keep <- complete.cases(d)
    expect_equal(res$n, sum(keep))
    expect_equal(res$meanDiff, mean(d$before[keep] - d$after[keep]))
    expect_true(res$ciLower <= res$meanDiff && res$meanDiff <= res$ciUpper)
})

test_that("bootProp returns counts and proportions for every level", {
    d <- data.frame(answer = factor(c("yes", "no", "yes", "yes", NA, "no")))
    res <- jboot:::bootProp(data = d, var = "answer", nBoot = 500,
                           seed = 8, ciMethod = "perc")$propTable$asDF
    res <- res[order(res$level), ]
    expect_equal(res$level, c("no", "yes"))
    expect_equal(res$count, c(2, 3))
    expect_equal(res$prop, c(2 / 5, 3 / 5))
    expect_true(all(res$ciLower >= 0 & res$ciUpper <= 1))
})
