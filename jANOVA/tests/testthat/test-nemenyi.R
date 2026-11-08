# Studentized-range convention: PMCMRplus frdAllPairsNemenyiTest.
# https://github.com/cran/PMCMRplus/blob/master/R/frdAllPairsNemenyiTest.R
test_that("Nemenyi reports the nonnegative studentized-range q, preserving p", {
    # Five blocks with ranks (1, 2, 3): mean-rank difference A-C = -2.
    m <- matrix(rep(c(1, 2, 3), each = 5), ncol = 3,
                dimnames = list(NULL, c("A", "B", "C")))
    result <- friedmanPairs(m)$pairs
    ac <- result[result$g1 == "A" & result$g2 == "C", ]
    expectedQ <- 2 / sqrt(3 * 4 / (12 * 5))
    expect_equal(ac$stat, expectedQ)
    expect_equal(ac$se, sqrt(3 * 4 / (6 * 5)))
    expect_equal(ac$diff, -2)
    expect_equal(ac$p, stats::ptukey(expectedQ, 3, Inf, lower.tail = FALSE))
    expect_true(all(result$stat >= 0))
    expect_equal(result$p, stats::ptukey(abs(result$diff / result$se) * sqrt(2), 3, Inf, lower.tail = FALSE))
    tied <- matrix(1, 5, 3, dimnames = list(NULL, c("A", "B", "C")))
    expect_equal(friedmanPairs(tied)$pairs$stat, rep(0, 3))
    expect_equal(friedmanPairs(tied)$pairs$p, rep(1, 3))
})
