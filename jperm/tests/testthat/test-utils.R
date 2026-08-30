test_that("permutation p-values use the plus-one correction", {
    dist <- c(-3, -1, 0, 1, 2, 4)
    expect_equal(permPValue(2, dist, "twoSided"), (3 + 1) / (6 + 1))
    expect_equal(permPValue(2, dist, "greater"), (2 + 1) / (6 + 1))
    expect_equal(permPValue(-1, dist, "less"), (2 + 1) / (6 + 1))
    expect_gt(permPValue(100, dist, "greater"), 0)
})

test_that("exact one-sample distribution enumerates every sign pattern", {
    x <- c(1, 2, 4)
    dist <- permDistOneSample(x, mu0 = 0, nPerm = 10, seed = 1, exact = TRUE)
    expected <- apply(expand.grid(rep(list(c(-1, 1)), length(x))), 1,
                      function(signs) mean(signs * x))
    expect_true(isTRUE(attr(dist, "exact")))
    expect_length(dist, 2^length(x))
    expect_equal(sort(unname(dist)), sort(unname(expected)))
    expect_equal(mean(dist), 0, tolerance = 1e-15)
})

test_that("exact two-sample distribution enumerates all allocations", {
    x <- c(1, 2, 5, 8)
    group <- factor(c("A", "A", "B", "B"))
    dist <- permDistTwoSample(x, group, nPerm = 10, seed = 1, exact = TRUE)
    combos <- utils::combn(seq_along(x), 2)
    expected <- apply(combos, 2, function(i) mean(x[i]) - mean(x[-i]))
    expect_true(isTRUE(attr(dist, "exact")))
    expect_length(dist, choose(4, 2))
    expect_equal(sort(unname(dist)), sort(unname(expected)))
})

test_that("Monte Carlo permutation distributions are reproducible", {
    oneA <- permDistOneSample(1:6, 0, 250, seed = 42, exact = FALSE)
    oneB <- permDistOneSample(1:6, 0, 250, seed = 42, exact = FALSE)
    expect_identical(oneA, oneB)
    expect_false(isTRUE(attr(oneA, "exact")))

    g <- factor(rep(c("A", "B"), each = 4))
    twoA <- permDistTwoSample(1:8, g, 250, seed = 43, exact = FALSE)
    twoB <- permDistTwoSample(1:8, g, 250, seed = 43, exact = FALSE)
    expect_identical(twoA, twoB)
})

test_that("paired permutation delegates to sign flips of differences", {
    d <- c(-2, 1, 3, 4)
    paired <- permDistPaired(d, 100, seed = 9, exact = TRUE)
    one <- permDistOneSample(d, 0, 100, seed = 9, exact = TRUE)
    expect_identical(paired, one)
})
