test_that("permutation p-values use the plus-one correction", {
    dist <- c(-3, -1, 0, 1, 2, 4)
    expect_equal(permPValue(2, dist, "different"), (3 + 1) / (6 + 1))
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

test_that("full enumeration includes the observed arrangement exactly once", {
    d <- permDistOneSample(1:6, 0, 100, 1, TRUE)
    expect_equal(permPValue(3.5, d, "different"), 2 / 64)
    expect_equal(permPValue(3.5, d, "greater"), 1 / 64)
    expect_equal(permPValue(3.5, d, "less"), 1)
    g <- factor(c("a", "a", "b", "b"))
    d <- permDistTwoSample(c(1, 2, 5, 8), g, 100, 1, TRUE)
    expect_equal(permPValue(-5, d, "less"), 1 / 6)
    expect_equal(permPValue(-5, d, "different"), 2 / 6)
})

test_that("exact tests count the observed arrangement despite rounding (audit 2026-11-05)", {
    x <- c(3.6, 5.1, 5.8, 3.9, 3.3, 5.1)
    d <- permDistOneSample(x, 0.1, 1000, 0, TRUE)
    expect_equal(length(d), 64)
    expect_equal(permPValue(mean(x) - 0.1, d, "greater"), 1 / 64)
    # the same distribution as enumerating every sign pattern
    signs <- as.matrix(expand.grid(rep(list(c(1, -1)), length(x))))
    ref <- as.vector(signs %*% (x - 0.1)) / length(x)
    expect_equal(sort(d), sort(ref))
    set.seed(7)
    for (k in 1:200) {
        x <- round(runif(8, 1, 6), 1)
        d <- permDistOneSample(x, 2.7, 1000, 0, TRUE)
        ref <- as.vector(as.matrix(expand.grid(rep(list(c(1, -1)), 8))) %*% (x - 2.7)) / 8
        obs <- mean(x) - 2.7
        for (h in c("greater", "less", "different")) {
            exact <- switch(h, greater = mean(round(ref, 10) >= round(obs, 10)),
                            less = mean(round(ref, 10) <= round(obs, 10)),
                            different = mean(round(abs(ref), 10) >= round(abs(obs), 10)))
            expect_equal(permPValue(obs, d, h), exact)
        }
    }
})

test_that("two-sample exact two-sided p counts mirrored ties (audit 2026-11-05)", {
    set.seed(11)
    for (k in 1:100) {
        x <- round(runif(10, 0, 3), 1); g <- factor(rep(c("a", "b"), c(4, 6)))
        d <- permDistTwoSample(x, g, 1000, 0, TRUE)
        obs <- mean(x[g == "a"]) - mean(x[g == "b"])
        expect_equal(permPValue(obs, d, "different"), mean(round(abs(d), 10) >= round(abs(obs), 10)))
    }
})

test_that("exact one-sample enumeration is fast at n = 20", {
    t0 <- proc.time()[["elapsed"]]
    d <- permDistOneSample(rnorm(20), 0, 1000, 0, TRUE)
    expect_equal(length(d), 2^20)
    expect_lt(proc.time()[["elapsed"]] - t0, 2)
})
