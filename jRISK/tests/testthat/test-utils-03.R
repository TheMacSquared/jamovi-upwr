# Tests of the 0.4 engine additions in R/utils.R: k-out-of-n groups,
# common cause, shared FTA events, one-sided upper bound, case weights.
if (!exists("riskSystemReliability"))
  source(file.path(testthat::test_path(), "..", "..", "R", "utils.R"))

test_that("k-out-of-n reliability matches enumeration for unequal r", {
  r <- c(0.9, 0.8, 0.7, 0.95)
  for (k in 1:4)
    expect_equal(riskKofNReliability(r, k),
                 riskSystemReliability(riskPhiKofN(4, k), r), tolerance = 1e-12)
  # 2-of-3 at r = 0.9 (exercise 9.5)
  expect_equal(riskKofNReliability(rep(0.9, 3), 2), 0.972, tolerance = 1e-12)
})

test_that("two-level structure with k-out-of-n groups", {
  r <- c(0.9, 0.9, 0.9, 0.95)
  sizes <- c(3, 1)
  closed <- riskTwoLevelReliability(r, sizes, "koutofn", "series", k = 1)
  # 1-of-3 = parallel; in a single-element group k = 1 means "the element works"
  expect_equal(closed, (1 - 0.1^3) * 0.95, tolerance = 1e-12)
  expect_equal(riskTwoLevelReliability(c(0.9, 0.9, 0.9), 3, "koutofn", "series", k = 2),
               0.972, tolerance = 1e-12)
  phi <- riskPhiTwoLevel(c(3, 2), "koutofn", "series", k = 2)
  rr <- c(0.9, 0.85, 0.8, 0.95, 0.9)
  expect_equal(riskSystemReliability(phi, rr),
               riskTwoLevelReliability(rr, c(3, 2), "koutofn", "series", k = 2),
               tolerance = 1e-12)
})

test_that("common cause element multiplies the system by 1 - q", {
  # exercise 9.4 / 9.c: two parallel units of 0.9 with shared supply q = 0.01
  phi <- riskPhiWithCommonCause(riskPhiParallel(2), 2)
  expect_equal(riskSystemReliability(phi, c(0.9, 0.9, 0.99)), 0.9801, tolerance = 1e-12)
  # with three units: 0.98901
  phi3 <- riskPhiWithCommonCause(riskPhiParallel(3), 3)
  expect_equal(riskSystemReliability(phi3, c(0.9, 0.9, 0.9, 0.99)), 0.98901, tolerance = 1e-12)
  # the extended structure stays coherent; the common cause is a cut of order 1
  expect_true(riskCoherence(phi, 3)$coherent)
  expect_true(any(vapply(riskMinimalCuts(phi, 3), identical, TRUE, 3L)))
})

test_that("coherence flags irrelevant components", {
  expect_true(riskCoherence(riskPhiTwoLevel(c(1, 2), "parallel", "series"), 3)$coherent)
  irr <- riskCoherence(function(x) as.integer(x[1] == 1), 2)
  expect_false(irr$coherent)
  expect_equal(irr$relevant, c(TRUE, FALSE))
})

test_that("shared FTA events are computed exactly", {
  # exercise 10.2: one leaf C (q = 0.05) in two branches -> 0.05 for AND and OR
  for (top in c("and", "or"))
    expect_equal(riskFtaTopProbShared(0.05, c(1, 1), c(1, 2), "and", top),
                 0.05, tolerance = 1e-12)
  # exercise 10.a: I AND (C OR (B1 AND B2)) through cuts {I, C}, {I, B1, B2}
  pEv <- c(I = 0.005, C = 0.01, B1 = 0.05, B2 = 0.08)
  ev <- c(1, 2, 1, 3, 4)
  br <- c(1, 1, 2, 2, 2)
  expect_equal(riskFtaTopProbShared(pEv, ev, br, "and", "or"),
               0.005 * (1 - 0.99 * (1 - 0.05 * 0.08)), tolerance = 1e-12)
  expect_equal(round(riskFtaTopProbShared(pEv, ev, br, "and", "or"), 7), 0.0000698)
  # without repetitions the exact routine equals the closed form
  p <- c(0.01, 0.05, 0.05)
  expect_equal(riskFtaTopProbShared(p, 1:3, c(1, 2, 2), "and", "or"),
               riskFtaTopProb(p, c(1, 2, 2), "and", "or"), tolerance = 1e-12)
  imp <- riskFtaImportanceShared(pEv, ev, br, "and", "or")
  expect_equal(imp[1], riskFtaTopProbShared(pEv, ev, br, "and", "or"), tolerance = 1e-12)
})

test_that("one-sided upper bound reduces to 1 - alpha^(1/n) at zero", {
  # exercise 4.5
  expect_equal(riskUpperBound(0, 100), 1 - 0.05^(1 / 100), tolerance = 1e-12)
  expect_equal(round(riskUpperBound(0, 100), 4), 0.0295)
  expect_equal(riskUpperBound(3, 50), stats::binom.test(3, 50, alternative = "less")$conf.int[2],
               tolerance = 1e-10)
  expect_equal(riskUpperBound(5, 5), 1)
})

test_that("case weights: count variable, then jamovi weights, then rows", {
  d <- data.frame(a = 1:3, n = c(2, 0, 5))
  expect_equal(riskCaseWeights(d, "n")$w, c(2, 0, 5))
  attr(d, "jmv-weights") <- c(1, 1, 3)
  expect_equal(riskCaseWeights(d, NULL)$w, c(1, 1, 3))
  expect_equal(riskCaseWeights(d, NULL)$source, "weights")
  expect_equal(riskCaseWeights(data.frame(a = 1:2), NULL)$w, c(1, 1))
})

test_that("diagram gets the common cause box at the system exit", {
  lay <- riskDiagramLayoutTwoLevel(c(1, 2), "parallel", "series", r = c(0.95, 0.9, 0.9))
  lay2 <- riskDiagramAppendSeries(lay, "CCF")
  expect_equal(nrow(lay2$boxes), nrow(lay$boxes) + 1)
  expect_gt(lay2$boxes$x[nrow(lay2$boxes)], max(lay$boxes$x))
})
