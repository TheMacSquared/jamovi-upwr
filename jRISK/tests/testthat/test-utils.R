# Tests of the pure computational core in R/utils.R.
# When the compiled package is absent, source the file so the tests can run
# standalone (Rscript from the module root).
if (!exists("riskSystemReliability"))
  source(file.path(testthat::test_path(), "..", "..", "R", "utils.R"))

test_that("lifetime functions match f/F identities for all three models", {
  cases <- list(
    list(dist = "exponential", par = list(rate = 1.5)),
    list(dist = "gamma",       par = list(shape = 2, rate = 1.5)),
    list(dist = "weibull",     par = list(shape = 1.5, scale = 2)))
  t <- c(0.1, 0.5, 1, 2, 5)
  for (cs in cases) {
    R <- riskLtReliability(t, cs$dist, cs$par)
    expect_equal(R, 1 - riskLtCdf(t, cs$dist, cs$par), tolerance = 1e-12)
    h <- riskLtHazard(t, cs$dist, cs$par)
    expect_equal(h, riskLtDensity(t, cs$dist, cs$par) / R, tolerance = 1e-9)
  }
})

test_that("hazard closed forms and characters", {
  # exponential: constant hazard = rate
  expect_equal(riskLtHazard(c(1, 10), "exponential", list(rate = 2)), c(2, 2))
  expect_equal(riskLtHazardCharacter("exponential", list(rate = 2)), "constant")
  # weibull: increasing for shape > 1, decreasing for shape < 1
  hInc <- riskLtHazard(c(1, 2), "weibull", list(shape = 2, scale = 1))
  expect_gt(hInc[2], hInc[1])
  expect_equal(riskLtHazardCharacter("weibull", list(shape = 2, scale = 1)), "increasing")
  hDec <- riskLtHazard(c(1, 2), "weibull", list(shape = 0.5, scale = 1))
  expect_lt(hDec[2], hDec[1])
  expect_equal(riskLtHazardCharacter("weibull", list(shape = 0.5, scale = 1)), "decreasing")
  # weibull with shape 1 is the exponential with rate 1/scale
  expect_equal(riskLtHazard(c(1, 5), "weibull", list(shape = 1, scale = 2)),
               c(0.5, 0.5), tolerance = 1e-12)
  # gamma hazard is stable far in the right tail (log-scale computation)
  hTail <- riskLtHazard(50, "gamma", list(shape = 2, rate = 1.5))
  expect_true(is.finite(hTail) && hTail > 0)
  expect_equal(riskLtHazardCharacter("gamma", list(shape = 0.5, rate = 1)), "decreasing")
})

test_that("MTTF and median", {
  expect_equal(riskLtMTTF("exponential", list(rate = 2)), 0.5)
  expect_equal(riskLtMTTF("gamma", list(shape = 3, rate = 2)), 1.5)
  expect_equal(riskLtMTTF("weibull", list(shape = 1.5, scale = 2)),
               2 * gamma(1 + 1 / 1.5), tolerance = 1e-12)
  expect_equal(riskLtMedian("exponential", list(rate = 1)), log(2), tolerance = 1e-12)
})

test_that("series and parallel systems match closed forms", {
  r <- c(0.9, 0.8, 0.95)
  expect_equal(riskSystemReliability(riskPhiSeries(3), r), prod(r), tolerance = 1e-12)
  expect_equal(riskSystemReliability(riskPhiParallel(3), r),
               1 - prod(1 - r), tolerance = 1e-12)
})

test_that("k-out-of-n matches the binomial formula for identical components", {
  p <- 0.9; n <- 3; k <- 2
  expect_equal(riskSystemReliability(riskPhiKofN(n, k), rep(p, n)),
               sum(dbinom(k:n, n, p)), tolerance = 1e-12)
  # the control value from the plan: 2-of-3 with p = 0.9
  expect_equal(riskSystemReliability(riskPhiKofN(3, 2), rep(0.9, 3)),
               0.972, tolerance = 1e-9)
})

test_that("series-parallel and parallel-series compositions", {
  # 2 blocks in series, each 2 in parallel, all r = 0.9
  r <- rep(0.9, 4)
  expect_equal(riskSystemReliability(riskPhiSeriesParallel(2, 2), r),
               (1 - 0.1^2)^2, tolerance = 1e-12)
  # 2 branches in parallel, each 2 in series
  expect_equal(riskSystemReliability(riskPhiParallelSeries(2, 2), r),
               1 - (1 - 0.9^2)^2, tolerance = 1e-12)
})

test_that("bridge reliability matches pivotal decomposition and the control value", {
  r <- rep(0.9, 5)
  # control value from the plan
  expect_equal(riskSystemReliability(riskPhiBridge(), r), 0.97848, tolerance = 1e-9)
  # pivotal decomposition on the crossover element 3:
  # with 3 working the bridge is (1 par 4) series (2 par 5)
  b3work <- (1 - (1 - r[1]) * (1 - r[4])) * (1 - (1 - r[2]) * (1 - r[5]))
  # with 3 failed it is (1,2) series parallel with (4,5) series
  b3fail <- 1 - (1 - r[1] * r[2]) * (1 - r[4] * r[5])
  expect_equal(riskSystemReliability(riskPhiBridge(), r),
               r[3] * b3work + (1 - r[3]) * b3fail, tolerance = 1e-12)
})

test_that("minimal paths and cuts of the bridge are the textbook sets", {
  phi <- riskPhiBridge()
  paths <- riskMinimalPaths(phi, 5)
  cuts <- riskMinimalCuts(phi, 5)
  canon <- function(sets) sort(vapply(sets, function(s) paste(s, collapse = ","), ""))
  expect_equal(canon(paths), sort(c("1,2", "4,5", "1,3,5", "2,3,4")))
  expect_equal(canon(cuts), sort(c("1,4", "2,5", "1,3,5", "2,3,4")))
})

test_that("coherence: canonical systems are coherent, a rigged one is not", {
  for (phi in list(riskPhiSeries(3), riskPhiParallel(3), riskPhiKofN(3, 2)))
    expect_true(riskCoherence(phi, 3)$coherent)
  expect_true(riskCoherence(riskPhiBridge(), 5)$coherent)
  # a system ignoring component 2 is not coherent (irrelevant component)
  phiIrr <- function(x) as.integer(x[1] == 1)
  co <- riskCoherence(phiIrr, 2)
  expect_true(co$monotone)
  expect_false(co$relevant[2])
  expect_false(co$coherent)
  # a non-monotone function is flagged
  phiNonMono <- function(x) as.integer(sum(x) == 1)
  expect_false(riskCoherence(phiNonMono, 2)$monotone)
})

test_that("structure function is monotone for every offered topology", {
  topologies <- list(
    riskPhiSeries(4), riskPhiParallel(4), riskPhiKofN(4, 2),
    riskPhiSeriesParallel(2, 2), riskPhiParallelSeries(2, 2), riskPhiBridge())
  ns <- c(4, 4, 4, 4, 4, 5)
  for (i in seq_along(topologies))
    expect_true(riskCoherence(topologies[[i]], ns[i])$monotone)
})

test_that("diagram layouts produce one box per component", {
  expect_equal(nrow(riskDiagramLayout("series", 4)$boxes), 4)
  expect_equal(nrow(riskDiagramLayout("parallel", 3)$boxes), 3)
  expect_equal(nrow(riskDiagramLayout("koutofn", 5)$boxes), 5)
  expect_equal(nrow(riskDiagramLayout("seriesParallel", 4, m = 2, npb = 2)$boxes), 4)
  expect_equal(nrow(riskDiagramLayout("parallelSeries", 6, m = 2, npb = 3)$boxes), 6)
  expect_equal(nrow(riskDiagramLayout("bridge", 5)$boxes), 5)
  # labels carry the reliabilities when given
  lay <- riskDiagramLayout("series", 2, r = c(0.9, 0.8))
  expect_true(grepl("0.9", lay$boxes$label[1]))
})

test_that("state table probabilities sum to 1 and split by phi", {
  r <- c(0.9, 0.8, 0.7)
  st <- riskStateTable(riskPhiSeries(3), r)
  expect_equal(sum(st$prob), 1, tolerance = 1e-12)
  expect_equal(sum(st$prob[st$phi == 1]),
               riskSystemReliability(riskPhiSeries(3), r), tolerance = 1e-12)
})
