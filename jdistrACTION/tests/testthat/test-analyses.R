# Integration tests calling the compiled analyses directly (the jmc-generated
# wrappers return analysis$results). They run only when the compiled
# distrACTION package is installed. The outputs are numbers, so that the
# number format of the app applies to them; nothing is rounded in R.

skip_if_not_installed("distrACTION")

getProb <- function(res)
  res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value

getQuantile <- function(res, rowNo = 1)
  res$Outputs$getCell(rowNo = rowNo, "QuantileResultColumn")$value

test_that("GammaDistribution matches pgamma/qgamma", {
  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 2, dp1 = 2, dp2 = 1.5)
  expect_equal(getProb(res), pgamma(2, 2, 1.5), tolerance = 1e-9)

  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "higher",
    x1 = 2, dp1 = 2, dp2 = 1.5)
  expect_equal(getProb(res), pgamma(2, 2, 1.5, lower.tail = FALSE),
               tolerance = 1e-9)

  res <- distrACTION::GammaDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.9, dp1 = 2, dp2 = 1.5)
  expect_equal(getQuantile(res), qgamma(0.9, 2, 1.5), tolerance = 1e-9)

  # shape < 1 (density unbounded at 0) must not error
  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, x1 = 1, dp1 = 0.5, dp2 = 1)
  expect_equal(getProb(res), pgamma(1, 0.5, 1), tolerance = 1e-9)
})

test_that("WeibullDistribution matches pweibull/qweibull", {
  res <- distrACTION::WeibullDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 2, dp1 = 1.5, dp2 = 2)
  expect_equal(getProb(res), pweibull(2, 1.5, 2), tolerance = 1e-9)

  res <- distrACTION::WeibullDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.5, dp1 = 1.5, dp2 = 2)
  expect_equal(getQuantile(res), qweibull(0.5, 1.5, 2), tolerance = 1e-9)
})

test_that("NegBinomialDistribution honours both conventions", {
  r <- 3; p <- 0.4

  # trials (default): P(X = 5) = choose(4,2) p^3 (1-p)^2
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 5, dp1 = r, dp2 = p)
  expect_equal(getProb(res), dnbinom(5 - r, r, p), tolerance = 1e-9)

  # trials: P(X <= 6)
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 6, dp1 = r, dp2 = p)
  expect_equal(getProb(res), pnbinom(6 - r, r, p), tolerance = 1e-9)

  # trials: x1 < r lies before the support
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 2, dp1 = r, dp2 = p)
  expect_equal(getProb(res), 0)

  # failures: plain dnbinom
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 2, dp1 = r, dp2 = p, RandomVariable = "failures")
  expect_equal(getProb(res), dnbinom(2, r, p), tolerance = 1e-9)

  # quantile shifts by r between conventions
  resT <- distrACTION::NegBinomialDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.8, dp1 = r, dp2 = p)
  resF <- distrACTION::NegBinomialDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.8, dp1 = r, dp2 = p, RandomVariable = "failures")
  expect_equal(getQuantile(resT), getQuantile(resF) + r)

  # p = 1 degenerates to X = r in the trials convention
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = r, dp1 = r, dp2 = 1)
  expect_equal(getProb(res), 1)
})

test_that("GeometricDistribution keeps the failures default and adds trials", {
  p <- 0.3

  # default (failures) unchanged: P(X = 0) = p
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 0, dp1 = p)
  expect_equal(getProb(res), dgeom(0, p), tolerance = 1e-9)

  # trials: P(X = 1) = p, P(X = 0) = 0
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 1, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), p, tolerance = 1e-9)
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 0, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), 0)

  # trials: P(X <= 4) = 1 - (1-p)^4
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 4, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), 1 - (1 - p)^4, tolerance = 1e-9)
})

test_that("t distribution: E[X] undefined for df <= 1, finite for large df (audit 2026-11-05)", {
    moment <- function(res, key) res$MomentsTable$getCell(rowKey = key, "ValueColumn")$value
    cauchy <- distrACTION:::TDistribution(x1 = 0, dp1 = 1, dp2 = 0, showMean = TRUE, showVariance = TRUE)
    expect_match(moment(cauchy, "mean"), "nieokreślone")
    expect_equal(moment(distrACTION:::TDistribution(x1 = 0, dp1 = 5, dp2 = 0, showMean = TRUE), "mean"), 0)
    big <- distrACTION:::TDistribution(x1 = 0, dp1 = 400, dp2 = 1, showMean = TRUE, showVariance = TRUE)
    expect_true(is.finite(moment(big, "mean")))
    expect_true(is.finite(moment(big, "var")))
})

test_that("t distribution: the plot covers the distribution for a negative ncp (audit 2026-11-05)", {
    res <- distrACTION:::TDistribution(x1 = -3, dp1 = 10, dp2 = -3)
    st <- res$plot$state
    expect_false(is.null(st))
    x <- st[[1]]   # first column: the x grid of the curve
    expect_lt(min(x), qt(0.001, 10, ncp = -3))
    expect_gt(max(x), qt(0.999, 10, ncp = -3))
})

test_that("discrete plots: huge supports are trimmed, usual ones unchanged (audit 2026-11-05)", {
    p <- distrACTION:::PoissonDistribution(x1 = 1e6, dp1 = 1e6, DistributionFunction = TRUE)
    xs <- p$plot$state$Datas$X
    expect_lt(length(xs), 20000)
    expect_true(1e6 >= min(xs) && 1e6 <= max(xs))
    expect_equal(range(distrACTION:::PoissonDistribution(x1 = 2, dp1 = 5)$plot$state$Datas$X), c(0, qpois(0.999, 5)))
    t0 <- proc.time()[["elapsed"]]
    b <- distrACTION:::BinomialDistribution(x1 = 5e6, dp1 = 1e7, dp2 = 0.5, DistributionFunction = TRUE)
    expect_lt(proc.time()[["elapsed"]] - t0, 5)
    expect_equal(range(distrACTION:::BinomialDistribution(x1 = 3, dp1 = 10, dp2 = 0.3)$plot$state$Dataset[[1]]), c(0, 10))
})

test_that("results are numbers, so the number format of the app applies", {
    res <- distrACTION::Normaldistribution(
        DistributionFunction = TRUE, x1 = -4, dp1 = 0, dp2 = 1,
        QuantileFunction = TRUE, QuantileFunctionType = "central", p = 0.95,
        showMean = TRUE, showVariance = TRUE)
    cell <- function(row, col) res$Outputs$getCell(rowNo = row, col)$value
    # a small probability is not rounded away to 0
    expect_equal(cell(1, "DistributionResultColumn"), pnorm(-4))
    expect_equal(cell(1, "QuantileLabelColumn"), "x1")
    expect_equal(cell(1, "QuantileResultColumn"), qnorm(0.025))
    expect_equal(cell(2, "QuantileLabelColumn"), "x2")
    expect_equal(cell(2, "QuantileResultColumn"), qnorm(0.975))
    expect_true(is.numeric(res$MomentsTable$getCell(rowKey = "var", "ValueColumn")$value))
    # quantiles of a discrete distribution are whole numbers, shown as they are
    res <- distrACTION::PoissonDistribution(
        QuantileFunction = TRUE, QuantileFunctionType = "cumulative", p = 0.9, dp1 = 4)
    expect_identical(res$Outputs$getCell(rowNo = 1, "QuantileResultColumn")$value,
                     as.integer(qpois(0.9, 4)))
})
