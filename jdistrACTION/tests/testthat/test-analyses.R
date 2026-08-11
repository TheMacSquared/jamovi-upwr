# Integration tests calling the compiled analyses directly (the jmc-generated
# wrappers return analysis$results). They run only when the compiled
# distrACTION package is installed; the outputs are rendered as text
# ("P = 0.632"), so values are parsed back before comparing.

skip_if_not_installed("distrACTION")

parseNum <- function(cell)
  as.numeric(sub("^[^=]*= *", "", cell))

getProb <- function(res)
  parseNum(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value)

getQuantile <- function(res, rowNo = 1)
  parseNum(res$Outputs$getCell(rowNo = rowNo, "QuantileResultColumn")$value)

test_that("GammaDistribution matches pgamma/qgamma", {
  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 2, dp1 = 2, dp2 = 1.5)
  expect_equal(getProb(res), round(pgamma(2, 2, 1.5), 3), tolerance = 1e-9)

  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "higher",
    x1 = 2, dp1 = 2, dp2 = 1.5)
  expect_equal(getProb(res), round(pgamma(2, 2, 1.5, lower.tail = FALSE), 3),
               tolerance = 1e-9)

  res <- distrACTION::GammaDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.9, dp1 = 2, dp2 = 1.5)
  expect_equal(getQuantile(res), round(qgamma(0.9, 2, 1.5), 3), tolerance = 1e-9)

  # shape < 1 (density unbounded at 0) must not error
  res <- distrACTION::GammaDistribution(
    DistributionFunction = TRUE, x1 = 1, dp1 = 0.5, dp2 = 1)
  expect_equal(getProb(res), round(pgamma(1, 0.5, 1), 3), tolerance = 1e-9)
})

test_that("WeibullDistribution matches pweibull/qweibull", {
  res <- distrACTION::WeibullDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 2, dp1 = 1.5, dp2 = 2)
  expect_equal(getProb(res), round(pweibull(2, 1.5, 2), 3), tolerance = 1e-9)

  res <- distrACTION::WeibullDistribution(
    QuantileFunction = TRUE, QuantileFunctionType = "cumulative",
    p = 0.5, dp1 = 1.5, dp2 = 2)
  expect_equal(getQuantile(res), round(qweibull(0.5, 1.5, 2), 3), tolerance = 1e-9)
})

test_that("NegBinomialDistribution honours both conventions", {
  r <- 3; p <- 0.4

  # trials (default): P(X = 5) = choose(4,2) p^3 (1-p)^2
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 5, dp1 = r, dp2 = p)
  expect_equal(getProb(res), round(dnbinom(5 - r, r, p), 3), tolerance = 1e-9)

  # trials: P(X <= 6)
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 6, dp1 = r, dp2 = p)
  expect_equal(getProb(res), round(pnbinom(6 - r, r, p), 3), tolerance = 1e-9)

  # trials: x1 < r lies before the support
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 2, dp1 = r, dp2 = p)
  expect_equal(getProb(res), 0)

  # failures: plain dnbinom
  res <- distrACTION::NegBinomialDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 2, dp1 = r, dp2 = p, RandomVariable = "failures")
  expect_equal(getProb(res), round(dnbinom(2, r, p), 3), tolerance = 1e-9)

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
  expect_equal(getProb(res), round(dgeom(0, p), 3), tolerance = 1e-9)

  # trials: P(X = 1) = p, P(X = 0) = 0
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 1, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), round(p, 3), tolerance = 1e-9)
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "is",
    x1 = 0, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), 0)

  # trials: P(X <= 4) = 1 - (1-p)^4
  res <- distrACTION::GeometricDistribution(
    DistributionFunction = TRUE, DistributionFunctionType = "lower",
    x1 = 4, dp1 = p, RandomVariable = "trials")
  expect_equal(getProb(res), round(1 - (1 - p)^4, 3), tolerance = 1e-9)
})
