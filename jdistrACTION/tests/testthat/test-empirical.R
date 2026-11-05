# Empirical data next to the binomial model ("Dane empiryczne").
# Reference: risk analysis list, task 4.3 — Bananpol devices, 12 checks,
# X ~ Bin(12; 0.15), observed counts 19, 46, 47, 25, 11, 2 for x = 0..5.

skip_if_not_installed("distrACTION")

bananpol <- data.frame(kontrole = rep(0:5, c(19, 46, 47, 25, 11, 2)))

test_that("observed / expected table reproduces the task 4.3 key", {
  res <- distrACTION::BinomialDistribution(
    data = bananpol, empVar = "kontrole", dp1 = 12, dp2 = 0.15,
    DistributionFunction = TRUE, DistributionFunctionType = "is", x1 = 0,
    empBand = TRUE)
  tab <- res$EmpTable$asDF
  expect_equal(tab$x, c(as.character(0:5), "≥ 6", "Razem"))
  expect_equal(tab$obs, c(19, 46, 47, 25, 11, 2, 0, 150))
  expect_equal(round(tab$exp[1:7], 1), c(21.3, 45.2, 43.9, 25.8, 10.2, 2.9, 0.7))
  expect_equal(tab$exp[8], 150, tolerance = 1e-9)
  # exact 95% range of a Bin(150, P(X = 0)) count
  p0 <- dbinom(0, 12, 0.15)
  expect_equal(tab$band[1], sprintf("%d–%d", qbinom(0.025, 150, p0), qbinom(0.975, 150, p0)))
  # empirical frequency of the selected event next to the model probability
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value,
               "0.127 (19 z 150)")
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(bananpol$kontrole), var(bananpol$kontrole)))
  expect_equal(mom$model, c(1.8, 12 * 0.15 * 0.85))
})

test_that("estimated p uses the mean and removes one degree of freedom", {
  given <- distrACTION::BinomialDistribution(
    data = bananpol, empVar = "kontrole", dp1 = 12, dp2 = 0.15, empChiSq = TRUE)
  est <- distrACTION::BinomialDistribution(
    data = bananpol, empVar = "kontrole", dp1 = 12, dp2 = 0.5,
    empParams = "estimated", empChiSq = TRUE)
  expect_equal(est$EmpMoments$asDF$model[1], mean(bananpol$kontrole), tolerance = 1e-12)
  g <- given$EmpChiSq$asDF
  e <- est$EmpChiSq$asDF
  expect_equal(e$df, g$df - 1)
  # classes pooled to E >= 5: {0}, {1}, {2}, {3}, {4..12} ({5..12} has E = 3.6)
  expect_equal(g$classes, 5)
  O <- c(19, 46, 47, 25, 13)
  E <- 150 * c(dbinom(0:3, 12, 0.15), pbinom(3, 12, 0.15, lower.tail = FALSE))
  expect_equal(g$chi, sum((O - E)^2 / E), tolerance = 1e-9)
  expect_equal(g$p, pchisq(sum((O - E)^2 / E), 4, lower.tail = FALSE), tolerance = 1e-9)
})

test_that("invalid data give a clear error and no variable keeps the calculator", {
  bad <- data.frame(v = c(1, 2.5, 3))
  expect_error(distrACTION::BinomialDistribution(data = bad, empVar = "v", dp1 = 12),
               "niecałkowite")
  over <- data.frame(v = c(1, 13))
  expect_error(distrACTION::BinomialDistribution(data = over, empVar = "v", dp1 = 12),
               "spoza zakresu")
  res <- distrACTION::BinomialDistribution(DistributionFunction = TRUE, x1 = 0,
                                           dp1 = 12, dp2 = 0.15)
  expect_false(res$EmpTable$visible)
  expect_equal(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value,
               "P = 0.142")
})
