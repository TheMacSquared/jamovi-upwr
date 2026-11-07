# Empirical data next to the binomial model ("Dane empiryczne").
# Reference: risk analysis list, task 4.3 — Bananpol devices, 12 checks,
# X ~ Bin(12; 0.15), observed counts 19, 46, 47, 25, 11, 2 for x = 0..5.

skip_if_not_installed("distrACTION")

bananpol <- data.frame(kontrole = rep(0:5, c(19, 46, 47, 25, 11, 2)))

test_that("observed / expected table reproduces the task 4.3 key", {
  res <- distrACTION::BinomialDistribution(
    data = bananpol, empVar = "kontrole", dp1 = 12, dp2 = 0.15,
    DistributionFunction = TRUE, DistributionFunctionType = "is", x1 = 0)
  tab <- res$EmpTable$asDF
  expect_equal(tab$x, c(as.character(0:5), "≥ 6", "Razem"))
  expect_equal(tab$obs, c(19, 46, 47, 25, 11, 2, 0, 150))
  expect_equal(round(tab$exp[1:7], 1), c(21.3, 45.2, 43.9, 25.8, 10.2, 2.9, 0.7))
  expect_equal(tab$exp[8], 150, tolerance = 1e-9)
  # empirical frequency of the selected event next to the model probability
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value,
               "0.127 (19 z 150)")
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(bananpol$kontrole), var(bananpol$kontrole)))
  expect_equal(mom$model, c(1.8, 12 * 0.15 * 0.85))
})

test_that("a 0/1 variable gives Bin(n, p-hat) next to Bin(n, p) from the parameters", {
  # 41 successes in 150 trials
  d <- data.frame(wada = factor(rep(c("nie", "tak"), c(109, 41))),
                  kod = rep(c(0, 1), c(109, 41)))
  res <- distrACTION::BinomialDistribution(
    data = d, empVar = "wada", empType = "binary", dp1 = 12, dp2 = 0.15,
    DistributionFunction = TRUE, DistributionFunctionType = "lower", x1 = 2)
  ph <- 41 / 150
  expect_false(res$EmpTable$visible)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(ph, 12 * ph, 12 * ph * (1 - ph)))
  expect_equal(mom$model, c(0.15, 1.8, 12 * 0.15 * 0.85))
  # the parameters still drive the main result; the data column uses p-hat
  expect_equal(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value,
               paste0("P = ", round(pbinom(2, 12, 0.15), 3)))
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value,
               paste0("P = ", round(pbinom(2, 12, ph), 3)))
  # numeric 0/1 column: default success = second category ("1")
  num <- distrACTION::BinomialDistribution(
    data = d, empVar = "kod", empType = "binary", dp1 = 12, dp2 = 0.15)
  expect_equal(num$EmpMoments$asDF$data[1], ph)
  # the success category can be switched
  rev <- distrACTION::BinomialDistribution(
    data = d, empVar = "wada", empType = "binary", empLevel = "nie", dp1 = 12, dp2 = 0.15)
  expect_equal(rev$EmpMoments$asDF$data[1], 109 / 150)
  three <- data.frame(v = factor(c("a", "b", "c")))
  expect_error(distrACTION::BinomialDistribution(
    data = three, empVar = "v", empType = "binary", dp1 = 12), "Nieprawidłowy format zmiennej")
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

test_that("a variable of the wrong content tells which option to choose", {
  d <- data.frame(wada = factor(c("nie", "tak", "nie")),
                  kod = factor(c("0", "2", "1")), bin = c(0, 1, 1))
  # text categories read as counts
  expect_error(distrACTION::BinomialDistribution(data = d, empVar = "wada", dp1 = 12),
               "Nieprawidłowy format zmiennej")
  # a nominal column with numeric labels is read as counts
  res <- distrACTION::BinomialDistribution(data = d, empVar = "kod", dp1 = 12)
  expect_equal(res$EmpMoments$asDF$data[1], 1)
  # only 0 and 1 read as counts with n > 1 -> a note pointing to the 0/1 option
  res <- distrACTION::BinomialDistribution(data = d, empVar = "bin", dp1 = 12)
  expect_match(res$EmpTable$asString(), "zero-jedynkowa")
})

# Normal distribution: a continuous variable next to N(mean, SD) from the
# parameters (moments, event frequency, density histogram).
test_that("normal: moments and event frequency of a continuous variable", {
  d <- data.frame(masa = c(240, 245, 250, 250, 255, 260, 248, 252),
                  tekst = factor(c("a", "b", "a", "b", "a", "b", "a", "b")))
  res <- distrACTION::Normaldistribution(
    data = d, empVar = "masa", dp1 = 250, dp2 = 10,
    DistributionFunction = TRUE, DistributionFunctionType = "lower", x1 = 248)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$stat, c("Średnia", "Wariancja", "Odch. std."))
  expect_equal(mom$data, c(mean(d$masa), var(d$masa), sd(d$masa)))
  expect_equal(mom$model, c(250, 100, 10))
  # no rounding of x1 for a continuous distribution: 240, 245, 248
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value,
               "0.375 (3 z 8)")
  expect_error(distrACTION::Normaldistribution(data = d, empVar = "tekst", x1 = 0, dp1 = 0, dp2 = 1),
               "Nieprawidłowy format zmiennej")
  # without a variable the calculator is unchanged
  res <- distrACTION::Normaldistribution(x1 = 0, dp1 = 0, dp2 = 1)
  expect_false(res$EmpMoments$visible)
})

test_that("normal: density histogram uses Sturges' classes and has area 1", {
  x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 10)
  h <- distrACTION:::empHistogram(x)
  expect_equal(nrow(h), ceiling(log2(10) + 1))
  expect_equal(sum(h$density * (h$xmax - h$xmin)), 1)
  expect_equal(nrow(distrACTION:::empHistogram(x, 3)), 3)
  # the maximum falls into the last class (closed on both sides)
  expect_gt(h$density[nrow(h)], 0)
})

# Poisson: counts of events next to Poisson(lambda) from the parameters.
test_that("poisson: observed / expected table, moments and event frequency", {
  d <- data.frame(zgl = rep(0:5, c(10, 20, 25, 20, 15, 10)),
                  tekst = factor(rep(c("a", "b"), 50)))
  res <- distrACTION::PoissonDistribution(
    data = d, empVar = "zgl", dp1 = 2.5,
    DistributionFunction = TRUE, DistributionFunctionType = "lower", x1 = 1)
  tab <- res$EmpTable$asDF
  expect_equal(tail(tab$x, 1), "Razem")
  expect_equal(tail(tab$obs, 1), 100)
  expect_equal(tail(tab$exp, 1), 100, tolerance = 1e-9)
  expect_equal(tab$obs[tab$x == "3"], 20)
  expect_equal(tab$exp[tab$x == "3"], 100 * dpois(3, 2.5), tolerance = 1e-9)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(d$zgl), var(d$zgl)))
  expect_equal(mom$model, c(2.5, 2.5))
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value,
               "0.3 (30 z 100)")
  expect_error(distrACTION::PoissonDistribution(data = d, empVar = "tekst", dp1 = 2.5),
               "Nieprawidłowy format zmiennej")
  # the axis reaches the largest observation
  far <- data.frame(zgl = c(0, 1, 2, 20))
  res <- distrACTION::PoissonDistribution(data = far, empVar = "zgl", dp1 = 2)
  expect_equal(max(res$plot$state$Datas$X), 20)
})

test_that("end classes are not drawn as single values", {
  # lambda = 3, data up to 10: the table ends with "≥ 11", which must not
  # become a point at x = 11
  d <- data.frame(zgl = c(rep(0:6, c(5, 15, 22, 22, 17, 10, 5)), 8, 10))
  res <- distrACTION::PoissonDistribution(data = d, empVar = "zgl", dp1 = 3)
  expect_true(any(grepl("^≥", res$EmpTable$asDF$x)))
  expect_equal(max(res$plot$state$emp$X), 10)
})

# Geometric: X read in the convention chosen in the parameters.
test_that("geometric: data in the trials convention next to the model", {
  d <- data.frame(rzuty = c(1, 1, 2, 2, 3, 4, 5, 7, 9, 12), zero = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  res <- distrACTION::GeometricDistribution(
    data = d, empVar = "rzuty", dp1 = 0.25, RandomVariable = "trials",
    DistributionFunction = TRUE, DistributionFunctionType = "lower", x1 = 2)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(d$rzuty), var(d$rzuty)))
  expect_equal(mom$model, c(1 / 0.25, 0.75 / 0.25^2))
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value, "0.4 (4 z 10)")
  tab <- res$EmpTable$asDF
  expect_equal(tail(tab$exp, 1), 10, tolerance = 1e-9)
  expect_equal(tab$exp[tab$x == "1"], 10 * 0.25, tolerance = 1e-9)
  # 0 is outside the trials support, fine for the failures convention
  expect_error(distrACTION::GeometricDistribution(data = d, empVar = "zero", dp1 = 0.25,
                                                  RandomVariable = "trials"),
               "mniejsze niż 1")
  res <- distrACTION::GeometricDistribution(data = d, empVar = "zero", dp1 = 0.25)
  expect_equal(res$EmpMoments$asDF$model[1], 0.75 / 0.25)
})

# Negative binomial: X read in the convention chosen in the parameters.
test_that("negative binomial: data in the trials convention next to the model", {
  d <- data.frame(tel = c(3, 5, 8, 10, 12, 15, 9, 20, 6, 14), male = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  res <- distrACTION::NegBinomialDistribution(
    data = d, empVar = "tel", dp1 = 3, dp2 = 0.2, RandomVariable = "trials",
    DistributionFunction = TRUE, DistributionFunctionType = "lower", x1 = 10)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(d$tel), var(d$tel)))
  expect_equal(mom$model, c(3 / 0.2, 3 * 0.8 / 0.2^2))
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value, "0.6 (6 z 10)")
  tab <- res$EmpTable$asDF
  expect_equal(tail(tab$exp, 1), 10, tolerance = 1e-9)
  expect_equal(tab$exp[tab$x == "3"], 10 * dnbinom(0, 3, 0.2), tolerance = 1e-9)
  # values below r are outside the trials support
  expect_error(distrACTION::NegBinomialDistribution(data = d, empVar = "male", dp1 = 3, dp2 = 0.2),
               "mniejsze niż 3")
  res <- distrACTION::NegBinomialDistribution(data = d, empVar = "male", dp1 = 3, dp2 = 0.2,
                                              RandomVariable = "failures")
  expect_equal(res$EmpMoments$asDF$model[1], 3 * 0.8 / 0.2)
})

# Exponential: a non-negative continuous variable next to Exp(lambda).
test_that("exponential: moments, event frequency and the support check", {
  d <- data.frame(czas = c(0.2, 0.5, 0.8, 1.1, 1.5, 2.0, 2.6, 3.5, 4.2, 6.0),
                  ujemne = c(-1, 0.5, 0.8, 1.1, 1.5, 2.0, 2.6, 3.5, 4.2, 6.0))
  res <- distrACTION::ExponentialDistribution(
    data = d, empVar = "czas", dp1 = 0.5,
    DistributionFunction = TRUE, DistributionFunctionType = "higher", x1 = 3)
  mom <- res$EmpMoments$asDF
  expect_equal(mom$data, c(mean(d$czas), var(d$czas), sd(d$czas)))
  expect_equal(mom$model, c(2, 4, 2))
  expect_equal(res$Outputs$getCell(rowNo = 1, "EmpiricalResultColumn")$value, "0.3 (3 z 10)")
  expect_error(distrACTION::ExponentialDistribution(data = d, empVar = "ujemne", dp1 = 0.5),
               "mniejsze niż 0")
  # the curve reaches the largest observation
  far <- data.frame(czas = c(0.1, 1, 50))
  res <- distrACTION::ExponentialDistribution(data = far, empVar = "czas", dp1 = 0.5)
  expect_equal(max(res$plot$state$Dataset[[1]]), 50)
})
