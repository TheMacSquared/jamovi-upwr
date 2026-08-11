# Integration tests of the 0.2 analyses (data modes and the new analyses),
# run against the compiled jRISK package.

skip_if_not_installed("jRISK")

test_that("lifetime data mode fits models to censored data", {
  set.seed(7)
  czas <- round(rweibull(120, 1.5, 24), 1)
  status <- ifelse(czas > 36, "pracuje", "awaria")
  df <- data.frame(czas = pmin(czas, 36), status = factor(status))

  res <- jRISK::lifetime(data = df, mode = "data",
                         timeVar = "czas", statusVar = "status",
                         failureLevel = "awaria", t = 12)

  counts <- res$dataCounts$asDF
  expect_equal(counts$n, 120)
  expect_equal(counts$events + counts$censored, 120)

  fit <- res$fitTable$asDF
  expect_equal(nrow(fit), 3)
  expect_true(all(is.finite(fit$aic)))

  # exponential MLE has the closed form events / total time
  par <- res$paramTable$asDF
  expRate <- par$est[par$model == "Wykładniczy"]
  st <- as.integer(df$status == "awaria")
  expect_equal(expRate, sum(st) / sum(df$czas), tolerance = 1e-6)

  # R(t) table contains KM and the three models
  at <- res$dataAtTable$asDF
  expect_equal(nrow(at), 4)
  expect_true(all(at$rt >= 0 & at$rt <= 1))
})

test_that("lifetime data mode refuses all-censored data", {
  df <- data.frame(czas = c(5, 6, 7), status = factor(rep("pracuje", 3), levels = c("awaria", "pracuje")))
  res <- jRISK::lifetime(data = df, mode = "data",
                         timeVar = "czas", statusVar = "status",
                         failureLevel = "awaria")
  expect_equal(res$dataCounts$status, "error")
})

test_that("eventtables reproduces hand-computed 2x2 probabilities", {
  df <- data.frame(
    stan = factor(c("awaria", "awaria", "awaria", "ok", "ok", "ok", "ok", "ok", "ok", "ok")),
    alarm = factor(c("tak", "tak", "nie", "tak", "nie", "nie", "nie", "nie", "nie", "nie")))
  res <- jRISK::eventtables(data = df, varA = "stan", levelA = "awaria",
                            varB = "alarm", levelB = "tak",
                            showDetector = TRUE)
  pr <- res$probTable$asDF
  expect_equal(pr$value[pr$quantity == "P(A)"], 0.3, tolerance = 1e-12)
  expect_equal(pr$value[pr$quantity == "P(B)"], 0.3, tolerance = 1e-12)
  expect_equal(pr$value[pr$quantity == "P(A ∩ B)"], 0.2, tolerance = 1e-12)
  expect_equal(pr$value[pr$quantity == "P(A | B)"], 2/3, tolerance = 1e-12)

  det <- res$detectorTable$asDF
  expect_equal(det$value[det$quantity == "Czułość"], 2/3, tolerance = 1e-12)
  expect_equal(det$value[det$quantity == "PPV"], 2/3, tolerance = 1e-12)
  expect_equal(det$value[det$quantity == "Swoistość"], 6/7, tolerance = 1e-12)
})

test_that("bernoulli summarizes a binary series", {
  df <- data.frame(
    wynik = factor(c("s", "p", "s", "s", "p", "s", "s", "s", "p", "s")),
    kolej = 1:10)
  res <- jRISK::bernoulli(data = df, outcomeVar = "wynik",
                          successLevel = "s", orderVar = "kolej")
  sm <- res$summaryTable$asDF
  expect_equal(sm$n, 10)
  expect_equal(sm$successes, 7)
  expect_equal(sm$phat, 0.7, tolerance = 1e-12)
  expect_true(sm$lower < 0.7 && 0.7 < sm$upper)
})

test_that("fta computes the top event and rejects duplicated labels", {
  df <- data.frame(
    zdarzenie = c("pompa", "zawor", "czujnik", "sterownik"),
    p = c(0.1, 0.2, 0.05, 0.1),
    galaz = c("hydraulika", "hydraulika", "elektronika", "elektronika"))
  res <- jRISK::fta(data = df, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz")
  top <- res$topTable$asDF
  expect_equal(top$ptop, 1 - (1 - 0.1 * 0.2) * (1 - 0.05 * 0.1),
               tolerance = 1e-12)
  cuts <- res$cutsTable$asDF
  expect_equal(nrow(cuts), 2)

  dup <- df
  dup$zdarzenie[2] <- "pompa"
  res <- jRISK::fta(data = dup, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz")
  expect_equal(res$topTable$status, "error")
})

test_that("relsystem data mode composes two-level structures from the sheet", {
  df <- data.frame(
    r = c(0.9, 0.9, 0.9, 0.9),
    kom = c("P1", "P2", "F1", "F2"),
    grupa = c("pompy", "pompy", "filtry", "filtry"))
  res <- jRISK::relsystem(data = df, mode = "data", relVar = "r",
                          labelVar = "kom", groupVar = "grupa",
                          innerGate = "parallel", outerGate = "series",
                          showPathsCuts = TRUE)
  rt <- res$resultTable$asDF
  expect_equal(rt$rel, (1 - 0.1^2)^2, tolerance = 1e-12)
  pc <- res$pathsTable$asDF
  expect_true(any(grepl("P1", pc$set)))
})
