# Tests of the 0.2 computational core: censored MLE, Kaplan-Meier wrapper,
# two-level structures and the fault-tree engine.
if (!exists("riskLtFit"))
  source(file.path(testthat::test_path(), "..", "..", "R", "utils.R"))

test_that("exponential censored MLE has the closed form events/total-time", {
  set.seed(11)
  t <- rexp(200, rate = 0.5)
  status <- as.integer(t <= 3)
  t <- pmin(t, 3)
  fit <- riskLtFit(t, status, "exponential")
  expect_true(fit$ok)
  expect_equal(fit$par$rate, sum(status) / sum(t), tolerance = 1e-12)
  expect_true(fit$lower$rate < 0.5 && 0.5 < fit$upper$rate)
  expect_equal(fit$AIC, -2 * fit$logLik + 2, tolerance = 1e-12)
})

test_that("weibull censored MLE agrees with survival::survreg", {
  set.seed(22)
  t <- rweibull(300, shape = 1.5, scale = 24)
  status <- as.integer(t <= 36)
  t <- pmin(t, 36)
  fit <- riskLtFit(t, status, "weibull")
  expect_true(fit$ok)
  sr <- survival::survreg(survival::Surv(t, status) ~ 1, dist = "weibull")
  expect_equal(fit$par$shape, 1 / sr$scale, tolerance = 1e-3)
  expect_equal(fit$par$scale, unname(exp(coef(sr))), tolerance = 1e-3)
})

test_that("gamma censored MLE recovers simulated parameters", {
  set.seed(33)
  t <- rgamma(500, shape = 2, rate = 0.4)
  status <- as.integer(t <= 12)
  t <- pmin(t, 12)
  fit <- riskLtFit(t, status, "gamma")
  expect_true(fit$ok)
  expect_equal(fit$par$shape, 2, tolerance = 0.25)
  expect_equal(fit$par$rate, 0.4, tolerance = 0.25)
  expect_false(fit$singular)
})

test_that("censored MLE edge cases", {
  # no events at all -> refuse to fit
  fit <- riskLtFit(c(5, 6, 7), c(0L, 0L, 0L), "exponential")
  expect_false(fit$ok)
  expect_equal(fit$message, "noEvents")
  # a single event still fits without error
  fit <- riskLtFit(c(5, 6, 7), c(1L, 0L, 0L), "exponential")
  expect_true(fit$ok)
  expect_equal(fit$par$rate, 1 / 18, tolerance = 1e-12)
})

test_that("Kaplan-Meier wrapper matches survfit and handles censoring", {
  t <- c(2, 3, 3, 5, 8, 10)
  status <- c(1, 1, 0, 1, 0, 1)
  km <- riskKaplanMeier(t, status)
  expect_equal(km$nEvents, 4)
  expect_true(all(diff(km$surv) <= 0))
  sf <- survival::survfit(survival::Surv(t, status) ~ 1)
  expect_equal(km$surv, sf$surv)
})

test_that("two-level closed form matches enumeration on small systems", {
  r <- c(0.9, 0.8, 0.95, 0.7, 0.85)
  sizes <- c(2, 3)
  for (inner in c("series", "parallel"))
    for (outer in c("series", "parallel")) {
      phi <- riskPhiTwoLevel(sizes, inner, outer)
      expect_equal(riskTwoLevelReliability(r, sizes, inner, outer),
                   riskSystemReliability(phi, r), tolerance = 1e-12,
                   label = paste(inner, outer))
    }
  # equal group sizes reduce to the canonical m x n topologies
  r4 <- rep(0.9, 4)
  expect_equal(riskTwoLevelReliability(r4, c(2, 2), "parallel", "series"),
               riskSystemReliability(riskPhiSeriesParallel(2, 2), r4),
               tolerance = 1e-12)
  expect_equal(riskTwoLevelReliability(r4, c(2, 2), "series", "parallel"),
               riskSystemReliability(riskPhiParallelSeries(2, 2), r4),
               tolerance = 1e-12)
})

test_that("fault tree probabilities for all gate combinations", {
  p <- c(0.1, 0.2, 0.05, 0.1)
  branch <- c(1, 1, 2, 2)
  # inner AND, top OR: 1 - (1 - p1 p2)(1 - p3 p4)
  expect_equal(riskFtaTopProb(p, branch, "and", "or"),
               1 - (1 - 0.1 * 0.2) * (1 - 0.05 * 0.1), tolerance = 1e-12)
  # inner OR, top AND: (1-(1-p1)(1-p2)) * (1-(1-p3)(1-p4))
  expect_equal(riskFtaTopProb(p, branch, "or", "and"),
               (1 - 0.9 * 0.8) * (1 - 0.95 * 0.9), tolerance = 1e-12)
  # inner OR, top OR = any event occurs
  expect_equal(riskFtaTopProb(p, branch, "or", "or"),
               1 - prod(1 - p), tolerance = 1e-12)
  # inner AND, top AND = all events occur
  expect_equal(riskFtaTopProb(p, branch, "and", "and"),
               prod(p), tolerance = 1e-12)
})

test_that("fault tree minimal cut sets via the occurrence function", {
  branch <- c(1, 1, 2, 2)
  canon <- function(sets) sort(vapply(sets, function(s) paste(s, collapse = ","), ""))
  # inner AND, top OR: cuts are the branches themselves
  occ <- riskFtaOccurrence(branch, "and", "or")
  expect_equal(canon(riskMinimalPaths(occ, 4)), sort(c("1,2", "3,4")))
  # inner OR, top AND: cuts are one event from each branch
  occ <- riskFtaOccurrence(branch, "or", "and")
  expect_equal(canon(riskMinimalPaths(occ, 4)),
               sort(c("1,3", "1,4", "2,3", "2,4")))
  # inner OR, top OR: every single event is a cut
  occ <- riskFtaOccurrence(branch, "or", "or")
  expect_equal(canon(riskMinimalPaths(occ, 4)), sort(c("1", "2", "3", "4")))
})

test_that("fault tree importance ranks the dominant event first", {
  p <- c(0.2, 0.01, 0.01, 0.01)
  branch <- c(1, 2, 3, 4)   # four single-event branches, top OR
  imp <- riskFtaImportance(p, branch, "or", "or")
  expect_equal(which.max(imp), 1)
  # making an event impossible can only lower P(top)
  expect_true(all(imp >= -1e-12))
})
