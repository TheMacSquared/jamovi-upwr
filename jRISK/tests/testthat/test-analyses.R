# Integration tests calling the compiled analyses directly (jmc-generated
# wrappers return analysis$results). They run only when the compiled jRISK
# package is installed.

skip_if_not_installed("jRISK")

test_that("lifetime reports f/F/R/h consistent with the model", {
  res <- jRISK::lifetime(distribution = "weibull",
                         weibullShape = 1.5, weibullScale = 2, t = 1)
  at <- res$atTable$asDF
  expect_equal(at$Ft, pweibull(1, 1.5, 2), tolerance = 1e-9)
  expect_equal(at$Rt, 1 - at$Ft, tolerance = 1e-9)
  expect_equal(at$ht, at$ft / at$Rt, tolerance = 1e-9)

  # hazard character lands in the summary table
  sm <- res$summaryTable$asDF
  expect_true(any(grepl("rosnąca", sm$value)))

  # exponential: constant hazard equal to the rate
  res <- jRISK::lifetime(distribution = "exponential", expRate = 2, t = 3)
  at <- res$atTable$asDF
  expect_equal(at$ht, 2, tolerance = 1e-9)

  # gamma shape < 1 at t = 0: infinite density/hazard handled, no error
  res <- jRISK::lifetime(distribution = "gamma",
                         gammaShape = 0.5, gammaRate = 1, t = 0)
  at <- res$atTable$asDF
  expect_true(is.na(at$ht))
})

test_that("relsystem reproduces the control values", {
  # bridge, all r = 0.9
  res <- jRISK::relsystem(structure = "bridge", sameReliability = TRUE,
                          componentReliability = 0.9)
  rt <- res$resultTable$asDF
  expect_equal(rt$rel, 0.97848, tolerance = 1e-9)
  expect_equal(rt$fail, 1 - 0.97848, tolerance = 1e-9)

  # 2-of-3 with p = 0.9
  res <- jRISK::relsystem(structure = "koutofn", nComponents = 3, kValue = 2,
                          sameReliability = TRUE, componentReliability = 0.9)
  expect_equal(res$resultTable$asDF$rel, 0.972, tolerance = 1e-9)

  # series with distinct reliabilities
  res <- jRISK::relsystem(structure = "series", nComponents = 3,
                          sameReliability = FALSE,
                          r1 = 0.9, r2 = 0.8, r3 = 0.95)
  expect_equal(res$resultTable$asDF$rel, 0.9 * 0.8 * 0.95, tolerance = 1e-9)

  # paths/cuts table for the bridge
  res <- jRISK::relsystem(structure = "bridge", showPathsCuts = TRUE)
  pc <- res$pathsTable$asDF
  expect_equal(sum(pc$type == "ścieżka minimalna"), 4)
  expect_equal(sum(pc$type == "przekrój minimalny"), 4)

  # Birnbaum importance on the bridge: the crossover (3) matters least
  res <- jRISK::relsystem(structure = "bridge", showImportance = TRUE)
  imp <- res$importanceTable$asDF
  expect_equal(nrow(imp), 5)
  expect_equal(imp$component[5], "3")
  expect_true(all(diff(imp$birnbaum) <= 1e-12))

  # k > n is rejected with an error, not computed
  res <- jRISK::relsystem(structure = "koutofn", nComponents = 3, kValue = 5)
  expect_error(expect_equal(res$resultTable$asDF$rel, numeric(1)))
})
