# Integration tests of the 0.4 additions on the exercise datasets and on
# inline variants of them (values are the keys of the "Analiza ryzyka"
# exercises).

skip_if_not_installed("jRISK")

readSet <- function(name)
  read.csv(file.path(testthat::test_path(), "..", "..", "data",
                     paste(name, ".csv", sep = "")),
           stringsAsFactors = TRUE)

# aggregated 2x2 table: one row per cell plus a count column
cells <- function(nameA, nameB, n) {
  d <- data.frame(A = factor(c("tak", "tak", "nie", "nie")),
                  B = factor(c("tak", "nie", "tak", "nie")),
                  licznosc = n)
  names(d)[1:2] <- c(nameA, nameB)
  d
}

test_that("eventtables takes aggregated counts (exercises 1.2, 3.1, 3.2, 3.4)", {
  d <- cells("brak_oznakowania", "mokra_posadzka", c(6, 22, 11, 61))
  res <- jRISK::eventtables(data = d, varA = "brak_oznakowania", levelA = "tak",
                            varB = "mokra_posadzka", levelB = "tak",
                            countVar = "licznosc")
  ct <- res$countsTable$asDF
  expect_equal(ct$total[3], 100)
  pr <- res$probTable$asDF
  expect_equal(pr$value[pr$quantity == "P(A ∪ B)"], 0.39, tolerance = 1e-12)

  d <- cells("awaria", "alarm", c(95, 5, 495, 9405))
  res <- jRISK::eventtables(data = d, varA = "awaria", levelA = "tak",
                            varB = "alarm", levelB = "tak",
                            countVar = "licznosc", showDetector = TRUE)
  det <- res$detectorTable$asDF
  expect_equal(det$value[det$quantity == "PPV"], 95 / 590, tolerance = 1e-12)
  expect_equal(det$value[det$quantity == "Czułość"], 0.95, tolerance = 1e-12)

  d <- cells("awaria", "alarm", c(95, 5, 4995, 94905))
  res <- jRISK::eventtables(data = d, varA = "awaria", levelA = "tak",
                            varB = "alarm", levelB = "tak",
                            countVar = "licznosc", showDetector = TRUE)
  det <- res$detectorTable$asDF
  expect_equal(round(det$value[det$quantity == "PPV"], 3), 0.019)

  d <- cells("stan", "wynik_dodatni", c(90, 10, 198, 9702))
  res <- jRISK::eventtables(data = d, varA = "stan", levelA = "tak",
                            varB = "wynik_dodatni", levelB = "tak",
                            countVar = "licznosc", showDetector = TRUE)
  det <- res$detectorTable$asDF
  expect_equal(det$value[det$quantity == "PPV"], 90 / 288, tolerance = 1e-12)
})

test_that("eventtables honours jamovi data weights", {
  d <- cells("awaria", "alarm", c(95, 5, 495, 9405))
  attr(d, "jmv-weights") <- d$licznosc
  attr(d, "jmv-weights-name") <- "licznosc"
  res <- jRISK::eventtables(data = d, varA = "awaria", levelA = "tak",
                            varB = "alarm", levelB = "tak", showDetector = TRUE)
  det <- res$detectorTable$asDF
  expect_equal(det$value[det$quantity == "PPV"], 95 / 590, tolerance = 1e-12)
})

test_that("bernoulli: upper bound for zero failures, aggregated counts (4.5)", {
  d <- data.frame(wada = factor(c("nie", "tak"), levels = c("nie", "tak")),
                  n = c(100, 0))
  res <- jRISK::bernoulli(data = d, outcomeVar = "wada", successLevel = "tak",
                          countVar = "n", showUpperBound = TRUE)
  sm <- res$summaryTable$asDF
  expect_equal(sm$n, 100)
  expect_equal(sm$successes, 0)
  expect_equal(sm$upperOne, 1 - 0.05^(1 / 100), tolerance = 1e-12)
  expect_false(res$runPlot$visible)
})

test_that("relsystem: braking system, coherence and common cause (9.1, 9.3, 9.4)", {
  d <- readSet("uklad_hamowania")
  res <- jRISK::relsystem(data = d, mode = "data", relVar = "niezawodnosc",
                          labelVar = "element", groupVar = "podsystem",
                          innerGate = "parallel", outerGate = "series",
                          showCoherence = TRUE, showPathsCuts = TRUE)
  expect_equal(res$resultTable$asDF$rel, 0.9405, tolerance = 1e-12)
  coh <- res$coherenceTable$asDF
  expect_equal(coh$value[coh$property == "System koherentny"], "tak")

  # common supply of the two lines A, B as a series element, q = 0.01
  dd <- d[d$element != "C", ]
  res <- jRISK::relsystem(data = dd, mode = "data", relVar = "niezawodnosc",
                          labelVar = "element", groupVar = "podsystem",
                          innerGate = "parallel", outerGate = "series",
                          commonCause = TRUE, ccfProb = 0.01,
                          showPathsCuts = TRUE, showImportance = TRUE)
  expect_equal(res$resultTable$asDF$rel, 0.9801, tolerance = 1e-12)
  pc <- res$pathsTable$asDF
  expect_true("{CCF}" %in% pc$set)
  imp <- res$importanceTable$asDF
  expect_equal(imp$component[1], "CCF")

  # manual mode: parallel n = 2 with the same common cause
  res <- jRISK::relsystem(structure = "parallel", nComponents = 2,
                          componentReliability = 0.9,
                          commonCause = TRUE, ccfProb = 0.01)
  expect_equal(res$resultTable$asDF$rel, 0.9801, tolerance = 1e-12)
})

test_that("relsystem data mode: k-out-of-n inside subsystems (9.5)", {
  d <- data.frame(r = c(0.9, 0.9, 0.9), kom = c("S1", "S2", "S3"),
                  grupa = c("czujniki", "czujniki", "czujniki"))
  res <- jRISK::relsystem(data = d, mode = "data", relVar = "r",
                          labelVar = "kom", groupVar = "grupa",
                          innerGate = "koutofn", kValue = 2,
                          outerGate = "series", showPathsCuts = TRUE)
  expect_equal(res$resultTable$asDF$rel, 0.972, tolerance = 1e-12)
  expect_equal(sum(res$pathsTable$asDF$type == "ścieżka minimalna"), 3)

  res <- jRISK::relsystem(data = d, mode = "data", relVar = "r",
                          labelVar = "kom", groupVar = "grupa",
                          innerGate = "koutofn", kValue = 4,
                          outerGate = "series")
  expect_equal(res$inputsTable$status, "error")
})

test_that("relsystem: thermal mission once the fan reliability is typed in (11.1)", {
  d <- data.frame(element = c("P", "C", "FAN1", "FAN2"),
                  podsystem = c("P", "C", "wentylatory", "wentylatory"),
                  niezawodnosc = c(0.98, 0.95, 0.5134171, 0.5134171))
  res <- jRISK::relsystem(data = d, mode = "data", relVar = "niezawodnosc",
                          labelVar = "element", groupVar = "podsystem",
                          innerGate = "parallel", outerGate = "series")
  expect_equal(round(res$resultTable$asDF$rel, 6), 0.710574)
})

test_that("fta on the exercise trees (10.1, 10.2, 10.3, 10.a)", {
  d <- data.frame(zdarzenie = c("C", "A", "B"), p = c(0.01, 0.05, 0.05),
                  galaz = c("C", "AB", "AB"))
  res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz", innerGate = "and", topGate = "or")
  expect_equal(res$topTable$asDF$ptop, 0.012475, tolerance = 1e-12)

  d <- data.frame(zdarzenie = c("I", "D0", "S0", "C"),
                  p = c(0.005, 0.05, 0.08, 0.01),
                  galaz = c("inicjacja", "bariera", "bariera", "bariera"))
  res <- jRISK::fta(data = d[d$zdarzenie != "C", ], labelVar = "zdarzenie",
                    probVar = "p", branchVar = "galaz",
                    innerGate = "or", topGate = "and")
  expect_equal(res$topTable$asDF$ptop, 0.00063, tolerance = 1e-12)
  res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz", innerGate = "or", topGate = "and")
  expect_equal(round(res$topTable$asDF$ptop, 7), 0.0006737)

  # repeated leaf: error by default, exact 0.05 when declared shared
  d <- data.frame(zdarzenie = c("C", "C"), p = c(0.05, 0.05), galaz = c("G1", "G2"))
  res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz", innerGate = "and", topGate = "and")
  expect_equal(res$topTable$status, "error")
  for (top in c("and", "or")) {
    res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                      branchVar = "galaz", innerGate = "and", topGate = top,
                      sharedEvents = TRUE)
    expect_equal(res$topTable$asDF$ptop, 0.05, tolerance = 1e-12)
  }

  d <- readSet("dwie_bariery")
  res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz", innerGate = "and", topGate = "or",
                    sharedEvents = TRUE)
  expect_equal(round(res$topTable$asDF$ptop, 7), 0.0000698)
  cuts <- res$cutsTable$asDF
  expect_setequal(cuts$cut, c("{I, C}", "{I, B1, B2}"))
  imp <- res$importanceTable$asDF
  expect_equal(nrow(imp), 4)
  expect_equal(imp$event[1], "I")

  # the same label with two different probabilities is rejected
  d$p[3] <- 0.006
  res <- jRISK::fta(data = d, labelVar = "zdarzenie", probVar = "p",
                    branchVar = "galaz", innerGate = "and", topGate = "or",
                    sharedEvents = TRUE)
  expect_equal(res$topTable$status, "error")
})

test_that("bananpol: raw alarm log and per-type lifetimes feed the system", {
  # raw shift log: old sensors, P(overheating | alarm) from individual rows
  a <- readSet("bananpol_alarmy")
  st <- a[a$czujnik == "stary", ]
  res <- jRISK::eventtables(data = st, varA = "przegrzanie", levelA = "tak",
                            varB = "alarm", levelB = "tak", showDetector = TRUE)
  det <- res$detectorTable$asDF
  tp <- sum(st$przegrzanie == "tak" & st$alarm == "tak")
  expect_equal(det$value[det$quantity == "PPV"],
               tp / sum(st$alarm == "tak"), tolerance = 1e-12)

  # lifetime per device type reproduces the reliabilities in bananpol_system
  b <- readSet("bananpol")
  b$awaria <- factor(b$awaria)
  res <- jRISK::lifetime(data = b, mode = "data", timeVar = "czas_pracy",
                         statusVar = "awaria", failureLevel = "1",
                         groupVar = "urzadzenie", t = 6)
  at <- res$dataAtTable$asDF
  wb <- at[at$model == "Weibulla", ]
  sys <- readSet("bananpol_system")
  for (u in c("agregat", "wentylator", "nawilzacz"))
    expect_equal(round(wb$rt[wb$group == u], 3),
                 unique(sys$niezawodnosc[sys$typ == u]))
  expect_equal(nrow(res$dataCounts$asDF), 3)
  expect_equal(sum(res$dataCounts$asDF$n), 150)

  # without a group the analysis keeps one pooled fit
  res <- jRISK::lifetime(data = b, mode = "data", timeVar = "czas_pracy",
                         statusVar = "awaria", failureLevel = "1", t = 6)
  expect_equal(nrow(res$dataCounts$asDF), 1)
  expect_equal(res$dataCounts$asDF$events, sum(b$awaria == "1"))
})

test_that("Bananpol keys of the exercise lists (2026-10 data)", {
  # week 3, task 6: old sensors from raw rows
  a <- readSet("bananpol_alarmy")
  st <- a[a$czujnik == "stary", ]
  res <- jRISK::eventtables(data = st, varA = "przegrzanie", levelA = "tak",
                            varB = "alarm", levelB = "tak", showDetector = TRUE)
  ct <- res$countsTable$asDF
  expect_equal(c(ct$b[1], ct$notb[1], ct$b[2], ct$notb[2]), c(27, 2, 174, 2797))
  det <- res$detectorTable$asDF
  expect_equal(round(det$value[det$quantity == "PPV"], 3), 0.134)
  for (sk in list(c("A", 0.045), c("C", 0.190))) {
    res <- jRISK::eventtables(data = st[st$sekcja == sk[1], ], varA = "przegrzanie",
                              levelA = "tak", varB = "alarm", levelB = "tak",
                              showDetector = TRUE)
    det <- res$detectorTable$asDF
    expect_equal(round(det$value[det$quantity == "PPV"], 3), as.numeric(sk[2]))
  }

  # week 4, task 6: missed overheating as a Bernoulli series
  ov <- a[a$przegrzanie == "tak", ]
  res <- jRISK::bernoulli(data = ov[ov$czujnik == "nowy", ], outcomeVar = "alarm",
                          successLevel = "nie", showUpperBound = TRUE)
  sm <- res$summaryTable$asDF
  expect_equal(c(sm$n, sm$successes), c(42, 0))
  expect_equal(round(sm$upperOne, 3), 0.069)
  res <- jRISK::bernoulli(data = ov[ov$czujnik == "stary", ], outcomeVar = "alarm",
                          successLevel = "nie", showUpperBound = TRUE)
  sm <- res$summaryTable$asDF
  expect_equal(c(sm$n, sm$successes), c(29, 2))
  expect_equal(round(sm$upperOne, 3), 0.202)

  # week 8, task 2: pooled fit
  b <- readSet("bananpol")
  b$awaria <- factor(b$awaria)
  res <- jRISK::lifetime(data = b, mode = "data", timeVar = "czas_pracy",
                         statusVar = "awaria", failureLevel = "1", t = 12)
  dc <- res$dataCounts$asDF
  expect_equal(c(dc$events, dc$censored), c(130, 20))
  pt <- res$paramTable$asDF
  expect_equal(round(pt$est[pt$model == "Weibulla"], 2), c(1.51, 23.77))
  ft <- res$fitTable$asDF
  expect_equal(round(ft$aic, 1), c(1078.1, 1058.3, 1054.6))

  # week 9, task 6 and week 10, task 4: the line and its fault tree
  sys <- readSet("bananpol_system")
  res <- jRISK::relsystem(data = sys, mode = "data", relVar = "niezawodnosc",
                          labelVar = "komponent", groupVar = "podsystem",
                          innerGate = "parallel", outerGate = "series",
                          showImportance = TRUE)
  expect_equal(round(res$resultTable$asDF$rel, 5), 0.95511)
  imp <- res$importanceTable$asDF
  expect_equal(imp$component[1], "STER")
  expect_equal(round(imp$birnbaum[1], 3), 0.975)
  res <- jRISK::fta(data = sys, labelVar = "komponent", probVar = "p_awarii",
                    branchVar = "podsystem", innerGate = "and", topGate = "or")
  expect_equal(round(res$topTable$asDF$ptop, 5), 0.04489)
})
