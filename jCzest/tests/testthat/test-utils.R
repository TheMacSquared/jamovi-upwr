t22 <- as.table(matrix(c(30, 10, 15, 25), 2, 2, byrow = TRUE,
                       dimnames = list(g = c("A", "B"), w = c("tak", "nie"))))
t34 <- as.table(matrix(c(20, 15, 10, 5, 12, 18, 14, 6, 8, 9, 20, 13), 3, 4, byrow = TRUE,
                       dimnames = list(w = c("w1", "w2", "w3"), k = c("k1", "k2", "k3", "k4"))))

test_that("chi-kwadrat, G2 i liczebnosci oczekiwane zgodne ze stats", {
    ref <- suppressWarnings(chisq.test(t34, correct = FALSE))
    r <- chiSqTest(t34, correct = FALSE)
    expect_equal(r$stat, unname(ref$statistic))
    expect_equal(r$df, unname(ref$parameter))
    expect_equal(r$p, unname(ref$p.value))
    expect_equal(expectedCounts(t34), ref$expected, ignore_attr = TRUE)

    rc <- chiSqTest(t22, correct = TRUE)
    expect_equal(rc$stat, unname(chisq.test(t22, correct = TRUE)$statistic))

    lr <- likeRatTest(t34)
    expect_equal(lr$df, 6)
    expect_gt(lr$stat, 0)
})

test_that("V Cramera: 2x2 = sqrt(chi2/n), zakres [0,1], zero przy niezaleznosci", {
    x2 <- unname(chisq.test(t22, correct = FALSE)$statistic)
    expect_equal(cramersV(t22), sqrt(x2 / sum(t22)))
    expect_lt(cramersV(as.table(matrix(rep(25, 4), 2, 2))), 1e-9)
    v <- cramersV(t34)
    expect_true(v >= 0 && v <= 1)
})

test_that("bootstrapowy CI dla V jest powtarzalny i zawiera estymate", {
    a <- cramersVCI(t34, nBoot = 300, seed = 42)
    b <- cramersVCI(t34, nBoot = 300, seed = 42)
    expect_identical(a, b)
    v <- cramersV(t34)
    expect_true(a[1] <= v && v <= a[2])
    expect_true(a[1] >= 0 && a[2] <= 1)
})

test_that("reszty standaryzowane zgodne z chisq.test$stdres", {
    ref <- suppressWarnings(chisq.test(t34))$stdres
    expect_equal(stdResiduals(t34), ref, ignore_attr = TRUE)
    # prog 1,96 jest wpisany na sztywno w .b.R (odpowiada alpha = 0,05)
    expect_equal(round(qnorm(0.975), 2), 1.96)
})

test_that("kontrola zalozenia E>=5 rozroznia tabele 2x2 i wieksze", {
    a <- checkAssumption(t34)
    expect_true(a$ok)
    expect_equal(a$nBelow5, 0)

    small <- checkAssumption(as.table(matrix(c(2, 3, 1, 2), 2, 2)))
    expect_false(small$ok)          # w 2x2 kazde E < 5 dyskwalifikuje
    expect_true(small$is2x2)
    expect_true(small$fisherFeasible)

    # regula Cochrana: do 20% komorek z E < 5 jest akceptowalne
    big <- checkAssumption(t34)
    expect_lte(big$pctBelow5, 20)
})

test_that("miary 2x2 liczone wzgledem pierwszego poziomu", {
    m <- twoByTwo(t22, level = 0.95)
    expect_equal(m$or$est, (30 * 25) / (10 * 15))
    expect_equal(m$rr$est, (30 / 40) / (15 / 40))
    expect_equal(m$dp$est, 30 / 40 - 15 / 40)
    expect_true(m$or$lower < m$or$est && m$or$est < m$or$upper)
    # OR jest niezmienniczy przy transpozycji tabeli 2x2 (szanse "wiersz wzgledem
    # kolumny" i "kolumna wzgledem wiersza" sa te same) — RR juz nie
    mc <- twoByTwo(t22, compare = "cols")
    expect_equal(mc$or$est, m$or$est)
    expect_false(isTRUE(all.equal(mc$rr$est, m$rr$est)))
    expect_null(twoByTwo(t34))
})

test_that("tau-b zgodne z cor(method='kendall'), gamma w [-1,1]", {
    idx <- which(t34 > 0, arr.ind = TRUE)
    d <- do.call(rbind, lapply(seq_len(nrow(idx)), function(i)
        data.frame(x = rep(idx[i, 1], t34[idx[i, 1], idx[i, 2]]),
                   y = rep(idx[i, 2], t34[idx[i, 1], idx[i, 2]]))))
    expect_equal(ordinalMeasures(t34)$taub, cor(d$x, d$y, method = "kendall"))

    perfect <- as.table(matrix(c(10, 0, 0, 0, 10, 0, 0, 0, 10), 3, 3, byrow = TRUE))
    expect_equal(ordinalMeasures(perfect)$gamma, 1)
    neg <- as.table(matrix(c(2, 5, 20, 5, 10, 10, 20, 5, 2), 3, 3, byrow = TRUE))
    expect_lt(ordinalMeasures(neg)$gamma, 0)
})

test_that("Cochran-Armitage wykrywa trend i nie wykrywa jego braku", {
    trend <- as.table(matrix(c(5, 10, 15, 20, 20, 15, 10, 5), 2, 4, byrow = TRUE))
    ca <- cochranArmitage(trend)
    expect_lt(ca$p, 0.01)
    expect_lte(ca$z^2, chiSqTest(trend)$stat + 1e-9)   # 1 df zamiast k-1
    expect_equal(cochranArmitage(t(trend))$z, ca$z)    # dziala tez dla kx2

    flat <- as.table(matrix(rep(10, 8), 2, 4))
    expect_gt(cochranArmitage(flat)$p, 0.9)
    expect_null(cochranArmitage(as.table(matrix(c(5, 5, 5, 5), 2, 2))))  # k < 3
})

test_that("porownania par wierszy z korekta Holma", {
    pw <- pairwiseRows(t34)
    expect_equal(nrow(pw), 3)
    expect_true(all(pw$p >= 0 & pw$p <= 1))
    expect_null(pairwiseRows(t22))   # potrzeba >= 3 wierszy
})

test_that("buildTable radzi sobie z licznosciami, brakami i pustymi komorkami", {
    d <- data.frame(r = rep(c("A", "B"), each = 2), c = rep(c("tak", "nie"), 2),
                    n = c(30, 10, 15, 25))
    bt <- buildTable(d$r, d$c, d$n)
    expect_equal(sum(bt), 80)
    expect_equal(bt["A", "tak"], 30)

    sparse <- buildTable(c("A", "A", "B"), c("x", "y", "x"), c(3, 4, 5))
    expect_equal(sparse["B", "y"], 0)     # brakujaca kombinacja to 0, nie NA
    expect_false(any(is.na(sparse)))

    withNA <- buildTable(c("A", "B", NA, "A"), c("x", NA, "y", "y"))
    expect_equal(sum(withNA), 2)          # tylko kompletne obserwacje
})
