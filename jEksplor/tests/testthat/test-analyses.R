skip_if_not_installed("jEksplor")

d <- data.frame(x = c(2, 4, 4, 4, 5, 5, 7, 9, 12, 30, NA), y = c(10:20),
                g = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b")))

test_that("core table in rows layout: N, missing, mean, median, quartiles, SD, min, max, V", {
    res <- jEksplor:::ilosciowe(data = d, vars = c("x", "y"))
    t <- res$desc$asDF
    expect_equal(nrow(t), 2)
    expect_equal(t$n, c(10L, 11L)); expect_equal(t$missing, c(1L, 0L))
    expect_equal(t$mean[1], mean(d$x, na.rm = TRUE)); expect_equal(t$q1[1], unname(quantile(d$x, 0.25, na.rm = TRUE)))
    expect_equal(t$v[2], 100 * sd(d$y) / mean(d$y))
    expect_false(res$metody$visible)
    expect_false(res$norm$visible); expect_false(res$hist$visible)
})

test_that("columns layout and split by", {
    res <- jEksplor:::ilosciowe(data = d, vars = "x", layout = "columns", mean = TRUE, median = FALSE, quart = FALSE, v = FALSE,
                                sd = FALSE, min = FALSE, max = FALSE, missing = FALSE)
    t <- res$desc$asDF
    expect_equal(t$stat, c("N", "Średnia"))
    expect_equal(t[[2]], c(10, mean(d$x, na.rm = TRUE)))
    sp <- jEksplor:::ilosciowe(data = d, vars = "x", splitBy = "g", sw = TRUE, gini = TRUE, metody = TRUE)
    t <- sp$desc$asDF
    expect_equal(t$g_g, c("a", "b")); expect_equal(t$n, c(5L, 5L)); expect_equal(t$missing, c(0L, 1L))
    expect_equal(t$mean[1], mean(d$x[d$g == "a"]))
    expect_equal(nrow(sp$norm$asDF), 2); expect_equal(nrow(sp$conc$asDF), 2)
    expect_true(grepl("podział według: „g”", sp$metody$content))
    spc <- jEksplor:::ilosciowe(data = d, vars = "x", splitBy = "g", layout = "columns")$desc$asDF
    expect_equal(ncol(spc), 3); expect_equal(spc[[2]][1], 5)
})

test_that("advanced statistics, percentiles, cut points, extreme values, normality", {
    res <- jEksplor:::ilosciowe(data = d, vars = "x", mode = TRUE, gmean = TRUE, tmean = TRUE, mad = TRUE, vq = TRUE, typical = TRUE,
                                skew = TRUE, kurt = TRUE, skewQuart = TRUE, pc = TRUE, pcValues = "10,90", pcEqGr = TRUE, pcNEqGr = 3,
                                extreme = TRUE, extremeN = 2, sw = TRUE, lillie = TRUE, ad = TRUE, metody = TRUE)
    t <- res$desc$asDF; x <- d$x[!is.na(d$x)]
    expect_equal(t$mode, 4); expect_equal(t$tmean, mean(x, trim = 0.1)); expect_equal(t$mad, mad(x, constant = 1))
    expect_equal(t$pc10, unname(quantile(x, 0.1))); expect_equal(t$cut2, unname(quantile(x, 2 / 3)))
    expect_equal(t$skew, skewKurt(x)$skew)
    ex <- res$extreme$get(key = "x")$asDF
    expect_equal(nrow(ex), 4); expect_equal(ex$value[1], 2); expect_equal(ex$row[3], 10)
    n <- res$norm$asDF
    expect_equal(n$w, shapiro.test(x)$statistic[[1]]); expect_equal(n$d, lillieTest(x)$stat); expect_equal(n$a, adTest(x)$stat)
    h <- res$metody$content
    expect_true(grepl("MAD", h) && grepl("Lilliefors", h) && grepl("Percentyle P10, P90", h) && grepl("3 równoliczne", h))
    expect_lt(regexpr("<b>Statystyki</b>", h), regexpr("<b>Założenia</b>", h))
})

test_that("szereg rozdzielczy analysis", {
    set.seed(7); s <- data.frame(w = round(rnorm(60, 50, 8)))
    res <- jEksplor:::szereg(data = s, var = "w", metody = TRUE, ogive = TRUE)
    t <- res$classes$asDF
    expect_equal(nrow(t), ceiling(log2(60) + 1) + 1)
    expect_equal(t$n[nrow(t)], 60L); expect_equal(t$klasa[nrow(t)], "Razem")
    expect_true(grepl("^\\[", t$klasa[1]) && grepl("\\]$", t$klasa[nrow(t) - 1]))
    st <- res$stats$asDF
    expect_equal(st$exact[1], mean(s$w)); expect_lt(abs(st$grouped[1] - mean(s$w)), 3)
    expect_true(grepl("Sturgesa", res$metody$content))
    r2 <- jEksplor:::szereg(data = s, var = "w", method = "width", width = 5, startAuto = FALSE, start = 30)$classes$asDF
    expect_equal(r2$klasa[1], "[30; 35)")
})

test_that("zmienne jakosciowe: simple and grouped tables, summary, plots", {
    q <- data.frame(kolor = factor(c("czerwony", "zielony", "czerwony", NA, "niebieski", "czerwony")),
                    plec = factor(c("K", "M", "K", "M", "M", "K")))
    res <- jEksplor:::jakosciowe(data = q, vars = "kolor", splitBy = NULL, cum = TRUE, bar = TRUE, mosaic = TRUE, metody = TRUE)
    t <- res$freqs$get(key = "kolor")$asDF
    expect_equal(t$n, c(3L, 1L, 1L, 5L)); expect_equal(t$pct[1], 60); expect_equal(t$cumPct[2], 80)
    sm <- res$summary$asDF
    expect_equal(sm$n, 5L); expect_equal(sm$missing, 1L); expect_equal(sm$k, 3L); expect_equal(sm$mode, "czerwony"); expect_equal(sm$modePct, 60)
    expect_true(grepl("Skumulowane", res$metody$content))
    g <- jEksplor:::jakosciowe(data = q, vars = "kolor", splitBy = "plec", pcRow = TRUE, pcTotal = TRUE)$freqs$get(key = "kolor")$asDF
    expect_equal(g$n_1, c(3L, 0L, 0L, 3L)); expect_equal(g$n_2, c(0L, 1L, 1L, 2L)); expect_equal(g$n_tot[4], 5L)
    expect_equal(g$pcCol_1[1], 100); expect_equal(g$pcRow_1[1], 100); expect_equal(g$pcTotal_2[2], 20)
})
