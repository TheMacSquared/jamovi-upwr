x <- c(2, 4, 4, 4, 5, 5, 7, 9, 12, 30)

test_that("descStats matches base R and textbook definitions", {
    s <- descStats(x)
    expect_equal(s$mean, mean(x)); expect_equal(s$median, median(x)); expect_equal(s$sd, sd(x))
    expect_equal(c(s$q1, s$q3), unname(quantile(x, c(0.25, 0.75))))
    expect_equal(s$v, 100 * sd(x) / mean(x)); expect_equal(s$mode, 4); expect_equal(s$iqr, IQR(x))
    expect_equal(s$gmean, exp(mean(log(x)))); expect_equal(s$hmean, 1 / mean(1 / x))
    expect_equal(s$tmean, mean(x, trim = 0.1)); expect_equal(s$wmean, mean(c(4, 4, 4, 4, 5, 5, 7, 9, 12, 12)))
    expect_equal(s$meanDev, mean(abs(x - mean(x)))); expect_equal(s$mad, mad(x, constant = 1))
    expect_equal(s$qdev, IQR(x) / 2); expect_equal(s$vq, 100 * IQR(x) / 2 / median(x))
    expect_equal(c(s$typLo, s$typHi), mean(x) + c(-1, 1) * sd(x))
    expect_equal(s$skewPearson, 3 * (mean(x) - median(x)) / sd(x))
    q <- quantile(x, c(0.25, 0.5, 0.75)); expect_equal(s$skewQuart, unname((q[3] + q[1] - 2 * q[2]) / (q[3] - q[1])))
    expect_true(is.na(descStats(c(1, 2, 3))$mode))
    expect_true(is.na(descStats(c(-1, 2, 3))$gmean))
})

test_that("skewness/kurtosis equal the SPSS-type formulas used by jamovi", {
    n <- length(x); d <- x - mean(x)
    g1 <- sqrt(n * (n - 1)) / (n - 2) * sqrt(n) * sum(d^3) / sum(d^2)^1.5
    sk <- skewKurt(x)
    expect_equal(sk$skew, g1)
    expect_equal(sk$seSkew, sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))))
    v <- var(x)
    g2 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(d^4) / v^2 - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
    expect_equal(sk$kurt, g2)
})

test_that("Gini and Lorenz", {
    expect_equal(gini(rep(5, 10)), 0)
    expect_gt(gini(c(0, 0, 0, 0, 100)), 0.79)
    expect_true(is.na(gini(c(-1, 2))))
    lz <- lorenz(x); expect_equal(lz$p[1], 0); expect_equal(tail(lz$L, 1), 1)
    expect_true(all(diff(lz$L) >= 0))
})

test_that("Lilliefors and Anderson-Darling reproduce nortest", {
    skip_if_not_installed("nortest")
    y <- c(148, 154, 158, 160, 161, 162, 166, 170, 182, 195, 236)
    for (v in list(y, { set.seed(1); rnorm(200) }, { set.seed(2); rexp(60) })) {
        li <- lillieTest(v); ad <- adTest(v)
        rl <- nortest::lillie.test(v); ra <- nortest::ad.test(v)
        expect_equal(li$stat, unname(rl$statistic)); expect_equal(li$p, rl$p.value)
        expect_equal(ad$stat, unname(ra$statistic)); expect_equal(ad$p, ra$p.value)
    }
    expect_equal(lillieTest(y)$stat, unname(suppressWarnings(ks.test(y, "pnorm", mean(y), sd(y))$statistic)))
    expect_true(is.na(lillieTest(c(1, 2, 3))$stat)); expect_true(is.na(adTest(1:5)$stat))
})

test_that("percentile list parsing", {
    expect_equal(parsePercentiles("25, 50; 75"), c(25, 50, 75))
    expect_equal(parsePercentiles("0,10,100,abc,10"), 10)
})

test_that("szereg: class construction and grouped statistics", {
    set.seed(7); x <- round(rnorm(50, 100, 15))
    br <- classBreaks(x, "sturges"); expect_equal(br$k, ceiling(log2(50) + 1)); expect_equal(br$breaks[1], min(x))
    tab <- classTable(x, br$breaks); expect_equal(sum(tab$n), 50); expect_equal(tail(tab$cumPct, 1), 100)
    expect_equal(tab$n, as.integer(table(cut(x, br$breaks, right = FALSE, include.lowest = TRUE))))
    bw <- classBreaks(x, "width", h = 10, start = 60); expect_equal(bw$h, 10); expect_true(max(bw$breaks) >= max(x))
    bc <- classBreaks(x, "count", k = 4); expect_equal(bc$k, 4)
    g <- groupedStats(tab)
    expect_equal(g$mean, sum(tab$mid * tab$n) / 50); expect_lt(abs(g$mean - mean(x)), br$h)
    expect_lt(abs(g$median - median(x)), br$h)
    expect_true(g$modalClass == which.max(tab$n))
    # textbook example: classes 10-20,20-30,30-40 with n = 5, 12, 3 -> mode 10 + 10*7/(7+9)... in class 2
    tb <- data.frame(lower = c(10, 20, 30), upper = c(20, 30, 40), mid = c(15, 25, 35), n = c(5, 12, 3))
    tb$cumN <- cumsum(tb$n); tb$pct <- 100 * tb$n / 20; tb$cumPct <- 100 * tb$cumN / 20
    gs <- groupedStats(tb)
    expect_equal(gs$mode, 20 + 10 * (12 - 5) / ((12 - 5) + (12 - 3)))
    expect_equal(gs$median, 20 + 10 * (10 - 5) / 12)
    expect_null(classBreaks(x, "width", h = 0))
})

test_that("classes keep every observation and the [a; b) rule (audit 2026-11-05)", {
    # the maximum used to fall one ulp beyond the last boundary
    x <- c(0.1, seq(0.15, 0.95, length.out = 16), 1, 1)
    expect_equal(sum(classTable(x, classBreaks(x, "sturges")$breaks)$n), length(x))
    # a value on an inner boundary belongs to the class starting there
    y <- c(0.1, 0.3, 0.7)
    tab <- classTable(y, classBreaks(y, "width", h = 0.1)$breaks)
    expect_equal(tab$n[tab$lower == 0.3], 1)
    expect_equal(tab$n[tab$lower == 0.2], 0)
    # with a given width the maximum opens a class of its own
    expect_equal(tail(tab$lower, 1), 0.7)
    # a boundary at zero is 0, not 4e-16
    z <- c(-1.7, -0.5, 0)
    expect_true(0 %in% classBreaks(z, "width", h = 0.1)$breaks)
    # a constant variable
    expect_equal(sum(classTable(rep(5, 10), classBreaks(rep(5, 10), "sturges")$breaks)$n), 10)
    set.seed(3)
    for (i in 1:2000) {
        n <- sample(5:60, 1); lo <- round(runif(1, -50, 50), 1); x <- round(lo + runif(n, 0, runif(1, 0, 30)), 1)
        m <- sample(c("sturges", "count", "width"), 1)
        br <- classBreaks(x, m, sample(3:12, 1), if (m == "width") sample(c(0, 0.1, 0.5, 1), 1) else 0)
        if (is.null(br) || br$k > 200) next
        tab <- classTable(x, br$breaks)
        expect_equal(sum(tab$n), n)
        b <- br$breaks; k <- length(b) - 1
        expect_equal(tab$n, tabulate(findInterval(round(x, 9), round(b, 9), rightmost.closed = m != "width"), k))
    }
})
