skip_if_not_installed("jTestyT")

test_that("one-sample analysis fills tables and plots", {
    res <- jTestyT:::ttestone(data = sleep, vars = "extra", testValue = 0, nonpar = TRUE, desc = TRUE)
    tt <- res$ttest$asDF
    expect_equal(nrow(tt), 2)
    expect_equal(tt$p[1], t.test(sleep$extra)$p.value)
    only <- jTestyT:::ttestone(data = sleep, vars = "extra", student = FALSE, nonpar = TRUE)$ttest$asDF
    expect_equal(nrow(only), 1)
    expect_equal(only$test, "Wilcoxona (rangowanych znaków)")
})

test_that("two-group analysis: Welch, Mann-Whitney, Levene, plots", {
    res <- jTestyT:::ttesttwo(data = sleep, vars = "extra", group = "group", welch = TRUE, nonpar = TRUE,
                             homog = TRUE)
    tt <- res$ttest$asDF
    expect_equal(tt$test, c("t Studenta", "t Welcha", "Manna-Whitneya U"))
    expect_equal(tt$p[2], t.test(extra ~ group, sleep)$p.value)
    expect_equal(tt$p[1], t.test(extra ~ group, sleep, var.equal = TRUE)$p.value)
    expect_equal(nrow(res$homog$asDF), 1)
})

test_that("paired analysis on wide data", {
    w <- data.frame(przed = sleep$extra[sleep$group == 1], po = sleep$extra[sleep$group == 2])
    res <- jTestyT:::ttestpaired(data = w, pairs = list(list(i1 = "przed", i2 = "po")), nonpar = TRUE)
    tt <- res$ttest$asDF
    expect_equal(tt$p[1], t.test(w$przed, w$po, paired = TRUE)$p.value)
    expect_equal(tt$est[1], mean(w$przed - w$po))
})

test_that("opis metod: ukryty domyslnie, po wlaczeniu opisuje testy i wykres bez wynikow", {
    res <- jTestyT:::ttesttwo(data = sleep, vars = "extra", group = "group")
    expect_false(res$metody$visible)
    res <- jTestyT:::ttesttwo(data = sleep, vars = "extra", group = "group", welch = TRUE, nonpar = TRUE,
                             homog = TRUE, qq = TRUE, metody = TRUE)
    h <- res$metody$content
    expect_true(res$metody$visible)
    expect_true(grepl("„1” i „2”", h))
    expect_true(grepl("t Welcha", h) && grepl("Manna-Whitneya", h) && grepl("Levene", h))
    expect_true(grepl("Gardner-Altman", h) && grepl("t Welcha\\)", h))
    expect_lt(regexpr("<b>Dane</b>", h), regexpr("<b>Testy</b>", h))
    expect_lt(regexpr("<b>Założenia</b>", h), regexpr("<b>Wykres</b>", h))
    expect_false(grepl(sprintf("%.3f", res$ttest$asDF$stat[1]), h))
    # nota pod tabela zostala jednozdaniowa
    expect_true(grepl("Różnica = 1 − 2\\.", paste(capture.output(print(res$ttest)), collapse = "\n")))

    one <- jTestyT:::ttestone(data = sleep, vars = "extra", testValue = 1, metody = TRUE)$metody$content
    expect_true(grepl("μ₀ = 1", one) && grepl("jednej próby", one))
    w <- data.frame(przed = sleep$extra[sleep$group == 1], po = sleep$extra[sleep$group == 2])
    pr <- jTestyT:::ttestpaired(data = w, pairs = list(list(i1 = "przed", i2 = "po")), nonpar = TRUE, metody = TRUE)$metody$content
    expect_true(grepl("„przed” − „po”", pr) && grepl("pseudomediana", pr) && grepl("SD różnic", pr))
})
