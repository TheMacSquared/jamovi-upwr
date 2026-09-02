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
                             homog = TRUE, ks = TRUE)
    tt <- res$ttest$asDF
    expect_equal(tt$test, c("t Studenta", "t Welcha", "Manna-Whitneya U", "Kołmogorowa-Smirnowa D"))
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
