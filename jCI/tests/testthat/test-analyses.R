# Integration tests run after jmc has installed the compiled jCI package.
skip_if_not_installed("jCI")

d <- data.frame(
    x = c(2, 4, 5, NA, 8, 9),
    before = c(10, 12, 9, 15, NA, 11),
    after = c(8, 11, 7, 12, 5, 10))

test_that("one-mean and paired-mean analyses match t.test", {
    one <- jCI:::cionemean(data = d, dep = "x", group = NULL, plot = FALSE)$table$asDF
    refOne <- t.test(d$x)
    expect_equal(one$estimate, mean(d$x, na.rm = TRUE))
    expect_equal(c(one$lower, one$upper), as.numeric(refOne$conf.int))
    paired <- jCI:::cipairedmeans(data = d, var1 = "before", var2 = "after", plot = FALSE)$table$asDF
    keep <- complete.cases(d[c("before", "after")])
    refPaired <- t.test(d$before[keep], d$after[keep], paired = TRUE)
    expect_equal(paired$estimate, unname(refPaired$estimate))
    expect_equal(c(paired$lower, paired$upper), as.numeric(refPaired$conf.int))
})

test_that("one-mean analysis: groups, median forces bootstrap, bootstrap is reproducible", {
    g <- data.frame(y = c(2, 3, 5, 7, 8, 10, 20, 21, 25), grp = factor(rep(c("A", "B", "C"), each = 3)))
    res <- jCI:::cionemean(data = g, dep = "y", group = "grp", plot = FALSE)$table$asDF
    expect_equal(nrow(res), 3); expect_equal(res$grp, c("A", "B", "C"))
    expect_equal(res$estimate[1], mean(g$y[g$grp == "A"]))
    med <- jCI:::cionemean(data = g, dep = "y", group = NULL, stat = "median", seed = 3, nBoot = 300, metody = TRUE)
    expect_equal(med$table$asDF$estimate, median(g$y))
    expect_true(grepl("bootstrap percentylowy", paste(capture.output(print(med$table)), collapse = "\n")))
    expect_true(grepl("przedział klasyczny nie istnieje", med$metody$content))
    b1 <- jCI:::cionemean(data = g, dep = "y", group = NULL, ciMethod = "bca", seed = 9, nBoot = 300, plot = FALSE)$table$asDF
    b2 <- jCI:::cionemean(data = g, dep = "y", group = NULL, ciMethod = "bca", seed = 9, nBoot = 300, plot = FALSE)$table$asDF
    expect_equal(b1$lower, b2$lower); expect_true(b1$lower < mean(g$y) && mean(g$y) < b1$upper)
})

test_that("two-mean analysis uses the selected groups; Welch and stratified bootstrap", {
    g <- data.frame(y = c(2, 3, 5, 7, 8, 10, 20, 21), grp = factor(c("A", "A", "A", "B", "B", "B", "C", "C")))
    res <- jCI:::citwomeans(data = g, dep = "y", group = "grp", level1 = "B", level2 = "A", plot = FALSE)$table$asDF
    ref <- t.test(g$y[g$grp == "B"], g$y[g$grp == "A"])
    expect_equal(res$group1, "B"); expect_equal(res$group2, "A")
    expect_equal(res$estimate, mean(g$y[g$grp == "B"]) - mean(g$y[g$grp == "A"]))
    expect_equal(c(res$lower, res$upper), as.numeric(ref$conf.int))
    bt <- jCI:::citwomeans(data = g, dep = "y", group = "grp", level1 = "B", level2 = "A", ciMethod = "perc", seed = 2, nBoot = 400,
                          bootPlot = TRUE, metody = TRUE)
    expect_equal(bt$table$asDF$estimate, res$estimate)
    expect_true(bt$bootPlot$visible)
    expect_true(grepl("warstwowy", bt$metody$content) && grepl("B = 400", bt$metody$content))
})

test_that("proportion analyses: Wilson default, level selection, Newcombe and bootstrap", {
    p <- data.frame(answer = factor(c("yes", "no", "yes", NA, "yes", "no", "yes", "no")),
                    grp = factor(c("a", "a", "a", "a", "b", "b", "b", "b")))
    prop <- jCI:::ciproportion(data = p, dep = "answer", level = "yes", plot = FALSE)$table$asDF
    expect_equal(prop$count, 4); expect_equal(prop$total, 7); expect_equal(prop$estimate, 4 / 7)
    expect_equal(c(prop$lower, prop$upper), unname(unlist(ciProportion(4, 7, 0.95, "wilson"))))
    dp <- jCI:::cidiffprop(data = p, dep = "answer", group = "grp", level = "yes", plot = FALSE)$table$asDF
    expect_equal(dp$p1, 2 / 3); expect_equal(dp$p2, 2 / 4)
    expect_equal(c(dp$lower, dp$upper), unname(unlist(ciDiffProportion(2, 3, 2, 4, 0.95, "newcombe")[c("lower", "upper")])))
    bp <- jCI:::ciproportion(data = p, dep = "answer", level = "yes", ciMethod = "perc", seed = 4, nBoot = 300, plot = FALSE)$table$asDF
    expect_true(bp$lower <= 4 / 7 && 4 / 7 <= bp$upper)
})

test_that("correlation and regression analyses", {
    set.seed(5); r <- data.frame(x = 1:25, y = 1:25 + rnorm(25, sd = 4))
    corr <- jCI:::cicorrelation(data = r, var1 = "x", var2 = "y", plot = FALSE)$table$asDF
    expect_equal(corr$estimate, cor(r$x, r$y))
    expect_equal(c(corr$lower, corr$upper), as.numeric(cor.test(r$x, r$y)$conf.int), tolerance = 1e-10)
    sp <- jCI:::cicorrelation(data = r, var1 = "x", var2 = "y", method = "spearman", metody = TRUE)
    expect_equal(sp$table$asDF$estimate, cor(r$x, r$y, method = "spearman"))
    expect_true(grepl("Bonetta-Wrighta", sp$metody$content))
    reg <- jCI:::ciregression(data = r, dep = "y", pred = "x", plot = FALSE)$table$asDF
    fit <- lm(y ~ x, r)
    expect_equal(reg$estimate, unname(coef(fit)))
    expect_equal(reg$lower, unname(confint(fit)[, 1]))
    regb <- jCI:::ciregression(data = r, dep = "y", pred = "x", ciMethod = "bca", seed = 1, nBoot = 300, bootPlot = TRUE, metody = TRUE)
    expect_equal(regb$table$asDF$estimate, unname(coef(fit)))
    expect_true(grepl("tej samej replikacji", regb$metody$content))
})

test_that("didactic bootstrap analysis lists samples and converges", {
    b <- jCI:::cibootstrap(data = d, dep = "x", nBoot = 5, seed = 42, showConvergence = TRUE, metody = TRUE)
    expect_equal(nrow(b$samplesTable$asDF), 5)
    expect_equal(nrow(b$convTable$asDF), 7)
    expect_equal(b$origTable$asDF$value[1], 5)
    expect_true(grepl("ręczny", b$metody$content) && grepl("Zbieżność", b$metody$content))
    expect_false(jCI:::cibootstrap(data = d, dep = "x")$metody$visible)
})

test_that("effect size with noncentral-t interval and the Student interval (moved from jTestyT)", {
    g <- data.frame(y = c(2, 3, 5, 7, 8, 10, 20, 21), grp = factor(c("A", "A", "A", "B", "B", "B", "C", "C")))
    res <- jCI:::citwomeans(data = g, dep = "y", group = "grp", level1 = "A", level2 = "B", ciMethod = "student", effSize = TRUE, plot = FALSE, metody = TRUE)
    t <- res$table$asDF; ref <- t.test(g$y[g$grp == "A"], g$y[g$grp == "B"], var.equal = TRUE)
    expect_equal(c(t$lower, t$upper), as.numeric(ref$conf.int))
    x1 <- g$y[g$grp == "A"]; x2 <- g$y[g$grp == "B"]; sp <- sqrt((var(x1) + var(x2)) / 2)
    expect_equal(t$d, (mean(x1) - mean(x2)) / sp)
    expect_true(t$dLower < t$d && t$d < t$dUpper)
    expect_true(grepl("wspólną wariancją", res$metody$content) && grepl("niecentralnego t", res$metody$content))
    w <- data.frame(a = c(10, 12, 9, 15, 11, 14), b = c(8, 11, 7, 12, 10, 12))
    pr <- jCI:::cipairedmeans(data = w, var1 = "a", var2 = "b", effSize = TRUE, plot = FALSE)$table$asDF
    dd <- w$a - w$b; expect_equal(pr$d, mean(dd) / sd(dd)); expect_true(pr$dLower < pr$d && pr$d < pr$dUpper)
    expect_false(jCI:::cipairedmeans(data = w, var1 = "a", var2 = "b", plot = FALSE)$table$getColumn("dLower")$visible)
})
