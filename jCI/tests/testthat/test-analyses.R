# Integration tests run after jmc has installed the compiled jCI package.
skip_if_not_installed("jCI")

test_that("one-mean and paired-mean analyses match t.test", {
    d <- data.frame(
        x = c(2, 4, 5, NA, 8, 9),
        before = c(10, 12, 9, 15, NA, 11),
        after = c(8, 11, 7, 12, 5, 10))

    one <- jCI:::cionemean(data = d, dep = "x", group = NULL,
                          showPlot = FALSE)$table$asDF
    refOne <- t.test(d$x)
    expect_equal(one$estimate, mean(d$x, na.rm = TRUE))
    expect_equal(c(one$lower, one$upper), as.numeric(refOne$conf.int))

    paired <- jCI:::cipairedmeans(data = d, var1 = "before", var2 = "after",
                                 showPlot = FALSE)$table$asDF
    keep <- complete.cases(d[c("before", "after")])
    refPaired <- t.test(d$before[keep], d$after[keep], paired = TRUE)
    expect_equal(paired$estimate, unname(refPaired$estimate))
    expect_equal(c(paired$lower, paired$upper), as.numeric(refPaired$conf.int))
})

test_that("two-mean analysis uses the selected groups and Welch interval", {
    d <- data.frame(
        y = c(2, 3, 5, 7, 8, 10, 20, 21),
        g = factor(c("A", "A", "A", "B", "B", "B", "C", "C")))
    res <- jCI:::citwomeans(data = d, dep = "y", group = "g",
                           level1 = "B", level2 = "A",
                           showPlot = FALSE)$table$asDF
    ref <- t.test(d$y[d$g == "B"], d$y[d$g == "A"])
    expect_equal(res$group1, "B")
    expect_equal(res$group2, "A")
    expect_equal(res$estimate, mean(d$y[d$g == "B"]) - mean(d$y[d$g == "A"]))
    expect_equal(c(res$lower, res$upper), as.numeric(ref$conf.int))
})

test_that("proportion and correlation analyses ignore missing observations", {
    d <- data.frame(
        answer = factor(c("yes", "no", "yes", NA, "yes", "no")),
        x = c(1, 2, 3, 4, NA, 6),
        y = c(2, 1, 5, NA, 8, 9))

    prop <- jCI:::ciproportion(data = d, dep = "answer", level = "yes",
                              method = "wilson", showPlot = FALSE)$table$asDF
    expect_equal(prop$count, 3)
    expect_equal(prop$total, 5)
    expect_equal(prop$estimate, 3 / 5)

    corr <- jCI:::cicorrelation(data = d, var1 = "x", var2 = "y",
                               showPlot = FALSE)$table$asDF
    keep <- complete.cases(d[c("x", "y")])
    expect_equal(corr$estimate, cor(d$x[keep], d$y[keep]))
})
