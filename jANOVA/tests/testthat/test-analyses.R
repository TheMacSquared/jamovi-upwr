# Integration tests run after jmc has installed the compiled jANOVA package.
skip_if_not_installed("jANOVA")

test_that("ANOVA analysis: table, letters, Welch", {
    res <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group",
                          welch = TRUE, showPairs = TRUE, ss = "1")
    a <- res$anova$asDF
    ref <- anova(lm(weight ~ group, PlantGrowth))
    expect_equal(a$ss[1:2], ref[["Sum Sq"]])
    expect_equal(res$means$get(key = "group")$asDF$letters, c("ab", "a", "b"))
    expect_equal(res$welch$asDF$p, oneway.test(weight ~ group, PlantGrowth)$p.value)
})

test_that("ANOVA analysis: two factors with interaction cells", {
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    res <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), phInter = TRUE)
    expect_equal(nrow(res$means$get(key = "supp:dose")$asDF), 6)
    expect_equal(res$anova$asDF$source[3], "supp × dose")
})

test_that("repeated-measures analysis on long data", {
    data(oats, package = "MASS")
    oats$Y <- as.numeric(oats$Y)
    oats$plot <- interaction(oats$B, oats$V)
    res <- jANOVA:::anovarm(data = oats, dep = "Y", subject = "plot", within = "N", between = "V",
                            spherCorr = "GG")
    a <- res$anova$asDF
    expect_equal(a$source, c("V", "N", "V × N"))
    expect_lt(a$df1[2], 3)   # GG-corrected df
    expect_equal(nrow(res$spher$asDF), 2)
    expect_equal(res$means$get(key = "N")$asDF$level, levels(oats$N))
})
