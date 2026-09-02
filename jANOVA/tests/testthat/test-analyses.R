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

test_that("nonparametric switches in the ANOVA analysis", {
    res <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group",
                          nonpar = TRUE, showPairs = TRUE)
    np <- res$npTests$asDF
    expect_equal(nrow(np), 1)
    expect_equal(np$p[1], kruskal.test(weight ~ group, PlantGrowth)$p.value)
    expect_equal(nrow(res$npMeans$asDF), 3)
    expect_equal(nrow(res$npPairs$asDF), 3)
    expect_false(res$art$visible)
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    r2 <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), nonpar = TRUE)
    expect_equal(r2$art$asDF$source, c("supp", "dose", "supp × dose"))
    expect_false(r2$npTests$visible)
})

test_that("Friedman and ART in the repeated-measures analysis", {
    set.seed(3)
    d <- data.frame(id = factor(rep(1:12, 3)), czas = factor(rep(c("t1", "t2", "t3"), each = 12)))
    d$y <- 10 + as.integer(d$czas) + rnorm(36)
    res <- jANOVA:::anovarm(data = d, dep = "y", subject = "id", within = "czas", nonpar = TRUE)
    expect_equal(nrow(res$npTests$asDF), 1)
    expect_equal(res$npMeans$asDF$level, c("t1", "t2", "t3"))
    expect_false(res$art$visible)
    data(oats, package = "MASS"); oats$Y <- as.numeric(oats$Y); oats$plot <- interaction(oats$B, oats$V)
    r2 <- jANOVA:::anovarm(data = oats, dep = "Y", subject = "plot", within = "N", between = "V", nonpar = TRUE)
    expect_equal(r2$art$asDF$source, c("N", "V", "N × V"))
})

test_that("ART main-effect comparisons produce letters", {
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    res <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), nonpar = TRUE, showPairs = TRUE)
    m <- res$artMeans$get(key = "dose")$asDF
    expect_equal(nrow(m), 3)
    expect_true(all(nchar(m$letters) >= 1))
    expect_equal(nrow(res$artPairs$get(key = "dose")$asDF), 3)
})
