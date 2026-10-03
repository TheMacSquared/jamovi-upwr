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

test_that("opis metod: anova i anovarm", {
    res <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group")
    expect_false(res$metody$visible)
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    res <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), welch = TRUE, nonpar = TRUE,
                          showPairs = TRUE, phES = TRUE, partEta = TRUE, homog = TRUE, norm = TRUE,
                          contrastType = "polynomial", plotInteraction = TRUE, metody = TRUE)
    h <- res$metody$content
    expect_true(res$metody$visible)
    expect_true(grepl("„supp”, „dose”", h) && grepl("typu III", h) && grepl("zrównoważony", h))
    expect_true(grepl("Welcha-Jamesa", h) && grepl("Aligned Rank Transform", h))
    # Welch with two factors: no parametric pairs; ART still uses Tukey
    expect_true(grepl("tylko dla jednego czynnika", h) && grepl("ART na wyrównanych rangach: test Tukeya", h))
    r2 <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), showPairs = TRUE,
                         phES = TRUE, metody = TRUE)
    h2 <- r2$metody$content
    expect_true(grepl("test Tukeya", h2) && grepl("HSD", h2) && grepl("√MS błędu", h2))
    expect_true(grepl("wielomianowe", h) && grepl("Bartletta", h) && grepl("Wykres interakcji", h))
    expect_lt(regexpr("<b>Dane</b>", h), regexpr("<b>Model</b>", h))
    expect_lt(regexpr("<b>Model</b>", h), regexpr("<b>Testy</b>", h))
    expect_lt(regexpr("<b>Założenia</b>", h), regexpr("<b>Wykres</b>", h))
    expect_false(grepl(sprintf("%.3f", res$anova$asDF$F[1]), h))
    # noty pod tabelami sa jednozdaniowe
    txt <- paste(capture.output(print(r2$means$get(key = "supp"))), collapse = "\n")
    expect_true(grepl("Ta sama litera", txt))
    expect_false(grepl("emmeans", txt))

    data(oats, package = "MASS")
    oats$Y <- as.numeric(oats$Y); oats$plot <- interaction(oats$B, oats$V)
    rm <- jANOVA:::anovarm(data = oats, dep = "Y", subject = "plot", within = "N", between = "V",
                           spherCorr = "GG", pes = TRUE, homog = TRUE, metody = TRUE)$metody$content
    expect_true(grepl("aov_ez", rm) && grepl("Greenhouse", rm) && grepl("Mauchly", rm) && grepl("η²p", rm))
})

test_that("Welch with one factor switches post-hoc to separate variances", {
    res <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group",
                          welch = TRUE, postHoc = "tukey", showPairs = TRUE, metody = TRUE)
    p <- res$pairs$get(key = "group")$asDF
    expect_false(all(p$df == round(p$df)))          # Welch df per pair
    ref <- compareWelch(PlantGrowth$weight, PlantGrowth$group, "gamesHowell", 0.05)
    expect_equal(p$p, ref$pairs$p)
    expect_true(grepl("Gamesa-Howella", res$metody$content))
    m <- res$means$get(key = "group")$asDF
    expect_equal(m$se[1], sd(PlantGrowth$weight[PlantGrowth$group == "ctrl"]) / sqrt(10))
    dw <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group",
                         welch = TRUE, postHoc = "dunnett", metody = TRUE)
    expect_true(grepl("osobnymi wariancjami grup", dw$metody$content))
    expect_equal(dw$means$get(key = "group")$asDF$letters[1], "(kontrola)")
})

test_that("Welch with several factors: means without letters and a note", {
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    res <- jANOVA:::anova(data = tg, dep = "len", factors = c("supp", "dose"), welch = TRUE)
    mt <- res$means$get(key = "dose")
    expect_false(mt$getColumn("letters")$visible)
    txt <- paste(capture.output(print(mt)), collapse = "\n")
    expect_true(grepl("tylko dla jednego czynnika", txt))
})

test_that("nonparametric with one factor hides the parametric comparisons", {
    res <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group", nonpar = TRUE,
                          showPairs = TRUE, metody = TRUE)
    expect_false(res$means$visible)
    expect_false(res$pairs$visible)
    expect_false(res$plotMeans$visible)
    expect_true(res$npMeans$visible)
    expect_false(grepl("Średnie brzegowe z literami", res$metody$content))
    r2 <- jANOVA:::anova(data = PlantGrowth, dep = "weight", factors = "group")
    expect_true(r2$means$visible && r2$plotMeans$visible)
})
