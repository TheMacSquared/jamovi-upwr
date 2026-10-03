test_that("type III table matches car::Anova and type I matches anova()", {
    d <- npk
    d$yield <- as.numeric(d$yield)
    r3 <- fitAnova(d, "yield", c("N", "P"), blocks = "block", ssType = "3")
    ref <- car::Anova(lm(yield ~ block + N * P, d, contrasts = list(block = "contr.sum", N = "contr.sum", P = "contr.sum")), type = 3)
    expect_equal(r3$anova$ss[r3$anova$term == "N"], ref["N", "Sum Sq"])
    expect_equal(r3$anova$p[r3$anova$term == "N:P"], ref["N:P", "Pr(>F)"])
    r1 <- fitAnova(d, "yield", c("N", "P"), blocks = "block", ssType = "1")
    ref1 <- anova(lm(yield ~ block + N * P, d))
    expect_equal(r1$anova$ss[1:5], ref1[["Sum Sq"]])
    expect_equal(r1$anova$source[6], "Ogółem")
})

test_that("effect sizes agree with textbook formulas", {
    r <- fitAnova(PlantGrowth, "weight", "group", ssType = "1")
    a <- r$anova
    ssTot <- sum((PlantGrowth$weight - mean(PlantGrowth$weight))^2)
    expect_equal(a$eta[1], a$ss[1] / ssTot)
    expect_equal(a$partEta[1], a$ss[1] / (a$ss[1] + a$ss[2]))
    expect_equal(a$omega[1], (a$ss[1] - 2 * r$mse) / (ssTot + r$mse))
})

test_that("effective post-hoc method follows the Welch switch", {
    expect_equal(phEffectiveMethod("tukey", TRUE), "gamesHowell")
    expect_equal(phEffectiveMethod("gamesHowell", FALSE), "tukey")
    expect_equal(phEffectiveMethod("dunnett", TRUE), "dunnettWelch")
    expect_equal(phEffectiveMethod("dunnett", FALSE), "dunnett")
    expect_equal(phEffectiveMethod("none", TRUE), "none")
})

test_that("Games-Howell matches rstatix::games_howell_test", {
    skip_if_not_installed("rstatix")
    # unequal n and variances
    set.seed(11)
    d <- data.frame(g = factor(rep(c("A", "B", "C", "D"), c(8, 12, 15, 10))))
    d$y <- rnorm(nrow(d), mean = c(A = 10, B = 11, C = 13, D = 10.5)[as.character(d$g)],
                 sd = c(A = 1, B = 3, C = 0.5, D = 2)[as.character(d$g)])
    gh <- compareWelch(d$y, d$g, "gamesHowell", 0.05)
    ref <- rstatix::games_howell_test(d, y ~ g)
    # rstatix reports group2 − group1; ours is g1 − g2 in combn order
    expect_equal(gh$pairs$g1, ref$group1)
    expect_equal(gh$pairs$diff, -ref$estimate, tolerance = 1e-8)
    expect_equal(gh$pairs$diff - gh$pairs$crit, -ref$conf.high, tolerance = 1e-6)
    expect_equal(gh$pairs$p, ref$p.adj, tolerance = 1e-3)
    # group SE and CI from each group's own variance
    sdC <- sd(d$y[d$g == "C"])
    expect_equal(gh$means$se[3], sdC / sqrt(15))
    expect_equal(gh$means$upper[3], mean(d$y[d$g == "C"]) + qt(0.975, 14) * sdC / sqrt(15))
    expect_true(all(nchar(gh$means$letters) >= 1))
})

test_that("Dunnett with separate variances matches multcomp on diag(s²/n)", {
    skip_if_not_installed("multcomp")
    set.seed(5)
    d <- data.frame(g = factor(rep(c("K", "T1", "T2", "T3"), c(10, 7, 12, 9))))
    d$y <- rnorm(nrow(d), mean = c(K = 5, T1 = 6.5, T2 = 5.2, T3 = 7)[as.character(d$g)],
                 sd = c(K = 0.6, T1 = 2, T2 = 1, T3 = 1.5)[as.character(d$g)])
    dw <- compareWelch(d$y, d$g, "dunnettWelch", 0.05, control = "K")
    expect_equal(dw$pairs$g2, rep("K", 3))
    expect_equal(dw$means$letters[1], "(kontrola)")
    w <- dw$means$var / dw$means$n
    est <- setNames(dw$means$mean, dw$means$level)
    K <- rbind(c(-1, 1, 0, 0), c(-1, 0, 1, 0), c(-1, 0, 0, 1))
    for (i in 1:3) {
        dfi <- floor(dw$pairs$df[i])
        ref <- summary(multcomp::glht(multcomp::parm(est, diag(w), df = dfi), linfct = K))
        # both sides integrate the multivariate t by quasi-Monte Carlo (abseps 1e-3)
        expect_lt(abs(dw$pairs$p[i] - unname(ref$test$pvalues[i])), 2e-3)
    }
    # Welch–Satterthwaite df per comparison
    wa <- w[2]; wb <- w[1]
    expect_equal(dw$pairs$df[1], unname((wa + wb)^2 / (wa^2 / 6 + wb^2 / 9)))
})

test_that("Tukey and Dunnett p-values on emmeans match references", {
    r <- fitAnova(PlantGrowth, "weight", "group", ssType = "3")
    tk <- compareTerm(r$fit, "group", "tukey", 0.05, mse = r$mse)
    ref <- TukeyHSD(aov(weight ~ group, PlantGrowth))$group
    expect_equal(tk$pairs$p, unname(ref[, "p adj"]), tolerance = 1e-6)
    expect_equal(tk$means$letters, c("ab", "a", "b"))
    dn <- compareTerm(r$fit, "group", "dunnett", 0.05, control = "ctrl", mse = r$mse)
    skip_if_not_installed("multcomp")
    refD <- summary(multcomp::glht(aov(weight ~ group, PlantGrowth),
        linfct = multcomp::mcp(group = "Dunnett")))$test$pvalues
    expect_equal(dn$pairs$p, as.numeric(refD), tolerance = 1e-3)
})

test_that("Welch and contrasts", {
    w <- welchTable(PlantGrowth, "weight", "group")
    ref <- oneway.test(weight ~ group, PlantGrowth)
    expect_equal(w$p, ref$p.value)
    r <- fitAnova(PlantGrowth, "weight", "group", ssType = "3")
    ct <- contrastTable(r$fit, "group", "simple")
    expect_equal(ct$estimate, c(mean(PlantGrowth$weight[PlantGrowth$group == "trt1"]) - mean(PlantGrowth$weight[PlantGrowth$group == "ctrl"]),
        mean(PlantGrowth$weight[PlantGrowth$group == "trt2"]) - mean(PlantGrowth$weight[PlantGrowth$group == "ctrl"])))
    hm <- contrastTable(r$fit, "group", "helmert")
    expect_equal(nrow(hm), 2)
})

test_that("compact letter display follows insert-absorb", {
    sp <- data.frame(g1 = c("a", "a", "a", "b", "b", "c"), g2 = c("b", "c", "d", "c", "d", "d"),
                     sig = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
    expect_equal(unname(cldLetters(c("a", "b", "c", "d"), sp)), c("a", "ab", "bc", "c"))
})

test_that("repeated measures via afex reproduces aov(Error()) strata", {
    skip_if_not_installed("afex")
    data(oats, package = "MASS")
    oats$Y <- as.numeric(oats$Y)
    oats$plot <- interaction(oats$B, oats$V)
    res <- fitRm(oats, "Y", "plot", "N", "V", ssType = "3")
    tb <- rmTable(res, "none")
    ref <- summary(aov(Y ~ N * V + Error(plot / N), oats))
    within <- ref[["Error: plot:N"]][[1]]
    expect_equal(tb$F[tb$term == "N"], within[["F value"]][1])
    expect_equal(tb$ss[tb$term == "N"], within[["Sum Sq"]][1], tolerance = 1e-6)
    sp <- sphericityTable(res$fit)
    expect_true(all(c("N", "V:N") %in% sp$term))
    m <- termMeans(res$fit, "N")$means
    expect_equal(m$level, levels(oats$N))
})

test_that("Kruskal-Wallis and Dunn", {
    y <- PlantGrowth$weight; g <- PlantGrowth$group
    kw <- kruskalTable(y, g)
    expect_equal(kw$p, kruskal.test(y, g)$p.value)
    expect_equal(kw$es, unname(kruskal.test(y, g)$statistic) / (length(y) - 1))
    dn <- dunnPairs(y, g, "holm")
    expect_equal(nrow(dn$pairs), 3)
    expect_equal(dn$levels$letters, c("ab", "a", "b"))
})

test_that("Friedman family matches stats::friedman.test", {
    set.seed(3)
    d <- data.frame(id = factor(rep(1:12, 3)), czas = factor(rep(c("t1", "t2", "t3"), each = 12)))
    d$y <- 10 + as.integer(d$czas) + rnorm(36)
    m <- rmMatrix(d, "y", "id", "czas")
    fr <- friedmanTable(m)
    expect_equal(fr$p, friedman.test(m)$p.value)
    expect_equal(fr$es, fr$stat / (12 * 2))
    ne <- friedmanPairs(m)
    expect_equal(ne$levels$level, c("t1", "t2", "t3"))
    expect_equal(nrow(ne$pairs), 3)
})

test_that("ART reproduces ARTool (fixed and repeated measures)", {
    skip_if_not_installed("ARTool")
    data(Higgins1990Table5, package = "ARTool"); h <- Higgins1990Table5
    mine <- artTable(h, "DryMatter", c("Moisture", "Fertilizer"))
    ref <- ARTool:::anova.art(ARTool::art(DryMatter ~ Moisture * Fertilizer, data = h))
    expect_equal(mine$F, ref[["F value"]], tolerance = 1e-8)
    expect_equal(mine$p, ref[["Pr(>F)"]], tolerance = 1e-8)
    skip_if_not_installed("afex")
    rm <- artTableRm(h, "DryMatter", "Tray", "Fertilizer", "Moisture")
    refRm <- ARTool:::anova.art(ARTool::art(DryMatter ~ Moisture * Fertilizer + Error(Tray), data = h))
    key <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(v) paste(sort(v), collapse = ":"), "")
    refF <- setNames(refRm[["F value"]], key(as.character(refRm$Term)))
    expect_equal(rm$F, unname(refF[key(rm$term)]), tolerance = 1e-8)
})

test_that("Welch-James reduces to Welch for one factor and matches welchADF for two", {
    w1 <- welchJamesTable(PlantGrowth, "weight", "group")
    ref <- oneway.test(weight ~ group, PlantGrowth)
    expect_equal(w1$F, unname(ref$statistic))
    expect_equal(w1$df2, unname(ref$parameter[2]))
    expect_equal(w1$p, ref$p.value)
    skip_if_not_installed("welchADF")
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    w2 <- welchJamesTable(tg, "len", c("supp", "dose"))
    ref2 <- welchADF::welchADF.test(len ~ supp * dose, data = tg)
    for (nm in setdiff(names(ref2), c("call", "model"))) {
        row <- w2[vapply(w2$term, function(t) setequal(strsplit(t, ":", fixed = TRUE)[[1]],
            strsplit(nm, ":", fixed = TRUE)[[1]]), TRUE), ]
        expect_equal(row$F, ref2[[nm]]$welch.T, tolerance = 1e-6, info = nm)
        expect_equal(row$df2, ref2[[nm]]$denominatorDF, tolerance = 1e-6, info = nm)
        expect_equal(row$p, ref2[[nm]]$pval, tolerance = 1e-6, info = nm)
    }
})
