test_that("split-plot strata match aov(Error()) on the Yates oats data", {
    data(oats, package = "MASS")
    oats$Y <- as.numeric(oats$Y)
    r <- fitDesign("splitplot", oats, "Y", "V", "N", block = "B")
    ref <- summary(aov(Y ~ N * V + Error(B / V), oats))
    a <- r$anova
    expect_equal(a$source, c("Bloki", "V", "Błąd (a)", "N", "V × N", "Błąd (b)", "Ogółem"))
    expect_equal(a$ss[a$source == "Błąd (a)"], ref[["Error: B:V"]][[1]]["Residuals", "Sum Sq"])
    expect_equal(a$ss[a$source == "Błąd (b)"], ref[["Error: Within"]][[1]]["Residuals", "Sum Sq"])
    # summary(aov) pads row names with spaces, so index by position
    expect_equal(a$F[a$source == "V"], ref[["Error: B:V"]][[1]][["F value"]][1])
    expect_equal(a$F[a$source == "N"], ref[["Error: Within"]][[1]][["F value"]][1])
    expect_true(r$balanced)
})

test_that("RCBD two-factor table equals sequential anova of lm", {
    d <- npk
    d$yield <- as.numeric(d$yield)
    r <- fitDesign("rcbd", d, "yield", "N", "P", block = "block")
    ref <- anova(lm(yield ~ block + N * P, npk))
    expect_equal(r$anova$ss[1:5], ref[["Sum Sq"]])
    expect_equal(r$anova$p[1:4], ref[["Pr(>F)"]][1:4])
    expect_equal(r$anova$ss[6], sum(ref[["Sum Sq"]]))
})

test_that("Tukey, LSD and Dunnett p-values match TukeyHSD / t / multcomp", {
    r <- fitDesign("crd", PlantGrowth, "weight", "group")
    tk <- compareTerm(r$terms$A, "tukey", 0.05)
    ref <- TukeyHSD(aov(weight ~ group, PlantGrowth))$group
    expect_equal(tk$pairs$p, unname(ref[, "p adj"]), tolerance = 1e-6)
    expect_equal(tk$pairs$diff, -unname(ref[, "diff"]))
    expect_equal(tk$means$letters, c("ab", "a", "b"))

    lsd <- compareTerm(r$terms$A, "lsd", 0.05)
    tt <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adjust.method = "none")$p.value
    expect_equal(lsd$pairs$p[1], tt["trt1", "ctrl"])
    expect_equal(lsd$pairs$p[3], tt["trt2", "trt1"])

    dn <- compareTerm(r$terms$A, "dunnett", 0.05, control = "ctrl")
    skip_if_not_installed("multcomp")
    refD <- summary(multcomp::glht(aov(weight ~ group, PlantGrowth),
        linfct = multcomp::mcp(group = "Dunnett")))$test$pvalues
    expect_equal(dn$pairs$p, as.numeric(refD), tolerance = 1e-3)
    expect_equal(dn$means$letters[[1]], "(kontrola)")
})

test_that("compact letter display follows insert-absorb", {
    sp <- data.frame(g1 = c("a", "a", "a", "b", "b", "c"),
                     g2 = c("b", "c", "d", "c", "d", "d"),
                     sig = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
    expect_equal(unname(cldLetters(c("a", "b", "c", "d"), sp)), c("a", "ab", "bc", "c"))
    none <- sp; none$sig <- FALSE
    expect_equal(unname(cldLetters(c("a", "b", "c", "d"), none)), rep("a", 4))
    all <- sp; all$sig <- TRUE
    expect_equal(unname(cldLetters(c("a", "b", "c", "d"), all)), c("a", "b", "c", "d"))
})

test_that("field plans are valid designs", {
    p <- planDesign("rcbd", 4, 3, 1, "", "", 7)
    expect_equal(nrow(p), 12)
    expect_true(all(table(p$block, p$A) == 1))

    l <- planDesign("latin", 4, 1, 1, "a,b,c,d", "", 3)
    expect_true(all(table(l$row, l$A) == 1))
    expect_true(all(table(l$col, l$A) == 1))

    s <- planDesign("splitplot", 3, 2, 2, "", "x,y", 5)
    expect_equal(nrow(s), 12)
    expect_true(all(table(s$block, s$A, s$B) == 1))
    expect_equal(sort(unique(s$B)), c("x", "y"))
})
