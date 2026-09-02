# Integration tests run after jmc has installed the compiled jDosw package.
skip_if_not_installed("jDosw")

test_that("CRD analysis fills the ANOVA and means tables", {
    res <- jDosw:::crd(data = PlantGrowth, dep = "weight", factorA = "group",
                       factorB = NULL, controlA = NULL, controlB = NULL,
                       showPairs = TRUE)
    a <- res$anova$asDF
    ref <- anova(lm(weight ~ group, PlantGrowth))
    expect_equal(a$ss[1:2], ref[["Sum Sq"]])
    m <- res$means$get(key = "A")$asDF
    expect_equal(m$letters, c("ab", "a", "b"))
    expect_equal(nrow(res$pairs$get(key = "A")$asDF), 3)
})

test_that("split-plot analysis reports both error strata", {
    data(oats, package = "MASS")
    oats$Y <- as.numeric(oats$Y)
    res <- jDosw:::splitplot(data = oats, dep = "Y", factorA = "V", factorB = "N",
                             block = "B", controlA = NULL, controlB = NULL, phAB = TRUE)
    a <- res$anova$asDF
    expect_equal(a$source[3], "Błąd (a)")
    # empty cells come back from asDF as NA, so only check the labelled rows
    expect_equal(a$err[c(1, 2, 4, 5)], c("a", "a", "b", "b"))
    expect_equal(nrow(res$means$get(key = "AB")$asDF), 12)
})

test_that("plan analysis produces one row per plot", {
    res <- jDosw:::plan(design = "latin", nTreat = 3, labelsA = "x,y,z")
    expect_equal(nrow(res$plan$asDF), 9)
})
