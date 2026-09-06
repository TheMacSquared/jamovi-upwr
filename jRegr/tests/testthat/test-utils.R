test_that("corPair matches cor.test and the jCI interval formulas", {
    set.seed(3); x <- rnorm(30); y <- x + rnorm(30)
    p <- corPair(x, y, "pearson")
    ct <- cor.test(x, y)
    expect_equal(p$r, unname(ct$estimate)); expect_equal(p$p, ct$p.value); expect_equal(p$n, 30)
    expect_equal(c(p$lower, p$upper), as.numeric(ct$conf.int), tolerance = 1e-10)
    s <- corPair(x, y, "spearman", hypothesis = "pos")
    expect_equal(s$p, suppressWarnings(cor.test(x, y, method = "spearman", alternative = "greater", exact = FALSE))$p.value)
    k <- corPair(x, y, "kendall")
    expect_equal(k$r, cor(x, y, method = "kendall")); expect_true(is.na(k$lower))
    y2 <- c(y, NA); x2 <- c(x, 1); expect_equal(corPair(x2, y2, "pearson")$n, 30)
    expect_true(is.na(corPair(1:2, 2:3)$r))
})

test_that("regression helpers: frame with reference levels, labels, betas, VIF, DW", {
    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    d <- regressionFrame(tg, "len", character(0), c("supp", "dose"), list(list(var = "dose", ref = "2")))
    expect_equal(levels(d$dose)[1], "2"); expect_equal(nrow(d), 60)
    fit <- lm(regressionFormula("len", character(0), c("supp", "dose")), d)
    lab <- coefLabels(d, character(0), c("supp", "dose"))
    expect_equal(unname(lab["suppVC"]), "supp: VC (vs OJ)"); expect_equal(unname(lab["dose0.5"]), "dose: 0.5 (vs 2)")
    f2 <- lm(mpg ~ wt + hp, mtcars)
    b <- stdBetas(f2); expect_equal(unname(b["wt"]), coef(f2)[["wt"]] * sd(mtcars$wt) / sd(mtcars$mpg)); expect_true(is.na(b["(Intercept)"]))
    v <- vifTable(f2); expect_equal(v$vif[1], 1 / (1 - summary(lm(wt ~ hp, mtcars))$r.squared))
    expect_null(vifTable(lm(mpg ~ wt, mtcars)))
    dw <- durbinWatson(f2); e <- residuals(f2); expect_equal(dw$dw, sum(diff(e)^2) / sum(e^2))
    cs <- cooksSummary(f2); expect_equal(cs$max, max(cooks.distance(f2)))
})

test_that("logistic helpers: classification, AUC equals Wilcoxon-based value, ROC endpoints", {
    y <- c(1, 1, 1, 0, 0, 0, 1, 0); p <- c(0.9, 0.8, 0.4, 0.3, 0.6, 0.2, 0.55, 0.45)
    cl <- classify(y, p, 0.5)
    expect_equal(c(cl$tp, cl$fn, cl$fp, cl$tn), c(3, 1, 1, 3)); expect_equal(cl$acc, 6 / 8); expect_equal(cl$sens, 0.75)
    w <- wilcox.test(p[y == 1], p[y == 0], exact = FALSE)$statistic
    expect_equal(aucValue(y, p), unname(w) / (4 * 4))
    roc <- rocCurve(y, p); expect_equal(roc$fpr[1], 0); expect_equal(roc$tpr[nrow(roc)], 1)
})

test_that("logistic LR uses estimable df and handles a constant predictor", {
    set.seed(4)
    d <- data.frame(y = rbinom(40, 1, .5), x = rnorm(40))
    d$x2 <- 2 * d$x
    full <- glm(y ~ x + x2, d, family = binomial())
    null <- glm(y ~ 1, d, family = binomial())
    ref <- anova(null, full, test = "LRT")
    result <- logisticLR(full, null)
    expect_equal(result$df, 1)
    expect_equal(result$p, ref$`Pr(>Chi)`[2])
    d$constant <- 1
    result <- logisticLR(glm(y ~ constant, d, family = binomial()), null)
    expect_equal(result$df, 0)
    expect_true(is.na(result$p))
})

test_that("separation detection distinguishes overlap, aliasing and quasi-separation", {
    d <- data.frame(y = rep(0:1, each = 6), x = rep(c(-1, 1), each = 6))
    fit <- glm(y ~ x, d, family = binomial())
    expect_true(fit$converged)
    expect_true(logisticSeparation(fit))
    # Two shared boundary observations; only some coefficients diverge.
    q <- data.frame(y = c(0, 0, 1, 1), x = c(-1, 0, 0, 1))
    expect_true(logisticSeparation(glm(y ~ x, q, family = binomial())))
    overlap <- data.frame(y = rep(0:1, 3), x = rep(-1:1, each = 2))
    overlap$x2 <- 2 * overlap$x
    expect_false(logisticSeparation(glm(y ~ x + x2, overlap, family = binomial())))
    d$x <- 1e9 + 1e6 * d$x
    expect_true(logisticSeparation(glm(y ~ x, d, family = binomial())))
    # Separation by a combination, with overlapping ranges of each predictor.
    multi <- data.frame(y = c(0, 0, 1, 1), x = c(-2, 1, -1, 2), z = c(1, -2, 2, -1))
    expect_true(logisticSeparation(suppressWarnings(glm(y ~ x + z, multi, family = binomial()))))
    expect_false(logisticSeparation(glm(am ~ wt, mtcars, family = binomial())))
})
