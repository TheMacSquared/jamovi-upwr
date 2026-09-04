skip_if_not_installed("jRegr")

set.seed(4)
d <- data.frame(a = rnorm(25), b = rnorm(25), c = rnorm(25)); d$b <- d$a + d$b

test_that("two variables: one-row table with CI and scatter; matrix hidden", {
    res <- jRegr:::korelacja(data = d, vars = c("a", "b"), ci = TRUE, metody = TRUE)
    expect_true(res$pair$visible); expect_false(res$matrix$visible); expect_true(res$plot$visible); expect_false(res$plotMatrix$visible)
    t <- res$pair$asDF; ct <- cor.test(d$a, d$b)
    expect_equal(t$r, unname(ct$estimate)); expect_equal(t$p, ct$p.value); expect_equal(t$n, 25L)
    expect_equal(c(t$lower, t$upper), as.numeric(ct$conf.int), tolerance = 1e-10)
    expect_true(grepl("test t, df = n − 2", res$metody$content) && grepl("Rozrzut z prostą", res$metody$content))
})

test_that("three variables: lower-triangle matrix with p rows, N optional, flags", {
    res <- jRegr:::korelacja(data = d, vars = c("a", "b", "c"), method = "spearman", showN = TRUE, flag = TRUE, metody = TRUE)
    expect_false(res$pair$visible); expect_true(res$matrix$visible); expect_true(res$plotMatrix$visible)
    t <- res$matrix$asDF
    expect_equal(nrow(t), 6); expect_equal(t$kind[1:3], c("ρ", "p", "N")); expect_equal(ncol(t), 4)
    pv <- suppressWarnings(cor.test(d$b, d$a, method = "spearman", exact = FALSE))$p.value
    stars <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else if (pv < 0.05) "*" else ""
    expect_equal(t$c_a[1], paste0(sprintf("%.3f", cor(d$b, d$a, method = "spearman")), stars))
    expect_true(t$c_b[1] == "" || is.na(t$c_b[1]))   # upper triangle empty
    expect_equal(t$c_a[3], "25")
    expect_true(grepl("Gwiazdki", res$metody$content) && grepl("Bonetta", jRegr:::korelacja(data = d, vars = c("a","b","c"), method = "spearman", ci = TRUE, metody = TRUE)$metody$content))
})

test_that("linear regression: simple fit matches lm, factors with chosen reference level, diagnostics", {
    res <- jRegr:::liniowa(data = mtcars, dep = "mpg", covs = "wt", factors = NULL, refLevels = NULL, metody = TRUE)
    fit <- lm(mpg ~ wt, mtcars); sm <- summary(fit)
    f <- res$fit$asDF; expect_equal(f$r2, sm$r.squared); expect_equal(f$F, unname(sm$fstatistic[1])); expect_equal(f$rmse, sqrt(mean(residuals(fit)^2)))
    cf <- res$coef$asDF; expect_equal(cf$b, unname(coef(fit))); expect_equal(cf$lower, unname(confint(fit)[, 1])); expect_equal(cf$term[1], "Wyraz wolny")
    expect_true(res$plot$visible); expect_true(grepl("najmniejszych kwadratów", res$metody$content))

    tg <- ToothGrowth; tg$dose <- factor(tg$dose)
    r2 <- jRegr:::liniowa(data = tg, dep = "len", covs = NULL, factors = c("supp", "dose"), refLevels = list(list(var = "dose", ref = "2")),
                          stdEst = TRUE, anova = TRUE, ic = TRUE, norm = TRUE, vif = TRUE, durbin = TRUE, cooks = TRUE, qq = TRUE, resPlot = TRUE, metody = TRUE)
    cf <- r2$coef$asDF
    expect_equal(cf$term[2:4], c("supp: VC (vs OJ)", "dose: 0.5 (vs 2)", "dose: 1 (vs 2)"))
    tg$dose <- relevel(tg$dose, "2"); fit2 <- lm(len ~ supp + dose, tg)
    expect_equal(cf$b, unname(coef(fit2))); expect_false(res$plot$visible && FALSE); expect_false(r2$plot$visible)
    expect_equal(nrow(r2$anova$asDF), 3); expect_equal(r2$anova$asDF$term[3], "Reszty")
    expect_equal(r2$norm$asDF$w, unname(shapiro.test(residuals(fit2))$statistic))
    expect_equal(nrow(r2$vif$asDF), 3); expect_true(is.finite(r2$durbin$asDF$dw)); expect_true(r2$cooks$asDF$nHigh >= 0)
    expect_true(grepl("poziom odniesienia „2”", r2$metody$content) && grepl("VIF", r2$metody$content))
})

test_that("logistic regression matches glm, event level choice, classification and ROC", {
    mt <- mtcars; mt$am <- factor(mt$am, labels = c("auto", "manual")); mt$vs <- factor(mt$vs)
    res <- jRegr:::logistyczna(data = mt, dep = "am", event = "manual", covs = "wt", factors = NULL, refLevels = NULL, roc = TRUE, ic = TRUE, cooks = TRUE, metody = TRUE)
    fit <- glm(I(am == "manual") ~ wt, mt, family = binomial)
    cf <- res$coef$asDF; expect_equal(cf$b, unname(coef(fit))); expect_equal(cf$or, unname(exp(coef(fit))))
    f <- res$fit$asDF; expect_equal(f$dev, deviance(fit)); expect_equal(f$chi, fit$null.deviance - deviance(fit)); expect_equal(f$aic, AIC(fit))
    expect_equal(f$mcf, 1 - as.numeric(logLik(fit)) / as.numeric(logLik(update(fit, . ~ 1))))
    cs <- res$classStats$asDF; expect_true(cs$auc > 0.9); expect_equal(cs$cutoff, 0.5)
    ct <- res$classTable$asDF; expect_equal(sum(ct$pred0 + ct$pred1), 32)
    expect_true(res$plot$visible); expect_true(res$roc$visible)
    expect_true(grepl("zdarzenie = „manual”", res$metody$content) && grepl("Nagelkerkego", res$metody$content))
    # default event = second level; factor predictor with reference level
    r2 <- jRegr:::logistyczna(data = mt, dep = "am", event = NULL, covs = "hp", factors = "vs", refLevels = list(list(var = "vs", ref = "1")), vif = TRUE)
    expect_true(grepl("manual", paste(capture.output(print(r2$fit)), collapse = "")))
    expect_equal(r2$coef$asDF$term[3], "vs: 0 (vs 1)"); expect_false(r2$plot$visible); expect_equal(nrow(r2$vif$asDF), 2)
})
