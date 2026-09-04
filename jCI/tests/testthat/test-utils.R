test_that("proportion intervals match their reference formulae", {
    x <- 7; n <- 20; phat <- x / n; z <- qnorm(0.975)
    wald <- ciProportion(x, n, 0.95, "wald")
    expect_equal(wald$lower, phat - z * sqrt(phat * (1 - phat) / n))
    expect_equal(wald$upper, phat + z * sqrt(phat * (1 - phat) / n))
    score <- prop.test(x, n, conf.level = 0.95, correct = FALSE)$conf.int
    expect_equal(unname(unlist(ciProportion(x, n, 0.95, "wilson"))), as.numeric(score), tolerance = 1e-12)
    expect_equal(unname(unlist(ciProportion(x, n, 0.95, "clopperPearson"))), as.numeric(binom.test(x, n)$conf.int), tolerance = 1e-12)
})

test_that("proportion intervals handle boundary counts", {
    for (method in c("wald", "wilson", "clopperPearson")) {
        zero <- ciProportion(0, 12, 0.95, method); all <- ciProportion(12, 12, 0.95, method)
        expect_equal(zero$lower, 0); expect_equal(all$upper, 1)
        expect_lte(zero$upper, 1); expect_gte(all$lower, 0)
    }
})

test_that("difference of proportions: Wald formula and Newcombe method 10 reference values", {
    ci <- ciDiffProportion(18, 30, 9, 30, 0.95, "wald")
    est <- 18 / 30 - 9 / 30
    se <- sqrt((18 / 30) * (12 / 30) / 30 + (9 / 30) * (21 / 30) / 30)
    expect_equal(ci$lower, est - qnorm(0.975) * se); expect_equal(ci$upper, est + qnorm(0.975) * se)
    # Newcombe (1998), Table II, example (a): 56/70 vs 48/80 -> 95% CI 0.0524 to 0.3339
    nc <- ciDiffProportion(56, 70, 48, 80, 0.95, "newcombe")
    expect_equal(c(nc$lower, nc$upper), c(0.0524, 0.3339), tolerance = 1e-3)
})

test_that("correlation CI: Pearson equals cor.test; Spearman uses the Bonett-Wright SE", {
    set.seed(3); x <- rnorm(30); y <- x + rnorm(30)
    p <- ciCorrelation(x, y, 0.95, "pearson")
    expect_equal(c(p$lower, p$upper), as.numeric(cor.test(x, y)$conf.int), tolerance = 1e-10)
    s <- ciCorrelation(x, y, 0.95, "spearman")
    r <- cor(x, y, method = "spearman"); se <- sqrt((1 + r^2 / 2) / 27)
    expect_equal(s$lower, tanh(atanh(r) - qnorm(0.975) * se))
    expect_gt(s$upper - s$lower, tanh(atanh(r) + qnorm(0.975) / sqrt(27)) - tanh(atanh(r) - qnorm(0.975) / sqrt(27)))
})

test_that("bootstrap wrapper: reproducible with a seed, percentile limits are quantiles, BCa runs, strata respected", {
    set.seed(1); v <- rexp(40)
    a <- bootCI(v, function(d, i) mean(d[i]), 500, 11, "perc", 0.95)
    b <- bootCI(v, function(d, i) mean(d[i]), 500, 11, "perc", 0.95)
    expect_identical(a$lower, b$lower)
    expect_equal(a$est, mean(v))
    expect_equal(a$se, sd(a$reps))
    expect_false(a$fallback)
    q <- unname(quantile(a$reps, c(0.025, 0.975), type = 6))
    expect_equal(c(a$lower, a$upper), q, tolerance = 0.02)
    bc <- bootCI(v, function(d, i) mean(d[i]), 500, 11, "bca", 0.95)
    expect_false(bc$fallback); expect_true(bc$lower < mean(v) && mean(v) < bc$upper)
    # constant replicates -> boot.ci cannot compute BCa -> percentile fallback flagged
    cst <- bootCI(rep(2, 10), function(d, i) mean(d[i]), 200, 1, "bca", 0.95)
    expect_true(cst$fallback)
    # two statistics from one resample (regression style)
    d <- data.frame(x = 1:20, y = 2 * (1:20) + rnorm(20))
    r2 <- bootCI(d, function(dd, i) coef(lm(y ~ x, data = dd[i, ])), 300, 5, "perc", 0.95)
    expect_length(r2, 2); expect_equal(r2[[2]]$est, unname(coef(lm(y ~ x, d))[2]))
})

test_that("dInterval brackets d and narrows with n; Student interval equals t.test(var.equal = TRUE)", {
    ci1 <- dInterval(0.5, 20, 20); ci2 <- dInterval(0.5, 200, 200)
    expect_true(ci1[1] < 0.5 && 0.5 < ci1[2]); expect_lt(diff(ci2), diff(ci1))
    set.seed(9); x1 <- rnorm(15); x2 <- rnorm(12, 1)
    st <- ciTwoMeansStudent(x1, x2); ref <- t.test(x1, x2, var.equal = TRUE)
    expect_equal(c(st$lower, st$upper), as.numeric(ref$conf.int)); expect_equal(st$df, 25)
})
