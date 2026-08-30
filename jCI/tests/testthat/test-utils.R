test_that("proportion intervals match their reference formulae", {
    alpha <- 0.05
    x <- 7
    n <- 20
    phat <- x / n
    z <- qnorm(1 - alpha / 2)

    wald <- ciProportion(x, n, 0.95, "wald")
    expect_equal(wald$lower, phat - z * sqrt(phat * (1 - phat) / n))
    expect_equal(wald$upper, phat + z * sqrt(phat * (1 - phat) / n))

    score <- prop.test(x, n, conf.level = 0.95, correct = FALSE)$conf.int
    wilson <- ciProportion(x, n, 0.95, "wilson")
    expect_equal(unname(unlist(wilson)), as.numeric(score), tolerance = 1e-12)

    exact <- ciProportion(x, n, 0.95, "clopperPearson")
    expect_equal(unname(unlist(exact)), as.numeric(binom.test(x, n)$conf.int),
                 tolerance = 1e-12)
})

test_that("proportion intervals handle boundary counts", {
    for (method in c("wald", "wilson", "clopperPearson")) {
        zero <- ciProportion(0, 12, 0.95, method)
        all <- ciProportion(12, 12, 0.95, method)
        expect_gte(zero$lower, 0)
        expect_lte(zero$upper, 1)
        expect_gte(all$lower, 0)
        expect_lte(all$upper, 1)
        expect_equal(zero$lower, 0)
        expect_equal(all$upper, 1)
    }
})

test_that("Wald interval for a difference of proportions is correct", {
    ci <- ciDiffProportion(18, 30, 9, 30, 0.95, "wald")
    est <- 18 / 30 - 9 / 30
    se <- sqrt((18 / 30) * (12 / 30) / 30 +
               (9 / 30) * (21 / 30) / 30)
    z <- qnorm(0.975)
    expect_equal(ci$lower, est - z * se)
    expect_equal(ci$upper, est + z * se)
})
