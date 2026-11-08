skip_if_not_installed("distrACTION")
jmvcore:::initProtoBuf()

test_that("F endpoint quantiles render without replacing their probability area", {
    for (p in c(0, 1)) {
        res <- distrACTION::FDistribution(QuantileFunction = TRUE, p = p, dp1 = 1, dp2 = 10)
        expect_equal(res$Outputs$getCell(rowNo = 1, "QuantileResultColumn")$value,
                     paste0("x1 = ", qf(p, 1, 10, 0)))
        st <- res$plot$state
        expect_true(all(is.finite(st[[1]])))
        expect_true(all(is.finite(st[[2]])))
        if (p == 1) expect_equal(st[[6]], st[[2]])
        file <- tempfile(fileext = ".png")
        suppressWarnings(suppressMessages(res$plot$saveAs(file)))
        expect_gt(file.info(file)$size, 100)
        unlink(file)
    }
})

test_that("F densities below two numerator degrees of freedom are genuine", {
    for (df1 in c(0.5, 1, 1.9)) {
        st <- distrACTION::FDistribution(dp1 = df1, dp2 = 10)$plot$state
        expect_gt(st[[1]][1], 0)
        expect_equal(st[[2]], df(st[[1]], df1, 10, 0))
        expect_true(all(diff(st[[1]]) > 0))
    }
    st <- distrACTION::FDistribution(dp1 = 3, dp2 = 10, dp3 = 100)$plot$state
    expect_gte(max(st[[1]]), qf(0.99, 3, 10, 100))
    expect_lte(sum(!is.na(st[[5]])), 12)
})

test_that("extreme Weibull shapes retain results and explain unrepresentable plots", {
    for (shape in c(0.0001, 0.001, 0.002)) {
        res <- distrACTION::WeibullDistribution(dp1 = shape, dp2 = 1,
            DistributionFunction = TRUE, x1 = 1, QuantileFunction = TRUE,
            QuantileFunctionType = "cumulative", showMean = TRUE, showVariance = TRUE)
        expect_equal(res$Outputs$getCell(rowNo = 1, "DistributionResultColumn")$value, "P = 0.632")
        expect_null(res$plot$state)
        expect_match(res$plot$asProtoBuf()$error$message, "możliwości liczbowe")
        expect_false(any(grepl("NaN", res$MomentsTable$asDF$ValueColumn)))
    }
    moments <- distrACTION:::weibullMoments(1.5, 3)
    expect_equal(moments$mean, 3 * gamma(1 + 1 / 1.5))
    expect_equal(moments$variance, 9 * (gamma(1 + 2 / 1.5) - gamma(1 + 1 / 1.5)^2))
    # The complete permitted shape range remains available; it is not clamped.
    res <- distrACTION::WeibullDistribution(dp1 = 0.003, dp2 = 1)
    expect_true(all(is.finite(res$plot$state$Dataset[[1]])))
})
