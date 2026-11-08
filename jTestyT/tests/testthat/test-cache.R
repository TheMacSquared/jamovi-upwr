skip_if_not_installed("jTestyT")

test_that("turning off Student discards the previous test rows", {
    jmvcore:::initProtoBuf()
    old <- jTestyT:::ttestone(data = sleep, vars = "extra", student = TRUE, nonpar = TRUE)
    fresh <- jTestyT:::ttestone(data = sleep, vars = "extra", student = FALSE, nonpar = TRUE)
    fresh$ttest$fromProtoBuf(old$ttest$asProtoBuf(), "student", character())
    expect_equal(nrow(fresh$ttest$asDF), 1)
    expect_match(fresh$ttest$asDF$test, "Wilcoxona")
})
