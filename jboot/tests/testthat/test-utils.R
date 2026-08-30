test_that("bootstrap CI extraction uses the correct boot.ci columns", {
    set.seed(17)
    b <- boot::boot(1:12, function(data, i) mean(data[i]), R = 399)

    for (method in c("perc", "basic", "bca")) {
        actual <- extractBootCI(b, 0.95, method)
        ref <- boot::boot.ci(b, conf = 0.95, type = method)
        values <- switch(method,
            perc = ref$percent,
            basic = ref$basic,
            bca = ref$bca)
        expect_equal(unname(unlist(actual)), as.numeric(values[4:5]))
        expect_lt(actual$lower, actual$upper)
    }
})

test_that("seed helper makes resampling reproducible", {
    draw <- function(seed) {
        setSeedIfNeeded(seed)
        sample.int(1000, 20, replace = TRUE)
    }
    expect_identical(draw(123), draw(123))
    expect_false(identical(draw(123), draw(124)))
})

test_that("CI method labels are stable", {
    expect_equal(ciMethodLabel("perc"), "percentylowy")
    expect_equal(ciMethodLabel("bca"), "BCa (skorygowany)")
    expect_equal(ciMethodLabel("basic"), "bazowy (basic)")
})
