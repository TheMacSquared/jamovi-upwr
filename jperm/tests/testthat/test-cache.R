skip_if_not_installed("jperm")

test_that("enabling a permutation plot does not restore a previous random sample", {
    jmvcore:::initProtoBuf()
    d <- data.frame(v = c(1, 2, 3, 7, 8, 10))
    old <- jperm:::permtestone(data = d, vars = "v", nPerm = 100, seed = 0, plot = TRUE)
    for (option in c("plot", "metody")) {
        fresh <- jperm:::permtestone(data = d, vars = "v", nPerm = 100, seed = 0, plot = TRUE)
        fresh$plots$get(key = "v")$setState(NULL)
        fresh$plots$fromProtoBuf(old$plots$asProtoBuf(), option, character())
        expect_null(fresh$plots$get(key = "v")$state)
    }
})
