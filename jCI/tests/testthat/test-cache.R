skip_if_not_installed("jCI")

test_that("plot and bootstrap caches are discarded when visibility or methods change", {
    jmvcore:::initProtoBuf()
    d <- data.frame(v = c(1, 2, 4, 5, 9, 10))
    old <- jCI:::cionemean(data = d, dep = "v", group = NULL, ciMethod = "perc", nBoot = 100,
                         seed = 0, plot = TRUE, bootPlot = TRUE)
    for (option in c("plot", "bootPlot", "metody")) {
        fresh <- jCI:::cionemean(data = d, dep = "v", group = NULL, ciMethod = "perc", nBoot = 100,
                                seed = 0, plot = TRUE, bootPlot = TRUE)
        fresh$plot$setState(NULL)
        fresh$plot$fromProtoBuf(old$plot$asProtoBuf(), option, character())
        expect_null(fresh$plot$state)
        fresh$bootPlots$get(key = "v")$setState(NULL)
        fresh$bootPlots$fromProtoBuf(old$bootPlots$asProtoBuf(), option, character())
        expect_null(fresh$bootPlots$get(key = "v")$state)
    }
})
