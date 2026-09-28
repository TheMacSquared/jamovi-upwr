test_that("each panel fills tables and produces plot states through generated jamovi API", {
    dates <- seq(as.Date("2010-01-01"), as.Date("2024-12-31"), by="day")
    d <- data.frame(date=as.character(dates),value=10+sin(seq_along(dates)/40)+seq_along(dates)/10000,station="A")
    check <- hydrocheck(data=d,date="date",value="value",station="station",showPlot=TRUE,metody=TRUE)
    expect_equal(check$summary$asDF$observed,nrow(d))
    expect_match(check$metody$content,"bez interpolacji",ignore.case=TRUE)
    regime <- hydroregime(data=d,date="date",value="value",station="station",showPlot=TRUE)
    expect_equal(nrow(regime$details$asDF),12)
    flow <- hydroflow(data=d,date="date",value="value",station="station",showPlot=TRUE)
    expect_equal(flow$summary$asDF$years,14)
    trend <- hydrotrend(data=d,date="date",value="value",station="station",showPlot=TRUE)
    expect_true(is.na(trend$summary$asDF$p))
    expect_gt(trend$summary$asDF$slope,0)
    low <- hydrolow(data=d,date="date",value="value",station="station",showPlot=TRUE)
    expect_gt(nrow(low$summary$asDF),0)
    for (r in list(check,regime,flow,trend,low)) {
        expect_false(is.null(r$plot$state))
        expect_false(is.null(r$plot2$state))
        for (secondary in c(FALSE,TRUE)) {
            file <- tempfile(fileext=".png")
            ragg::agg_png(file, width=1000, height=650)
            ok <- tryCatch(hydroPlot(r$plot$state,secondary,ggplot2::theme_minimal()),finally=grDevices::dev.off())
            expect_true(ok)
            expect_gt(file.info(file)$size,1000)
            unlink(file)
        }
    }
})

test_that("optional station, changing kind and monthly groundwater reach the generated API", {
    d <- data.frame(date=as.character(seq(as.Date("2000-01-01"),by="month",length.out=240)),value=seq_len(240)/100)
    r <- hydrotrend(data=d,date="date",value="value",resolution="monthly",kind="depth",trendMode="seasonal",inferIndependent=TRUE)
    expect_equal(r$summary$asDF$slope,.12,tolerance=1e-10)
    expect_lt(r$summary$asDF$p,.01)
    a <- hydrocheckOptions$new(date="date",value="value",resolution="monthly",kind="depth")
    analysis <- hydrocheckClass$new(options=a,data=d)
    analysis$run()
    expect_equal(analysis$results$summary$asDF$observed,240)
})
