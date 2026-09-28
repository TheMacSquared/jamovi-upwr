series <- function(date, value) data.frame(date = as.Date(date), value = value, station = "A")

test_that("ISO dates, duplicate conflicts, sorting and missing days are explicit", {
    expect_true(is.na(hydroDates("2020-02-30")))
    expect_true(is.na(hydroDates("01/02/2020")))
    expect_equal(hydroDates("2020-02-29"), as.Date("2020-02-29"))
    d <- c("2020-02-29", "2020-02-27", "2020-02-27", "bad")
    expect_error(hydroPrepare(d, 1:4), "błędne daty")
    r <- hydroPrepare(d, 1:4, audit = TRUE)[[1]]
    expect_equal(r$quality$invalid, 1)
    expect_equal(r$quality$duplicates, 1)
    expect_equal(r$quality$absent, 1)
    expect_equal(r$quality$observed, 1)
    expect_equal(r$quality$longest, 2)
    expect_equal(r$series$value, c(NA_real_, NA_real_, 1))
    expect_error(hydroPrepare(d[1:3], 1:3), "powtórzone")
    expect_error(hydroPrepare("2020-01-01", -1), "ujemne")
    expect_error(hydroPrepare("2020-01-01", 1, ""), "identyfikator")
    expect_error(hydroPrepare("2020-01-01", Inf), "nieskończone")
    expect_equal(hydroPrepare("2020-01-01", -1, o = hydroOptions(kind = "level"))[[1]]$series$value, -1)
})

test_that("leap years, November boundaries and missing whole years stay on the calendar", {
    expect_equal(hydroYear(as.Date(c("2020-10-31", "2020-11-01"))), c(2020L, 2021L))
    dates <- seq(as.Date("2019-11-01"), as.Date("2020-10-31"), by = "day")
    d <- series(dates, rep(2, length(dates)))
    a <- hydroAggregate(d, hydroOptions(), "annual")
    expect_equal(a$expected, 366)
    expect_equal(a$coverage, 100)
    d$value[1] <- NA
    expect_true(is.na(hydroAggregate(d, hydroOptions(), "annual")$value))
    expect_equal(hydroAggregate(d, hydroOptions(completeness = 99), "annual")$value, 2)
    d <- series(c("2019-01-01", "2021-01-01"), c(1, 2))
    expect_equal(hydroAggregate(d, hydroOptions(yearStart = 1), "annual")$observed, c(1, 0, 1))
    expect_error(hydroPrepare("2020-02-15", 1, o = hydroOptions(resolution = "monthly")), "pierwszy")
})

test_that("monthly means use day weights and precipitation is never averaged", {
    dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
    d <- series(dates, 1:12)
    a <- hydroAggregate(d, hydroOptions(resolution = "monthly", yearStart = 1), "annual")
    expect_equal(a$value, weighted.mean(1:12, c(31,29,31,30,31,30,31,31,30,31,30,31)))
    expect_equal(hydroAggregate(d, hydroOptions(resolution = "monthly", kind = "rain", yearStart = 1), "annual")$value, 78)
    d$value[2] <- NA_real_
    expect_true(is.na(hydroAggregate(d, hydroOptions(resolution = "monthly", kind = "rain", yearStart = 1), "annual")$value))
})

test_that("flow definitions, exceedance direction and volume units agree with hand calculations", {
    dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
    d <- series(dates, ifelse(base::format(dates, "%Y") == "2020", 2, 4))
    r <- hydroFlow(d, hydroOptions(yearStart = 1), area = 10)
    expect_equal(r$summary$nnq, 2)
    expect_equal(r$summary$snq, 3)
    expect_equal(r$summary$ssq, 3)
    expect_equal(r$summary$swq, 3)
    expect_equal(r$summary$wwq, 4)
    expect_equal(r$summary$specific, 300)
    expect_equal(r$details$volume, c(2*366, 4*365)*86400)
    expect_equal(r$details$depth, r$details$volume / 10000)
    expect_equal(r$summary$q95, 2)
    expect_true(all(diff(r$fdc$value) <= 0))
    expect_error(hydroFlow(d, hydroOptions(kind = "rain")), "dobowego")
    d$value[1] <- NA_real_
    expect_equal(hydroFlow(d, hydroOptions(yearStart = 1))$summary$years, 1)
})

test_that("regime anomalies compare like months and use the requested reference", {
    d <- series(seq(as.Date("2018-01-01"), by = "month", length.out = 36), rep(1:12, 3) + rep(c(0,2,4), each=12))
    r <- hydroRegime(d, hydroOptions(resolution="monthly"), refStart=2018, refEnd=2018)
    expect_equal(r$summary$anomaly, rep(c(0,2,4), each=12))
    expect_equal(r$details$n, rep(3L,12))
})

test_that("Sen respects real time gaps; MK has tie correction and opt-in inference", {
    r <- hydroMK(c(2,4,8,10), c(1,2,4,5), inference=TRUE)
    expect_equal(r$slope, 2)
    expect_equal(c(r$lower,r$upper), c(2,2))
    expect_equal(r$s, 6)
    expect_equal(r$variance, 26/3)
    expect_equal(r$p, 2*pnorm(-5/sqrt(26/3)))
    expect_true(is.na(hydroMK(1:4, 1:4)$p))
    expect_equal(hydroMK(rep(2,5), 1:5, inference=TRUE)$p, 1)
    expect_equal(hydroMK(c(1,1,2),1:3)$variance, 8/3)
    # Independent reference: stats Kendall test, asymptotic with continuity correction.
    x <- c(4,2,2,6,3,8,5,9,7,11)
    expect_equal(hydroMK(x, seq_along(x), inference=TRUE)$p,
                 unname(cor.test(seq_along(x),x,method="kendall",exact=FALSE,continuity=TRUE)$p.value))
    seasonal <- hydroMK(rep(c(1,100),4)+rep(0:3,each=2), rep(2000:2003,each=2)+rep(c(0,.5),4), rep(1:2,4), TRUE)
    expect_equal(seasonal$slope, 1)
    expect_equal(seasonal$s, 12)
    expect_equal(seasonal$variance, 52/3)
    pt <- hydroPettitt(c(rep(1,10),rep(10,10)))
    expect_equal(pt$index, 10L)
    expect_equal(pt$statistic, 100)
    expect_equal(pt$p, 2*exp(-6*100^2/(20^3+20^2)))
    expect_true(is.na(hydroPettitt(rep(1,10))$index))
})

test_that("lag diagnostics do not compress gaps", {
    x <- c(1,NA,3,2,5,4)
    expect_equal(hydroLag(x,1), cor(c(3,2,5),c(2,5,4)))
    expect_true(is.na(hydroLag(rep(1,10),1)))
})

test_that("event pooling cannot bridge NA; deficit and censoring are explicit", {
    d <- series(seq(as.Date("2020-10-28"), by="day",length.out=10), c(1,1,3,1,NA,1,1,3,1,3))
    r <- hydroEvents(d, rep(2,10), poolDays=1)
    expect_equal(r$catalog$duration,c(4L,4L))
    expect_equal(r$catalog$lowDays,c(3L,3L))
    expect_equal(r$catalog$deficit,c(3,3)*86400)
    expect_equal(r$catalog$censored,c("tak","tak"))
    expect_equal(nrow(hydroEvents(d,rep(2,10),minDuration=4,poolDays=1)$catalog),0)
    expect_equal(nrow(hydroEvents(d,rep(1,10))$catalog),0)
})

test_that("events are split into hydrological years without counting starts twice", {
    dates <- seq(as.Date("2019-11-01"),as.Date("2021-10-31"),by="day")
    d <- series(dates,rep(3,length(dates)))
    d$value[d$date >= as.Date("2020-10-30") & d$date <= as.Date("2020-11-02")] <- 1
    r <- hydroLow(d,hydroOptions(),mode="fixed",threshold=2)
    expect_equal(r$details$events,c(1,0))
    expect_equal(r$details$lowDays,c(2,2))
    expect_equal(r$details$deficit,c(2,2)*86400)
    expect_equal(r$summary$censored,"nie")
    d$value[1] <- NA_real_
    expect_true(is.na(hydroLow(d,hydroOptions(),mode="fixed")$details$events[1]))
})

test_that("Qp thresholds filter incomplete reference months and differ by month", {
    dates <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="day")
    d <- series(dates,as.numeric(base::format(dates,"%m")))
    th <- hydroThreshold(d,hydroOptions(),mode="monthly")
    expect_equal(th,d$value)
    d$value[1] <- NA_real_
    expect_error(hydroThreshold(d,hydroOptions(),mode="monthly"),"10 pomiarów")
    expect_error(hydroThreshold(d,hydroOptions(),refStart=2030),"odniesienia")
})
