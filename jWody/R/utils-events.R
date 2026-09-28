hydroThreshold <- function(d, o, mode = "quantile", exceedance = 90, threshold = 1, refStart = 0, refEnd = 0) {
    hydroRequireFlow(o)
    if (!mode %in% c("fixed", "quantile", "monthly")) stop("Nieznany próg niżówki.")
    if (!is.finite(exceedance) || exceedance <= 0 || exceedance >= 100) stop("Przewyższenie musi należeć do (0, 100).")
    if (mode == "fixed") {
        if (!is.finite(threshold) || threshold < 0) stop("Próg musi być nieujemny.")
        return(rep(threshold, nrow(d)))
    }
    ref <- hydroReference(d, refStart, refEnd)
    if (!nrow(ref)) stop("Brak danych w okresie odniesienia.")
    # Reject incomplete reference months before estimating either type of threshold.
    complete <- hydroAggregate(ref, o, "monthly")
    ref <- ref[base::format(ref$date, "%Y-%m") %in% complete$period[complete$accepted], ]
    estimate <- function(x) {
        x <- x[is.finite(x)]
        if (length(x) < 10L) stop("Próg Qp wymaga co najmniej 10 pomiarów z przyjętych miesięcy (dla progu miesięcznego: w każdym użytym miesiącu).")
        unname(stats::quantile(x, 1 - exceedance / 100, type = 7))
    }
    if (mode == "quantile") return(rep(estimate(ref$value), nrow(d)))
    months <- as.integer(base::format(d$date, "%m"))
    rmonths <- as.integer(base::format(ref$date, "%m"))
    out <- rep(NA_real_, nrow(d))
    for (m in unique(months)) out[months == m] <- estimate(ref$value[rmonths == m])
    out
}

hydroEvents <- function(d, threshold, minDuration = 1L, poolDays = 0L) {
    if (length(threshold) != nrow(d) || any(!is.finite(threshold)) || any(threshold < 0)) stop("Błędny próg.")
    if (minDuration < 1 || minDuration != as.integer(minDuration) || poolDays < 0 || poolDays != as.integer(poolDays)) stop("Długości zdarzeń muszą być całkowite i nieujemne.")
    if (nrow(d) > 1 && any(diff(d$date) != 1)) stop("Zdarzenia wymagają pełnej siatki dobowej (luki jako NA).")
    low <- !is.na(d$value) & d$value < threshold
    runs <- rle(low); ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1L
    events <- list()
    for (i in which(runs$values)) {
        a <- starts[i]; b <- ends[i]
        previous <- length(events)
        if (previous) {
            prevEnd <- events[[previous]][2]
            gap <- seq.int(prevEnd + 1L, a - 1L)
            if (length(gap) <= poolDays && all(!is.na(d$value[gap]))) {
                events[[previous]][2] <- b
                next
            }
        }
        events[[previous + 1L]] <- c(a, b)
    }
    catalog <- data.frame(station = character(), start = character(), end = character(), duration = integer(),
        lowDays = integer(), minimum = numeric(), deficit = numeric(), censored = character())
    membership <- rep(0L, nrow(d))
    for (event in events) {
        ix <- seq.int(event[1], event[2]); days <- sum(low[ix])
        if (days < minDuration) next
        a <- event[1]; b <- event[2]
        left <- a == 1L || is.na(d$value[a - 1L])
        right <- b == nrow(d) || is.na(d$value[b + 1L])
        catalog <- rbind(catalog, data.frame(station = d$station[1], start = as.character(d$date[a]), end = as.character(d$date[b]),
            duration = length(ix), lowDays = days, minimum = min(d$value[ix]),
            deficit = sum(pmax(threshold[ix] - d$value[ix], 0)) * 86400,
            censored = if (left || right) "tak" else "nie"))
        membership[ix] <- nrow(catalog)
    }
    list(catalog = catalog, membership = membership, low = low)
}

hydroLow <- function(d, o, mode = "quantile", exceedance = 90, threshold = 1, minDuration = 1,
                     poolDays = 0, refStart = 0, refEnd = 0) {
    th <- hydroThreshold(d, o, mode, exceedance, threshold, refStart, refEnd)
    ev <- hydroEvents(d, th, minDuration, poolDays)
    a <- hydroAggregate(d, o, "annual")
    year <- hydroYear(d$date, o$yearStart)
    startYears <- hydroYear(as.Date(ev$catalog$start), o$yearStart)
    a$events <- a$lowDays <- 0L; a$deficit <- 0
    for (i in seq_len(nrow(a))) {
        yy <- as.integer(a$period[i]); ix <- which(year == yy & ev$membership > 0)
        a$events[i] <- sum(startYears == yy)
        a$lowDays[i] <- sum(ev$low[ix])
        a$deficit[i] <- sum(pmax(th[ix] - d$value[ix], 0)) * 86400
    }
    d$threshold <- th; d$event <- ev$membership
    # Missing periods are never interpreted as zero events for an annual total.
    a$events[!a$accepted] <- NA_integer_
    a$lowDays[!a$accepted] <- NA_integer_
    a$deficit[!a$accepted] <- NA_real_
    list(summary = ev$catalog, details = a, series = d)
}
