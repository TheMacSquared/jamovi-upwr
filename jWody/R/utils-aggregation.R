hydroAggregate <- function(d, o, scale = "monthly") {
    monthly <- scale == "monthly"
    if (!scale %in% c("monthly", "annual")) stop("Nieznana skala agregacji.")
    if (monthly) {
        starts <- seq(as.Date(base::format(min(d$date), "%Y-%m-01")), as.Date(base::format(max(d$date), "%Y-%m-01")), by = "month")
        ends <- seq(starts[1], by = "month", length.out = length(starts) + 1L)[-1L] - 1
        labels <- base::format(starts, "%Y-%m")
    } else {
        years <- seq(min(hydroYear(d$date, o$yearStart)), max(hydroYear(d$date, o$yearStart)))
        b <- hydroBounds(years, o$yearStart)
        starts <- b$start; ends <- b$end; labels <- as.character(years)
    }
    rows <- lapply(seq_along(starts), function(i) {
        ix <- which(d$date >= starts[i] & d$date <= ends[i])
        v <- d$value[ix]; present <- !is.na(v)
        expected <- if (o$resolution == "daily") as.integer(ends[i] - starts[i]) + 1L else if (monthly) 1L else 12L
        n <- sum(present)
        coverage <- 100 * n / expected
        accepted <- n > 0 && coverage + 1e-9 >= o$completeness
        # Monthly means are weighted by calendar duration, monthly rain is summed.
        weights <- rep(1, length(ix))
        if (o$resolution == "monthly" && !monthly && length(ix)) {
            dd <- d$date[ix]
            weights <- as.numeric(as.Date(base::format(dd + 32, "%Y-%m-01")) - dd)
        }
        val <- if (!accepted) NA_real_ else if (o$kind == "rain") sum(v[present]) else stats::weighted.mean(v[present], weights[present])
        data.frame(station = d$station[1], period = labels[i], date = starts[i], end = ends[i],
            expected = expected, observed = n, coverage = coverage, accepted = accepted,
            value = val, minimum = if (accepted) min(v[present]) else NA_real_,
            maximum = if (accepted) max(v[present]) else NA_real_)
    })
    do.call(rbind, rows)
}

hydroRegime <- function(d, o, scale = "monthly", refStart = 0, refEnd = 0) {
    a <- hydroAggregate(d, o, scale)
    # Reference filters use calendar years of the period end, including hydrological years.
    ref <- a
    ref$date <- ref$end
    ref <- hydroReference(ref, refStart, refEnd)
    a$reference <- NA_real_
    for (i in seq_len(nrow(a))) {
        r <- ref$value
        if (scale == "monthly") r <- r[base::format(ref$date, "%m") == base::format(a$date[i], "%m")]
        r <- r[is.finite(r)]
        if (length(r)) a$reference[i] <- mean(r)
    }
    a$anomaly <- a$value - a$reference
    m <- hydroAggregate(d, o, "monthly")
    season <- do.call(rbind, lapply(1:12, function(mm) {
        x <- m$value[as.integer(base::format(m$date, "%m")) == mm & m$accepted]
        data.frame(station = d$station[1], month = mm, n = length(x),
            mean = if (length(x)) mean(x) else NA_real_, median = if (length(x)) stats::median(x) else NA_real_,
            min = if (length(x)) min(x) else NA_real_, max = if (length(x)) max(x) else NA_real_)
    }))
    list(summary = a, details = season, monthly = m)
}

hydroRequireFlow <- function(o) {
    if (o$kind != "flow" || o$resolution != "daily") stop("Ta analiza wymaga dobowego przepływu w m³/s.")
}

hydroFlow <- function(d, o, area = 0) {
    hydroRequireFlow(o)
    if (!is.finite(area) || area < 0) stop("Powierzchnia zlewni nie może być ujemna.")
    a <- hydroAggregate(d, o, "annual")
    validYears <- as.integer(a$period[a$accepted])
    x <- d$value[hydroYear(d$date, o$yearStart) %in% validYears]
    x <- x[is.finite(x)]
    a$volume <- a$depth <- NA_real_
    for (i in which(a$accepted)) {
        v <- d$value[hydroYear(d$date, o$yearStart) == as.integer(a$period[i])]
        a$volume[i] <- sum(v, na.rm = TRUE) * 86400
        if (area > 0) a$depth[i] <- a$volume[i] / (area * 1000)
    }
    # SSQ is the mean of annual SQ, with equal weight for each accepted year.
    v <- a[a$accepted, ]
    summary <- data.frame(station = d$station[1], years = nrow(v),
        nnq = if (nrow(v)) min(v$minimum) else NA_real_, snq = if (nrow(v)) mean(v$minimum) else NA_real_,
        ssq = if (nrow(v)) mean(v$value) else NA_real_, swq = if (nrow(v)) mean(v$maximum) else NA_real_,
        wwq = if (nrow(v)) max(v$maximum) else NA_real_,
        q90 = if (length(x)) unname(stats::quantile(x, .10, type = 7)) else NA_real_,
        q95 = if (length(x)) unname(stats::quantile(x, .05, type = 7)) else NA_real_,
        specific = if (nrow(v) && area > 0) mean(v$value) * 1000 / area else NA_real_)
    fdc <- data.frame(station = rep(d$station[1], length(x)), exceedance = 100 * seq_along(x) / (length(x) + 1), value = sort(x, decreasing = TRUE))
    list(summary = summary, details = a, fdc = fdc)
}
