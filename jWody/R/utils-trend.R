# MK tie correction and seasonal sum follow Hirsch et al. (1982).
hydroMK <- function(x, time, season = rep(1L, length(x)), inference = FALSE) {
    keep <- is.finite(x) & is.finite(time) & !is.na(season)
    x <- x[keep]; time <- time[keep]; season <- season[keep]
    ord <- order(time); x <- x[ord]; time <- time[ord]; season <- season[ord]
    if (length(x) < 3L) stop("Trend wymaga co najmniej trzech przyjętych okresów.")
    if (anyDuplicated(time)) stop("Trend wymaga unikalnych dat.")
    if (length(x) > 2400L) stop("Limit trendu: 2400 agregatów. Wybierz agregację roczną lub krótszy okres.")
    s <- variance <- 0
    slopes <- list()
    for (g in unique(season)) {
        ix <- which(season == g); y <- x[ix]; t <- time[ix]; n <- length(ix)
        if (n < 2L) next
        pairs <- utils::combn(n, 2L)
        differences <- y[pairs[2L, ]] - y[pairs[1L, ]]
        s <- s + sum(sign(differences))
        ties <- as.numeric(table(y))
        variance <- variance + (n * (n - 1) * (2 * n + 5) - sum(ties * (ties - 1) * (2 * ties + 5))) / 18
        slopes[[length(slopes) + 1L]] <- differences / (t[pairs[2L, ]] - t[pairs[1L, ]])
    }
    slopes <- sort(unlist(slopes))
    if (!length(slopes)) stop("Za mało powtórzeń w tych samych miesiącach do trendu sezonowego.")
    z <- if (variance == 0) 0 else sign(s) * (abs(s) - 1) / sqrt(variance)
    ci <- c(NA_real_, NA_real_)
    if (inference) {
        # Rank-based normal approximation; clip fractional ranks at endpoints.
        cAlpha <- stats::qnorm(.975) * sqrt(variance)
        ranks <- c((length(slopes) - cAlpha) / 2, (length(slopes) + cAlpha) / 2 + 1)
        ci <- if (length(slopes) == 1L) rep(slopes, 2) else stats::approx(seq_along(slopes), slopes, xout = ranks, rule = 2)$y
    }
    list(n = length(x), s = s, variance = variance, slope = stats::median(slopes),
         lower = ci[1], upper = ci[2], p = if (inference) 2 * stats::pnorm(-abs(z)) else NA_real_)
}

hydroPettitt <- function(x) {
    n <- length(x)
    if (n < 3L || anyNA(x)) stop("Pettitt wymaga co najmniej trzech kompletnych agregatów.")
    u <- 2 * cumsum(rank(x)) - seq_len(n) * (n + 1)
    k <- max(abs(u))
    list(index = if (k == 0) NA_integer_ else which.max(abs(u)), statistic = k,
         p = min(1, 2 * exp(-6 * k^2 / (n^3 + n^2))))
}

hydroLag <- function(x, lag) {
    if (length(x) <= lag) return(NA_real_)
    a <- head(x, -lag); b <- tail(x, -lag)
    keep <- is.finite(a) & is.finite(b)
    if (sum(keep) < 3L || stats::sd(a[keep]) < 1e-12 || stats::sd(b[keep]) < 1e-12) return(NA_real_)
    stats::cor(a[keep], b[keep])
}

hydroTrend <- function(d, o, mode = "annual", inference = FALSE, pettitt = FALSE) {
    if (!mode %in% c("annual", "seasonal")) stop("Nieznany wariant trendu.")
    a <- hydroAggregate(d, o, if (mode == "annual") "annual" else "monthly")
    # Calendar years remain spaced correctly when an entire year is missing.
    time <- if (mode == "annual") as.numeric(a$period) else as.integer(base::format(a$date, "%Y")) + (as.integer(base::format(a$date, "%m")) - 1) / 12
    season <- if (mode == "annual") rep(1L, nrow(a)) else as.integer(base::format(a$date, "%m"))
    result <- hydroMK(a$value, time, season, inference)
    residual <- a$value - result$slope * (time - min(time))
    for (s in unique(season)) {
        ix <- season == s
        center <- stats::median(residual[ix], na.rm = TRUE)
        residual[ix] <- residual[ix] - center
    }
    result$acf1 <- hydroLag(residual, 1L)
    result$acf12 <- if (mode == "seasonal") hydroLag(residual, 12L) else NA_real_
    result$change <- ""; result$pettittP <- NA_real_
    if (pettitt && inference && mode == "annual") {
        valid <- which(is.finite(a$value))
        pt <- hydroPettitt(a$value[valid])
        if (!is.na(pt$index)) result$change <- a$period[valid[pt$index]]
        result$pettittP <- pt$p
    }
    result$station <- d$station[1]
    a$fitted <- result$slope * (time - min(time))
    for (s in unique(season)) {
        ix <- season == s
        a$fitted[ix] <- a$fitted[ix] + stats::median(a$value[ix] - a$fitted[ix], na.rm = TRUE)
    }
    list(summary = as.data.frame(result), details = a,
         acf = data.frame(station = d$station[1], lag = seq_len(if (mode == "annual") 5L else 24L),
            value = vapply(seq_len(if (mode == "annual") 5L else 24L), function(k) hydroLag(residual, k), numeric(1))))
}
