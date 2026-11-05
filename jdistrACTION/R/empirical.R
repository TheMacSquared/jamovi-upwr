# Helpers comparing a discrete distribution with empirical data
# ("Dane empiryczne" section). The distribution-specific code supplies the
# probability mass function on its support; everything below is shared.

# Reads the selected variable as whole numbers within the support
# [lower, upper]; missing values are dropped.
empReadCounts <- function(data, name, lower = 0, upper = Inf, what = "") {
    x <- jmvcore::toNumeric(data[[name]])
    x <- x[!is.na(x)]
    if (length(x) == 0)
        stop(sprintf("Zmienna „%s” nie ma żadnych wartości.", name), call. = FALSE)
    if (any(abs(x - round(x)) > 1e-8))
        stop(sprintf("Zmienna „%s” ma wartości niecałkowite — rozkład dyskretny opisuje liczby całkowite.", name),
             call. = FALSE)
    x <- round(x)
    if (any(x < lower) || any(x > upper))
        stop(sprintf("Zmienna „%s” ma wartości spoza zakresu %s–%s%s.",
                     name, lower, upper, what), call. = FALSE)
    x
}

# Rows of the observed / expected table: single values from `lo` to `hi`
# (all observed values and every value with N·P ≥ 1), the rest of the
# support pooled into "≤ lo − 1" and "≥ hi + 1".
# `support` = all values with non-zero probability, `pmf` = P(X = support).
empClasses <- function(x, support, pmf) {
    N <- length(x)
    obsAll <- vapply(support, function(v) sum(x == v), numeric(1))
    big <- support[N * pmf >= 1]
    lo <- min(c(x, big))
    hi <- max(c(x, big))
    rows <- list()
    if (lo > min(support)) {
        idx <- support < lo
        rows[[length(rows) + 1]] <- data.frame(
            label = paste0("≤ ", lo - 1), lo = min(support), hi = lo - 1,
            obs = sum(obsAll[idx]), P = sum(pmf[idx]))
    }
    for (v in lo:hi) {
        idx <- support == v
        rows[[length(rows) + 1]] <- data.frame(
            label = as.character(v), lo = v, hi = v,
            obs = sum(obsAll[idx]), P = sum(pmf[idx]))
    }
    if (hi < max(support)) {
        idx <- support > hi
        rows[[length(rows) + 1]] <- data.frame(
            label = paste0("≥ ", hi + 1), lo = hi + 1, hi = max(support),
            obs = sum(obsAll[idx]), P = sum(pmf[idx]))
    }
    out <- do.call(rbind, rows)
    out$exp <- N * out$P
    # Counts O_x of N independent units are Bin(N, P(X = x)) under the
    # model, so the 95% range is exact and needs no simulation.
    out$bandLo <- stats::qbinom(0.025, N, pmin(out$P, 1))
    out$bandHi <- stats::qbinom(0.975, N, pmin(out$P, 1))
    out
}

# Chi-squared goodness of fit: adjacent values pooled from the left until
# the expected count reaches 5; a short last class joins the previous one.
empChiSq <- function(x, support, pmf, nEstimated = 0) {
    N <- length(x)
    obs <- vapply(support, function(v) sum(x == v), numeric(1))
    E <- N * pmf
    groups <- integer(length(support))
    g <- 1
    acc <- 0
    for (i in seq_along(support)) {
        groups[i] <- g
        acc <- acc + E[i]
        if (acc >= 5 && i < length(support)) {
            g <- g + 1
            acc <- 0
        }
    }
    O <- tapply(obs, groups, sum)
    Eg <- tapply(E, groups, sum)
    k <- length(Eg)
    if (k > 1 && Eg[k] < 5) {
        O[k - 1] <- O[k - 1] + O[k]
        Eg[k - 1] <- Eg[k - 1] + Eg[k]
        O <- O[-k]
        Eg <- Eg[-k]
        k <- k - 1
    }
    df <- k - 1 - nEstimated
    chi <- sum((O - Eg)^2 / Eg)
    list(chi = chi, df = df, classes = k,
         p = if (df >= 1) stats::pchisq(chi, df, lower.tail = FALSE) else NA)
}

# Observed relative frequency of the event selected in the distribution
# function, using the same rounding of x1 / x2 as the model probability.
empEventFreq <- function(x, type, x1, x2) {
    hit <- switch(type,
        is       = x == x1,
        lower    = x <= floor(x1),
        higher   = x >= ceiling(x1),
        interval = x >= ceiling(x1) & x <= floor(x2))
    sprintf("%s (%d z %d)", round(mean(hit), 3), sum(hit), length(x))
}

# Fills the observed / expected table, the moments table and the
# chi-squared test; returns the per-value data for the plot.
empFillResults <- function(results, options, x, support, pmf,
                           modelMean, modelVar, nEstimated, estimatedNote) {
    cls <- empClasses(x, support, pmf)
    tab <- results$EmpTable
    for (i in seq_len(nrow(cls))) {
        tab$addRow(rowKey = i, values = list(
            x = cls$label[i], obs = cls$obs[i], obsPct = cls$obs[i] / length(x),
            exp = cls$exp[i], diff = cls$obs[i] - cls$exp[i],
            band = sprintf("%d–%d", cls$bandLo[i], cls$bandHi[i])))
    }
    tab$addRow(rowKey = "total", values = list(
        x = "Razem", obs = length(x), obsPct = 1,
        exp = sum(cls$exp), diff = length(x) - sum(cls$exp), band = ""))
    tab$addFormat(rowKey = "total", col = 1, jmvcore::Cell.BEGIN_END_GROUP)
    notes <- character(0)
    if (options$empBand)
        notes <- c(notes, "Zakres 95%: liczności, które model daje w 95% prób tej samej wielkości (osobno w każdym wierszu)")
    if (nEstimated > 0)
        notes <- c(notes, estimatedNote)
    if (length(notes) > 0)
        tab$setNote("note", paste0(paste(notes, collapse = "; "), "."))

    mom <- results$EmpMoments
    mom$addRow(rowKey = "mean", values = list(
        stat = "Średnia", data = mean(x), model = modelMean))
    mom$addRow(rowKey = "var", values = list(
        stat = "Wariancja", data = if (length(x) > 1) stats::var(x) else NaN,
        model = modelVar))

    if (options$empChiSq) {
        ch <- empChiSq(x, support, pmf, nEstimated)
        chiTab <- results$EmpChiSq
        chiTab$setRow(rowNo = 1, values = list(
            chi = ch$chi, df = ch$df, p = ch$p, classes = ch$classes))
        if (ch$df < 1)
            chiTab$setNote("df", "Za mało klas z liczebnością oczekiwaną ≥ 5, by przeprowadzić test.")
    }

    single <- cls[cls$lo == cls$hi, ]
    N <- length(x)
    data.frame(X = single$lo, Freq = single$obs / N,
               Lo = single$bandLo / N, Hi = single$bandHi / N)
}

# Layers drawing the empirical data on a discrete distribution plot:
# points = observed relative frequency, error bars = 95% range of the model.
empPlotLayers <- function(emp, band, theme) {
    ink <- if (!is.null(theme$color)) theme$color[1] else "#333333"
    layers <- list()
    if (band)
        layers <- c(layers, list(ggplot2::geom_errorbar(
            data = emp, inherit.aes = FALSE,
            mapping = ggplot2::aes(x = X, ymin = Lo, ymax = Hi),
            width = 0.35, colour = ink, alpha = 0.6)))
    c(layers, list(
        ggplot2::geom_point(
            data = emp, inherit.aes = FALSE,
            mapping = ggplot2::aes(x = X, y = Freq, shape = "Dane (częstość)"),
            colour = ink, size = 2.5),
        ggplot2::scale_shape_manual(values = 19)))
}
