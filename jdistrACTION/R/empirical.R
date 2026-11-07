# Helpers comparing a discrete distribution with empirical data
# ("Dane empiryczne" section). The comparison is descriptive only (counts,
# frequencies, moments; no tests or ranges): it is used in class before
# tests and confidence intervals are introduced. The distribution-specific
# code supplies the probability mass function on its support; everything
# below is shared.

# Reads the selected variable as whole numbers within the support
# [lower, upper]; missing values are dropped. Text categories mean the
# variable does not hold counts (short message, as asked for class use).
empReadCounts <- function(data, name, lower = 0, upper = Inf, what = "") {
    x <- data[[name]]
    if (is.factor(x) || is.character(x)) {
        # nominal / ordinal columns arrive as factors; their labels must be
        # numbers (e.g. an integer column set to nominal)
        lab <- as.character(x)
        num <- suppressWarnings(as.numeric(lab))
        if (any(!is.na(lab) & is.na(num)))
            stop("Nieprawidłowy format zmiennej.", call. = FALSE)
        x <- num
    } else {
        x <- jmvcore::toNumeric(x)
    }
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

# Reads a continuous variable for a continuous distribution; missing values
# are dropped. Nominal / ordinal columns with numeric labels are read as
# numbers, text categories are a wrong variable format.
empReadContinuous <- function(data, name) {
    x <- data[[name]]
    if (is.factor(x) || is.character(x)) {
        lab <- as.character(x)
        num <- suppressWarnings(as.numeric(lab))
        if (any(!is.na(lab) & is.na(num)))
            stop("Nieprawidłowy format zmiennej.", call. = FALSE)
        x <- num
    } else {
        x <- jmvcore::toNumeric(x)
    }
    x <- x[is.finite(x)]
    if (length(x) == 0)
        stop(sprintf("Zmienna „%s” nie ma żadnych wartości.", name), call. = FALSE)
    x
}

# Density histogram of a continuous variable (area = 1). Classes as in
# jEksplor "Szereg rozdzielczy": k from Sturges' rule ⌈log₂ N + 1⌉ unless
# given (k > 0), equal width from the minimum, left-closed, the last one
# closed on both sides.
empHistogram <- function(x, k = 0) {
    N <- length(x)
    if (is.null(k) || k < 1)
        k <- ceiling(log2(N) + 1)
    lo <- min(x)
    hi <- max(x)
    if (hi == lo) {
        lo <- lo - 0.5
        hi <- hi + 0.5
    }
    br <- seq(lo, hi, length.out = k + 1)
    n <- tabulate(findInterval(x, br, rightmost.closed = TRUE), nbins = k)
    data.frame(xmin = br[-length(br)], xmax = br[-1],
               density = n / (N * diff(br)))
}

# Reads a two-category variable where every row is one trial; `level` is the
# category chosen as "success" (by default the second one, as in jCzest and
# jRegr). Numeric 0/1 columns work too: their categories are "0" and "1".
# More than two categories: the variable is not a 0/1 one.
empReadBinary <- function(data, name, level) {
    x <- data[[name]]
    x <- x[!is.na(x)]
    if (length(x) == 0)
        stop(sprintf("Zmienna „%s” nie ma żadnych wartości.", name), call. = FALSE)
    x <- factor(x)
    lv <- levels(x)
    if (length(lv) > 2)
        stop("Nieprawidłowy format zmiennej.", call. = FALSE)
    if (!is.null(level) && length(level) == 1 && nzchar(level))
        success <- level
    else if (length(lv) == 2)
        success <- lv[2]
    else
        success <- if (lv == "0") "1" else lv
    s <- sum(as.character(x) == success)
    list(successes = s, N = length(x), level = success, phat = s / length(x))
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
    out
}

# Observed relative frequency of the event selected in the distribution
# function, using the same rounding of x1 / x2 as the model probability
# (no rounding for a continuous distribution).
empEventFreq <- function(x, type, x1, x2, discrete = TRUE) {
    if (!discrete) {
        hit <- switch(type,
            lower    = x <= x1,
            higher   = x >= x1,
            interval = x >= x1 & x <= x2)
    } else hit <- switch(type,
        is       = x == x1,
        lower    = x <= floor(x1),
        higher   = x >= ceiling(x1),
        interval = x >= ceiling(x1) & x <= floor(x2))
    sprintf("%s (%d z %d)", round(mean(hit), 3), sum(hit), length(x))
}

# Fills the observed / expected table and the moments table; returns the
# per-value data for the plot.
empFillResults <- function(results, x, support, pmf, modelMean, modelVar) {
    cls <- empClasses(x, support, pmf)
    tab <- results$EmpTable
    for (i in seq_len(nrow(cls))) {
        tab$addRow(rowKey = i, values = list(
            x = cls$label[i], obs = cls$obs[i], obsPct = cls$obs[i] / length(x),
            exp = cls$exp[i], diff = cls$obs[i] - cls$exp[i]))
    }
    tab$addRow(rowKey = "total", values = list(
        x = "Razem", obs = length(x), obsPct = 1,
        exp = sum(cls$exp), diff = length(x) - sum(cls$exp)))
    tab$addFormat(rowKey = "total", col = 1, jmvcore::Cell.BEGIN_END_GROUP)

    mom <- results$EmpMoments
    mom$addRow(rowKey = "mean", values = list(
        stat = "Średnia", data = mean(x), model = modelMean))
    mom$addRow(rowKey = "var", values = list(
        stat = "Wariancja", data = if (length(x) > 1) stats::var(x) else NaN,
        model = modelVar))

    # single values only: an end class ("≤ k" / "≥ k") may cover just one
    # value of the support, but it stands for the whole tail
    single <- cls[!grepl("^[≤≥]", cls$label), ]
    data.frame(X = single$lo, Freq = single$obs / length(x))
}

# Layers drawing the empirical data on a discrete distribution plot as
# points: the observed relative frequency, or, for a 0/1 variable, the
# model drawn from the data.
empPlotLayers <- function(emp, theme, label = "Dane (częstość)") {
    ink <- if (!is.null(theme$color)) theme$color[1] else "#333333"
    list(
        ggplot2::geom_point(
            data = emp, inherit.aes = FALSE,
            mapping = ggplot2::aes(x = X, y = Freq, shape = label),
            colour = ink, size = 2.5),
        ggplot2::scale_shape_manual(values = 19))
}

# Layer drawing a density histogram (empHistogram) under the curve of a
# continuous distribution; the fill joins the plot's manual fill scale
# under `label`; `outline` separates the bars (the plot background colour).
empHistogramLayer <- function(hist, outline = "white", label = "Dane (histogram)") {
    ggplot2::geom_rect(
        data = hist, inherit.aes = FALSE,
        mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = density, fill = label),
        colour = outline, alpha = 0.6)
}

