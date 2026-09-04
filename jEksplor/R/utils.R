# jEksplor: descriptive-statistics engine in pure R. Pure functions here,
# jamovi glue in the .b.R files.

optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

# ---------------------------------------------------------------------------
# Single statistics
# ---------------------------------------------------------------------------

modeValue <- function(x) {
    if (length(x) == 0) return(NA_real_)
    tb <- table(x)
    if (max(tb) == 1) return(NA_real_)      # every value once: no mode
    as.numeric(names(tb)[which.max(tb)])    # first of the tied most frequent
}

winsorMean <- function(x, trim = 0.1) {
    n <- length(x); k <- floor(n * trim)
    if (k == 0) return(mean(x))
    s <- sort(x); s[seq_len(k)] <- s[k + 1]; s[(n - k + 1):n] <- s[n - k]
    mean(s)
}

# Sample skewness G1 and excess kurtosis G2 with standard errors (SPSS/jmv formulas)
skewKurt <- function(x) {
    n <- length(x)
    if (n < 4) return(list(skew = NA_real_, seSkew = NA_real_, kurt = NA_real_, seKurt = NA_real_))
    d <- x - mean(x); s2 <- sum(d^2); s4 <- sum(d^4); v <- s2 / (n - 1)
    skew <- sqrt(n * (n - 1)) / (n - 2) * sqrt(n) * sum(d^3) / s2^1.5
    kurt <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * s4 / v^2 - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
    varSkew <- 6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))
    list(skew = skew, seSkew = sqrt(varSkew), kurt = kurt, seKurt = sqrt(4 * (n^2 - 1) * varSkew / ((n - 3) * (n + 5))))
}

# Gini coefficient for non-negative values (NA otherwise); Lorenz curve points
gini <- function(x) {
    if (length(x) < 2 || any(x < 0) || sum(x) == 0) return(NA_real_)
    s <- sort(x); n <- length(s)
    2 * sum(seq_len(n) * s) / (n * sum(s)) - (n + 1) / n
}
lorenz <- function(x) {
    s <- sort(x); n <- length(s)
    data.frame(p = c(0, seq_len(n) / n), L = c(0, cumsum(s) / sum(s)))
}

#' All descriptive statistics for one numeric vector (NA already removed);
#' `trim` for the trimmed and winsorized means.
descStats <- function(x, trim = 0.1) {
    n <- length(x)
    if (n == 0) return(list(n = 0L))
    q <- unname(stats::quantile(x, c(0.25, 0.5, 0.75), type = 7))
    m <- mean(x); s <- if (n > 1) stats::sd(x) else NA_real_
    sk <- skewKurt(x)
    pos <- all(x > 0)
    list(n = n, mean = m, median = q[2], q1 = q[1], q3 = q[3], sd = s,
         min = min(x), max = max(x), v = if (m != 0) 100 * s / m else NA_real_,
         mode = modeValue(x), sum = sum(x),
         gmean = if (pos) exp(mean(log(x))) else NA_real_,
         hmean = if (pos) n / sum(1 / x) else NA_real_,
         tmean = mean(x, trim = trim), wmean = winsorMean(x, trim),
         variance = if (n > 1) stats::var(x) else NA_real_, range = max(x) - min(x), iqr = q[3] - q[1],
         meanDev = mean(abs(x - m)), mad = stats::median(abs(x - q[2])), qdev = (q[3] - q[1]) / 2,
         vq = if (q[2] != 0) 100 * (q[3] - q[1]) / 2 / q[2] else NA_real_,
         typLo = m - s, typHi = m + s,
         skew = sk$skew, seSkew = sk$seSkew, kurt = sk$kurt, seKurt = sk$seKurt,
         skewPearson = if (isTRUE(s > 0)) 3 * (m - q[2]) / s else NA_real_,
         skewQuart = if (q[3] > q[1]) (q[3] + q[1] - 2 * q[2]) / (q[3] - q[1]) else NA_real_,
         gini = gini(x))
}

# ---------------------------------------------------------------------------
# Normality tests (pure R; Lilliefors and Anderson-Darling p-values use the
# Dallal-Wilkinson and Stephens approximations, as in package nortest)
# ---------------------------------------------------------------------------

shapiroTest <- function(x) {
    if (length(x) < 3 || length(x) > 5000 || stats::sd(x) == 0) return(list(stat = NA_real_, p = NA_real_))
    t <- stats::shapiro.test(x); list(stat = unname(t$statistic), p = t$p.value)
}

lillieTest <- function(x) {
    n <- length(x)
    if (n < 5 || stats::sd(x) == 0) return(list(stat = NA_real_, p = NA_real_))
    p <- stats::pnorm((sort(x) - mean(x)) / stats::sd(x))
    K <- max(seq_len(n) / n - p, p - (seq_len(n) - 1) / n)
    if (n <= 100) { Kd <- K; nd <- n } else { Kd <- K * (n / 100)^0.49; nd <- 100 }
    pv <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598 / sqrt(nd) + 1.67997 / nd)
    if (pv > 0.1) {
        KK <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * K
        pv <- if (KK <= 0.302) 1
              else if (KK <= 0.5) 2.76773 - 19.828315 * KK + 80.709644 * KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
              else if (KK <= 0.9) -4.901232 + 40.662806 * KK - 97.490286 * KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
              else if (KK <= 1.31) 6.198765 - 19.558097 * KK + 23.186922 * KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
              else 0
    }
    list(stat = K, p = min(1, max(0, pv)))
}

adTest <- function(x) {
    n <- length(x)
    if (n < 8 || stats::sd(x) == 0) return(list(stat = NA_real_, p = NA_real_))
    p <- stats::pnorm((sort(x) - mean(x)) / stats::sd(x))
    p <- pmin(pmax(p, 1e-300), 1 - 1e-16)
    h <- (2 * seq_len(n) - 1) * (log(p) + log(1 - rev(p)))
    A <- -n - mean(h); AA <- (1 + 0.75 / n + 2.25 / n^2) * A
    pv <- if (AA < 0.2) 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
          else if (AA < 0.34) 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
          else if (AA < 0.6) exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
          else if (AA < 10) exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
          else 3.7e-24
    list(stat = A, p = min(1, max(0, pv)))
}

# ---------------------------------------------------------------------------
# Percentiles and cut points requested as text ("25,50,75") / k groups
# ---------------------------------------------------------------------------

parsePercentiles <- function(txt) {
    v <- suppressWarnings(as.numeric(trimws(strsplit(as.character(txt), "[,;]")[[1]])))
    v <- v[is.finite(v) & v > 0 & v < 100]
    unique(v)
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

accent <- function(theme) if (length(theme$color) >= 2) theme$color[2] else "firebrick"

# d: data.frame(y, group) where group is a factor (one level = no split)
histPlot <- function(d, label, dens, ggtheme, theme) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = y))
    if (dens) p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = theme$fill[2], colour = theme$color[1]) +
                      ggplot2::geom_density(colour = accent(theme), linewidth = 1)
    else p <- p + ggplot2::geom_histogram(bins = 30, fill = theme$fill[2], colour = theme$color[1])
    if (nlevels(d$group) > 1) p <- p + ggplot2::facet_wrap(~ group, ncol = 1, scales = "free_y")
    p + ggplot2::labs(x = label, y = if (dens) "Gęstość" else "Liczność") + ggtheme
}

boxPlot <- function(d, label, violin, dot, meanPt, ggtheme, theme) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = group, y = y))
    if (violin) p <- p + ggplot2::geom_violin(fill = theme$fill[2], colour = theme$color[1], alpha = 0.5, width = 0.8)
    p <- p + ggplot2::geom_boxplot(width = if (violin) 0.25 else 0.5, fill = if (violin) "white" else theme$fill[2],
                                   colour = theme$color[1], outlier.shape = if (dot) NA else 19)
    if (dot) p <- p + ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.45, size = 1.8, colour = theme$color[1])
    if (meanPt) p <- p + ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 4.5, colour = accent(theme))
    p + ggplot2::labs(x = if (nlevels(d$group) > 1) NULL else "", y = label,
                      subtitle = if (meanPt) "Pudełko: mediana i kwartyle; romb: średnia" else NULL) + ggtheme
}

qqPlotDesc <- function(d, label, ggtheme, theme) {
    p <- ggplot2::ggplot(d, ggplot2::aes(sample = y)) +
        ggplot2::stat_qq_line(colour = "grey50") + ggplot2::stat_qq(alpha = 0.7, colour = theme$color[1])
    if (nlevels(d$group) > 1) p <- p + ggplot2::facet_wrap(~ group)
    p + ggplot2::labs(x = "Kwantyle teoretyczne", y = label) + ggtheme
}

ecdfPlot <- function(d, label, ggtheme, theme) {
    if (nlevels(d$group) > 1)
        p <- ggplot2::ggplot(d, ggplot2::aes(x = y, colour = group)) + ggplot2::stat_ecdf(linewidth = 0.9) + ggplot2::labs(colour = NULL)
    else p <- ggplot2::ggplot(d, ggplot2::aes(x = y)) + ggplot2::stat_ecdf(linewidth = 0.9, colour = theme$color[1])
    p + ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
        ggplot2::labs(x = label, y = "Dystrybuanta empiryczna F(x)") + ggtheme
}

lorenzPlot <- function(d, label, ggtheme, theme) {
    parts <- split(d$y, d$group)
    lz <- do.call(rbind, lapply(names(parts), function(g) {
        v <- parts[[g]]
        if (length(v) == 0 || any(v < 0)) return(NULL)
        cbind(lorenz(v), group = g)
    }))
    lz$group <- factor(lz$group, levels = levels(d$group))
    p <- ggplot2::ggplot(lz, ggplot2::aes(x = p, y = L)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50")
    if (nlevels(d$group) > 1) p <- p + ggplot2::geom_line(ggplot2::aes(colour = group), linewidth = 1) + ggplot2::labs(colour = NULL)
    else p <- p + ggplot2::geom_line(colour = accent(theme), linewidth = 1)
    p + ggplot2::scale_x_continuous(labels = function(v) paste0(round(100 * v), "%")) +
        ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
        ggplot2::coord_fixed() +
        ggplot2::labs(x = "Skumulowany udział jednostek", y = sprintf("Skumulowany udział sumy „%s”", label),
                      subtitle = "Przekątna = równy rozkład; im dalej krzywa, tym większa koncentracja") + ggtheme
}
