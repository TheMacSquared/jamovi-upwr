# jCI: shared engine for confidence intervals — classical formulae and
# bootstrap (package boot). Pure functions first, jamovi glue at the end.

`%||%` <- function(a, b) if (is.null(a)) b else a
optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

# ---------------------------------------------------------------------------
# Bootstrap
# ---------------------------------------------------------------------------

isBoot <- function(method) method %in% c("perc", "bca")
bootLabel <- function(method) switch(method, perc = "bootstrap percentylowy", bca = "bootstrap BCa", method)

# data: vector or data.frame; statFun(data, idx) -> numeric (length 1 or k).
# Returns for each statistic: est, se (sd of replicates), lower, upper, reps,
# fallback (TRUE when boot.ci failed and percentile quantiles were used).
bootCI <- function(data, statFun, nBoot, seed, method = "perc", level = 0.95, strata = NULL) {
    if (!is.null(seed) && seed > 0) set.seed(seed)
    b <- if (is.null(strata)) boot::boot(data, statFun, R = nBoot)
         else boot::boot(data, statFun, R = nBoot, strata = strata)
    k <- ncol(b$t)
    out <- lapply(seq_len(k), function(j) {
        reps <- b$t[, j]
        ci <- tryCatch(suppressWarnings({
            # boot.ci prints (not signals) "All values of t are equal" and returns NULL
            junk <- utils::capture.output(c0 <- boot::boot.ci(b, conf = level, type = method, index = j))
            v <- if (method == "perc") c0$percent else c0$bca
            if (is.null(v) || length(v) < 5) NULL else list(lower = v[4], upper = v[5], fallback = FALSE)
        }), error = function(e) NULL)
        if (is.null(ci) || any(!is.finite(c(ci$lower, ci$upper)))) {
            a <- (1 - level) / 2
            ci <- list(lower = unname(stats::quantile(reps, a, na.rm = TRUE)),
                       upper = unname(stats::quantile(reps, 1 - a, na.rm = TRUE)), fallback = TRUE)
        }
        list(est = unname(b$t0[j]), se = stats::sd(reps, na.rm = TRUE), lower = ci$lower, upper = ci$upper,
             reps = reps, fallback = ci$fallback)
    })
    if (k == 1) out[[1]] else out
}

# ---------------------------------------------------------------------------
# Classical intervals
# ---------------------------------------------------------------------------

#' t interval for a mean (or a mean difference of paired data)
ciMeanT <- function(x, level = 0.95) {
    n <- length(x); est <- mean(x); se <- stats::sd(x) / sqrt(n)
    tc <- stats::qt(1 - (1 - level) / 2, df = n - 1)
    list(est = est, se = se, lower = est - tc * se, upper = est + tc * se, df = n - 1)
}

#' Welch interval for a difference of two independent means
ciTwoMeansWelch <- function(x1, x2, level = 0.95) {
    n1 <- length(x1); n2 <- length(x2); v1 <- stats::var(x1); v2 <- stats::var(x2)
    est <- mean(x1) - mean(x2); se <- sqrt(v1 / n1 + v2 / n2)
    df <- (v1 / n1 + v2 / n2)^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
    tc <- stats::qt(1 - (1 - level) / 2, df = df)
    list(est = est, se = se, lower = est - tc * se, upper = est + tc * se, df = df)
}

#' Confidence interval for a proportion: wald / wilson / clopperPearson
ciProportion <- function(x, n, ciWidth, method) {
    alpha <- 1 - ciWidth
    phat <- x / n
    z <- stats::qnorm(1 - alpha / 2)
    if (method == "wald") {
        se <- sqrt(phat * (1 - phat) / n)
        lower <- max(0, phat - z * se); upper <- min(1, phat + z * se)
    } else if (method == "wilson") {
        z2 <- z^2; denom <- 1 + z2 / n
        centre <- (phat + z2 / (2 * n)) / denom
        margin <- z * sqrt((phat * (1 - phat) / n + z2 / (4 * n^2))) / denom
        lower <- max(0, centre - margin); upper <- min(1, centre + margin)
    } else if (method == "clopperPearson") {
        lower <- if (x == 0) 0 else stats::qbeta(alpha / 2, x, n - x + 1)
        upper <- if (x == n) 1 else stats::qbeta(1 - alpha / 2, x + 1, n - x)
    } else stop("ciProportion: unknown method ", method)
    list(lower = lower, upper = upper)
}

#' Difference of two independent proportions: wald / newcombe (hybrid score,
#' Newcombe 1998 method 10: no extra z factor — the Wilson limits carry it).
ciDiffProportion <- function(x1, n1, x2, n2, ciWidth, method) {
    p1 <- x1 / n1; p2 <- x2 / n2; est <- p1 - p2
    if (method == "wald") {
        z <- stats::qnorm(1 - (1 - ciWidth) / 2)
        se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
        lower <- est - z * se; upper <- est + z * se
    } else if (method == "newcombe") {
        w1 <- ciProportion(x1, n1, ciWidth, "wilson"); w2 <- ciProportion(x2, n2, ciWidth, "wilson")
        lower <- est - sqrt((p1 - w1$lower)^2 + (w2$upper - p2)^2)
        upper <- est + sqrt((w1$upper - p1)^2 + (p2 - w2$lower)^2)
    } else stop("ciDiffProportion: unknown method ", method)
    list(est = est, lower = max(-1, lower), upper = min(1, upper))
}

#' Correlation with Fisher-z interval. For Spearman the Bonett-Wright (2000)
#' standard error sqrt((1 + r^2/2)/(n - 3)) replaces 1/sqrt(n - 3).
ciCorrelation <- function(x, y, level = 0.95, method = "pearson") {
    n <- length(x); r <- stats::cor(x, y, method = method)
    se <- if (method == "spearman") sqrt((1 + r^2 / 2) / (n - 3)) else 1 / sqrt(n - 3)
    z <- stats::qnorm(1 - (1 - level) / 2)
    list(est = r, lower = tanh(atanh(r) - z * se), upper = tanh(atanh(r) + z * se), n = n)
}

# ---------------------------------------------------------------------------
# Descriptions (jmvcore::metodyNew) and table notes shared by the analyses
# ---------------------------------------------------------------------------

#' One line in the description and one short note under the table about the
#' interval method. `classic` = sentence for the classical method.
metodyPrzedzial <- function(m, o, method, classic, scheme, extra = NULL) {
    if (isBoot(method)) {
        m$add("Przedział ufności", "%s (poziom %g%%): B = %s losowań, %s; SE = odchylenie standardowe replikacji.",
              if (method == "bca") "Bootstrap BCa (z korektą obciążenia i przyspieszenia)" else "Bootstrap percentylowy (kwantyle replikacji)",
              o$ciWidth, format(o$nBoot, big.mark = " "), scheme)
        m$add("Przedział ufności", if (o$seed > 0) sprintf("Ziarno generatora %d — wynik powtarzalny.", o$seed)
              else "Bez ustawionego ziarna — granice zmieniają się nieznacznie między uruchomieniami.")
    } else {
        m$add("Przedział ufności", "%s (poziom %g%%).", classic, o$ciWidth)
    }
    if (!is.null(extra)) m$add("Przedział ufności", extra)
    invisible(m)
}

ciNote <- function(table, o, method, classicLabel, fallback = FALSE) {
    txt <- if (isBoot(method)) sprintf("%g%% CI: %s, B = %s.", o$ciWidth, bootLabel(method), format(o$nBoot, big.mark = " "))
           else sprintf("%g%% CI: %s.", o$ciWidth, classicLabel)
    table$setNote("ci", txt)
    if (isTRUE(fallback)) table$setNote("fb", "Metody BCa nie dało się policzyć (za mało zróżnicowane replikacje) — użyto kwantyli percentylowych.")
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

accent <- function(theme) if (length(theme$color) >= 2) theme$color[2] else "firebrick"

#' Raw data (jitter) with estimate and CI for one or more groups
buildGroupedMeanCIPlot <- function(label, groups, ciWidth, statLabel, ggtheme, theme, refLine = NULL) {
    gn <- names(groups)
    raw <- do.call(rbind, lapply(gn, function(g) data.frame(group = g, y = groups[[g]]$x)))
    ci <- do.call(rbind, lapply(gn, function(g) data.frame(group = g, est = groups[[g]]$estimate,
                                                          lower = groups[[g]]$lower, upper = groups[[g]]$upper)))
    raw$group <- factor(raw$group, levels = gn); ci$group <- factor(ci$group, levels = gn)
    acc <- accent(theme)
    p <- ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 2, colour = theme$color[1])
    if (!is.null(refLine)) p <- p + ggplot2::geom_hline(yintercept = refLine, linetype = "dashed", colour = "grey40")
    p + ggplot2::geom_errorbar(data = ci, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
                               width = 0.3, linewidth = 1.2, colour = acc) +
        ggplot2::geom_point(data = ci, ggplot2::aes(x = group, y = est), inherit.aes = FALSE, shape = 18, size = 6, colour = acc) +
        ggplot2::labs(x = NULL, y = label, subtitle = if (length(gn) == 1)
            sprintf("%s = %.3g; %g%% CI [%.3g; %.3g]", statLabel, ci$est[1], ciWidth, ci$lower[1], ci$upper[1])
            else sprintf("%s z %g%% przedziałem ufności", statLabel, ciWidth)) +
        ggtheme
}

#' Gardner-Altman: two groups + difference on a secondary axis anchored at group 2
buildTwoMeansCIPlot <- function(x1, x2, group1, group2, estimate, lower, upper, ciWidth, ylab, ggtheme, theme) {
    lv <- c(group1, group2, "Różnica")
    raw <- data.frame(group = factor(c(rep(group1, length(x1)), rep(group2, length(x2))), levels = lv), y = c(x1, x2))
    means <- data.frame(group = factor(c(group1, group2), levels = lv), mean = c(mean(x1), mean(x2)))
    ref <- mean(x2); acc <- accent(theme)
    diffDF <- data.frame(group = factor("Różnica", levels = lv), y = ref + estimate, lower = ref + lower, upper = ref + upper)
    ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 2, colour = theme$color[1]) +
        ggplot2::geom_hline(yintercept = ref, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_point(data = means, ggplot2::aes(x = group, y = mean), inherit.aes = FALSE, shape = 18, size = 5, colour = acc) +
        ggplot2::geom_errorbar(data = diffDF, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
                               width = 0.3, linewidth = 1.2, colour = acc) +
        ggplot2::geom_point(data = diffDF, ggplot2::aes(x = group, y = y), inherit.aes = FALSE, shape = 18, size = 6, colour = acc) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . - ref, name = sprintf("Różnica względem średniej „%s”", group2))) +
        ggplot2::labs(x = NULL, y = ylab, subtitle = sprintf("Różnica = %.3g; %g%% CI [%.3g; %.3g]", estimate, ciWidth, lower, upper)) +
        ggtheme
}

#' Waffle 10x10 for a proportion: dark = below the lower limit, light = CI band
buildProportionIconPlot <- function(label, estimate, lower, upper, ciWidth, ggtheme, theme) {
    grid <- expand.grid(x = 1:10, y = 1:10); grid$idx <- seq_len(100)
    lo <- round(lower * 100); hi <- round(upper * 100)
    grid$cat <- "Porażka"
    grid$cat[grid$idx <= hi] <- "Przedział ufności"
    grid$cat[grid$idx <= lo] <- "Sukces"
    grid$cat <- factor(grid$cat, levels = c("Sukces", "Przedział ufności", "Porażka"))
    acc <- accent(theme)
    ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = cat)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1.5) +
        ggplot2::scale_fill_manual(values = c("Sukces" = acc, "Przedział ufności" = "#E89090", "Porażka" = "#E0E0E0"), drop = FALSE) +
        ggplot2::coord_fixed() + ggplot2::scale_y_reverse() +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                      subtitle = sprintf("%s: %.1f%%; %g%% CI [%.1f%%; %.1f%%]", label, 100 * estimate, ciWidth, 100 * lower, 100 * upper)) +
        ggtheme +
        ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(), legend.position = "bottom",
                       legend.title = ggplot2::element_blank(), legend.key.size = ggplot2::unit(0.8, "lines"))
}

#' Two proportions with their Wilson intervals and the difference (Gardner-Altman
#' layout on the proportion scale, difference anchored at group 2)
buildDiffPropPlot <- function(group1, group2, p1, p2, ci1, ci2, estimate, lower, upper, ciWidth, level, ggtheme, theme) {
    lv <- c(group1, group2, "Różnica"); acc <- accent(theme)
    pts <- data.frame(group = factor(c(group1, group2), levels = lv), y = c(p1, p2),
                      lower = c(ci1$lower, ci2$lower), upper = c(ci1$upper, ci2$upper))
    ref <- p2
    diffDF <- data.frame(group = factor("Różnica", levels = lv), y = ref + estimate, lower = ref + lower, upper = ref + upper)
    ggplot2::ggplot(pts, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_hline(yintercept = ref, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.3, linewidth = 1, colour = theme$color[1]) +
        ggplot2::geom_point(shape = 18, size = 5, colour = theme$color[1]) +
        ggplot2::geom_errorbar(data = diffDF, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
                               width = 0.3, linewidth = 1.2, colour = acc) +
        ggplot2::geom_point(data = diffDF, ggplot2::aes(x = group, y = y), inherit.aes = FALSE, shape = 18, size = 6, colour = acc) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"),
                                    sec.axis = ggplot2::sec_axis(~ . - ref, name = sprintf("Różnica względem „%s”", group2),
                                                                 labels = function(v) paste0(round(100 * v), " p.p."))) +
        ggplot2::labs(x = NULL, y = sprintf("Udział kategorii „%s”", level),
                      subtitle = sprintf("Różnica = %.1f p.p.; %g%% CI [%.1f; %.1f] p.p.", 100 * estimate, ciWidth, 100 * lower, 100 * upper)) +
        ggtheme
}

#' Scatter with a fitted line (no OLS band — the interval in the table is for r)
buildCorrelationPlot <- function(x1, x2, var1, var2, estimate, lower, upper, ciWidth, method, ggtheme, theme) {
    df <- data.frame(x = x1, y = x2)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.5, size = 2.5, colour = theme$color[1]) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = accent(theme), linewidth = 0.9) +
        ggplot2::labs(x = var1, y = var2, subtitle = sprintf("%s = %.3f; %g%% CI [%.3f; %.3f]",
            if (method == "pearson") "r Pearsona" else "ρ Spearmana", estimate, ciWidth, lower, upper)) +
        ggtheme
}

#' Regression line with a confidence band for the mean response (classical:
#' predict(); bootstrap: pointwise quantiles of the replicated lines)
buildRegressionPlot <- function(x, y, xlab, ylab, band, ciWidth, bandLabel, ggtheme, theme) {
    ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_ribbon(data = band, ggplot2::aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE,
                             fill = accent(theme), alpha = 0.2) +
        ggplot2::geom_point(alpha = 0.5, size = 2.2, colour = theme$color[1]) +
        ggplot2::geom_line(data = band, ggplot2::aes(x = x, y = fit), inherit.aes = FALSE, colour = accent(theme), linewidth = 1) +
        ggplot2::labs(x = xlab, y = ylab, subtitle = sprintf("Pasmo: %g%% przedział ufności dla wartości średniej (%s)", ciWidth, bandLabel)) +
        ggtheme
}

#' Histogram of bootstrap replicates with the estimate and CI limits
buildBootHist <- function(reps, est, lower, upper, xlab, ggtheme, theme) {
    acc <- accent(theme)
    ggplot2::ggplot(data.frame(x = reps), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(bins = 40, fill = theme$fill[2], colour = theme$color[1], alpha = 0.7) +
        ggplot2::annotate("rect", xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, fill = acc, alpha = 0.12) +
        ggplot2::geom_vline(xintercept = est, colour = acc, linewidth = 1) +
        ggplot2::geom_vline(xintercept = c(lower, upper), colour = acc, linetype = "dashed", linewidth = 0.8) +
        ggplot2::labs(x = xlab, y = "Liczba replikacji", subtitle = "Linia ciągła = estymata z danych; przerywane = granice przedziału") +
        ggtheme
}

#' CI limits as a function of B (log scale)
buildConvPlot <- function(pd, ggtheme, theme) {
    long <- data.frame(b = rep(pd$b, 2), value = c(pd$lower, pd$upper),
                       bound = rep(c("Dolna granica", "Górna granica"), each = nrow(pd)))
    ggplot2::ggplot(long, ggplot2::aes(x = b, y = value, colour = bound)) +
        ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_log10(breaks = pd$b) +
        ggplot2::labs(x = "Liczba losowań B (skala log)", y = "Granice przedziału", colour = NULL) +
        ggtheme
}

# ---------------------------------------------------------------------------
# Group selection shared by the two-group analyses
# ---------------------------------------------------------------------------

#' Returns c(level1, level2) or NULL (after writing an error note)
pickTwoLevels <- function(table, allLevs, level1, level2) {
    if (optNonEmpty(level1) && optNonEmpty(level2)) {
        l1 <- as.character(level1); l2 <- as.character(level2)
        if (l1 == l2) { table$setNote("err", "Grupa 1 i Grupa 2 muszą być różne."); return(NULL) }
        if (!(l1 %in% allLevs) || !(l2 %in% allLevs)) { table$setNote("err", "Wybrane grupy nie istnieją w zmiennej."); return(NULL) }
        return(c(l1, l2))
    }
    if (length(allLevs) < 2) { table$setNote("err", "Zmienna grupująca musi mieć co najmniej 2 poziomy."); return(NULL) }
    if (length(allLevs) > 2)
        table$setNote("info", sprintf("Zmienna ma %d poziomów — użyto pierwszych dwóch (%s, %s); wybierz grupy w panelu.",
                                      length(allLevs), allLevs[1], allLevs[2]))
    allLevs[1:2]
}

#' First level of a factor-like column when the user has not chosen one
pickLevel <- function(column, level) {
    if (optNonEmpty(level)) return(as.character(level))
    if (is.factor(column)) return(levels(column)[1])
    v <- sort(unique(column[!is.na(column)])); if (length(v) == 0) NULL else as.character(v[1])
}
