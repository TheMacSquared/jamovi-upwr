# jRegr: shared engine (pure R) for correlation and regression analyses.

optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0
accent <- function(theme) if (length(theme$color) >= 2) theme$color[2] else "firebrick"

# ---------------------------------------------------------------------------
# Correlation
# ---------------------------------------------------------------------------

corMethodLabel <- function(method) switch(method, pearson = "r Pearsona", spearman = "ρ Spearmana", kendall = "τ-b Kendalla", method)
corAlt <- function(hypothesis) switch(hypothesis, pos = "greater", neg = "less", "two.sided")

#' One pair: coefficient, p (cor.test), N and — for Pearson/Spearman — a Fisher-z
#' interval (Spearman with the Bonett-Wright standard error). Kendall: no interval.
corPair <- function(x, y, method = "pearson", level = 0.95, hypothesis = "corr") {
    ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
    if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0)
        return(list(r = NA_real_, p = NA_real_, n = n, lower = NA_real_, upper = NA_real_))
    ct <- suppressWarnings(stats::cor.test(x, y, method = method, alternative = corAlt(hypothesis), exact = FALSE))
    r <- unname(ct$estimate); lower <- upper <- NA_real_
    if (method != "kendall" && n > 3) {
        se <- if (method == "spearman") sqrt((1 + r^2 / 2) / (n - 3)) else 1 / sqrt(n - 3)
        z <- stats::qnorm(1 - (1 - level) / 2)
        lower <- tanh(atanh(r) - z * se); upper <- tanh(atanh(r) + z * se)
    }
    list(r = r, p = ct$p.value, n = n, lower = lower, upper = upper)
}

scatterPairPlot <- function(x, y, xlab, ylab, r, method, ggtheme, theme) {
    p <- ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.55, size = 2.2, colour = theme$color[1])
    if (method == "pearson") p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = accent(theme), linewidth = 0.9)
    p + ggplot2::labs(x = xlab, y = ylab, subtitle = sprintf("%s = %.3f", corMethodLabel(method), r)) + ggtheme
}

# All pairs as facets (lower triangle), each panel titled with the coefficient
scatterMatrixPlot <- function(d, coefs, method, ggtheme, theme) {
    vars <- names(d); pairs <- utils::combn(length(vars), 2)
    long <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(k) {
        i <- pairs[1, k]; j <- pairs[2, k]
        data.frame(pair = sprintf("%s ~ %s: %.2f", vars[j], vars[i], coefs[[paste(vars[i], vars[j])]]), x = d[[i]], y = d[[j]])
    }))
    long$pair <- factor(long$pair, levels = unique(long$pair))
    p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point(alpha = 0.5, size = 1.6, colour = theme$color[1])
    if (method == "pearson") p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = accent(theme), linewidth = 0.7)
    p + ggplot2::facet_wrap(~ pair, scales = "free") + ggplot2::labs(x = NULL, y = NULL) + ggtheme
}

# ---------------------------------------------------------------------------
# Regression: design, coefficient labels, diagnostics
# ---------------------------------------------------------------------------

#' Reference levels chosen in the panel (list of list(var, ref)) -> named character
refLevelMap <- function(refLevels) {
    out <- character(0)
    for (rl in refLevels) if (!is.null(rl$var) && optNonEmpty(rl$ref)) out[[rl$var]] <- as.character(rl$ref)
    out
}

#' Model frame with releveled factors; complete cases only
regressionFrame <- function(data, dep, covs, factors, refLevels = list(), depFactor = FALSE) {
    d <- data[c(dep, covs, factors)]
    if (!depFactor) d[[dep]] <- jmvcore::toNumeric(d[[dep]])
    for (v in covs) d[[v]] <- jmvcore::toNumeric(d[[v]])
    refs <- refLevelMap(refLevels)
    for (v in factors) {
        d[[v]] <- droplevels(factor(d[[v]]))
        if (!is.null(refs[v]) && !is.na(refs[v]) && refs[[v]] %in% levels(d[[v]])) d[[v]] <- stats::relevel(d[[v]], ref = refs[[v]])
    }
    d <- d[stats::complete.cases(d), , drop = FALSE]
    for (v in factors) d[[v]] <- droplevels(d[[v]])
    d
}

bt <- function(x) paste0("`", x, "`")
regressionFormula <- function(dep, covs, factors) stats::as.formula(paste(bt(dep), "~", if (length(c(covs, factors))) paste(bt(c(covs, factors)), collapse = " + ") else "1"))

#' Human labels for the coefficient rows: covariate name, or "factor: level (vs reference)"
coefLabels <- function(d, covs, factors) {
    lab <- c("(Intercept)" = "Wyraz wolny")
    for (v in covs) lab[[v]] <- v
    for (v in factors) { lv <- levels(d[[v]]); for (l in lv[-1]) lab[[paste0(v, l)]] <- sprintf("%s: %s (vs %s)", v, l, lv[1]) }
    lab
}

#' Standardized coefficients beta = b * sd(x_j) / sd(y) on the model-matrix columns
stdBetas <- function(fit) {
    X <- stats::model.matrix(fit); y <- stats::model.response(stats::model.frame(fit))
    b <- stats::coef(fit); sx <- apply(X, 2, stats::sd); sy <- stats::sd(y)
    out <- b * sx / sy; out["(Intercept)"] <- NA_real_; out
}

#' VIF per model-matrix column (1 / (1 - R2 of that column on the others)); tolerance = 1/VIF
vifTable <- function(fit) {
    X <- stats::model.matrix(fit); X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    if (ncol(X) < 2) return(NULL)
    vif <- vapply(seq_len(ncol(X)), function(j) {
        r2 <- summary(stats::lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared
        if (r2 >= 1) Inf else 1 / (1 - r2)
    }, numeric(1))
    data.frame(term = colnames(X), vif = vif, tol = 1 / vif, stringsAsFactors = FALSE)
}

durbinWatson <- function(fit, seed = 1) {
    e <- stats::residuals(fit); dw <- sum(diff(e)^2) / sum(e^2); r1 <- sum(e[-1] * e[-length(e)]) / sum(e^2)
    p <- tryCatch({ set.seed(seed); car::durbinWatsonTest(fit, reps = 1000)$p }, error = function(err) NA_real_)
    list(dw = dw, r = r1, p = p)
}

cooksSummary <- function(fit) {
    cd <- stats::cooks.distance(fit); n <- length(cd)
    list(mean = mean(cd), max = max(cd), nHigh = sum(cd > 4 / n), thr = 4 / n, values = cd)
}

# ---------------------------------------------------------------------------
# Regression plots
# ---------------------------------------------------------------------------

simpleRegressionPlot <- function(x, y, xlab, ylab, band, ggtheme, theme) {
    ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_ribbon(data = band, ggplot2::aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = accent(theme), alpha = 0.18) +
        ggplot2::geom_point(alpha = 0.55, size = 2.2, colour = theme$color[1]) +
        ggplot2::geom_line(data = band, ggplot2::aes(x = x, y = fit), inherit.aes = FALSE, colour = accent(theme), linewidth = 1) +
        ggplot2::labs(x = xlab, y = ylab, subtitle = "Prosta MNK z pasmem przedziału ufności dla wartości średniej") + ggtheme
}

residFittedPlot <- function(fitted, resid, ggtheme, theme) {
    ggplot2::ggplot(data.frame(f = fitted, r = resid), ggplot2::aes(x = f, y = r)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_point(alpha = 0.55, size = 2, colour = theme$color[1]) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = accent(theme), linewidth = 0.8) +
        ggplot2::labs(x = "Wartości dopasowane", y = "Reszty", subtitle = "Linia = wygładzenie loess; oczekiwany brak wzorca") + ggtheme
}

qqResidPlot <- function(resid, ggtheme, theme) {
    ggplot2::ggplot(data.frame(r = resid), ggplot2::aes(sample = r)) +
        ggplot2::stat_qq_line(colour = "grey50") + ggplot2::stat_qq(alpha = 0.7, colour = theme$color[1]) +
        ggplot2::labs(x = "Kwantyle teoretyczne", y = "Reszty") + ggtheme
}

# ---------------------------------------------------------------------------
# Logistic regression helpers
# ---------------------------------------------------------------------------

#' Classification at a cut-off: confusion counts and accuracy / sensitivity / specificity
classify <- function(y, prob, cutoff = 0.5) {
    pred <- as.integer(prob >= cutoff)
    tp <- sum(pred == 1 & y == 1); tn <- sum(pred == 0 & y == 0); fp <- sum(pred == 1 & y == 0); fn <- sum(pred == 0 & y == 1)
    list(tp = tp, tn = tn, fp = fp, fn = fn, acc = (tp + tn) / length(y),
         sens = if (tp + fn > 0) tp / (tp + fn) else NA_real_, spec = if (tn + fp > 0) tn / (tn + fp) else NA_real_)
}

#' AUC = Mann-Whitney probability that a random event scores above a random non-event
aucValue <- function(y, prob) {
    n1 <- sum(y == 1); n0 <- sum(y == 0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    r <- rank(prob); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

rocCurve <- function(y, prob) {
    th <- c(Inf, sort(unique(prob), decreasing = TRUE), -Inf)
    do.call(rbind, lapply(th, function(t) { pred <- prob >= t
        data.frame(fpr = sum(pred & y == 0) / sum(y == 0), tpr = sum(pred & y == 1) / sum(y == 1)) }))
}

rocPlot <- function(roc, auc, ggtheme, theme) {
    ggplot2::ggplot(roc, ggplot2::aes(x = fpr, y = tpr)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_path(colour = accent(theme), linewidth = 1) +
        ggplot2::coord_fixed() +
        ggplot2::labs(x = "1 − swoistość (odsetek fałszywie dodatnich)", y = "Czułość (odsetek prawdziwie dodatnich)", subtitle = sprintf("AUC = %.3f", auc)) + ggtheme
}

logisticSimplePlot <- function(x, y, xlab, eventLabel, fit, covName, ggtheme, theme) {
    xg <- seq(min(x), max(x), length.out = 100); nd <- data.frame(xg); names(nd) <- covName
    pr <- stats::predict(fit, newdata = nd, type = "link", se.fit = TRUE)
    band <- data.frame(x = xg, fit = stats::plogis(pr$fit), lower = stats::plogis(pr$fit - 1.96 * pr$se.fit), upper = stats::plogis(pr$fit + 1.96 * pr$se.fit))
    ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_ribbon(data = band, ggplot2::aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = accent(theme), alpha = 0.18) +
        ggplot2::geom_jitter(width = 0, height = 0.03, alpha = 0.5, size = 2, colour = theme$color[1]) +
        ggplot2::geom_line(data = band, ggplot2::aes(x = x, y = fit), inherit.aes = FALSE, colour = accent(theme), linewidth = 1) +
        ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%")) +
        ggplot2::labs(x = xlab, y = sprintf("P(%s)", eventLabel), subtitle = "Krzywa logistyczna z pasmem ±1.96 SE (skala logitu)") + ggtheme
}
