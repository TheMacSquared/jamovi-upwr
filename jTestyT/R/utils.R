# jTestyT: shared engine for the three t-test analyses.

`%||%` <- function(a, b) if (is.null(a)) b else a
optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

altR <- function(hypothesis) switch(hypothesis, greater = "greater", less = "less", "two.sided")
altLabel <- function(hypothesis, what = "różnica") switch(hypothesis,
    greater = paste(what, "> 0"), less = paste(what, "< 0"), paste(what, "≠ 0"))

# ---------------------------------------------------------------------------
# Cohen's d with a noncentral-t confidence interval
# ---------------------------------------------------------------------------

dInterval <- function(d, n1, n2 = NULL, level = 0.95) {
    if (is.null(n2)) { t <- d * sqrt(n1); df <- n1 - 1; scale <- 1 / sqrt(n1) }
    else { t <- d * sqrt(n1 * n2 / (n1 + n2)); df <- n1 + n2 - 2; scale <- sqrt((n1 + n2) / (n1 * n2)) }
    if (!is.finite(t) || df < 1) return(c(NA_real_, NA_real_))
    a <- (1 - level) / 2
    f <- function(nc, prob) suppressWarnings(stats::pt(t, df, ncp = nc)) - prob
    span <- abs(t) + 10
    lo <- tryCatch(stats::uniroot(f, c(t - span, t + span), prob = 1 - a)$root, error = function(e) NA_real_)
    hi <- tryCatch(stats::uniroot(f, c(t - span, t + span), prob = a)$root, error = function(e) NA_real_)
    c(lo * scale, hi * scale)
}

# ---------------------------------------------------------------------------
# One sample / paired differences
# ---------------------------------------------------------------------------

oneSampleT <- function(x, mu = 0, hypothesis = "different", level = 0.95) {
    tt <- stats::t.test(x, mu = mu, alternative = altR(hypothesis), conf.level = level)
    n <- length(x); d <- (mean(x) - mu) / stats::sd(x)
    ci <- dInterval(d, n, level = level)
    list(test = "t Studenta", stat = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
        est = mean(x) - mu, lower = tt$conf.int[1] - mu, upper = tt$conf.int[2] - mu,
        es = d, esLower = ci[1], esUpper = ci[2], esLabel = "d Cohena")
}

wilcoxOne <- function(x, mu = 0, hypothesis = "different", level = 0.95) {
    w <- suppressWarnings(stats::wilcox.test(x, mu = mu, alternative = altR(hypothesis),
        conf.int = TRUE, conf.level = level, exact = FALSE, correct = TRUE))
    dif <- x - mu; dif <- dif[dif != 0]
    S <- length(dif) * (length(dif) + 1) / 2
    r <- if (S > 0) (2 * unname(w$statistic) - S) / S else NA_real_   # matched-pairs rank-biserial
    list(test = "Wilcoxona (rangowanych znaków)", stat = unname(w$statistic), df = NA, p = w$p.value,
        est = unname(w$estimate), lower = w$conf.int[1], upper = w$conf.int[2],
        es = r, esLower = NA, esUpper = NA, esLabel = "r rangowo-dwuseryjne")
}

# ---------------------------------------------------------------------------
# Two independent groups
# ---------------------------------------------------------------------------

twoSampleT <- function(y, g, welch = FALSE, hypothesis = "different", level = 0.95) {
    lv <- levels(g); x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
    tt <- stats::t.test(x1, x2, var.equal = !welch, alternative = altR(hypothesis), conf.level = level)
    n1 <- length(x1); n2 <- length(x2)
    sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
    d <- (mean(x1) - mean(x2)) / sp
    ci <- dInterval(d, n1, n2, level)
    list(test = if (welch) "t Welcha" else "t Studenta", stat = unname(tt$statistic), df = unname(tt$parameter),
        p = tt$p.value, est = mean(x1) - mean(x2), lower = tt$conf.int[1], upper = tt$conf.int[2],
        es = d, esLower = ci[1], esUpper = ci[2], esLabel = "d Cohena")
}

mannWhitney <- function(y, g, hypothesis = "different", level = 0.95) {
    lv <- levels(g); x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
    w <- suppressWarnings(stats::wilcox.test(x1, x2, alternative = altR(hypothesis), conf.int = TRUE,
        conf.level = level, exact = FALSE, correct = TRUE))
    U <- unname(w$statistic)
    list(test = "Manna-Whitneya U", stat = U, df = NA, p = w$p.value,
        est = unname(w$estimate), lower = w$conf.int[1], upper = w$conf.int[2],
        es = 1 - 2 * U / (length(x1) * length(x2)), esLower = NA, esUpper = NA, esLabel = "r rangowo-dwuseryjne")
}

leveneTwo <- function(y, g) {
    lev <- car::leveneTest(y, g, center = "median")
    list(F = lev[["F value"]][1], df1 = lev[["Df"]][1], df2 = lev[["Df"]][2], p = lev[["Pr(>F)"]][1])
}

shapiroRow <- function(x) {
    if (length(x) < 3 || length(x) > 5000) return(list(w = NA, p = NA))
    s <- stats::shapiro.test(x); list(w = unname(s$statistic), p = s$p.value)
}

descRow <- function(x) list(n = length(x), mean = mean(x), median = stats::median(x), sd = stats::sd(x),
    se = stats::sd(x) / sqrt(length(x)))

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

accent <- function(theme) if (length(theme$color) >= 2) theme$color[2] else "firebrick"

# state: kind ("one" | "two" | "paired"), groups = list(label = values), est, lower, upper,
# means = list(label = list(mean, lower, upper)), refLine (one sample), level
estimationPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    labs <- names(s$groups)
    raw <- do.call(rbind, lapply(labs, function(l) data.frame(group = l, y = s$groups[[l]], id = seq_along(s$groups[[l]]))))
    means <- do.call(rbind, lapply(labs, function(l) data.frame(group = l, mean = s$means[[l]]$mean,
        lower = s$means[[l]]$lower, upper = s$means[[l]]$upper)))
    col <- theme$color[1]; acc <- accent(theme)
    if (s$kind == "one") {
        raw$group <- factor(raw$group, levels = labs); means$group <- factor(means$group, levels = labs)
        p <- ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y)) +
            ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.45, size = 2, colour = col) +
            ggplot2::geom_hline(yintercept = s$refLine, linetype = "dashed", colour = "grey45") +
            ggplot2::annotate("text", x = 0.5, y = s$refLine, label = sprintf("H₀: %g", s$refLine),
                hjust = 0, vjust = -0.4, size = 3.4, colour = "grey30") +
            ggplot2::geom_errorbar(data = means, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
                width = 0.22, linewidth = 1, colour = acc) +
            ggplot2::geom_point(data = means, ggplot2::aes(x = group, y = mean), inherit.aes = FALSE,
                shape = 18, size = 5, colour = acc) +
            ggplot2::labs(x = NULL, y = s$ylab, subtitle = sprintf("Średnia = %.3g, %g%% CI [%.3g; %.3g]",
                means$mean[1], 100 * s$level, means$lower[1], means$upper[1]))
        return(p + ggtheme)
    }
    # two groups / paired: Gardner-Altman layout, difference aligned to the second group's mean
    ref <- means$mean[2]
    diffLab <- "Różnica"
    lv <- c(labs, diffLab)
    raw$group <- factor(raw$group, levels = lv); means$group <- factor(means$group, levels = lv)
    diffDF <- data.frame(group = factor(diffLab, levels = lv), y = ref + s$est, lower = ref + s$lower, upper = ref + s$upper)
    p <- ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y))
    if (s$kind == "paired") {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = id), colour = "grey70", alpha = 0.7, linewidth = 0.4) +
            ggplot2::geom_point(alpha = 0.55, size = 2, colour = col)
        if (!is.null(s$diffs)) {
            dd <- data.frame(group = factor(diffLab, levels = lv), y = ref + s$diffs)
            p <- p + ggplot2::geom_jitter(data = dd, ggplot2::aes(x = group, y = y), width = 0.1, height = 0,
                alpha = 0.35, size = 1.8, colour = col)
        }
    } else {
        p <- p + ggplot2::geom_jitter(width = 0.14, height = 0, alpha = 0.45, size = 2, colour = col)
    }
    p <- p +
        ggplot2::geom_hline(yintercept = ref, linetype = "dashed", colour = "grey55") +
        ggplot2::geom_errorbar(data = means, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
            width = 0.22, linewidth = 1, colour = acc) +
        ggplot2::geom_point(data = means, ggplot2::aes(x = group, y = mean), inherit.aes = FALSE, shape = 18, size = 5, colour = acc) +
        ggplot2::geom_errorbar(data = diffDF, ggplot2::aes(x = group, ymin = lower, ymax = upper), inherit.aes = FALSE,
            width = 0.22, linewidth = 1.1, colour = acc) +
        ggplot2::geom_point(data = diffDF, ggplot2::aes(x = group, y = y), inherit.aes = FALSE, shape = 18, size = 6, colour = acc) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . - ref,
            name = sprintf("Różnica względem średniej „%s\"", labs[2]))) +
        ggplot2::labs(x = NULL, y = s$ylab,
            subtitle = sprintf("Różnica = %.3g, %g%% CI [%.3g; %.3g]", s$est, 100 * s$level, s$lower, s$upper))
    p + ggtheme
}

boxPlotTwo <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    labs <- names(s$groups)
    raw <- do.call(rbind, lapply(labs, function(l) data.frame(group = l, y = s$groups[[l]])))
    raw$group <- factor(raw$group, levels = labs)
    means <- data.frame(group = factor(labs, levels = labs), mean = vapply(labs, function(l) s$means[[l]]$mean, 1))
    ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_boxplot(width = 0.5, alpha = 0.25, outlier.shape = NA, fill = theme$color[1], colour = "grey30") +
        ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.5, size = 2, colour = theme$color[1]) +
        ggplot2::geom_point(data = means, ggplot2::aes(x = group, y = mean), shape = 18, size = 5, colour = accent(theme)) +
        ggplot2::labs(x = NULL, y = s$ylab, subtitle = "Pudełko: mediana i kwartyle; romb: średnia") + ggtheme
}

qqPlotResid <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    r <- sort(s$x); n <- length(r); z <- (r - mean(r)) / stats::sd(r)
    df <- data.frame(theo = stats::qnorm(stats::ppoints(n)), z = z)
    ggplot2::ggplot(df, ggplot2::aes(x = theo, y = z)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
        ggplot2::geom_point(alpha = 0.7, colour = theme$color[1]) +
        ggplot2::labs(x = "Kwantyle teoretyczne", y = "Standaryzowane wartości", subtitle = s$label) + ggtheme
}

# ---------------------------------------------------------------------------
# Methods description shared by the three analyses (jmvcore::metodyNew)
# ---------------------------------------------------------------------------

# kind: "one" | "two" | "paired"; diffLab: what "difference" means in this analysis
# homog: only ttesttwo has the option (o$homog would throw elsewhere)
metodyWspolne <- function(m, o, kind, diffLab, homog = FALSE) {
    what <- if (kind == "one") "średnia − wartość testowa" else "różnica"
    m$add("Testy", "%s; H₁: %s; przedziały ufności %g%%.", diffLab, altLabel(o$hypothesis, what), o$ciWidth)
    if (isTRUE(o$nonpar))
        m$add("Testy", paste(
            if (kind == "two") "U Manna-Whitneya" else "Test Wilcoxona rangowanych znaków",
            "z przybliżeniem normalnym i poprawką ciągłości; estymator = %s Hodgesa-Lehmanna z przedziałem ufności."),
            if (kind == "two") "przesunięcie" else "pseudomediana")
    m$add("Wielkość efektu", "d Cohena = %s; przedział ufności z rozkładu niecentralnego t.",
          switch(kind, one = "(średnia − wartość testowa) / SD",
                 two = "różnica średnich / łączone SD (Studenta)",
                 paired = "średnia różnica / SD różnic"))
    m$addIf(o$nonpar, "Wielkość efektu", "Dla testu rangowego: r rangowo-dwuseryjne (%s), bez przedziału ufności.",
            if (kind == "two") "1 − 2U / (n₁ n₂)" else "z sumy rang dodatnich i ujemnych")
    m$addIf(o$desc, "Dodatkowe", "Statystyki opisowe: n, średnia, mediana, SD, SE%s.",
            if (kind == "paired") " — dla obu zmiennych i dla różnic" else if (kind == "two") " — osobno w grupach" else "")
    m$addIf(o$norm, "Założenia", "Normalność: test Shapiro-Wilka %s (od 3 do 5000 obserwacji).",
            switch(kind, one = "zmiennej", two = "osobno w każdej grupie", paired = "różnic"))
    m$addIf(homog, "Założenia", "Jednorodność wariancji: test Levene’a na odchyleniach od mediany (car::leveneTest).")
    m$addIf(o$qq, "Założenia", "Wykres Q-Q: kwantyle %s wobec kwantyli rozkładu normalnego.",
            switch(kind, one = "standaryzowanej zmiennej", two = "reszt w grupach (wartości minus średnia grupy)",
                   paired = "standaryzowanych różnic"))
    if (isTRUE(o$plot)) {
        if (kind == "two" && identical(o$plotType, "box"))
            m$add("Wykres", "Pudełkowy z punktami: pudełko = mediana i kwartyle, romb = średnia.")
        else if (kind == "one")
            m$add("Wykres", "Punkty obserwacji, romb = średnia z przedziałem ufności %g%%, linia przerywana = wartość testowa.", o$ciWidth)
        else
            m$add("Wykres", paste(
                "Estymacyjny (Gardner-Altman): punkty%s, romby = średnie z przedziałami ufności %g%%;",
                "różnica z przedziałem na osi po prawej, zakotwiczonej w średniej drugiej grupy%s."),
                if (kind == "paired") " połączone liniami w pary, różnice jako punkty przy „Różnica”" else "",
                o$ciWidth,
                if (kind == "two" && isTRUE(o$welch)) " (przedział różnicy z t Welcha)" else "")
    }
    invisible(m)
}

# ---------------------------------------------------------------------------
# Table helpers shared by the three analyses
# ---------------------------------------------------------------------------

addTestRow <- function(table, key, var, r) {
    table$addRow(rowKey = key, values = list(var = var, test = r$test, stat = r$stat, df = r$df, p = r$p,
        est = r$est, lower = r$lower, upper = r$upper, es = r$es, esLower = r$esLower, esUpper = r$esUpper))
}
