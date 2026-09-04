# jTestyT: shared engine for the three t-test analyses.
# Decision 2026-09-04 (roadmap 1.0.0, item 5): jTestyT does TESTING only —
# statistic, df, p, the difference and Cohen's d as point values. Confidence
# intervals, bootstrap and the Gardner-Altman estimation plot live in jCI.

`%||%` <- function(a, b) if (is.null(a)) b else a
optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

altR <- function(hypothesis) switch(hypothesis, greater = "greater", less = "less", "two.sided")
altLabel <- function(hypothesis, what = "różnica") switch(hypothesis,
    greater = paste(what, "> 0"), less = paste(what, "< 0"), paste(what, "≠ 0"))

# ---------------------------------------------------------------------------
# One sample / paired differences
# ---------------------------------------------------------------------------

oneSampleT <- function(x, mu = 0, hypothesis = "different") {
    tt <- stats::t.test(x, mu = mu, alternative = altR(hypothesis))
    list(test = "t Studenta", stat = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
        est = mean(x) - mu, es = (mean(x) - mu) / stats::sd(x), esLabel = "d Cohena")
}

wilcoxOne <- function(x, mu = 0, hypothesis = "different") {
    w <- suppressWarnings(stats::wilcox.test(x, mu = mu, alternative = altR(hypothesis), conf.int = TRUE, exact = FALSE, correct = TRUE))
    dif <- x - mu; dif <- dif[dif != 0]
    S <- length(dif) * (length(dif) + 1) / 2
    r <- if (S > 0) (2 * unname(w$statistic) - S) / S else NA_real_   # matched-pairs rank-biserial
    list(test = "Wilcoxona (rangowanych znaków)", stat = unname(w$statistic), df = NA, p = w$p.value,
        est = unname(w$estimate), es = r, esLabel = "r rangowo-dwuseryjne")
}

# ---------------------------------------------------------------------------
# Two independent groups
# ---------------------------------------------------------------------------

twoSampleT <- function(y, g, welch = FALSE, hypothesis = "different") {
    lv <- levels(g); x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
    tt <- stats::t.test(x1, x2, var.equal = !welch, alternative = altR(hypothesis))
    n1 <- length(x1); n2 <- length(x2)
    sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
    list(test = if (welch) "t Welcha" else "t Studenta", stat = unname(tt$statistic), df = unname(tt$parameter),
        p = tt$p.value, est = mean(x1) - mean(x2), es = (mean(x1) - mean(x2)) / sp, esLabel = "d Cohena")
}

mannWhitney <- function(y, g, hypothesis = "different") {
    lv <- levels(g); x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
    w <- suppressWarnings(stats::wilcox.test(x1, x2, alternative = altR(hypothesis), conf.int = TRUE, exact = FALSE, correct = TRUE))
    U <- unname(w$statistic)
    list(test = "Manna-Whitneya U", stat = U, df = NA, p = w$p.value, est = unname(w$estimate),
        es = 1 - 2 * U / (length(x1) * length(x2)), esLabel = "r rangowo-dwuseryjne")
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
# Plots: box plot with points and the mean; a dashed reference line for the
# test value (one sample) or zero (paired differences)
# ---------------------------------------------------------------------------

accent <- function(theme) if (length(theme$color) >= 2) theme$color[2] else "firebrick"

# state: groups = list(label = values), ylab, refLine (optional)
boxPlotTests <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    labs <- names(s$groups)
    raw <- do.call(rbind, lapply(labs, function(l) data.frame(group = l, y = s$groups[[l]])))
    raw$group <- factor(raw$group, levels = labs)
    means <- data.frame(group = factor(labs, levels = labs), mean = vapply(labs, function(l) mean(s$groups[[l]]), 1))
    p <- ggplot2::ggplot(raw, ggplot2::aes(x = group, y = y))
    if (!is.null(s$refLine)) p <- p + ggplot2::geom_hline(yintercept = s$refLine, linetype = "dashed", colour = "grey45")
    p + ggplot2::geom_boxplot(width = 0.5, alpha = 0.25, outlier.shape = NA, fill = theme$color[1], colour = "grey30") +
        ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.5, size = 2, colour = theme$color[1]) +
        ggplot2::geom_point(data = means, ggplot2::aes(x = group, y = mean), shape = 18, size = 5, colour = accent(theme)) +
        ggplot2::labs(x = NULL, y = s$ylab, subtitle = paste0("Pudełko: mediana i kwartyle; romb: średnia",
            if (!is.null(s$refLine)) sprintf("; linia przerywana: %s", s$refLabel %||% "H₀") else "")) + ggtheme
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

# kind: "one" | "two" | "paired"; homog: only ttesttwo has the option (o$homog would throw elsewhere)
metodyWspolne <- function(m, o, kind, diffLab, homog = FALSE) {
    what <- if (kind == "one") "średnia − wartość testowa" else "różnica"
    m$add("Testy", "%s; H₁: %s.", diffLab, altLabel(o$hypothesis, what))
    if (isTRUE(o$nonpar))
        m$add("Testy", paste(
            if (kind == "two") "U Manna-Whitneya" else "Test Wilcoxona rangowanych znaków",
            "z przybliżeniem normalnym i poprawką ciągłości; estymator = %s Hodgesa-Lehmanna."),
            if (kind == "two") "przesunięcie" else "pseudomediana")
    m$add("Wielkość efektu", "d Cohena = %s (wartość punktowa).",
          switch(kind, one = "(średnia − wartość testowa) / SD",
                 two = "różnica średnich / łączone SD (Studenta)",
                 paired = "średnia różnica / SD różnic"))
    m$addIf(o$nonpar, "Wielkość efektu", "Dla testu rangowego: r rangowo-dwuseryjne (%s).",
            if (kind == "two") "1 − 2U / (n₁ n₂)" else "z sumy rang dodatnich i ujemnych")
    m$add("Wielkość efektu", "Przedziały ufności dla różnicy i d Cohena, bootstrap i wykres estymacyjny: menu „Przedziały ufności” (%s).",
          switch(kind, one = "Średnia", two = "Różnica średnich", paired = "Średnia różnic"))
    m$addIf(o$desc, "Dodatkowe", "Statystyki opisowe: n, średnia, mediana, SD, SE%s.",
            if (kind == "paired") " — dla obu zmiennych i dla różnic" else if (kind == "two") " — osobno w grupach" else "")
    m$addIf(o$norm, "Założenia", "Normalność: test Shapiro-Wilka %s (od 3 do 5000 obserwacji).",
            switch(kind, one = "zmiennej", two = "osobno w każdej grupie", paired = "różnic"))
    m$addIf(homog, "Założenia", "Jednorodność wariancji: test Levene’a na odchyleniach od mediany (car::leveneTest).")
    m$addIf(o$qq, "Założenia", "Wykres Q-Q: kwantyle %s wobec kwantyli rozkładu normalnego.",
            switch(kind, one = "standaryzowanej zmiennej", two = "reszt w grupach (wartości minus średnia grupy)",
                   paired = "standaryzowanych różnic"))
    m$addIf(o$plot, "Wykres", "Pudełkowy z punktami: pudełko = mediana i kwartyle, romb = średnia%s.",
            switch(kind, one = ", linia przerywana = wartość testowa", paired = "; trzecie pudełko = różnice w parach z linią zera", ""))
    invisible(m)
}

# ---------------------------------------------------------------------------
# Table helpers shared by the three analyses
# ---------------------------------------------------------------------------

addTestRow <- function(table, key, var, r) {
    table$addRow(rowKey = key, values = list(var = var, test = r$test, stat = r$stat, df = r$df, p = r$p, est = r$est, es = r$es))
}
