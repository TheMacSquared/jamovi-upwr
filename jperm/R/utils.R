# Permutation test engine and plot utilities for jperm module
# Pure R implementation using sample()/replicate() for didactic clarity

#' Calculate permutation p-value
#' Full enumeration includes the observed arrangement; Monte Carlo uses plus one.
#'
#' @param observed numeric, observed test statistic
#' @param permDist numeric vector, permutation distribution
#' @param hypothesis character, one of "different", "greater", "less"
#' @return numeric p-value
permPValue <- function(observed, permDist, hypothesis) {
    B <- length(permDist)
    count <- sum(atLeastAsExtreme(permDist, observed, hypothesis))

    if (isTRUE(attr(permDist, "exact"))) return(count / B)
    return((count + 1) / (B + 1))
}

#' Which permuted statistics are at least as extreme as the observed one.
#' Compared with a tolerance: an arrangement equal to the observed one in
#' exact arithmetic can differ in the last bits (mean(x) - mu against
#' mean(x - mu), mirrored arrangements), and must still be counted --
#' otherwise an exact test can miss the observed arrangement itself (p = 0)
atLeastAsExtreme <- function(permDist, observed, hypothesis) {
    tol <- 1e-9 * max(abs(observed), abs(permDist), 1e-300)
    switch(hypothesis,
        different = abs(permDist) >= abs(observed) - tol,
        greater  = permDist >= observed - tol,
        less     = permDist <= observed + tol
    )
}

#' Generate permutation distribution for one-sample test
#' H0: mean(x) = mu0, using sign-flip of centered values
#'
#' @param x numeric vector of observations
#' @param mu0 numeric, hypothesized mean
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration of all sign-flips
#' @return numeric vector of permuted test statistics
permDistOneSample <- function(x, mu0, nPerm, seed, exact) {
    centered <- x - mu0
    n <- length(centered)

    if (exact && n <= 20) {
        # All 2^n sign-flip combinations, built by doubling: each value is
        # added with + and with - to every sum so far (n vector steps
        # instead of a 2^n loop: n = 20 took ~6 s per variable)
        sums <- 0
        for (v in centered)
            sums <- c(sums + v, sums - v)
        permDist <- sums / n
        attr(permDist, "exact") <- TRUE
        return(permDist)
    }

    # Monte Carlo approximation
    if (seed != 0) set.seed(seed)

    permDist <- replicate(nPerm, {
        signs <- sample(c(-1, 1), n, replace = TRUE)
        mean(signs * centered)
    })

    attr(permDist, "exact") <- FALSE
    return(permDist)
}

#' Generate permutation distribution for two independent samples
#' H0: mean(group1) = mean(group2), using label shuffling
#'
#' @param x numeric vector of all observations
#' @param group factor with exactly 2 levels
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration
#' @return numeric vector of permuted test statistics
permDistTwoSample <- function(x, group, nPerm, seed, exact) {
    levs <- levels(group)
    n <- length(x)
    n1 <- sum(group == levs[1])

    if (exact && choose(n, n1) <= 100000) {
        # Enumerate all possible group assignments
        combos <- utils::combn(n, n1)
        nAll <- ncol(combos)
        permDist <- numeric(nAll)
        for (i in seq_len(nAll)) {
            idx1 <- combos[, i]
            permDist[i] <- mean(x[idx1]) - mean(x[-idx1])
        }
        attr(permDist, "exact") <- TRUE
        return(permDist)
    }

    # Monte Carlo approximation
    if (seed != 0) set.seed(seed)

    permDist <- replicate(nPerm, {
        shuffled <- sample(group)
        mean(x[shuffled == levs[1]]) - mean(x[shuffled == levs[2]])
    })

    attr(permDist, "exact") <- FALSE
    return(permDist)
}

#' Generate permutation distribution for paired samples
#' Delegates to one-sample sign-flip on differences with mu0 = 0
#'
#' @param d numeric vector of paired differences
#' @param nPerm integer, number of Monte Carlo permutations
#' @param seed integer, random seed (0 = no seed)
#' @param exact logical, use exact enumeration
#' @return numeric vector of permuted test statistics
permDistPaired <- function(d, nPerm, seed, exact) {
    return(permDistOneSample(d, mu0 = 0, nPerm = nPerm, seed = seed, exact = exact))
}

#' Build permutation distribution plot
#' Histogram with observed value (red dashed line) and shaded rejection region
#'
#' @param permDist numeric vector of permutation distribution
#' @param observed numeric, observed test statistic
#' @param hypothesis character, one of "different", "greater", "less"
#' @param ggtheme ggplot2 theme object from jamovi
#' @param theme list with jamovi theme colors
#' @param label optional variable label appended to the x axis title
#' @return ggplot object
buildPermPlot <- function(permDist, observed, hypothesis, ggtheme, theme, label = NULL) {
    df <- data.frame(stat = permDist)

    # Determine rejection region for shading
    df$reject <- atLeastAsExtreme(df$stat, observed, hypothesis)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = stat)) +
        ggplot2::geom_histogram(
            bins = 40,
            fill = theme$fill[2],
            color = theme$color[1],
            alpha = 0.7
        ) +
        ggplot2::geom_histogram(
            data = df[df$reject, , drop = FALSE],
            ggplot2::aes(x = stat),
            bins = 40,
            fill = "firebrick",
            color = theme$color[1],
            alpha = 0.7
        ) +
        ggplot2::geom_vline(
            xintercept = observed,
            linetype = "dashed",
            color = "red",
            linewidth = 1.2
        ) +
        ggplot2::labs(
            x = if (is.null(label)) "Statystyka permutacyjna" else paste0("Statystyka permutacyjna: ", label),
            y = "Liczebność"
        ) +
        ggtheme

    return(p)
}

# ---------------------------------------------------------------------------
# Shared helpers for the three analyses (jUPWR panel/description conventions)
# ---------------------------------------------------------------------------

optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

altLabel <- function(hypothesis, what = "różnica") switch(hypothesis,
    greater = paste(what, "> 0"), less = paste(what, "< 0"), paste(what, "≠ 0"))

# One-sentence note under the table: which exactness applied for this row key
exactNote <- function(table, key, permDist, exact) {
    if (isTRUE(attr(permDist, "exact")))
        table$setNote(paste0("ex", key), sprintf("%s: test dokładny (wszystkie układy).", key))
    else if (isTRUE(exact))
        table$setNote(paste0("ex", key), sprintf("%s: za dużo układów na test dokładny — użyto Monte Carlo.", key))
}

# kind: "one" | "two" | "paired"; limit: description of the exact-enumeration threshold
metodyPerm <- function(m, o, kind, diffLab) {
    m$add("Testy", "%s; statystyka testowa = %s; H₁: %s.", diffLab,
          switch(kind, one = "średnia − wartość testowa", two = "różnica średnich grup", paired = "średnia różnic"),
          altLabel(o$hypothesis, if (kind == "one") "średnia − wartość testowa" else "różnica"))
    m$add("Testy", "Rozkład przy H₀ przez %s%s; b = liczba układów ze statystyką co najmniej tak skrajną jak obserwowana. Pełna enumeracja: p = b / B (układ obserwowany już wliczony); Monte Carlo: p = (b + 1) / (B + 1).",
          if (kind == "two") "losowe przetasowanie etykiet grup" else "losową zmianę znaków odchyleń (sign-flip)",
          if (o$hypothesis == "different") ", dwustronnie po wartości bezwzględnej" else "")
    if (isTRUE(o$exact))
        m$add("Testy", "Test dokładny: wyliczenie wszystkich układów, gdy jest ich najwyżej %s; w przeciwnym razie Monte Carlo jak niżej.",
              if (kind == "two") "100 000 (liczba podziałów na grupy)" else "2²⁰ (n ≤ 20 obserwacji)")
    m$add("Testy", "Monte Carlo: B = %s losowych układów%s.", format(o$nPerm, big.mark = " "),
          if (o$seed > 0) sprintf(", ziarno generatora %d (wynik powtarzalny)", o$seed) else ", bez ustawionego ziarna (wynik zmienia się między uruchomieniami)")
    m$addIf(o$plot, "Wykres", "Histogram rozkładu permutacyjnego statystyki; linia przerywana = wartość obserwowana, słupki zaznaczone = układy co najmniej tak skrajne (obszar liczony do p).")
    invisible(m)
}
