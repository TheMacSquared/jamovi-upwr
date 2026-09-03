# Wspólny silnik jCzest — czyste funkcje, testowalne bez jamovi.
# Konwencja jak w jANOVA/jRol: logika liczbowa tutaj, .b.R tylko układa wyniki.

#' Czy opcja zmiennej jest ustawiona (jamovi daje NULL albo pusty łańcuch)
optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nzchar(x[1])

#' Tabela kontyngencji z surowych danych; `counts` = zmienna z licznościami
#' (dane zagregowane) albo NULL (jedna obserwacja = jeden wiersz).
buildTable <- function(rows, cols, counts = NULL) {
    ok <- !is.na(rows) & !is.na(cols)
    if (!is.null(counts)) ok <- ok & !is.na(counts) & counts >= 0
    rows <- droplevels(factor(rows[ok])); cols <- droplevels(factor(cols[ok]))
    if (is.null(counts)) return(table(rows, cols))
    tab <- tapply(counts[ok], list(rows, cols), sum)
    tab[is.na(tab)] <- 0
    storage.mode(tab) <- "double"
    as.table(tab)
}

#' Liczebności oczekiwane przy niezależności
expectedCounts <- function(tab) outer(rowSums(tab), colSums(tab)) / sum(tab)

#' Kontrola założenia chi-kwadrat.
#' Reguła Cochrana: wszystkie E >= 1 ORAZ co najwyżej 20% komórek z E < 5.
#' Dla tabel 2x2 wymóg jest ostrzejszy — tam każde E < 5 psuje przybliżenie.
checkAssumption <- function(tab) {
    e <- expectedCounts(tab)
    n <- length(e)
    below5 <- sum(e < 5)
    pct <- 100 * below5 / n
    is2x2 <- all(dim(tab) == c(2, 2))
    ok <- if (is2x2) below5 == 0 else (min(e) >= 1 && pct <= 20)
    list(minExpected = min(e), nBelow5 = below5, nCells = n, pctBelow5 = pct,
         is2x2 = is2x2, ok = ok,
         # Fisher liczy się w rozsądnym czasie tylko dla małych tabel
         fisherFeasible = (prod(dim(tab)) <= 25 && sum(tab) <= 200) || is2x2)
}

#' Test chi-kwadrat niezależności (bez i z poprawką ciągłości)
chiSqTest <- function(tab, correct = FALSE) {
    t <- suppressWarnings(stats::chisq.test(tab, correct = correct))
    list(stat = unname(t$statistic), df = unname(t$parameter), p = unname(t$p.value))
}

#' Test ilorazu wiarygodności (G²)
likeRatTest <- function(tab) {
    e <- expectedCounts(tab)
    nz <- tab > 0
    g <- 2 * sum(tab[nz] * log(tab[nz] / e[nz]))
    df <- (nrow(tab) - 1) * (ncol(tab) - 1)
    list(stat = g, df = df, p = stats::pchisq(g, df, lower.tail = FALSE))
}

#' V Craméra. Dla tabel 2x2 równoważne |phi|.
cramersV <- function(tab) {
    n <- sum(tab)
    if (n == 0) return(NA_real_)
    x2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
    sqrt(unname(x2) / (n * (min(dim(tab)) - 1)))
}

#' Bootstrapowy przedział percentylowy dla V Craméra.
#' Losujemy n obserwacji z rozkładu wielomianowego zadanego tabelą.
cramersVCI <- function(tab, level = 0.95, nBoot = 1000, seed = 1) {
    n <- sum(tab)
    if (n == 0 || any(dim(tab) < 2)) return(c(NA_real_, NA_real_))
    set.seed(seed)
    p <- as.vector(tab) / n
    d <- dim(tab)
    vs <- vapply(seq_len(nBoot), function(i) {
        draw <- stats::rmultinom(1, n, p)
        m <- matrix(draw, nrow = d[1], ncol = d[2])
        if (any(rowSums(m) == 0) || any(colSums(m) == 0)) return(NA_real_)
        cramersV(as.table(m))
    }, numeric(1))
    vs <- vs[is.finite(vs)]
    if (length(vs) < 50) return(c(NA_real_, NA_real_))
    a <- (1 - level) / 2
    unname(stats::quantile(vs, c(a, 1 - a)))
}

#' Skorygowane reszty standaryzowane (adjusted Pearson).
#' Przy prawdziwej niezależności mają w przybliżeniu rozkład N(0,1), więc
#' |z| > z(1-alpha/2) wskazuje komórki decydujące o zależności.
stdResiduals <- function(tab) {
    e <- expectedCounts(tab)
    n <- sum(tab)
    rp <- rowSums(tab) / n
    cp <- colSums(tab) / n
    (tab - e) / sqrt(e * outer(1 - rp, 1 - cp))
}

#' Wartość krytyczna dla reszt przy zadanym alpha (dwustronnie)
residCritical <- function(alpha = 0.05) stats::qnorm(1 - alpha / 2)

#' Miary dla tabel 2x2: iloraz szans, ryzyko względne, różnica proporcji.
#' `compare` = "rows" porównuje wiersze (domyślnie), "cols" kolumny.
twoByTwo <- function(tab, level = 0.95, compare = "rows") {
    if (!all(dim(tab) == c(2, 2))) return(NULL)
    m <- if (compare == "cols") t(tab) else tab
    a <- m[1, 1]; b <- m[1, 2]; c0 <- m[2, 1]; d <- m[2, 2]
    z <- stats::qnorm(1 - (1 - level) / 2)
    or <- (a * d) / (b * c0)
    seLogOr <- sqrt(1/a + 1/b + 1/c0 + 1/d)
    p1 <- a / (a + b); p2 <- c0 / (c0 + d)
    rr <- p1 / p2
    seLogRr <- sqrt(1/a - 1/(a + b) + 1/c0 - 1/(c0 + d))
    dp <- p1 - p2
    seDp <- sqrt(p1 * (1 - p1) / (a + b) + p2 * (1 - p2) / (c0 + d))
    list(
        or = list(est = or, lower = exp(log(or) - z * seLogOr), upper = exp(log(or) + z * seLogOr)),
        rr = list(est = rr, lower = exp(log(rr) - z * seLogRr), upper = exp(log(rr) + z * seLogRr)),
        dp = list(est = dp, lower = dp - z * seDp, upper = dp + z * seDp)
    )
}

#' Miary dla zmiennych porządkowych: gamma Goodmana-Kruskala i tau-b Kendalla
ordinalMeasures <- function(tab) {
    n <- sum(tab); nr <- nrow(tab); nc <- ncol(tab)
    conc <- disc <- 0
    for (i in seq_len(nr)) for (j in seq_len(nc)) {
        if (i < nr && j < nc) conc <- conc + tab[i, j] * sum(tab[(i+1):nr, (j+1):nc])
        if (i < nr && j > 1)  disc <- disc + tab[i, j] * sum(tab[(i+1):nr, 1:(j-1)])
    }
    gamma <- if (conc + disc == 0) NA_real_ else (conc - disc) / (conc + disc)
    tr <- sum(rowSums(tab) * (rowSums(tab) - 1)) / 2
    tc <- sum(colSums(tab) * (colSums(tab) - 1)) / 2
    tot <- n * (n - 1) / 2
    taub <- if (tot <= tr || tot <= tc) NA_real_ else (conc - disc) / sqrt((tot - tr) * (tot - tc))
    list(gamma = gamma, taub = taub, conc = conc, disc = disc)
}

#' Test trendu Cochrana-Armitage'a dla tabeli 2 x k (kategorie uporządkowane).
#' `scores` domyślnie 1..k. Statystyka z, dwustronne p.
cochranArmitage <- function(tab, scores = NULL) {
    if (nrow(tab) != 2) {
        if (ncol(tab) != 2) return(NULL)
        tab <- t(tab)
    }
    k <- ncol(tab)
    if (k < 3) return(NULL)
    if (is.null(scores)) scores <- seq_len(k)
    nj <- colSums(tab); n <- sum(nj)
    xj <- tab[1, ]                       # liczba "sukcesów" w kolumnie
    pbar <- sum(xj) / n
    sbar <- sum(nj * scores) / n
    num <- sum(xj * (scores - sbar))
    den <- pbar * (1 - pbar) * sum(nj * (scores - sbar)^2)
    if (den <= 0) return(NULL)
    z <- num / sqrt(den)
    list(z = z, p = 2 * stats::pnorm(-abs(z)), scores = scores)
}

#' Porównania wielokrotne par kategorii wiersza: dla każdej pary wierszy
#' test chi-kwadrat na podtabeli 2 x nc, p korygowane metodą Holma.
pairwiseRows <- function(tab, method = "holm") {
    lv <- rownames(tab)
    if (length(lv) < 3) return(NULL)
    cmb <- utils::combn(length(lv), 2)
    res <- lapply(seq_len(ncol(cmb)), function(i) {
        sub <- tab[cmb[, i], , drop = FALSE]
        sub <- sub[, colSums(sub) > 0, drop = FALSE]
        if (ncol(sub) < 2) return(list(stat = NA_real_, df = NA_real_, p = NA_real_))
        chiSqTest(sub, correct = FALSE)
    })
    p <- stats::p.adjust(vapply(res, function(r) r$p, numeric(1)), method = method)
    data.frame(
        g1 = lv[cmb[1, ]], g2 = lv[cmb[2, ]],
        stat = vapply(res, function(r) r$stat, numeric(1)),
        df = vapply(res, function(r) r$df, numeric(1)),
        p = p, stringsAsFactors = FALSE
    )
}
