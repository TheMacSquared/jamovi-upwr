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

#' Kontrola założenia chi-kwadrat na podstawie samych liczebności oczekiwanych.
#' Reguła Cochrana: wszystkie E >= 1 ORAZ co najwyżej 20% komórek z E < 5.
#' `strict = TRUE` (tabela 2x2) zaostrza wymóg — tam każde E < 5 psuje przybliżenie.
#' Wspólne dla testu niezależności i testu zgodności — warunek jest ten sam.
checkAssumptionE <- function(e, strict = FALSE) {
    n <- length(e)
    below5 <- sum(e < 5)
    pct <- 100 * below5 / n
    list(minExpected = min(e), nBelow5 = below5, nCells = n, pctBelow5 = pct,
         ok = if (strict) below5 == 0 else (min(e) >= 1 && pct <= 20))
}

#' Kontrola założenia dla tabeli kontyngencji
checkAssumption <- function(tab) {
    is2x2 <- all(dim(tab) == c(2, 2))
    a <- checkAssumptionE(expectedCounts(tab), strict = is2x2)
    a$is2x2 <- is2x2
    # Fisher liczy się w rozsądnym czasie tylko dla małych tabel
    a$fisherFeasible <- (prod(dim(tab)) <= 25 && sum(tab) <= 200) || is2x2
    a
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
#' |z| > 1,96 (czyli alpha = 0,05) wskazuje komórki decydujące o zależności.
stdResiduals <- function(tab) {
    e <- expectedCounts(tab)
    n <- sum(tab)
    rp <- rowSums(tab) / n
    cp <- colSums(tab) / n
    (tab - e) / sqrt(e * outer(1 - rp, 1 - cp))
}

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

# --- test zgodności (jedna zmienna) --------------------------------------

#' Wektor liczności dla jednej zmiennej; `counts` = zmienna z licznościami
#' (dane zagregowane) albo NULL. Zachowuje ZADEKLAROWANE poziomy czynnika,
#' żeby kategoria bez obserwacji nie znikała z tabeli.
gofCounts <- function(x, counts = NULL) {
    if (!is.factor(x)) x <- factor(x)
    ok <- !is.na(x)
    if (!is.null(counts)) ok <- ok & !is.na(counts) & counts >= 0
    x <- x[ok]
    if (is.null(counts)) return(table(x))
    out <- tapply(counts[ok], x, sum)
    out[is.na(out)] <- 0
    storage.mode(out) <- "double"
    as.table(out)
}

#' Liczebności oczekiwane z proporcji. `ratio` to wagi (niekoniecznie sumujące
#' się do 1) — NULL oznacza rozkład równomierny.
gofExpected <- function(obs, ratio = NULL) {
    k <- length(obs); n <- sum(obs)
    if (is.null(ratio) || length(ratio) == 0) ratio <- rep(1, k)
    if (length(ratio) != k) return(NULL)
    ratio <- as.numeric(ratio)
    if (any(!is.finite(ratio)) || any(ratio < 0) || sum(ratio) <= 0) return(NULL)
    e <- n * ratio / sum(ratio)
    stats::setNames(e, names(obs))
}

#' Test chi-kwadrat zgodności
chiSqGof <- function(obs, e) {
    stat <- sum((obs - e)^2 / e)
    df <- length(obs) - 1
    list(stat = stat, df = df, p = stats::pchisq(stat, df, lower.tail = FALSE))
}

#' Dokładny test dwumianowy dla dwóch kategorii; p0 = oczekiwana proporcja
#' PIERWSZEJ kategorii. `hypothesis`: notequal / greater / less.
binomGof <- function(obs, p0, hypothesis = "notequal") {
    alt <- switch(hypothesis, greater = "greater", less = "less", "two.sided")
    bt <- stats::binom.test(as.integer(obs[1]), as.integer(sum(obs)), p = p0, alternative = alt)
    list(stat = unname(bt$statistic), n = unname(bt$parameter), p = bt$p.value,
         estimate = unname(bt$estimate), p0 = p0)
}

#' w Cohena — wielkość efektu dla testu zgodności (w = sqrt(chi2 / n)).
#' Progi Cohena: 0.1 slaby, 0.3 umiarkowany, 0.5 silny (niezależne od k).
cohensW <- function(obs, e) {
    n <- sum(obs)
    if (n == 0) return(NA_real_)
    sqrt(sum((obs - e)^2 / e) / n)
}

#' Skorygowane reszty standaryzowane dla testu zgodności.
#' (O - E) / sqrt(E (1 - p)) ma w przybliżeniu rozkład N(0,1),
#' więc wskazuje kategorie odstające od oczekiwań.
gofResiduals <- function(obs, e) {
    n <- sum(obs)
    p <- e / n
    stats::setNames(as.vector((obs - e) / sqrt(e * (1 - p))), names(obs))
}

#' Dokładny test wielomianowy: sumuje prawdopodobieństwa wszystkich układów
#' nie bardziej prawdopodobnych niż zaobserwowany. Wykonalny tylko dla małych
#' n i k — zwraca NULL, gdy przestrzeń układów jest za duża.
multinomExact <- function(obs, e, maxStates = 2e5) {
    n <- as.integer(sum(obs)); k <- length(obs)
    if (k < 2) return(NULL)
    if (choose(n + k - 1, k - 1) > maxStates) return(NULL)
    p <- e / sum(e)
    obsLogP <- stats::dmultinom(as.integer(obs), n, p, log = TRUE)
    total <- 0
    # rekurencyjne wyliczenie wszystkich kompozycji n na k części
    walk <- function(remaining, idx, acc) {
        if (idx == k) {
            cand <- c(acc, remaining)
            lp <- stats::dmultinom(cand, n, p, log = TRUE)
            if (lp <= obsLogP + 1e-9) total <<- total + exp(lp)
            return(invisible(NULL))
        }
        for (v in 0:remaining) walk(remaining - v, idx + 1, c(acc, v))
    }
    walk(n, 1, integer(0))
    list(p = min(1, total), nStates = choose(n + k - 1, k - 1))
}

# --- próby zależne: McNemar i Q Cochrana ---------------------------------

#' Tabela par dla dwóch pomiarów tej samej jednostki (wide).
#' Poziomy scalane, żeby tabela była kwadratowa nawet gdy jeden pomiar
#' nie zawiera którejś kategorii.
pairedTable <- function(v1, v2, counts = NULL) {
    lv <- union(levels(factor(v1)), levels(factor(v2)))
    f1 <- factor(v1, levels = lv); f2 <- factor(v2, levels = lv)
    ok <- !is.na(f1) & !is.na(f2)
    if (!is.null(counts)) ok <- ok & !is.na(counts) & counts >= 0
    f1 <- f1[ok]; f2 <- f2[ok]
    if (is.null(counts)) return(table(f1, f2))
    tab <- tapply(counts[ok], list(f1, f2), sum)
    tab[is.na(tab)] <- 0
    storage.mode(tab) <- "double"
    dimnames(tab) <- list(lv, lv)
    as.table(tab)
}

#' Test McNemara na parach niezgodnych (tylko tabele 2x2).
#' `correct` = poprawka ciągłości Yatesa.
mcnemar <- function(tab, correct = FALSE) {
    if (!all(dim(tab) == c(2, 2))) return(NULL)
    b <- tab[1, 2]; c0 <- tab[2, 1]
    disc <- b + c0
    if (disc == 0) return(list(stat = NA_real_, df = 1L, p = NA_real_, discordant = 0))
    num <- abs(b - c0)
    if (correct) num <- max(0, num - 1)
    stat <- num^2 / disc
    list(stat = stat, df = 1L, p = stats::pchisq(stat, 1, lower.tail = FALSE),
         discordant = disc, b = b, c = c0)
}

#' Dokładny McNemar: rozkład dwumianowy na parach niezgodnych.
#' Właściwy wybór, gdy par niezgodnych jest mało (przybliżenie χ² zawodzi).
mcnemarExact <- function(tab) {
    if (!all(dim(tab) == c(2, 2))) return(NULL)
    b <- as.integer(tab[1, 2]); c0 <- as.integer(tab[2, 1])
    if (b + c0 == 0) return(list(p = NA_real_, discordant = 0))
    list(p = stats::binom.test(b, b + c0, 0.5)$p.value, discordant = b + c0)
}

#' Iloraz szans dla par niezgodnych (OR McNemara) z przedziałem ufności.
#' To jedyna sensowna miara efektu dla par — jmv jej nie podaje.
mcnemarOR <- function(tab, level = 0.95) {
    if (!all(dim(tab) == c(2, 2))) return(NULL)
    b <- tab[1, 2]; c0 <- tab[2, 1]
    if (b == 0 || c0 == 0) return(list(est = NA_real_, lower = NA_real_, upper = NA_real_))
    z <- stats::qnorm(1 - (1 - level) / 2)
    or <- b / c0
    se <- sqrt(1 / b + 1 / c0)
    list(est = or, lower = exp(log(or) - z * se), upper = exp(log(or) + z * se))
}

#' Warunek stosowalności McNemara: przybliżenie χ² wymaga dostatecznej liczby
#' par NIEZGODNYCH (reguła kciuka: b + c >= 25). Poniżej — test dokładny.
checkMcnemar <- function(tab) {
    m <- mcnemar(tab)
    if (is.null(m)) return(NULL)
    list(discordant = m$discordant, ok = m$discordant >= 25)
}

#' Q Cochrana dla k pomiarów binarnych na tych samych jednostkach.
#' `mat` = macierz 0/1 (wiersze = jednostki, kolumny = pomiary), bez braków.
#' Dla k = 2 równoważne testowi McNemara bez poprawki.
cochranQ <- function(mat) {
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]
    k <- ncol(mat); n <- nrow(mat)
    if (k < 2 || n == 0) return(NULL)
    Gj <- colSums(mat); Li <- rowSums(mat)
    denom <- k * sum(Li) - sum(Li^2)
    if (denom == 0) return(list(stat = NA_real_, df = k - 1L, p = NA_real_, n = n))
    stat <- (k - 1) * (k * sum(Gj^2) - sum(Gj)^2) / denom
    list(stat = stat, df = k - 1L, p = stats::pchisq(stat, k - 1, lower.tail = FALSE),
         n = n, props = Gj / n)
}

#' Post-hoc do Q Cochrana: McNemar dla każdej pary pomiarów, p korygowane Holmem.
pairwiseMcnemar <- function(mat, names_ = NULL, method = "holm") {
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]
    k <- ncol(mat)
    if (k < 3) return(NULL)
    if (is.null(names_)) names_ <- colnames(mat)
    cmb <- utils::combn(k, 2)
    res <- lapply(seq_len(ncol(cmb)), function(i) {
        a <- mat[, cmb[1, i]]; b <- mat[, cmb[2, i]]
        n01 <- sum(a == 0 & b == 1); n10 <- sum(a == 1 & b == 0)
        if (n01 + n10 == 0)
            return(list(stat = NA_real_, p = NA_real_, disc = 0,
                        or = NA_real_, lower = NA_real_, upper = NA_real_))
        st <- (n01 - n10)^2 / (n01 + n10)
        # OR par niezgodnych dla tej pary — ta sama miara co przy dwoch pomiarach,
        # dzieki czemu wielkosc efektu jest dostepna takze przy k >= 3
        z <- stats::qnorm(0.975)
        or <- if (n01 > 0 && n10 > 0) n10 / n01 else NA_real_
        se <- if (is.finite(or)) sqrt(1 / n01 + 1 / n10) else NA_real_
        list(stat = st, p = stats::pchisq(st, 1, lower.tail = FALSE), disc = n01 + n10,
             or = or,
             lower = if (is.finite(or)) exp(log(or) - z * se) else NA_real_,
             upper = if (is.finite(or)) exp(log(or) + z * se) else NA_real_)
    })
    data.frame(
        g1 = names_[cmb[1, ]], g2 = names_[cmb[2, ]],
        stat = vapply(res, function(r) r$stat, numeric(1)),
        disc = vapply(res, function(r) r$disc, numeric(1)),
        p = stats::p.adjust(vapply(res, function(r) r$p, numeric(1)), method = method),
        or = vapply(res, function(r) r$or, numeric(1)),
        lower = vapply(res, function(r) r$lower, numeric(1)),
        upper = vapply(res, function(r) r$upper, numeric(1)),
        stringsAsFactors = FALSE
    )
}
