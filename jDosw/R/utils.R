# jDosw: shared engine for classical experimental designs.
#
# Design of this file:
#   fitDesign()        -> ANOVA table with the correct error strata per design
#                         plus, for every term that can be compared, the group
#                         means and an errFun(g1, g2) returning the MSE/df that a
#                         pairwise comparison between g1 and g2 should use.
#   pairwiseTable()    -> all pairwise comparisons for one term (any method).
#   cldLetters()       -> compact letter display (insert-absorb, Piepho 2004),
#                         'a' goes to the group holding the smallest mean.
#   runDesignAnalysis()-> glue between the R6 analysis class and the engine.
#   plan*()            -> randomisation of a field plan.

# ---------------------------------------------------------------------------
# Labels
# ---------------------------------------------------------------------------

phMethodLabel <- function(method) {
    switch(method,
        tukey   = "test Tukeya (HSD)",
        lsd     = "NIR (test t Fishera, LSD)",
        scheffe = "test Scheffégo",
        dunnett = "test Dunnetta (vs kontrola)",
        holm    = "test t z poprawką Holma",
        none    = "",
        method)
}

phCritLabel <- function(method) {
    switch(method,
        tukey   = "HSD",
        lsd     = "NIR",
        scheffe = "różnica graniczna Scheffégo",
        dunnett = "różnica graniczna Dunnetta",
        holm    = "",
        "")
}

cellLabel <- function(a, b) paste(a, b, sep = " × ")

# ---------------------------------------------------------------------------
# ANOVA per design
# ---------------------------------------------------------------------------

# Sequential (type I) ANOVA of a fitted lm, rows renamed to Polish labels.
# `labels` maps model terms to display names; `errorRows` marks rows that are
# error strata (kept, renamed) rather than tested effects.
anovaTable <- function(fit, labels) {
    an <- stats::anova(fit)
    src <- rownames(an)
    out <- data.frame(
        term   = src,
        source = ifelse(src %in% names(labels), labels[src], src),
        ss     = an[["Sum Sq"]],
        df     = an[["Df"]],
        ms     = an[["Mean Sq"]],
        F      = an[["F value"]],
        p      = an[["Pr(>F)"]],
        stringsAsFactors = FALSE)
    out$source[src == "Residuals"] <- "Błąd"
    out$term[src == "Residuals"] <- "error"
    out
}

addTotalRow <- function(tab) {
    rbind(tab, data.frame(term = "total", source = "Ogółem",
        ss = sum(tab$ss), df = sum(tab$df), ms = NA, F = NA, p = NA, err = "",
        stringsAsFactors = FALSE))
}

groupStats <- function(y, g) {
    g <- droplevels(g)
    levs <- levels(g)
    list(
        levels = levs,
        means  = tapply(y, g, mean)[levs],
        ns     = as.numeric(table(g)[levs]),
        sds    = tapply(y, g, sd)[levs])
}

constErr <- function(mse, df) function(g1, g2) list(mse = mse, df = df)

# Returns list(anova, terms, resid, fitted, note, balanced)
# terms: named list; each has label, kind ("A","B","AB"), stats (groupStats),
#        errFun, and for AB the factor levels for plotting.
fitDesign <- function(design, d, dep, A, B = NULL, block = NULL,
                      row = NULL, col = NULL) {
    y <- d[[dep]]
    labels <- c()
    terms <- list()
    note <- NULL

    if (design == "crd") {
        if (is.null(B)) {
            f <- stats::as.formula(paste0("`", dep, "` ~ `", A, "`"))
        } else {
            f <- stats::as.formula(paste0("`", dep, "` ~ `", A, "` * `", B, "`"))
        }
    } else if (design == "rcbd") {
        rhs <- if (is.null(B)) paste0("`", A, "`") else paste0("`", A, "` * `", B, "`")
        f <- stats::as.formula(paste0("`", dep, "` ~ `", block, "` + ", rhs))
        labels[block] <- "Bloki"
    } else if (design == "latin") {
        f <- stats::as.formula(paste0("`", dep, "` ~ `", row, "` + `", col, "` + `", A, "`"))
        labels[row] <- "Wiersze"
        labels[col] <- "Kolumny"
    } else if (design == "splitplot") {
        # block:A is the whole-plot error (error a); Residuals is error b
        f <- stats::as.formula(paste0("`", dep, "` ~ `", block, "` + `", A, "` + `",
            block, "`:`", A, "` + `", B, "` + `", A, "`:`", B, "`"))
        labels[block] <- "Bloki"
        labels[paste0(block, ":", A)] <- "Błąd (a)"
    }
    labels[A] <- A
    if (!is.null(B)) {
        labels[B] <- B
        labels[paste0(A, ":", B)] <- cellLabel(A, B)
    }

    fit <- stats::lm(f, data = d)
    tab <- anovaTable(fit, labels)

    # balance check: every treatment cell (and block) has the same count
    cellsF <- if (is.null(B)) d[[A]] else interaction(d[[A]], d[[B]], sep = " × ")
    balanced <- length(unique(table(cellsF))) == 1
    if (!is.null(block))
        balanced <- balanced && length(unique(table(cellsF, d[[block]]))) == 1

    resRow <- tab$term == "error"
    mse <- tab$ms[resRow]
    dfe <- tab$df[resRow]

    if (design == "splitplot") {
        eaRow <- tab$term == paste0(block, ":", A)
        msea <- tab$ms[eaRow]
        dfea <- tab$df[eaRow]
        tab$source[resRow] <- "Błąd (b)"
        tab$term[resRow] <- "errorB"
        tab$term[eaRow] <- "errorA"
        # A is tested against error a, not the residual
        tab$F[tab$term == A] <- tab$ms[tab$term == A] / msea
        tab$p[tab$term == A] <- stats::pf(tab$F[tab$term == A], tab$df[tab$term == A],
            dfea, lower.tail = FALSE)
        tab$F[tab$term == block] <- tab$ms[tab$term == block] / msea
        tab$p[tab$term == block] <- stats::pf(tab$F[tab$term == block],
            tab$df[tab$term == block], dfea, lower.tail = FALSE)
        tab$F[eaRow] <- NA
        tab$p[eaRow] <- NA
        tab$err <- ""
        tab$err[tab$term %in% c(block, A)] <- "a"
        tab$err[tab$term %in% c(B, paste0(A, ":", B))] <- "b"
        # lm() lists main effects before interactions; show the strata in
        # textbook order instead
        tab <- tab[match(c(block, A, "errorA", B, paste0(A, ":", B), "errorB"), tab$term), ]
        rownames(tab) <- NULL
        nB <- nlevels(droplevels(d[[B]]))
        # A levels compared at the same or different level of B: combined error
        # (classic split-plot formula with Satterthwaite df)
        msComb <- ((nB - 1) * mse + msea) / nB
        dfComb <- ((nB - 1) * mse + msea)^2 /
            (((nB - 1) * mse)^2 / dfe + msea^2 / dfea)
        terms$A <- list(label = A, kind = "A", stats = groupStats(y, d[[A]]),
            errFun = constErr(msea, dfea))
        terms$B <- list(label = B, kind = "B", stats = groupStats(y, d[[B]]),
            errFun = constErr(mse, dfe))
        cellsA <- as.character(d[[A]])
        st <- groupStats(y, cellsF)
        cellA <- vapply(strsplit(st$levels, " × ", fixed = TRUE), `[`, "", 1)
        names(cellA) <- st$levels
        terms$AB <- list(label = cellLabel(A, B), kind = "AB", stats = st,
            factorA = cellA,
            factorB = vapply(strsplit(st$levels, " × ", fixed = TRUE), `[`, "", 2),
            errFun = function(g1, g2) {
                if (cellA[[g1]] == cellA[[g2]]) list(mse = mse, df = dfe)
                else list(mse = msComb, df = dfComb)
            })
        note <- sprintf(paste0("Czynnik A (%s) testowany wobec błędu (a), ",
            "czynnik B (%s) i interakcja wobec błędu (b). Porównania ",
            "poziomów A przy różnych poziomach B używają ",
            "błędu łączonego (df Satterthwaite'a = %.1f)."),
            A, B, dfComb)
    } else {
        tab$err <- ""
        terms$A <- list(label = A, kind = "A", stats = groupStats(y, d[[A]]),
            errFun = constErr(mse, dfe))
        if (!is.null(B)) {
            terms$B <- list(label = B, kind = "B", stats = groupStats(y, d[[B]]),
                errFun = constErr(mse, dfe))
            st <- groupStats(y, cellsF)
            terms$AB <- list(label = cellLabel(A, B), kind = "AB", stats = st,
                factorA = vapply(strsplit(st$levels, " × ", fixed = TRUE), `[`, "", 1),
                factorB = vapply(strsplit(st$levels, " × ", fixed = TRUE), `[`, "", 2),
                errFun = constErr(mse, dfe))
        }
    }

    tab <- addTotalRow(tab)
    list(anova = tab, terms = terms, resid = stats::residuals(fit),
        fitted = stats::fitted(fit), fit = fit, note = note, balanced = balanced,
        mse = mse, dfe = dfe)
}

# ---------------------------------------------------------------------------
# Pairwise comparisons
# ---------------------------------------------------------------------------

dunnettCorr <- function(ns, n0) {
    k <- length(ns)
    R <- matrix(0, k, k)
    for (i in seq_len(k)) for (j in seq_len(k))
        R[i, j] <- if (i == j) 1 else 1 / sqrt((1 + n0 / ns[i]) * (1 + n0 / ns[j]))
    R
}

# means, ns: named by level. errFun(g1, g2) -> list(mse, df).
# Returns data.frame(g1, g2, diff, se, df, stat, p, crit, lower, upper, sig)
pairwiseTable <- function(means, ns, errFun, method, alpha, control = NULL) {
    levs <- names(means)
    k <- length(levs)
    if (method == "dunnett") {
        if (is.null(control) || !(control %in% levs)) control <- levs[1]
        others <- setdiff(levs, control)
        pairs <- cbind(others, rep(control, length(others)))
    } else {
        pairs <- t(utils::combn(levs, 2))
    }
    n <- nrow(pairs)
    out <- data.frame(g1 = pairs[, 1], g2 = pairs[, 2], diff = NA_real_,
        se = NA_real_, df = NA_real_, stat = NA_real_, p = NA_real_,
        crit = NA_real_, lower = NA_real_, upper = NA_real_,
        stringsAsFactors = FALSE)
    for (i in seq_len(n)) {
        g1 <- pairs[i, 1]; g2 <- pairs[i, 2]
        e <- errFun(g1, g2)
        se <- sqrt(e$mse * (1 / ns[[g1]] + 1 / ns[[g2]]))
        out$diff[i] <- means[[g1]] - means[[g2]]
        out$se[i] <- se
        out$df[i] <- e$df
        out$stat[i] <- out$diff[i] / se
    }
    t <- out$stat; df <- out$df; se <- out$se
    if (method == "lsd") {
        out$p <- 2 * stats::pt(-abs(t), df)
        out$crit <- stats::qt(1 - alpha / 2, df) * se
    } else if (method == "tukey") {
        out$p <- stats::ptukey(abs(t) * sqrt(2), k, df, lower.tail = FALSE)
        out$crit <- stats::qtukey(1 - alpha, k, df) / sqrt(2) * se
    } else if (method == "scheffe") {
        out$p <- stats::pf(t^2 / (k - 1), k - 1, df, lower.tail = FALSE)
        out$crit <- sqrt((k - 1) * stats::qf(1 - alpha, k - 1, df)) * se
    } else if (method == "holm") {
        out$p <- stats::p.adjust(2 * stats::pt(-abs(t), df), method = "holm")
    } else if (method == "dunnett") {
        R <- dunnettCorr(ns[out$g1], ns[[control]])
        dfi <- round(df[1])
        for (i in seq_len(n)) {
            out$p[i] <- 1 - mvtnorm::pmvt(lower = rep(-abs(t[i]), n),
                upper = rep(abs(t[i]), n), df = dfi, corr = R)[1]
        }
        q <- mvtnorm::qmvt(1 - alpha, tail = "both.tails", df = dfi, corr = R)$quantile
        out$crit <- q * se
    }
    if (method != "holm") {
        out$lower <- out$diff - out$crit
        out$upper <- out$diff + out$crit
    }
    out$sig <- !is.na(out$p) & out$p < alpha
    out
}

# Compact letter display. `levs` in the order letters should be assigned
# (ascending mean -> 'a' for the smallest). `sigPairs` data.frame(g1, g2, sig).
cldLetters <- function(levs, sigPairs) {
    k <- length(levs)
    cols <- list(rep(TRUE, k))              # one group holding everybody
    sp <- sigPairs[sigPairs$sig, c("g1", "g2"), drop = FALSE]
    for (i in seq_len(nrow(sp))) {
        a <- match(sp$g1[i], levs); b <- match(sp$g2[i], levs)
        newCols <- list()
        for (col in cols) {
            if (col[a] && col[b]) {
                c1 <- col; c1[a] <- FALSE
                c2 <- col; c2[b] <- FALSE
                newCols <- c(newCols, list(c1), list(c2))
            } else {
                newCols <- c(newCols, list(col))
            }
        }
        # absorb: drop columns contained in another column, and duplicates
        keep <- rep(TRUE, length(newCols))
        for (i1 in seq_along(newCols)) for (i2 in seq_along(newCols)) {
            if (i1 == i2 || !keep[i1] || !keep[i2]) next
            if (all(newCols[[i1]] <= newCols[[i2]])) {
                if (all(newCols[[i1]] == newCols[[i2]]) && i1 < i2) next
                keep[i1] <- FALSE
            }
        }
        cols <- newCols[keep]
    }
    # order groups by their first member (smallest mean gets 'a')
    firsts <- vapply(cols, function(c) which(c)[1], 1L)
    cols <- cols[order(firsts)]
    letters <- vapply(seq_len(k), function(i) {
        paste(letters[which(vapply(cols, function(c) c[i], TRUE))], collapse = "")
    }, "")
    names(letters) <- levs
    letters
}

# Runs comparisons for one term; returns list(pairs, means(data.frame), crit note)
compareTerm <- function(term, method, alpha, control = NULL) {
    st <- term$stats
    means <- st$means; ns <- st$ns; names(ns) <- st$levels
    pairs <- pairwiseTable(means, ns, term$errFun, method, alpha, control)
    ord <- st$levels[order(means)]
    if (method == "dunnett") {
        if (is.null(control) || !(control %in% st$levels)) control <- st$levels[1]
        mark <- rep("", length(st$levels)); names(mark) <- st$levels
        mark[control] <- "(kontrola)"
        sigVs <- pairs$g1[pairs$sig]
        mark[sigVs] <- "*"
        letters <- mark
    } else {
        letters <- cldLetters(ord, pairs)[st$levels]
    }
    se <- vapply(st$levels, function(g) sqrt(term$errFun(g, g)$mse / ns[[g]]), 1)
    tab <- data.frame(level = st$levels, n = ns, mean = as.numeric(means),
        se = se, sd = as.numeric(st$sds), letters = letters,
        stringsAsFactors = FALSE)
    critNote <- NULL
    if (method %in% c("tukey", "lsd", "scheffe", "dunnett")) {
        if (length(unique(round(pairs$crit, 10))) == 1) {
            critNote <- sprintf("%s = %.4g (α = %g)", phCritLabel(method), pairs$crit[1], alpha)
        } else {
            critNote <- sprintf(paste0("%s różni się między parami ",
                "(nierówne liczebności lub różne błędy); ",
                "wartości w tabeli par (α = %g)"), phCritLabel(method), alpha)
        }
    } else if (method == "holm") {
        critNote <- sprintf("p skorygowane metodą Holma; α = %g", alpha)
    }
    list(pairs = pairs, means = tab, critNote = critNote)
}

# ---------------------------------------------------------------------------
# Glue: shared .run for the four design analyses
# ---------------------------------------------------------------------------

optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0

# Option lookup that tolerates options a given design does not declare
# (e.g. the Latin square has no factor B), since options$x errors when missing.
optGet <- function(opts, name) if (exists(name, envir = opts)) opts[[name]] else NULL

runDesignAnalysis <- function(self, design) {
    opts <- self$options
    dep <- opts$dep; A <- opts$factorA
    B <- if (design %in% c("crd", "rcbd", "splitplot") && optNonEmpty(optGet(opts, "factorB"))) opts$factorB else NULL
    block <- if (design %in% c("rcbd", "splitplot")) optGet(opts, "block") else NULL
    row <- if (design == "latin") optGet(opts, "row") else NULL
    col <- if (design == "latin") optGet(opts, "col") else NULL

    needed <- c(dep, A, B, block, row, col)
    if (any(!vapply(list(dep, A), optNonEmpty, TRUE))) return()
    if (design %in% c("rcbd", "splitplot") && !optNonEmpty(block)) return()
    if (design == "splitplot" && !optNonEmpty(B)) return()
    if (design == "latin" && (!optNonEmpty(row) || !optNonEmpty(col))) return()

    d <- self$data[needed]
    d[[dep]] <- jmvcore::toNumeric(d[[dep]])
    for (v in setdiff(needed, dep)) d[[v]] <- factor(d[[v]])
    d <- d[stats::complete.cases(d), , drop = FALSE]
    for (v in setdiff(needed, dep)) d[[v]] <- droplevels(d[[v]])
    if (nrow(d) < 3) return()
    if (nlevels(d[[A]]) < 2) {
        self$results$anova$setNote("err", "Czynnik musi mieć co najmniej 2 poziomy.")
        return()
    }
    if (design == "latin") {
        k <- nlevels(d[[A]])
        if (nlevels(d[[row]]) != k || nlevels(d[[col]]) != k || nrow(d) != k * k) {
            self$results$anova$setNote("err", paste0("Kwadrat łaciński wymaga tej samej ",
                "liczby poziomów czynnika, wierszy i kolumn (k) oraz k² obserwacji."))
            return()
        }
    }

    res <- tryCatch(fitDesign(design, d, dep, A, B, block, row, col),
        error = function(e) e)
    if (inherits(res, "error")) {
        self$results$anova$setNote("err", paste("Błąd dopasowania modelu:",
            conditionMessage(res)))
        return()
    }
    if (is.na(res$mse) || res$dfe < 1) {
        self$results$anova$setNote("err", paste0("Brak stopni swobody dla błędu ",
            "(za mało powtórzeń)."))
        return()
    }

    # --- ANOVA table
    at <- self$results$anova
    for (i in seq_len(nrow(res$anova))) {
        r <- res$anova[i, ]
        at$addRow(rowKey = r$term, values = list(source = r$source, ss = r$ss,
            df = r$df, ms = r$ms, F = r$F, p = r$p, err = r$err))
    }
    if (!is.null(res$note)) at$setNote("strata", res$note)
    if (!res$balanced)
        at$setNote("unbal", paste0("Układ niezrównoważony: sumy kwadratów ",
            "sekwencyjne (typ I) w kolejności wierszy tabeli; średnie w tabelach ",
            "porównań są średnimi arytmetycznymi obserwacji."))
    if (design == "splitplot" && !res$balanced)
        at$setNote("unbalsp", paste0("Split-plot zakłada układ zrównoważony; ",
            "wyniki dla danych niekompletnych są przybliżone."))

    # --- comparisons
    method <- opts$postHoc
    alpha <- opts$alpha
    which <- c(A = isTRUE(opts$phA), B = isTRUE(optGet(opts, "phB")) && !is.null(B),
        AB = isTRUE(optGet(opts, "phAB")) && !is.null(B))
    ctrlB <- optGet(opts, "controlB")
    controls <- list(A = if (optNonEmpty(opts$controlA)) as.character(opts$controlA) else NULL,
        B = if (optNonEmpty(ctrlB)) as.character(ctrlB) else NULL,
        AB = NULL)
    eb <- opts$errorBars
    for (key in c("A", "B", "AB")) {
        used <- which[[key]] && !is.null(res$terms[[key]])
        self$results$means$get(key = key)$setVisible(used)
        self$results$pairs$get(key = key)$setVisible(used)
        self$results$plotMeans$get(key = key)$setVisible(used)
        if (!used) next
        term <- res$terms[[key]]
        mt <- self$results$means$get(key = key)
        pt <- self$results$pairs$get(key = key)
        img <- self$results$plotMeans$get(key = key)
        mt$setTitle(sprintf("Średnie: %s", term$label))
        pt$setTitle(sprintf("Porównania parami: %s", term$label))
        if (method == "none") {
            st <- term$stats
            for (i in seq_along(st$levels)) {
                g <- st$levels[i]
                mt$addRow(rowKey = g, values = list(level = g, n = st$ns[i],
                    mean = as.numeric(st$means[i]),
                    se = sqrt(term$errFun(g, g)$mse / st$ns[i]), letters = ""))
            }
            mt$getColumn("letters")$setVisible(FALSE)
            cmpMeans <- data.frame(level = st$levels, mean = as.numeric(st$means),
                se = vapply(st$levels, function(g) sqrt(term$errFun(g, g)$mse / st$ns[which(st$levels == g)]), 1),
                sd = as.numeric(st$sds), letters = "", stringsAsFactors = FALSE)
            cmpDf <- vapply(st$levels, function(g) term$errFun(g, g)$df, 1)
        } else {
            ctrl <- controls[[key]]
            if (method == "dunnett" && key == "AB") ctrl <- term$stats$levels[1]
            cmp <- compareTerm(term, method, alpha, ctrl)
            for (i in seq_len(nrow(cmp$means))) {
                r <- cmp$means[i, ]
                mt$addRow(rowKey = r$level, values = list(level = r$level, n = r$n,
                    mean = r$mean, se = r$se, letters = r$letters))
            }
            if (method == "dunnett") {
                mt$getColumn("letters")$setTitle("vs kontrola")
                mt$setNote("dun", paste0("* różni się istotnie od kontroli; ",
                    cmp$critNote))
            } else {
                mt$setNote("cld", paste0(phMethodLabel(method), "; ", cmp$critNote,
                    ". Poziomy z tą samą literą nie różnią się ",
                    "istotnie; litera a = grupa z najniższą średnią."))
            }
            for (i in seq_len(nrow(cmp$pairs))) {
                r <- cmp$pairs[i, ]
                pt$addRow(rowKey = i, values = list(g1 = r$g1, g2 = r$g2, diff = r$diff,
                    se = r$se, df = r$df, stat = r$stat, p = r$p, crit = r$crit,
                    lower = r$lower, upper = r$upper))
            }
            if (method == "holm") {
                pt$getColumn("crit")$setVisible(FALSE)
                pt$getColumn("lower")$setVisible(FALSE)
                pt$getColumn("upper")$setVisible(FALSE)
                pt$setNote("holm", "p skorygowane metodą Holma.")
            } else {
                pt$setNote("crit", sprintf(paste0("%s; przedział ufności = różnica ",
                    "± %s (poziom %g%%)."), phMethodLabel(method), phCritLabel(method),
                    100 * (1 - alpha)))
            }
            cmpMeans <- cmp$means
            cmpDf <- vapply(cmpMeans$level, function(g) term$errFun(g, g)$df, 1)
        }
        # plot state
        cmpMeans$df <- as.numeric(cmpDf)
        cmpMeans$factorA <- if (key == "AB") term$factorA else cmpMeans$level
        cmpMeans$factorB <- if (key == "AB") term$factorB else NA
        img$setState(list(means = cmpMeans, key = key, label = term$label,
            dep = dep, alpha = alpha, errorBars = eb, A = A, B = B))
    }

    # --- diagnostics
    cells <- if (is.null(B)) d[[A]] else interaction(d[[A]], d[[B]], sep = " × ")
    if (isTRUE(opts$homog)) {
        ht <- self$results$homog
        lev <- tryCatch(car::leveneTest(d[[dep]], cells, center = "median"), error = function(e) NULL)
        if (!is.null(lev))
            ht$addRow(rowKey = "levene", values = list(test = "Levene'a (mediana)",
                stat = lev[["F value"]][1], df1 = lev[["Df"]][1], df2 = lev[["Df"]][2],
                p = lev[["Pr(>F)"]][1]))
        bt <- tryCatch(stats::bartlett.test(d[[dep]], cells), error = function(e) NULL)
        if (!is.null(bt))
            ht$addRow(rowKey = "bartlett", values = list(test = "Bartletta",
                stat = unname(bt$statistic), df1 = unname(bt$parameter), df2 = NA,
                p = bt$p.value))
        ht$setNote("cells", "Jednorodność wariancji między obiektami (kombinacjami czynników).")
    }
    if (isTRUE(opts$norm)) {
        nt <- self$results$norm
        r <- res$resid
        if (length(r) >= 3 && length(r) <= 5000) {
            sw <- stats::shapiro.test(r)
            nt$setRow(rowNo = 1, values = list(w = unname(sw$statistic), p = sw$p.value))
        } else {
            nt$setNote("n", "Test Shapiro-Wilka wymaga od 3 do 5000 reszt.")
        }
    }
    self$results$residPlot$setState(data.frame(fitted = res$fitted, resid = res$resid))
    if (!is.null(B)) {
        st <- res$terms$AB$stats
        self$results$plotInteraction$setState(list(
            means = data.frame(A = res$terms$AB$factorA, B = res$terms$AB$factorB,
                mean = as.numeric(st$means), stringsAsFactors = FALSE),
            A = A, B = B, dep = dep))
    }
}

initDesignAnalysis <- function(self, design) {
    opts <- self$options
    hasB <- optNonEmpty(optGet(opts, "factorB"))
    used <- c(A = isTRUE(opts$phA), B = hasB && isTRUE(optGet(opts, "phB")),
        AB = hasB && isTRUE(optGet(opts, "phAB")))
    for (key in c("A", "B", "AB")) {
        self$results$means$addItem(key = key)
        self$results$pairs$addItem(key = key)
        self$results$plotMeans$addItem(key = key)
        self$results$means$get(key = key)$setVisible(used[[key]])
        self$results$pairs$get(key = key)$setVisible(used[[key]])
        self$results$plotMeans$get(key = key)$setVisible(used[[key]])
    }
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

meansPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    m <- s$means
    m$factorA <- factor(m$factorA, levels = unique(m$factorA))
    eb <- s$errorBars
    if (eb == "se") { m$lo <- m$mean - m$se; m$hi <- m$mean + m$se; ebLab <- "± SE" }
    else if (eb == "ci") {
        q <- stats::qt(1 - s$alpha / 2, m$df)
        m$lo <- m$mean - q * m$se; m$hi <- m$mean + q * m$se
        ebLab <- sprintf("%g%% CI", 100 * (1 - s$alpha))
    } else if (eb == "sd") { m$lo <- m$mean - m$sd; m$hi <- m$mean + m$sd; ebLab <- "± SD" }
    else { m$lo <- m$mean; m$hi <- m$mean; ebLab <- NULL }
    span <- diff(range(c(m$lo, m$hi, m$mean)))
    if (!is.finite(span) || span == 0) span <- abs(mean(m$mean)) + 1
    m$labY <- m$hi + 0.06 * span
    if (s$key == "AB") {
        m$factorB <- factor(m$factorB, levels = unique(m$factorB))
        pd <- ggplot2::position_dodge(width = 0.6)
        p <- ggplot2::ggplot(m, ggplot2::aes(x = factorA, y = mean, colour = factorB, group = factorB)) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.25, position = pd) +
            ggplot2::geom_point(size = 3.5, position = pd) +
            ggplot2::geom_text(ggplot2::aes(y = labY, label = letters), position = pd,
                show.legend = FALSE, size = 4) +
            ggplot2::labs(x = s$A, y = s$dep, colour = s$B)
    } else {
        p <- ggplot2::ggplot(m, ggplot2::aes(x = factorA, y = mean)) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.25,
                colour = theme$color[1]) +
            ggplot2::geom_point(size = 3.5, colour = theme$color[1]) +
            ggplot2::geom_text(ggplot2::aes(y = labY, label = letters), size = 4.5) +
            ggplot2::labs(x = s$label, y = s$dep)
    }
    sub <- if (is.null(ebLab)) NULL else paste("Słupki:", ebLab)
    p <- p + ggplot2::labs(subtitle = sub) + ggtheme
    p
}

interactionPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    m <- s$means
    m$A <- factor(m$A, levels = unique(m$A))
    m$B <- factor(m$B, levels = unique(m$B))
    ggplot2::ggplot(m, ggplot2::aes(x = A, y = mean, colour = B, group = B)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 3) +
        ggplot2::labs(x = s$A, y = paste("Średnia", s$dep), colour = s$B) +
        ggtheme
}

residPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    ggplot2::ggplot(s, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_point(alpha = 0.7, colour = theme$color[1]) +
        ggplot2::labs(x = "Wartości dopasowane", y = "Reszty") +
        ggtheme
}

# ---------------------------------------------------------------------------
# Field plan (randomisation)
# ---------------------------------------------------------------------------

parseLabels <- function(txt, n, prefix) {
    labs <- if (optNonEmpty(txt)) trimws(strsplit(txt, ",")[[1]]) else character(0)
    labs <- labs[nchar(labs) > 0]
    if (length(labs) >= n) return(labs[seq_len(n)])
    c(labs, paste0(prefix, seq(length(labs) + 1, n)))
}

# Returns data.frame(plot, block, row, col, A, B) with row/col the field layout.
planDesign <- function(design, nTreat, nRep, nSub, labelsA, labelsB, seed) {
    set.seed(seed)
    tA <- parseLabels(labelsA, nTreat, "A")
    if (design == "crd") {
        n <- nTreat * nRep
        trt <- sample(rep(tA, nRep))
        ncol <- nTreat
        df <- data.frame(plot = seq_len(n), block = NA,
            row = (seq_len(n) - 1) %/% ncol + 1, col = (seq_len(n) - 1) %% ncol + 1,
            A = trt, B = NA, stringsAsFactors = FALSE)
    } else if (design == "rcbd") {
        rows <- lapply(seq_len(nRep), function(b) data.frame(block = b,
            row = b, col = seq_len(nTreat), A = sample(tA), stringsAsFactors = FALSE))
        df <- do.call(rbind, rows)
        df$plot <- seq_len(nrow(df)); df$B <- NA
    } else if (design == "latin") {
        k <- nTreat
        base <- outer(seq_len(k), seq_len(k), function(i, j) (i + j - 2) %% k + 1)
        base <- base[sample(k), sample(k)]
        sym <- sample(tA)
        df <- expand.grid(row = seq_len(k), col = seq_len(k))
        df$A <- sym[base[cbind(df$row, df$col)]]
        df$block <- NA; df$B <- NA
        df <- df[order(df$row, df$col), ]
        df$plot <- seq_len(nrow(df))
    } else {
        tB <- parseLabels(labelsB, nSub, "B")
        rows <- list()
        for (b in seq_len(nRep)) {
            wp <- sample(tA)
            for (i in seq_along(wp)) {
                sp <- sample(tB)
                rows[[length(rows) + 1]] <- data.frame(block = b, row = b,
                    col = (i - 1) * nSub + seq_len(nSub), A = wp[i], B = sp,
                    wholeplot = i, stringsAsFactors = FALSE)
            }
        }
        df <- do.call(rbind, rows)
        df$plot <- seq_len(nrow(df))
    }
    rownames(df) <- NULL
    df[c("plot", "block", "row", "col", "A", "B", setdiff(names(df), c("plot", "block", "row", "col", "A", "B")))]
}

planPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    df <- s$plan
    df$A <- factor(df$A, levels = unique(df$A))
    lab <- if (all(is.na(df$B))) as.character(df$A) else paste0(df$A, "\n", df$B)
    df$lab <- lab
    p <- ggplot2::ggplot(df, ggplot2::aes(x = col, y = row, fill = A)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = lab), size = 3.2) +
        ggplot2::scale_y_reverse(breaks = seq_len(max(df$row))) +
        ggplot2::scale_x_continuous(breaks = seq_len(max(df$col))) +
        ggplot2::coord_fixed() +
        ggplot2::labs(x = "Kolumna", y = s$rowLabel, fill = s$aLabel) +
        ggtheme +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    if (s$design == "splitplot") {
        # outline whole plots
        wp <- stats::aggregate(col ~ block + wholeplot, data = df, FUN = range)
        wpdf <- data.frame(block = wp$block, xmin = wp$col[, 1] - 0.5, xmax = wp$col[, 2] + 0.5)
        p <- p + ggplot2::geom_rect(data = wpdf, ggplot2::aes(xmin = xmin, xmax = xmax,
            ymin = block - 0.5, ymax = block + 0.5), inherit.aes = FALSE,
            fill = NA, colour = "black", linewidth = 0.9)
    }
    p
}
