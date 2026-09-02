# jANOVA: shared engine.
#
#   fitAnova()      -> lm fit, ANOVA table (type I/II/III) with effect sizes
#   termMeans()     -> estimated marginal means (emmeans) for a term: means,
#                      SE, df, CI and the covariance matrix used for pairs
#   pairwiseFromEmm()-> all pairwise comparisons for one term, any method
#   cldLetters()    -> compact letter display (insert-absorb), 'a' = smallest mean
#   contrastTable() -> planned contrasts for one factor (emmeans::contrast)
#   plot helpers    -> means with letters, interaction, residual, Q-Q

`%||%` <- function(a, b) if (is.null(a)) b else a

optNonEmpty <- function(x) !is.null(x) && length(x) > 0 && nchar(as.character(x)[1]) > 0
optGet <- function(opts, name) if (exists(name, envir = opts)) opts[[name]] else NULL

bt <- function(x) paste0("`", x, "`")
termLabel <- function(term) gsub(":", " × ", term, fixed = TRUE)

phMethodLabel <- function(method) {
    switch(method,
        tukey   = "test Tukeya (HSD)",
        lsd     = "NIR (test t Fishera, LSD)",
        scheffe = "test Scheffégo",
        dunnett = "test Dunnetta (vs kontrola)",
        holm    = "test t z poprawką Holma",
        bonf    = "test t z poprawką Bonferroniego",
        none    = "",
        method)
}

phCritLabel <- function(method) {
    switch(method,
        tukey   = "HSD",
        lsd     = "NIR",
        scheffe = "różnica graniczna Scheffégo",
        dunnett = "różnica graniczna Dunnetta",
        "")
}

# ---------------------------------------------------------------------------
# Model and ANOVA table
# ---------------------------------------------------------------------------

buildFormula <- function(dep, factors, blocks = NULL, covs = NULL, interactions = TRUE) {
    parts <- c(if (length(blocks)) bt(blocks), if (length(covs)) bt(covs))
    fac <- paste(bt(factors), collapse = if (interactions) " * " else " + ")
    stats::as.formula(paste(bt(dep), "~", paste(c(parts, fac), collapse = " + ")))
}

# Fit with sum-to-zero contrasts (needed for type III; harmless otherwise).
fitAnova <- function(d, dep, factors, blocks = NULL, covs = NULL,
                     interactions = TRUE, ssType = "3") {
    f <- buildFormula(dep, factors, blocks, covs, interactions)
    facVars <- c(factors, blocks)
    ctr <- stats::setNames(rep(list("contr.sum"), length(facVars)), facVars)
    fit <- stats::lm(f, data = d, contrasts = ctr)

    if (ssType == "1") {
        an <- stats::anova(fit)
        tab <- data.frame(term = rownames(an), ss = an[["Sum Sq"]], df = an[["Df"]],
            ms = an[["Mean Sq"]], F = an[["F value"]], p = an[["Pr(>F)"]],
            stringsAsFactors = FALSE)
    } else {
        an <- car::Anova(fit, type = as.integer(ssType))
        an <- an[rownames(an) != "(Intercept)", , drop = FALSE]
        tab <- data.frame(term = rownames(an), ss = an[["Sum Sq"]], df = an[["Df"]],
            ms = an[["Sum Sq"]] / an[["Df"]], F = an[["F value"]], p = an[["Pr(>F)"]],
            stringsAsFactors = FALSE)
    }
    resRow <- tab$term == "Residuals"
    sse <- tab$ss[resRow]; dfe <- tab$df[resRow]; mse <- sse / dfe
    ssTotal <- sum((d[[dep]] - mean(d[[dep]]))^2)
    eff <- !resRow
    tab$eta <- tab$partEta <- tab$omega <- NA_real_
    tab$eta[eff] <- tab$ss[eff] / ssTotal
    tab$partEta[eff] <- tab$ss[eff] / (tab$ss[eff] + sse)
    tab$omega[eff] <- pmax(0, (tab$ss[eff] - tab$df[eff] * mse) / (ssTotal + mse))
    tab$source <- ifelse(resRow, "Błąd (reszty)", termLabel(tab$term))
    tab$term[resRow] <- "error"
    tab$kind <- ifelse(tab$term %in% blocks, "block",
        ifelse(tab$term %in% covs, "cov", ifelse(resRow, "error", "effect")))
    if (ssType == "1")
        tab <- rbind(tab, data.frame(term = "total", ss = sum(tab$ss), df = sum(tab$df),
            ms = NA, F = NA, p = NA, eta = NA, partEta = NA, omega = NA,
            source = "Ogółem", kind = "total", stringsAsFactors = FALSE))
    list(fit = fit, anova = tab, mse = mse, dfe = dfe, formula = f)
}

welchTable <- function(d, dep, factor) {
    w <- stats::oneway.test(stats::as.formula(paste(bt(dep), "~", bt(factor))), data = d,
        var.equal = FALSE)
    data.frame(F = unname(w$statistic), df1 = unname(w$parameter[1]),
        df2 = unname(w$parameter[2]), p = w$p.value)
}

# ---------------------------------------------------------------------------
# Marginal means and comparisons (works for lm and afex_aov)
# ---------------------------------------------------------------------------

# term: character vector of factor names (1 = main effect, 2 = cells)
termMeans <- function(fit, term, alpha = 0.05) {
    emm <- emmFor(fit, term)
    s <- as.data.frame(summary(emm, level = 1 - alpha, infer = c(TRUE, FALSE)))
    levs <- do.call(paste, c(lapply(term, function(v) as.character(s[[v]])), sep = " × "))
    ciCols <- grep("^(lower|upper)\\.", names(s), value = TRUE)
    out <- data.frame(level = levs, mean = s$emmean, se = s$SE, df = s$df,
        lower = s[[ciCols[1]]], upper = s[[ciCols[2]]], stringsAsFactors = FALSE)
    for (v in term) out[[v]] <- as.character(s[[v]])
    V <- as.matrix(vcov(emm)); dimnames(V) <- list(levs, levs)
    list(means = out, vcov = V, emm = emm)
}

dunnettCorrFromV <- function(V, idx, ctrl) {
    # correlation of the contrasts (level_i - control)
    n <- length(idx); R <- matrix(0, n, n)
    var_i <- function(i) V[i, i] + V[ctrl, ctrl] - 2 * V[i, ctrl]
    for (a in seq_len(n)) for (b in seq_len(n)) {
        i <- idx[a]; j <- idx[b]
        cov_ij <- V[i, j] - V[i, ctrl] - V[j, ctrl] + V[ctrl, ctrl]
        R[a, b] <- cov_ij / sqrt(var_i(i) * var_i(j))
    }
    R
}

# means: data.frame(level, mean, df); V: covariance of means.
pairwiseFromEmm <- function(means, V, method, alpha, control = NULL, mse = NULL) {
    levs <- means$level; k <- length(levs)
    m <- stats::setNames(means$mean, levs)
    df <- stats::setNames(means$df, levs)
    if (method == "dunnett") {
        if (is.null(control) || !(control %in% levs)) control <- levs[1]
        pairs <- cbind(setdiff(levs, control), control)
    } else {
        pairs <- t(utils::combn(levs, 2))
    }
    n <- nrow(pairs)
    out <- data.frame(g1 = pairs[, 1], g2 = pairs[, 2], stringsAsFactors = FALSE)
    out$diff <- m[out$g1] - m[out$g2]
    out$se <- sqrt(V[cbind(out$g1, out$g1)] + V[cbind(out$g2, out$g2)] - 2 * V[cbind(out$g1, out$g2)])
    out$df <- pmin(df[out$g1], df[out$g2])
    out$stat <- out$diff / out$se
    t <- out$stat; dfp <- out$df; se <- out$se
    out$p <- NA_real_; out$crit <- NA_real_
    if (method == "lsd") {
        out$p <- 2 * stats::pt(-abs(t), dfp)
        out$crit <- stats::qt(1 - alpha / 2, dfp) * se
    } else if (method == "tukey") {
        out$p <- stats::ptukey(abs(t) * sqrt(2), k, dfp, lower.tail = FALSE)
        out$crit <- stats::qtukey(1 - alpha, k, dfp) / sqrt(2) * se
    } else if (method == "scheffe") {
        out$p <- stats::pf(t^2 / (k - 1), k - 1, dfp, lower.tail = FALSE)
        out$crit <- sqrt((k - 1) * stats::qf(1 - alpha, k - 1, dfp)) * se
    } else if (method %in% c("holm", "bonf")) {
        raw <- 2 * stats::pt(-abs(t), dfp)
        out$p <- stats::p.adjust(raw, method = if (method == "holm") "holm" else "bonferroni")
        if (method == "bonf")
            out$crit <- stats::qt(1 - alpha / (2 * n), dfp) * se
    } else if (method == "dunnett") {
        R <- dunnettCorrFromV(V, match(out$g1, levs), match(control, levs))
        dfi <- max(1L, as.integer(round(dfp[1])))
        for (i in seq_len(n))
            out$p[i] <- 1 - mvtnorm::pmvt(lower = rep(-abs(t[i]), n), upper = rep(abs(t[i]), n),
                df = dfi, corr = R)[1]
        q <- mvtnorm::qmvt(1 - alpha, tail = "both.tails", df = dfi, corr = R)$quantile
        out$crit <- q * se
    }
    out$lower <- out$diff - out$crit
    out$upper <- out$diff + out$crit
    out$d <- if (!is.null(mse) && is.finite(mse) && mse > 0) out$diff / sqrt(mse) else NA_real_
    out$sig <- !is.na(out$p) & out$p < alpha
    rownames(out) <- NULL
    out
}

cldLetters <- function(levs, sigPairs) {
    k <- length(levs)
    cols <- list(rep(TRUE, k))
    sp <- sigPairs[sigPairs$sig, c("g1", "g2"), drop = FALSE]
    for (i in seq_len(nrow(sp))) {
        a <- match(sp$g1[i], levs); b <- match(sp$g2[i], levs)
        newCols <- list()
        for (col in cols) {
            if (col[a] && col[b]) {
                c1 <- col; c1[a] <- FALSE
                c2 <- col; c2[b] <- FALSE
                newCols <- c(newCols, list(c1), list(c2))
            } else newCols <- c(newCols, list(col))
        }
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
    firsts <- vapply(cols, function(c) which(c)[1], 1L)
    cols <- cols[order(firsts)]
    out <- vapply(seq_len(k), function(i)
        paste(letters[which(vapply(cols, function(c) c[i], TRUE))], collapse = ""), "")
    names(out) <- levs
    out
}

# Full comparison bundle for one term.
compareTerm <- function(fit, term, method, alpha, control = NULL, mse = NULL) {
    tm <- termMeans(fit, term, alpha)
    means <- tm$means
    if (method == "none") {
        means$letters <- ""
        return(list(means = means, pairs = NULL, critNote = NULL))
    }
    pairs <- pairwiseFromEmm(means, tm$vcov, method, alpha, control, mse)
    if (method == "dunnett") {
        if (is.null(control) || !(control %in% means$level)) control <- means$level[1]
        mark <- rep("", nrow(means)); names(mark) <- means$level
        mark[control] <- "(kontrola)"
        mark[pairs$g1[pairs$sig]] <- "*"
        means$letters <- unname(mark)
    } else {
        ord <- means$level[order(means$mean)]
        means$letters <- unname(cldLetters(ord, pairs)[means$level])
    }
    critNote <- NULL
    if (method %in% c("tukey", "lsd", "scheffe", "dunnett")) {
        if (length(unique(round(pairs$crit, 10))) == 1)
            critNote <- sprintf("%s = %.4g (α = %g)", phCritLabel(method), pairs$crit[1], alpha)
        else
            critNote <- sprintf("%s różni się między parami (nierówne liczebności lub różne błędy); wartości w tabeli par (α = %g)",
                phCritLabel(method), alpha)
    } else if (method == "holm") {
        critNote <- sprintf("p skorygowane metodą Holma; α = %g", alpha)
    } else if (method == "bonf") {
        critNote <- sprintf("p skorygowane metodą Bonferroniego; α = %g", alpha)
    }
    list(means = means, pairs = pairs, critNote = critNote)
}

# ---------------------------------------------------------------------------
# Planned contrasts
# ---------------------------------------------------------------------------

helmertMatrix <- function(k, reverse = FALSE) {
    # helmert: level i vs mean of later levels; reverse ("difference"):
    # level i+1 vs mean of earlier levels
    out <- list()
    for (i in seq_len(k - 1)) {
        v <- rep(0, k)
        if (!reverse) { v[i] <- 1; v[(i + 1):k] <- -1 / (k - i) }
        else { v[i + 1] <- 1; v[1:i] <- -1 / i }
        out[[i]] <- v
    }
    out
}

contrastTable <- function(fit, factor, type) {
    emm <- emmFor(fit, factor)
    levs <- as.character(summary(emm)[[factor]]); k <- length(levs)
    if (k < 2) return(NULL)
    method <- switch(type,
        deviation  = "eff",
        simple     = "trt.vs.ctrl",
        simpleLast = "trt.vs.ctrlk",
        repeated   = "consec",
        polynomial = "poly",
        helmert    = stats::setNames(helmertMatrix(k), vapply(seq_len(k - 1), function(i)
            sprintf("%s vs średnia(%s)", levs[i], paste(levs[(i + 1):k], collapse = ", ")), "")),
        difference = stats::setNames(helmertMatrix(k, TRUE), vapply(seq_len(k - 1), function(i)
            sprintf("%s vs średnia(%s)", levs[i + 1], paste(levs[1:i], collapse = ", ")), "")),
        NULL)
    if (is.null(method)) return(NULL)
    if (identical(method, "poly") && k > 7) method <- "poly"  # emmeans caps at degree 6
    ct <- as.data.frame(summary(emmeans::contrast(emm, method = method, adjust = "none")))
    lab <- as.character(ct$contrast)
    if (type == "deviation") lab <- paste(gsub(" effect$", "", lab), "vs średnia ogólna")
    data.frame(contrast = lab, estimate = ct$estimate, se = ct$SE, df = ct$df,
        t = ct$t.ratio, p = ct$p.value, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Descriptives and assumptions
# ---------------------------------------------------------------------------

cellsFactor <- function(d, factors) {
    if (length(factors) == 1) droplevels(factor(d[[factors]]))
    else droplevels(interaction(d[factors], sep = " × ", lex.order = TRUE))
}

descriptivesTable <- function(d, dep, factors) {
    cells <- cellsFactor(d, factors)
    y <- d[[dep]]
    data.frame(level = levels(cells), n = as.integer(table(cells)),
        mean = as.numeric(tapply(y, cells, mean)), sd = as.numeric(tapply(y, cells, sd)),
        median = as.numeric(tapply(y, cells, stats::median)), stringsAsFactors = FALSE)
}

homogeneityTable <- function(y, cells) {
    out <- list()
    lev <- tryCatch(car::leveneTest(y, cells, center = "median"), error = function(e) NULL)
    if (!is.null(lev)) out$levene <- list(test = "Levene'a (mediana)",
        stat = lev[["F value"]][1], df1 = lev[["Df"]][1], df2 = lev[["Df"]][2], p = lev[["Pr(>F)"]][1])
    bt <- tryCatch(stats::bartlett.test(y, cells), error = function(e) NULL)
    if (!is.null(bt)) out$bartlett <- list(test = "Bartletta", stat = unname(bt$statistic),
        df1 = unname(bt$parameter), df2 = NA, p = bt$p.value)
    out
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

meansPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    m <- s$means
    m$xf <- factor(m$xf, levels = unique(m$xf))
    eb <- s$errorBars
    if (eb == "se") { m$lo <- m$mean - m$se; m$hi <- m$mean + m$se; ebLab <- "± SE" }
    else if (eb == "ci") { m$lo <- m$lower; m$hi <- m$upper; ebLab <- sprintf("%g%% CI", 100 * (1 - s$alpha)) }
    else { m$lo <- m$mean; m$hi <- m$mean; ebLab <- NULL }
    span <- diff(range(c(m$lo, m$hi, m$mean)))
    if (!is.finite(span) || span == 0) span <- abs(mean(m$mean)) + 1
    m$labY <- m$hi + 0.06 * span
    if (!is.null(s$groupLabel)) {
        m$gf <- factor(m$gf, levels = unique(m$gf))
        pd <- ggplot2::position_dodge(width = 0.6)
        p <- ggplot2::ggplot(m, ggplot2::aes(x = xf, y = mean, colour = gf, group = gf)) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.25, position = pd) +
            ggplot2::geom_point(size = 3.5, position = pd) +
            ggplot2::geom_text(ggplot2::aes(y = labY, label = letters), position = pd,
                show.legend = FALSE, size = 4) +
            ggplot2::labs(x = s$xLabel, y = s$dep, colour = s$groupLabel)
    } else {
        p <- ggplot2::ggplot(m, ggplot2::aes(x = xf, y = mean)) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.25, colour = theme$color[1]) +
            ggplot2::geom_point(size = 3.5, colour = theme$color[1]) +
            ggplot2::geom_text(ggplot2::aes(y = labY, label = letters), size = 4.5) +
            ggplot2::labs(x = s$xLabel, y = s$dep)
    }
    sub <- if (is.null(ebLab)) NULL else paste("Słupki:", ebLab)
    p + ggplot2::labs(subtitle = sub) + ggtheme
}

interactionPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    m <- s$means
    m$A <- factor(m$A, levels = unique(m$A)); m$B <- factor(m$B, levels = unique(m$B))
    ggplot2::ggplot(m, ggplot2::aes(x = A, y = mean, colour = B, group = B)) +
        ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 3) +
        ggplot2::labs(x = s$A, y = paste("Średnia", s$dep), colour = s$B) + ggtheme
}

residPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    ggplot2::ggplot(s, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::geom_point(alpha = 0.7, colour = theme$color[1]) +
        ggplot2::labs(x = "Wartości dopasowane", y = "Reszty") + ggtheme
}

qqPlot <- function(image, ggtheme, theme) {
    s <- image$state
    if (is.null(s)) return(FALSE)
    r <- sort(s$resid); n <- length(r)
    z <- (r - mean(r)) / stats::sd(r)
    df <- data.frame(theo = stats::qnorm(stats::ppoints(n)), z = z)
    ggplot2::ggplot(df, ggplot2::aes(x = theo, y = z)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
        ggplot2::geom_point(alpha = 0.7, colour = theme$color[1]) +
        ggplot2::labs(x = "Kwantyle teoretyczne", y = "Standaryzowane reszty") + ggtheme
}

# ---------------------------------------------------------------------------
# Repeated measures (long format) via afex
# ---------------------------------------------------------------------------

# Shared emmeans entry point: afex models use the univariate (aov) model so
# that df and pooled errors match the ANOVA table; within-factor levels come
# back as syntactic names (make.names), which we map back to the originals.
emmFor <- function(fit, term) {
    spec <- stats::as.formula(paste("~", paste(bt(term), collapse = " * ")))
    if (inherits(fit, "afex_aov")) {
        emm <- suppressMessages(emmeans::emmeans(fit, specs = spec, model = "univariate"))
        origLevels <- attr(fit, "jupwrLevels")
        for (v in term) {
            lv <- origLevels[[v]]
            if (is.null(lv)) next
            mn <- make.names(lv)
            vals <- as.character(emm@grid[[v]])
            hit <- vals %in% mn & !(vals %in% lv)
            vals[hit] <- lv[match(vals[hit], mn)]
            emm@grid[[v]] <- factor(vals, levels = lv)
            emm@levels[[v]] <- lv
        }
        emm
    } else {
        suppressMessages(emmeans::emmeans(fit, specs = spec))
    }
}

fitRm <- function(d, dep, subject, within, between = NULL, covs = NULL, ssType = "3") {
    args <- list(id = subject, dv = dep, data = d, within = within,
        type = as.integer(ssType), include_aov = TRUE, fun_aggregate = mean,
        anova_table = list(correction = "none"))
    if (length(between)) args$between <- between
    if (length(covs)) { args$covariate <- covs; args$factorize <- FALSE }
    fit <- suppressWarnings(suppressMessages(do.call(afex::aov_ez, args)))
    # afex turns within-factor levels into syntactic names (make.names);
    # keep the originals so tables and plots show the user's labels
    attr(fit, "jupwrLevels") <- stats::setNames(lapply(c(within, between), function(v) levels(d[[v]])), c(within, between))
    an0 <- suppressWarnings(stats::anova(fit, correction = "none", es = c("ges", "pes")))
    list(fit = fit, an0 = an0)
}

rmTable <- function(fitRes, correction = "none") {
    an0 <- fitRes$an0
    anC <- if (correction == "none") an0 else
        suppressWarnings(stats::anova(fitRes$fit, correction = correction, es = c("ges", "pes")))
    terms <- rownames(an0)
    data.frame(term = terms, source = termLabel(terms),
        ss = an0$F * an0$MSE * an0[["num Df"]],
        df1 = anC[["num Df"]], df2 = anC[["den Df"]], mse = anC$MSE, F = anC$F,
        p = anC[["Pr(>F)"]], ges = an0$ges, pes = an0$pes, stringsAsFactors = FALSE)
}

sphericityTable <- function(fit) {
    s <- suppressWarnings(summary(fit))
    st <- s$sphericity.tests
    if (is.null(st) || nrow(st) == 0) return(NULL)
    adj <- s$pval.adjustments
    st <- as.data.frame(unclass(st)); adj <- as.data.frame(unclass(adj))
    data.frame(term = rownames(st), source = termLabel(rownames(st)), W = st[[1]], p = st[[2]],
        gg = adj[rownames(st), "GG eps"], hf = pmin(1, adj[rownames(st), "HF eps"]),
        stringsAsFactors = FALSE)
}

# MSE of the stratum that tests `term` (for Cohen's d); NULL if not found
rmMseFor <- function(an0, term) {
    key <- paste(term, collapse = ":")
    rn <- rownames(an0)
    hit <- which(vapply(rn, function(r) setequal(strsplit(r, ":", fixed = TRUE)[[1]], term), TRUE))
    if (length(hit)) an0$MSE[hit[1]] else NULL
}

subjectMeans <- function(d, dep, subject, between) {
    f <- stats::as.formula(paste(bt(dep), "~", paste(bt(c(subject, between)), collapse = " + ")))
    stats::aggregate(f, data = d, FUN = mean)
}

# ---------------------------------------------------------------------------
# Nonparametric: one factor (Kruskal-Wallis family)
# ---------------------------------------------------------------------------

tiesCorrectionSum <- function(y) {
    t <- table(y); sum(t^3 - t)
}

kruskalTable <- function(y, g) {
    k <- stats::kruskal.test(y, g)
    n <- length(y)
    list(test = "Kruskala-Wallisa", stat = unname(k$statistic), df = unname(k$parameter),
        p = k$p.value, es = unname(k$statistic) / (n - 1))   # ε² = H / (n − 1)
}

# Jonckheere-Terpstra: trend across levels in factor order; normal approximation
# with the tie-corrected variance (Hollander & Wolfe).
jonckheereTable <- function(y, g) {
    g <- droplevels(factor(g)); levs <- levels(g); k <- length(levs)
    if (k < 3) return(NULL)
    ys <- split(y, g); ns <- lengths(ys); N <- sum(ns)
    JT <- 0
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
        a <- ys[[i]]; b <- ys[[j]]
        JT <- JT + sum(outer(a, b, "<")) + 0.5 * sum(outer(a, b, "=="))
    }
    tj <- as.numeric(table(y))
    mu <- (N^2 - sum(ns^2)) / 4
    v <- (N * (N - 1) * (2 * N + 5) - sum(ns * (ns - 1) * (2 * ns + 5)) - sum(tj * (tj - 1) * (2 * tj + 5))) / 72 +
        sum(ns * (ns - 1) * (ns - 2)) * sum(tj * (tj - 1) * (tj - 2)) / (36 * N * (N - 1) * (N - 2)) +
        sum(ns * (ns - 1)) * sum(tj * (tj - 1)) / (8 * N * (N - 1))
    z <- (JT - mu) / sqrt(v)
    list(test = "Jonckheere'a-Terpstry (trend)", stat = JT, z = z, df = NA,
        p = 2 * stats::pnorm(-abs(z)), es = NA)
}

# Mood's median test: chi-square on counts above the grand median
medianTable <- function(y, g) {
    med <- stats::median(y)
    tab <- table(factor(y > med, levels = c(FALSE, TRUE)), g)
    ct <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    list(test = "mediany (Mooda)", stat = unname(ct$statistic), df = unname(ct$parameter),
        p = ct$p.value, es = NA, median = med)
}

# Dunn's pairwise z tests on mean ranks (tie-corrected), p adjusted by `adjust`
dunnPairs <- function(y, g, adjust = "holm", alpha = 0.05) {
    g <- droplevels(factor(g)); levs <- levels(g); k <- length(levs)
    r <- rank(y); N <- length(y)
    Rm <- tapply(r, g, mean); ns <- as.numeric(table(g)); names(ns) <- levs
    Tc <- tiesCorrectionSum(y)
    pairs <- t(utils::combn(levs, 2))
    out <- data.frame(g1 = pairs[, 1], g2 = pairs[, 2], stringsAsFactors = FALSE)
    out$diff <- Rm[out$g1] - Rm[out$g2]
    out$se <- sqrt((N * (N + 1) / 12 - Tc / (12 * (N - 1))) * (1 / ns[out$g1] + 1 / ns[out$g2]))
    out$stat <- out$diff / out$se
    out$p <- stats::p.adjust(2 * stats::pnorm(-abs(out$stat)), method = adjust)
    out$sig <- out$p < alpha
    rownames(out) <- NULL
    levelsTab <- data.frame(level = levs, n = ns, median = as.numeric(tapply(y, g, stats::median)),
        meanRank = as.numeric(Rm), stringsAsFactors = FALSE)
    ord <- levs[order(Rm)]
    levelsTab$letters <- unname(cldLetters(ord, out)[levs])
    list(levels = levelsTab, pairs = out)
}

# ---------------------------------------------------------------------------
# Nonparametric: one within factor (Friedman family)
# ---------------------------------------------------------------------------

# wide matrix subjects × levels (mean over duplicates)
rmMatrix <- function(d, dep, subject, within) {
    m <- tapply(d[[dep]], list(d[[subject]], d[[within]]), mean)
    m[stats::complete.cases(m), , drop = FALSE]
}

friedmanTable <- function(m) {
    f <- stats::friedman.test(m)
    n <- nrow(m); k <- ncol(m)
    list(test = "Friedmana", stat = unname(f$statistic), df = unname(f$parameter),
        p = f$p.value, es = unname(f$statistic) / (n * (k - 1)))   # W Kendalla
}

# Page's L for a monotone trend in level order (normal approximation)
pageTable <- function(m) {
    n <- nrow(m); k <- ncol(m)
    if (k < 3) return(NULL)
    R <- t(apply(m, 1, rank))
    L <- sum(seq_len(k) * colSums(R))
    mu <- n * k * (k + 1)^2 / 4
    v <- n * k^2 * (k + 1) * (k^2 - 1) / 144
    z <- (L - mu) / sqrt(v)
    list(test = "Page'a (trend)", stat = L, z = z, df = NA, p = 2 * stats::pnorm(-abs(z)), es = NA)
}

# Pairwise after Friedman: Nemenyi (studentized range, no adjustment needed)
# or Conover (t on rank sums, p adjusted by `adjust`)
friedmanPairs <- function(m, method = "nemenyi", adjust = "holm", alpha = 0.05) {
    n <- nrow(m); k <- ncol(m); levs <- colnames(m)
    R <- t(apply(m, 1, rank))
    Rsum <- colSums(R); Rmean <- Rsum / n
    pairs <- t(utils::combn(levs, 2))
    out <- data.frame(g1 = pairs[, 1], g2 = pairs[, 2], stringsAsFactors = FALSE)
    out$diff <- Rmean[out$g1] - Rmean[out$g2]
    if (method == "nemenyi") {
        out$se <- sqrt(k * (k + 1) / (6 * n))
        out$stat <- out$diff / out$se
        out$p <- stats::ptukey(abs(out$stat) * sqrt(2), k, Inf, lower.tail = FALSE)
    } else {
        A1 <- sum(R^2); B1 <- sum(Rsum^2) / n
        dfC <- (n - 1) * (k - 1)
        seSum <- sqrt(2 * (A1 - B1) / dfC)     # for rank-sum differences
        out$se <- seSum / n
        out$stat <- (Rsum[out$g1] - Rsum[out$g2]) / seSum
        out$p <- stats::p.adjust(2 * stats::pt(-abs(out$stat), dfC), method = adjust)
    }
    out$sig <- out$p < alpha
    rownames(out) <- NULL
    levelsTab <- data.frame(level = levs, n = n, median = apply(m, 2, stats::median),
        meanRank = as.numeric(Rmean), stringsAsFactors = FALSE)
    ord <- levs[order(Rmean)]
    levelsTab$letters <- unname(cldLetters(ord, out)[levs])
    list(levels = levelsTab, pairs = out)
}

# ---------------------------------------------------------------------------
# ART: aligned rank transform (Wobbrock et al. 2011), full factorial
# ---------------------------------------------------------------------------

# Returns data.frame of aligned ranks, one column per term (names "A", "A:B", ...)
artAlignedRanks <- function(d, dep, factors) {
    y <- d[[dep]]
    k <- length(factors)
    termSets <- unlist(lapply(seq_len(k), function(o) utils::combn(factors, o, simplify = FALSE)), recursive = FALSE)
    cellMean <- function(vars) {
        if (length(vars) == 0) return(rep(mean(y), length(y)))
        stats::ave(y, d[vars], FUN = mean)
    }
    full <- cellMean(factors)
    resid <- y - full
    out <- list()
    for (ts in termSets) {
        # inclusion–exclusion over the sub-terms of ts (grand mean included)
        est <- 0
        for (o in 0:length(ts)) {
            subs <- if (o == 0) list(character(0)) else utils::combn(ts, o, simplify = FALSE)
            sgn <- if ((length(ts) - o) %% 2 == 0) 1 else -1
            for (sb in subs) est <- est + sgn * cellMean(sb)
        }
        aligned <- resid + est
        out[[paste(ts, collapse = ":")]] <- rank(round(aligned, 12))
    }
    as.data.frame(out, check.names = FALSE, optional = TRUE)
}

# Fixed-effects ART table: one full-factorial lm per term, type III F of that term
artTable <- function(d, dep, factors) {
    ranks <- artAlignedRanks(d, dep, factors)
    ctr <- stats::setNames(rep(list("contr.sum"), length(factors)), factors)
    rows <- list()
    for (term in names(ranks)) {
        dd <- d[factors]; dd$.r <- ranks[[term]]
        f <- stats::as.formula(paste(".r ~", paste(bt(factors), collapse = " * ")))
        fit <- stats::lm(f, data = dd, contrasts = ctr)
        an <- car::Anova(fit, type = 3)
        key <- rownames(an)[vapply(rownames(an), function(r)
            setequal(strsplit(r, ":", fixed = TRUE)[[1]], strsplit(term, ":", fixed = TRUE)[[1]]), TRUE)][1]
        rows[[term]] <- data.frame(term = term, source = termLabel(term), F = an[key, "F value"],
            df1 = an[key, "Df"], df2 = an["Residuals", "Df"], p = an[key, "Pr(>F)"], stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

# Repeated-measures ART: aligned ranks per term, then afex on each with the
# proper error strata; report the term's own F
artTableRm <- function(d, dep, subject, within, between = NULL, ssType = "3") {
    factors <- c(within, between)
    ranks <- artAlignedRanks(d, dep, factors)
    rows <- list()
    for (term in names(ranks)) {
        dd <- d[c(subject, factors)]; dd$.r <- ranks[[term]]
        res <- fitRm(dd, ".r", subject, within, between, NULL, ssType)
        an <- res$an0
        key <- rownames(an)[vapply(rownames(an), function(r)
            setequal(strsplit(r, ":", fixed = TRUE)[[1]], strsplit(term, ":", fixed = TRUE)[[1]]), TRUE)][1]
        rows[[term]] <- data.frame(term = term, source = termLabel(term), F = an[key, "F"],
            df1 = an[key, "num Df"], df2 = an[key, "den Df"], p = an[key, "Pr(>F)"], stringsAsFactors = FALSE)
    }
    out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

# ART post-hoc for main effects (as ARTool::art.con for a single factor):
# pairwise comparisons on the ranks aligned for that factor, via the same
# emmeans-based engine as the parametric post-hoc.
artMainEffectComparisons <- function(d, dep, factors, term, method, alpha,
                                     subject = NULL, within = NULL, between = NULL, ssType = "3") {
    ranks <- artAlignedRanks(d, dep, factors)
    dd <- d[c(subject, factors)]; dd$.r <- ranks[[term]]
    if (is.null(subject)) {
        ctr <- stats::setNames(rep(list("contr.sum"), length(factors)), factors)
        f <- stats::as.formula(paste(".r ~", paste(bt(factors), collapse = " * ")))
        fit <- stats::lm(f, data = dd, contrasts = ctr)
        mse <- sum(stats::residuals(fit)^2) / stats::df.residual(fit)
    } else {
        res <- fitRm(dd, ".r", subject, within, between, NULL, ssType)
        fit <- res$fit
        mse <- rmMseFor(res$an0, term)
    }
    compareTerm(fit, term, method, alpha, control = NULL, mse = mse)
}
