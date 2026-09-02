#' @importFrom jmvcore .
anovaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
        .termKeys = function() {
            factors <- self$options$factors
            keys <- list()
            for (f in factors) keys[[f]] <- f
            if (isTRUE(self$options$phInter) && isTRUE(self$options$interactions) && length(factors) >= 2)
                for (pr in utils::combn(factors, 2, simplify = FALSE))
                    keys[[paste(pr, collapse = ":")]] <- pr
            keys
        },
        .init = function() {
            keys <- private$.termKeys()
            for (k in names(keys)) {
                self$results$means$addItem(key = k)
                self$results$pairs$addItem(key = k)
                self$results$plotMeans$addItem(key = k)
                lab <- termLabel(k)
                self$results$means$get(key = k)$setTitle(paste("Średnie:", lab))
                self$results$pairs$get(key = k)$setTitle(paste("Porównania parami:", lab))
            }
            for (f in self$options$factors) {
                self$results$contrasts$addItem(key = f)
                self$results$artMeans$addItem(key = f)
                self$results$artPairs$addItem(key = f)
                self$results$artMeans$get(key = f)$setTitle(paste("ART, średnie rang:", f))
                self$results$artPairs$get(key = f)$setTitle(paste("ART, porównania parami:", f))
            }
        },
        .fillComparison = function(mt, pt, cmp, method, alpha, what) {
            for (i in seq_len(nrow(cmp$means))) {
                r <- cmp$means[i, ]
                vals <- list(level = r$level, mean = r$mean, se = r$se, letters = r$letters)
                if (what == "means") { vals$lower <- r$lower; vals$upper <- r$upper }
                mt$addRow(rowKey = r$level, values = vals)
            }
            if (method == "none") {
                mt$getColumn("letters")$setVisible(FALSE)
                mt$setNote("emm", sprintf("Średnie brzegowe z modelu (emmeans), %g%% CI.", 100 * (1 - alpha)))
            } else if (method == "dunnett") {
                mt$getColumn("letters")$setTitle("vs kontrola")
                mt$setNote("dun", paste0("Kontrola = pierwszy poziom; * różni się istotnie od kontroli; ", cmp$critNote))
            } else {
                mt$setNote("cld", paste0(if (what == "means") "Średnie brzegowe z modelu; " else "",
                    phMethodLabel(method), "; ", cmp$critNote,
                    ". Poziomy z tą samą literą nie różnią się istotnie; litera a = grupa z najniższą średnią."))
            }
            if (!is.null(cmp$pairs)) {
                for (i in seq_len(nrow(cmp$pairs))) {
                    r <- cmp$pairs[i, ]
                    vals <- list(g1 = r$g1, g2 = r$g2, diff = r$diff, se = r$se, df = r$df, stat = r$stat, p = r$p)
                    if (what == "means") { vals$crit <- r$crit; vals$lower <- r$lower; vals$upper <- r$upper; vals$d <- r$d }
                    pt$addRow(rowKey = i, values = vals)
                }
                if (what == "means") {
                    if (method == "holm") {
                        for (cn in c("crit", "lower", "upper")) pt$getColumn(cn)$setVisible(FALSE)
                        pt$setNote("holm", "p skorygowane metodą Holma.")
                    } else {
                        pt$setNote("crit", sprintf("%s; przedział ufności = różnica ± %s (poziom %g%%).",
                            phMethodLabel(method), phCritLabel(method), 100 * (1 - alpha)))
                    }
                }
            }
        },
        .run = function() {
            opts <- self$options
            dep <- opts$dep; factors <- opts$factors
            blocks <- opts$blocks; covs <- opts$covs
            if (!optNonEmpty(dep) || length(factors) == 0) return()

            vars <- c(dep, factors, blocks, covs)
            d <- self$data[vars]
            d[[dep]] <- jmvcore::toNumeric(d[[dep]])
            for (v in covs) d[[v]] <- jmvcore::toNumeric(d[[v]])
            for (v in c(factors, blocks)) d[[v]] <- factor(d[[v]])
            complete <- stats::complete.cases(d)
            d <- d[complete, , drop = FALSE]
            for (v in c(factors, blocks)) d[[v]] <- droplevels(d[[v]])
            if (nrow(d) < 3) return()
            for (v in c(factors, blocks)) if (nlevels(d[[v]]) < 2) {
                self$results$anova$setNote("err", sprintf("Zmienna %s musi mieć co najmniej 2 poziomy.", v))
                return()
            }

            res <- tryCatch(fitAnova(d, dep, factors, blocks, covs, isTRUE(opts$interactions), opts$ss),
                error = function(e) e)
            if (inherits(res, "error")) {
                self$results$anova$setNote("err", paste("Błąd dopasowania modelu:", conditionMessage(res)))
                return()
            }
            if (!is.finite(res$mse) || res$dfe < 1) {
                self$results$anova$setNote("err", "Brak stopni swobody dla błędu (za mało obserwacji na komórkę).")
                return()
            }
            method <- opts$postHoc; alpha <- opts$alpha
            oneFactor <- length(factors) == 1 && length(blocks) == 0 && length(covs) == 0
            factorsOnly <- length(blocks) == 0 && length(covs) == 0

            # --- ANOVA table
            at <- self$results$anova
            for (i in seq_len(nrow(res$anova))) {
                r <- res$anova[i, ]
                at$addRow(rowKey = r$term, values = list(source = r$source, ss = r$ss, df = r$df,
                    ms = r$ms, F = r$F, p = r$p, eta = r$eta, partEta = r$partEta, omega = r$omega))
            }
            at$setNote("ss", switch(opts$ss, '1' = "Sumy kwadratów typu I (sekwencyjne, w kolejności wierszy).",
                '2' = "Sumy kwadratów typu II.", '3' = "Sumy kwadratów typu III."))
            cells <- cellsFactor(d, factors)
            if (length(unique(table(cells))) > 1 && opts$ss == "1")
                at$setNote("unbal", "Układ niezrównoważony: przy typie I wynik zależy od kolejności czynników.")

            # --- Welch / Welch-James
            if (isTRUE(opts$welch)) {
                wt <- self$results$welch
                if (!factorsOnly) {
                    wt$setNote("na", "Test Welcha jest dostępny tylko dla modelu z samymi czynnikami (bez bloków i kowariant).")
                } else {
                    w <- tryCatch(welchJamesTable(d, dep, factors), error = function(e) e)
                    if (inherits(w, "error")) wt$setNote("err", conditionMessage(w))
                    else {
                        for (i in seq_len(nrow(w))) {
                            r <- w[i, ]
                            wt$addRow(rowKey = r$term, values = list(source = r$source, F = r$F, df1 = r$df1, df2 = r$df2, p = r$p))
                        }
                        wt$setNote("wj", if (length(factors) == 1) "Test Welcha: osobne wariancje grup, przybliżone stopnie swobody."
                            else "Test Welcha-Jamesa (Johansen, 1980): osobne wariancje komórek, przybliżone stopnie swobody, pełny model czynnikowy.")
                    }
                }
            }

            # --- nonparametric: Kruskal-Wallis + Dunn (one factor) or ART (factorial)
            useKW <- isTRUE(opts$nonpar) && oneFactor
            useART <- isTRUE(opts$nonpar) && !oneFactor && factorsOnly
            self$results$npTests$setVisible(useKW || (isTRUE(opts$nonpar) && !factorsOnly))
            self$results$npMeans$setVisible(useKW)
            self$results$npPairs$setVisible(useKW && isTRUE(opts$showPairs))
            self$results$art$setVisible(useART)
            self$results$artMeans$setVisible(useART && method != "none")
            self$results$artPairs$setVisible(useART && method != "none" && isTRUE(opts$showPairs))
            if (isTRUE(opts$nonpar) && !factorsOnly) {
                self$results$npTests$setNote("na", "Analiza nieparametryczna jest dostępna tylko dla modelu z samymi czynnikami (bez bloków i kowariant).")
            }
            if (useKW) {
                y <- d[[dep]]; g <- d[[factors]]
                kw <- kruskalTable(y, g)
                npt <- self$results$npTests
                npt$addRow(rowKey = "kw", values = list(test = kw$test, stat = kw$stat, df = kw$df, p = kw$p, es = kw$es))
                npt$setNote("es", "Test Kruskala-Wallisa na rangach; ε² = H/(n − 1).")
                dn <- dunnPairs(y, g, adjust = "holm", alpha = alpha)
                nm <- self$results$npMeans
                for (i in seq_len(nrow(dn$levels))) {
                    r <- dn$levels[i, ]
                    nm$addRow(rowKey = r$level, values = list(level = r$level, n = r$n, median = r$median,
                        meanRank = r$meanRank, letters = r$letters))
                }
                nm$setNote("dunn", "Test Dunna z poprawką Holma; poziomy z tą samą literą nie różnią się istotnie; litera a = grupa z najniższą średnią rangą.")
                np <- self$results$npPairs
                for (i in seq_len(nrow(dn$pairs))) {
                    r <- dn$pairs[i, ]
                    np$addRow(rowKey = i, values = list(g1 = r$g1, g2 = r$g2, diff = r$diff, se = r$se, stat = r$stat, p = r$p))
                }
            }
            if (useART) {
                artT <- self$results$art
                at2 <- tryCatch(artTable(d, dep, factors), error = function(e) e)
                if (inherits(at2, "error")) artT$setNote("err", conditionMessage(at2))
                else {
                    for (i in seq_len(nrow(at2))) {
                        r <- at2[i, ]
                        artT$addRow(rowKey = r$term, values = list(source = r$source, F = r$F, df1 = r$df1, df2 = r$df2, p = r$p))
                    }
                    artT$setNote("art", paste0("Aligned Rank Transform (Wobbrock i in., 2011): dla każdego efektu odpowiedź ",
                        "wyrównana względem pozostałych efektów, zrangowana i poddana ANOVIE typu III; raportowany jest F tego efektu. ",
                        "Przy kilku czynnikach to nieparametryczny odpowiednik ANOVY czynnikowej."))
                    if (method != "none") for (f in factors) {
                        mt <- self$results$artMeans$get(key = f)
                        pt <- self$results$artPairs$get(key = f)
                        cmp <- tryCatch(artMainEffectComparisons(d, dep, factors, f, method, alpha), error = function(e) e)
                        if (inherits(cmp, "error")) { mt$setNote("err", conditionMessage(cmp)); next }
                        private$.fillComparison(mt, pt, cmp, method, alpha, "art")
                        mt$setNote("cld", paste0("Rangi wyrównane dla efektu ", f, "; ", phMethodLabel(method), "; ",
                            cmp$critNote %||% "", ". Poziomy z tą samą literą nie różnią się istotnie."))
                    }
                }
            }

            # --- parametric comparisons
            keys <- private$.termKeys()
            for (k in names(keys)) {
                term <- keys[[k]]
                mt <- self$results$means$get(key = k)
                pt <- self$results$pairs$get(key = k)
                img <- self$results$plotMeans$get(key = k)
                cmp <- tryCatch(compareTerm(res$fit, term, method, alpha, control = NULL, mse = res$mse),
                    error = function(e) e)
                if (inherits(cmp, "error")) {
                    mt$setNote("err", paste("Nie można policzyć średnich:", conditionMessage(cmp)))
                    next
                }
                private$.fillComparison(mt, pt, cmp, method, alpha, "means")
                m <- cmp$means
                if (length(term) == 2) {
                    st <- list(means = data.frame(xf = m[[term[1]]], gf = m[[term[2]]], mean = m$mean, se = m$se,
                        lower = m$lower, upper = m$upper, letters = m$letters, stringsAsFactors = FALSE),
                        xLabel = term[1], groupLabel = term[2], dep = dep, alpha = alpha, errorBars = opts$errorBars)
                } else {
                    st <- list(means = data.frame(xf = m$level, mean = m$mean, se = m$se, lower = m$lower,
                        upper = m$upper, letters = m$letters, stringsAsFactors = FALSE),
                        xLabel = term, groupLabel = NULL, dep = dep, alpha = alpha, errorBars = opts$errorBars)
                }
                img$setState(st)
            }

            # --- contrasts
            if (opts$contrastType != "none") {
                for (f in factors) {
                    ct <- tryCatch(contrastTable(res$fit, f, opts$contrastType), error = function(e) NULL)
                    tab <- self$results$contrasts$get(key = f)
                    tab$setTitle(paste("Kontrasty:", f))
                    if (is.null(ct)) next
                    for (i in seq_len(nrow(ct))) {
                        r <- ct[i, ]
                        tab$addRow(rowKey = i, values = list(contrast = r$contrast, estimate = r$estimate,
                            se = r$se, df = r$df, t = r$t, p = r$p))
                    }
                }
            }

            # --- descriptives and assumptions
            if (isTRUE(opts$desc)) {
                ds <- descriptivesTable(d, dep, factors)
                for (i in seq_len(nrow(ds))) self$results$desc$addRow(rowKey = i, values = as.list(ds[i, ]))
            }
            if (isTRUE(opts$homog)) {
                ht <- self$results$homog
                for (r in homogeneityTable(d[[dep]], cells)) ht$addRow(rowKey = r$test, values = r)
                ht$setNote("cells", "Jednorodność wariancji między komórkami (kombinacjami czynników).")
            }
            resid <- stats::residuals(res$fit)
            if (isTRUE(opts$norm)) {
                nt <- self$results$norm
                if (length(resid) >= 3 && length(resid) <= 5000) {
                    sw <- stats::shapiro.test(resid)
                    nt$setRow(rowNo = 1, values = list(w = unname(sw$statistic), p = sw$p.value))
                } else nt$setNote("n", "Test Shapiro-Wilka wymaga od 3 do 5000 reszt.")
            }
            self$results$residPlot$setState(data.frame(fitted = stats::fitted(res$fit), resid = resid))
            self$results$qq$setState(list(resid = resid))
            if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
                full <- rep(NA_real_, length(complete)); full[complete] <- resid
                self$results$residsOV$setValues(full)
            }
            if (length(factors) >= 2) {
                tm <- tryCatch(termMeans(res$fit, factors[1:2], alpha), error = function(e) NULL)
                if (!is.null(tm)) {
                    m <- tm$means
                    self$results$plotInteraction$setState(list(
                        means = data.frame(A = m[[factors[1]]], B = m[[factors[2]]], mean = m$mean, stringsAsFactors = FALSE),
                        A = factors[1], B = factors[2], dep = dep))
                    if (length(factors) > 2)
                        self$results$plotInteraction$setTitle(sprintf("Wykres interakcji: %s × %s (pierwsze dwa czynniki)", factors[1], factors[2]))
                }
            }
        },
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .residPlot = function(image, ggtheme, theme, ...) residPlot(image, ggtheme, theme),
        .qqPlot = function(image, ggtheme, theme, ...) qqPlot(image, ggtheme, theme)
    )
)
