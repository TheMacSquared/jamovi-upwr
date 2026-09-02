#' @importFrom jmvcore .
anovarmClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "anovarmClass",
    inherit = anovarmBase,
    private = list(
        .termKeys = function() {
            o <- self$options
            keys <- list()
            if (isTRUE(o$phWithin)) for (f in o$within) keys[[f]] <- f
            if (isTRUE(o$phBetween)) for (f in o$between) keys[[f]] <- f
            all <- c(o$within, o$between)
            if (isTRUE(o$phInter) && length(all) >= 2)
                for (pr in utils::combn(all, 2, simplify = FALSE))
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
            for (f in c(self$options$within, self$options$between))
                self$results$contrasts$addItem(key = f)
        },
        .run = function() {
            o <- self$options
            dep <- o$dep; subject <- o$subject
            within <- o$within; between <- o$between; covs <- o$covs
            if (!optNonEmpty(dep) || !optNonEmpty(subject) || length(within) == 0) return()

            vars <- c(dep, subject, within, between, covs)
            d <- self$data[vars]
            d[[dep]] <- jmvcore::toNumeric(d[[dep]])
            for (v in covs) d[[v]] <- jmvcore::toNumeric(d[[v]])
            d[[subject]] <- factor(d[[subject]])
            for (v in c(within, between)) d[[v]] <- factor(d[[v]])
            d <- d[stats::complete.cases(d), , drop = FALSE]
            for (v in c(subject, within, between)) d[[v]] <- droplevels(d[[v]])
            if (nrow(d) < 4) return()
            at <- self$results$anova
            for (v in c(within, between)) if (nlevels(d[[v]]) < 2) {
                at$setNote("err", sprintf("Zmienna %s musi mieć co najmniej 2 poziomy.", v)); return()
            }
            # every subject must appear in every within cell
            wcells <- cellsFactor(d, within)
            tab <- table(d[[subject]], wcells)
            if (any(tab == 0)) {
                at$setNote("err", paste0("Dane niekompletne: każda jednostka musi mieć obserwację w każdej ",
                    "kombinacji czynników wewnątrzobiektowych (", sum(tab == 0), " brakujących komórek)."))
                return()
            }
            if (any(tab > 1))
                at$setNote("agg", "Powtórzone obserwacje w tej samej komórce jednostki uśredniono.")
            if (length(between)) {
                bs <- unique(d[c(subject, between)])
                if (any(duplicated(bs[[subject]]))) {
                    at$setNote("err", "Czynnik międzyobiektowy musi być stały w obrębie jednostki."); return()
                }
            }

            res <- tryCatch(fitRm(d, dep, subject, within, between, covs, o$ss), error = function(e) e)
            if (inherits(res, "error")) {
                at$setNote("err", paste("Błąd dopasowania modelu:", conditionMessage(res))); return()
            }
            tb <- rmTable(res, o$spherCorr)
            for (i in seq_len(nrow(tb))) {
                r <- tb[i, ]
                at$addRow(rowKey = r$term, values = list(source = r$source, ss = r$ss, df1 = r$df1,
                    df2 = r$df2, mse = r$mse, F = r$F, p = r$p, ges = r$ges, pes = r$pes))
            }
            corrNote <- switch(o$spherCorr, none = "bez poprawki na sferyczność",
                GG = "stopnie swobody z poprawką Greenhouse'a-Geissera",
                HF = "stopnie swobody z poprawką Huynha-Feldta")
            at$setNote("ss", sprintf("Sumy kwadratów typu %s; %s. Efekty wewnątrzobiektowe testowane wobec błędu swojej warstwy (MS błędu, df błędu).",
                o$ss, corrNote))

            if (isTRUE(o$spherTests)) {
                st <- self$results$spher
                sp <- tryCatch(sphericityTable(res$fit), error = function(e) NULL)
                if (is.null(sp)) {
                    st$setNote("na", "Test sferyczności wymaga czynnika wewnątrzobiektowego o co najmniej 3 poziomach.")
                } else {
                    for (i in seq_len(nrow(sp))) {
                        r <- sp[i, ]
                        st$addRow(rowKey = r$term, values = list(source = r$source, W = r$W, p = r$p, gg = r$gg, hf = r$hf))
                    }
                    st$setNote("eps", "p < α oznacza naruszenie sferyczności; wtedy użyj poprawki GG (ε < 0,75) lub HF (ε ≥ 0,75).")
                }
            }

            # nonparametric (one within factor, nothing else)
            oneWithin <- length(within) == 1 && length(between) == 0 && length(covs) == 0
            if (isTRUE(o$friedman) || isTRUE(o$page)) {
                npt <- self$results$npTests
                if (!oneWithin) {
                    npt$setNote("na", "Testy Friedmana i Page'a są dostępne tylko dla jednego czynnika wewnątrzobiektowego bez czynników międzyobiektowych i kowariant.")
                } else {
                    m <- rmMatrix(d, dep, subject, within)
                    addNp <- function(key, r) if (!is.null(r))
                        npt$addRow(rowKey = key, values = list(test = r$test, stat = r$stat, df = r$df,
                            z = r$z %||% NA, p = r$p, es = r$es))
                    if (isTRUE(o$friedman)) addNp("fr", friedmanTable(m))
                    if (isTRUE(o$page)) {
                        pg <- pageTable(m)
                        if (is.null(pg)) npt$setNote("pg", "Test Page'a wymaga co najmniej 3 poziomów.")
                        else addNp("pg", pg)
                    }
                    npt$setNote("es", "W Kendalla = χ²/(n(k − 1)). Trend Page'a: kierunek wg kolejności poziomów (z > 0 = rosnący), p dwustronne.")
                    if (isTRUE(o$friedman)) {
                        fp <- friedmanPairs(m, method = o$npPostHoc, adjust = "holm", alpha = o$alpha)
                        nm <- self$results$npMeans
                        for (i in seq_len(nrow(fp$levels))) {
                            r <- fp$levels[i, ]
                            nm$addRow(rowKey = r$level, values = list(level = r$level, n = r$n, median = r$median,
                                meanRank = r$meanRank, letters = r$letters))
                        }
                        nm$setNote("np", sprintf("Test %s; poziomy z tą samą literą nie różnią się istotnie; litera a = grupa z najniższą średnią rangą (rangi w obrębie jednostki).",
                            if (o$npPostHoc == "nemenyi") "Nemenyiego" else "Conovera z poprawką Holma"))
                        np <- self$results$npPairs
                        for (i in seq_len(nrow(fp$pairs))) {
                            r <- fp$pairs[i, ]
                            np$addRow(rowKey = i, values = list(g1 = r$g1, g2 = r$g2, diff = r$diff, se = r$se, stat = r$stat, p = r$p))
                        }
                        np$getColumn("stat")$setTitle(if (o$npPostHoc == "nemenyi") "q" else "t")
                    }
                }
            }
            if (isTRUE(o$art)) {
                artT <- self$results$art
                if (length(covs) > 0) {
                    artT$setNote("na", "ART wymaga modelu z samymi czynnikami (bez kowariant).")
                } else {
                    at2 <- tryCatch(artTableRm(d, dep, subject, within, between, o$ss), error = function(e) e)
                    if (inherits(at2, "error")) artT$setNote("err", conditionMessage(at2))
                    else {
                        for (i in seq_len(nrow(at2))) {
                            r <- at2[i, ]
                            artT$addRow(rowKey = r$term, values = list(source = r$source, F = r$F, df1 = r$df1, df2 = r$df2, p = r$p))
                        }
                        artT$setNote("art", paste0("Aligned Rank Transform (Wobbrock i in., 2011): dla każdego efektu odpowiedź ",
                            "wyrównana względem pozostałych efektów, zrangowana i poddana ANOVIE powtórzonych pomiarów ",
                            "z właściwymi warstwami błędu; raportowany jest F tego efektu."))
                    }
                }
            }

            keys <- private$.termKeys()
            method <- o$postHoc; alpha <- o$alpha
            for (k in names(keys)) {
                term <- keys[[k]]
                mt <- self$results$means$get(key = k)
                pt <- self$results$pairs$get(key = k)
                img <- self$results$plotMeans$get(key = k)
                mse <- rmMseFor(res$an0, term)
                cmp <- tryCatch(compareTerm(res$fit, term, method, alpha, control = NULL, mse = mse),
                    error = function(e) e)
                if (inherits(cmp, "error")) {
                    mt$setNote("err", paste("Nie można policzyć średnich:", conditionMessage(cmp))); next
                }
                for (i in seq_len(nrow(cmp$means))) {
                    r <- cmp$means[i, ]
                    mt$addRow(rowKey = r$level, values = list(level = r$level, mean = r$mean, se = r$se,
                        lower = r$lower, upper = r$upper, letters = r$letters))
                }
                if (method == "none") {
                    mt$getColumn("letters")$setVisible(FALSE)
                    mt$setNote("emm", sprintf("Średnie brzegowe z modelu, %g%% CI.", 100 * (1 - alpha)))
                } else if (method == "dunnett") {
                    mt$getColumn("letters")$setTitle("vs kontrola")
                    mt$setNote("dun", paste0("Kontrola = pierwszy poziom; * różni się istotnie od kontroli; ", cmp$critNote))
                } else {
                    mt$setNote("cld", paste0("Średnie brzegowe z modelu; ", phMethodLabel(method), "; ", cmp$critNote,
                        ". Poziomy z tą samą literą nie różnią się istotnie; litera a = grupa z najniższą średnią."))
                }
                if (!is.null(cmp$pairs)) {
                    for (i in seq_len(nrow(cmp$pairs))) {
                        r <- cmp$pairs[i, ]
                        pt$addRow(rowKey = i, values = list(g1 = r$g1, g2 = r$g2, diff = r$diff, se = r$se,
                            df = r$df, stat = r$stat, p = r$p, crit = r$crit, lower = r$lower, upper = r$upper, d = r$d))
                    }
                    if (method == "holm") {
                        for (cn in c("crit", "lower", "upper")) pt$getColumn(cn)$setVisible(FALSE)
                        pt$setNote("holm", "p skorygowane metodą Holma.")
                    } else {
                        pt$setNote("crit", sprintf("%s; przedział ufności = różnica ± %s (poziom %g%%).",
                            phMethodLabel(method), if (method == "bonf") "różnica graniczna Bonferroniego" else phCritLabel(method),
                            100 * (1 - alpha)))
                    }
                }
                m <- cmp$means
                if (length(term) == 2) {
                    st <- list(means = data.frame(xf = m[[term[1]]], gf = m[[term[2]]], mean = m$mean, se = m$se,
                        lower = m$lower, upper = m$upper, letters = m$letters, stringsAsFactors = FALSE),
                        xLabel = term[1], groupLabel = term[2], dep = dep, alpha = alpha, errorBars = o$errorBars)
                } else {
                    st <- list(means = data.frame(xf = m$level, mean = m$mean, se = m$se, lower = m$lower,
                        upper = m$upper, letters = m$letters, stringsAsFactors = FALSE),
                        xLabel = term, groupLabel = NULL, dep = dep, alpha = alpha, errorBars = o$errorBars)
                }
                img$setState(st)
            }

            if (o$contrastType != "none") {
                for (f in c(within, between)) {
                    ct <- tryCatch(contrastTable(res$fit, f, o$contrastType), error = function(e) NULL)
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

            if (isTRUE(o$desc)) {
                ds <- descriptivesTable(d, dep, c(within, between))
                for (i in seq_len(nrow(ds))) self$results$desc$addRow(rowKey = i, values = as.list(ds[i, ]))
            }

            if (isTRUE(o$homog)) {
                ht <- self$results$homog
                if (length(between)) {
                    sm <- subjectMeans(d, dep, subject, between)
                    for (r in homogeneityTable(sm[[dep]], cellsFactor(sm, between))) ht$addRow(rowKey = r$test, values = r)
                    ht$setNote("cells", "Test na średnich jednostek (uśrednionych po czynnikach wewnątrzobiektowych) między grupami czynników międzyobiektowych.")
                } else ht$setNote("na", "Test Levene'a wymaga czynnika międzyobiektowego.")
            }
            resid <- tryCatch(as.numeric(stats::residuals(res$fit$lm)), error = function(e) NULL)
            if (isTRUE(o$norm)) {
                nt <- self$results$norm
                if (!is.null(resid) && length(resid) >= 3 && length(resid) <= 5000) {
                    sw <- stats::shapiro.test(resid)
                    nt$setRow(rowNo = 1, values = list(w = unname(sw$statistic), p = sw$p.value))
                } else nt$setNote("n", "Test Shapiro-Wilka wymaga od 3 do 5000 reszt.")
            }
            if (!is.null(resid)) self$results$qq$setState(list(resid = resid))

            all <- c(within, between)
            if (length(all) >= 2) {
                tm <- tryCatch(termMeans(res$fit, all[1:2], alpha), error = function(e) NULL)
                if (!is.null(tm)) {
                    m <- tm$means
                    self$results$plotInteraction$setState(list(
                        means = data.frame(A = m[[all[1]]], B = m[[all[2]]], mean = m$mean, stringsAsFactors = FALSE),
                        A = all[1], B = all[2], dep = dep))
                }
            }
        },
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .qqPlot = function(image, ggtheme, theme, ...) qqPlot(image, ggtheme, theme)
    )
)
