#' @importFrom jmvcore .
tabelaClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "tabelaClass",
    inherit = tabelaBase,
    private = list(

        # kolumny tabeli liczności i reszt zależą od poziomów zmiennej kolumnowej,
        # więc budujemy je dopiero, gdy zmienne są wybrane
        .init = function() {
            o <- self$options
            if (!optNonEmpty(o$rows) || !optNonEmpty(o$cols)) return()
            lv <- private$.colLevels()
            if (length(lv) == 0) return()

            freqs <- self$results$freqs
            freqs$addColumn(name = "row", title = "", type = "text", combineBelow = TRUE)
            freqs$addColumn(name = "kind", title = "", type = "text")
            for (l in lv)
                freqs$addColumn(name = paste0("c_", l), title = l, type = "number",
                                superTitle = o$cols)
            freqs$addColumn(name = "total", title = "Ogółem", type = "number")

            res <- self$results$resid
            res$addColumn(name = "row", title = "", type = "text")
            for (l in lv)
                res$addColumn(name = paste0("c_", l), title = l, type = "number",
                              superTitle = o$cols)
        },

        .colLevels = function() {
            o <- self$options
            v <- self$data[[o$cols]]
            if (is.null(v)) return(character(0))
            levels(droplevels(factor(v[!is.na(v)])))
        },

        .table = function() {
            o <- self$options
            r <- self$data[[o$rows]]; c0 <- self$data[[o$cols]]
            cnt <- if (optNonEmpty(o$counts)) jmvcore::toNumeric(self$data[[o$counts]]) else NULL
            buildTable(r, c0, cnt)
        },

        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$rows) || !optNonEmpty(o$cols)) return()
            tab <- private$.table()
            if (any(dim(tab) < 2) || sum(tab) == 0) {
                self$results$tests$setNote("n", "Każda zmienna musi mieć co najmniej 2 poziomy z obserwacjami.")
                return()
            }

            private$.fillFreqs(tab)
            private$.assumptionNotice(tab)
            private$.fillTests(tab)
            private$.fillEffSize(tab)
            private$.fillMeasures(tab)
            private$.fillOrdinal(tab)
            private$.fillTrend(tab)
            private$.fillResid(tab)
            private$.fillPairwise(tab)

            if (o$plot != "none")
                self$results$plot$setState(list(tab = tab, kind = o$plot, pc = o$pc,
                                                xlab = o$rows, fill = o$cols))
        },

        # --- tabela liczności: obserwowane, oczekiwane, procenty ---
        .fillFreqs = function(tab) {
            o <- self$options
            t <- self$results$freqs
            lv <- colnames(tab)
            e <- expectedCounts(tab)
            pcLab <- switch(o$pc, row = "% wierszem", col = "% kolumną", total = "% ogółu", NULL)

            for (i in seq_len(nrow(tab))) {
                rn <- rownames(tab)[i]
                vals <- list(row = rn, kind = "Liczność")
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- tab[i, j]
                vals$total <- sum(tab[i, ])
                t$addRow(rowKey = paste0(rn, "_n"), values = vals)

                if (isTRUE(o$exp)) {
                    vals <- list(row = rn, kind = "Oczekiwana")
                    for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- e[i, j]
                    vals$total <- sum(e[i, ])
                    t$addRow(rowKey = paste0(rn, "_e"), values = vals)
                }

                if (!is.null(pcLab)) {
                    den <- switch(o$pc, row = sum(tab[i, ]), total = sum(tab), col = NA)
                    vals <- list(row = rn, kind = pcLab)
                    for (j in seq_along(lv)) {
                        d <- if (o$pc == "col") sum(tab[, j]) else den
                        vals[[paste0("c_", lv[j])]] <- if (d > 0) 100 * tab[i, j] / d else NA_real_
                    }
                    vals$total <- if (o$pc == "row") 100 else
                        if (o$pc == "total") 100 * sum(tab[i, ]) / sum(tab) else NA_real_
                    t$addRow(rowKey = paste0(rn, "_p"), values = vals)
                }
            }

            vals <- list(row = "Ogółem", kind = "Liczność")
            for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sum(tab[, j])
            vals$total <- sum(tab)
            t$addRow(rowKey = "._total", values = vals)
        },

        # --- SEDNO MODUŁU: automatyczna kontrola warunku E >= 5 ---
        .assumptionNotice = function(tab) {
            a <- checkAssumption(tab)
            if (isTRUE(a$ok)) return()
            msg <- if (a$is2x2)
                sprintf(paste("W tabeli 2×2 najmniejsza liczebność oczekiwana wynosi %.2f (< 5),",
                              "więc przybliżenie χ² jest zawodne.%s"),
                        a$minExpected,
                        if (a$fisherFeasible) " Użyj dokładnego testu Fishera (sekcja „Założenia”)." else "")
            else
                sprintf(paste("%d z %d komórek (%.0f%%) ma liczebność oczekiwaną < 5,",
                              "a najmniejsza wynosi %.2f — przybliżenie χ² jest zawodne.%s"),
                        a$nBelow5, a$nCells, a$pctBelow5, a$minExpected,
                        if (a$fisherFeasible) " Rozważ dokładny test Fishera lub połączenie kategorii."
                        else " Rozważ połączenie kategorii.")

            notice <- jmvcore::Notice$new(self$options, name = ".assumption",
                                          type = jmvcore::NoticeType$WARNING, content = msg)
            self$results$insert(1, notice)
        },

        .fillTests = function(tab) {
            o <- self$options
            t <- self$results$tests
            n <- sum(tab)
            if (isTRUE(o$chiSq)) {
                r <- chiSqTest(tab, correct = FALSE)
                t$addRow(rowKey = "chi", values = list(test = "χ²", stat = r$stat, df = r$df, p = r$p))
            }
            if (isTRUE(o$chiSqCorr)) {
                if (all(dim(tab) == c(2, 2))) {
                    r <- chiSqTest(tab, correct = TRUE)
                    t$addRow(rowKey = "chic", values = list(test = "χ² z poprawką ciągłości",
                                                           stat = r$stat, df = r$df, p = r$p))
                } else {
                    t$setNote("cc", "Poprawka ciągłości dotyczy wyłącznie tabel 2×2.")
                }
            }
            if (isTRUE(o$likeRat)) {
                r <- likeRatTest(tab)
                t$addRow(rowKey = "lr", values = list(test = "G² (iloraz wiarygodności)",
                                                     stat = r$stat, df = r$df, p = r$p))
            }
            if (isTRUE(o$fisher)) {
                ft <- try(stats::fisher.test(tab), silent = TRUE)
                if (inherits(ft, "try-error"))
                    t$setNote("f", "Dokładny test Fishera nie policzył się dla tej tabeli (za duża).")
                else
                    t$addRow(rowKey = "fi", values = list(test = "Dokładny test Fishera",
                                                         stat = NA_real_, df = NA_integer_, p = ft$p.value))
            }
            t$addRow(rowKey = "n", values = list(test = "N", stat = n, df = NA_integer_, p = NA_real_))
        },

        .fillEffSize = function(tab) {
            o <- self$options
            if (!isTRUE(o$effSize)) return()
            t <- self$results$effsize
            v <- cramersV(tab)
            ci <- if (isTRUE(o$effSizeCI)) cramersVCI(tab, level = o$ciWidth / 100) else c(NA_real_, NA_real_)
            # progi Cohena zależą od mniejszego wymiaru tabeli (df* = min(r,c) - 1)
            dfStar <- min(dim(tab)) - 1
            small <- 0.1 / sqrt(dfStar); med <- 0.3 / sqrt(dfStar); large <- 0.5 / sqrt(dfStar)
            interp <- if (!is.finite(v)) "" else if (v < small) "poniżej słabego"
                      else if (v < med) "słaby" else if (v < large) "umiarkowany" else "silny"
            t$addRow(rowKey = "v", values = list(measure = "V Craméra", value = v,
                                                 lower = ci[1], upper = ci[2], interp = interp))
            t$setNote("th", sprintf("Progi Cohena dla tej tabeli: słaby %.2f, umiarkowany %.2f, silny %.2f.",
                                    small, med, large))
        },

        .fillMeasures = function(tab) {
            o <- self$options
            if (!(isTRUE(o$odds) || isTRUE(o$relRisk) || isTRUE(o$diffProp))) return()
            t <- self$results$measures
            if (!all(dim(tab) == c(2, 2))) {
                t$setNote("d", "Te miary są określone tylko dla tabel 2×2.")
                return()
            }
            m <- twoByTwo(tab, level = o$ciWidth / 100, compare = o$compare)
            add <- function(key, lab, x)
                t$addRow(rowKey = key, values = list(measure = lab, value = x$est,
                                                     lower = x$lower, upper = x$upper))
            if (isTRUE(o$odds))     add("or", "Iloraz szans (OR)", m$or)
            if (isTRUE(o$relRisk))  add("rr", "Ryzyko względne (RR)", m$rr)
            if (isTRUE(o$diffProp)) add("dp", "Różnica proporcji", m$dp)

            # Kierunek miar zależy od kolejności poziomów, a ta jest alfabetyczna —
            # bez tej noty student nie wie, czy dostał OR czy jego odwrotność.
            grp <- if (o$compare == "cols") colnames(tab) else rownames(tab)
            ev  <- if (o$compare == "cols") rownames(tab) else colnames(tab)
            t$setNote("dir", sprintf(
                "Porównanie: „%s” względem „%s”, pod względem udziału kategorii „%s”. Poziomy uporządkowane alfabetycznie — odwrotna kolejność odwraca OR i RR.",
                grp[1], grp[2], ev[1]))
        },

        .fillOrdinal = function(tab) {
            o <- self$options
            if (!(isTRUE(o$gamma) || isTRUE(o$taub))) return()
            t <- self$results$ordinal
            m <- ordinalMeasures(tab)
            if (isTRUE(o$gamma)) t$addRow(rowKey = "g", values = list(measure = "Gamma", value = m$gamma))
            if (isTRUE(o$taub))  t$addRow(rowKey = "t", values = list(measure = "Tau-b Kendalla", value = m$taub))
            t$setNote("o", "Miary mają sens tylko dla kategorii uporządkowanych.")
        },

        .fillTrend = function(tab) {
            if (!isTRUE(self$options$trend)) return()
            t <- self$results$trend
            ca <- cochranArmitage(tab)
            if (is.null(ca)) {
                t$setNote("t", "Test trendu wymaga tabeli 2×k (lub k×2) z co najmniej 3 uporządkowanymi kategoriami.")
                return()
            }
            t$addRow(rowKey = "ca", values = list(z = ca$z, p = ca$p))
            t$setNote("s", "Kategoriom przypisano kolejne liczby naturalne jako wartości.")
        },

        .fillResid = function(tab) {
            o <- self$options
            if (!isTRUE(o$resid)) return()
            t <- self$results$resid
            sr <- stdResiduals(tab)
            crit <- residCritical(o$alpha)
            lv <- colnames(tab)
            for (i in seq_len(nrow(tab))) {
                vals <- list(row = rownames(tab)[i])
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- sr[i, j]
                t$addRow(rowKey = rownames(tab)[i], values = vals)
            }
            t$setNote("c", sprintf("Komórki z |z| > %.2f (α = %s) decydują o zależności.", crit, format(o$alpha)))
        },

        .fillPairwise = function(tab) {
            if (!isTRUE(self$options$pairwise)) return()
            t <- self$results$pairwise
            pw <- pairwiseRows(tab)
            if (is.null(pw)) {
                t$setNote("p", "Porównania par wymagają co najmniej 3 poziomów w wierszach.")
                return()
            }
            for (i in seq_len(nrow(pw)))
                t$addRow(rowKey = i, values = list(g1 = pw$g1[i], g2 = pw$g2[i],
                                                   stat = pw$stat[i], df = pw$df[i], p = pw$p[i]))
        },

        .plot = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            tab <- st$tab
            df <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
            names(df) <- c("w", "k", "n")

            if (st$kind == "bar") {
                if (st$pc == "row") {
                    tot <- tapply(df$n, df$w, sum)
                    df$val <- 100 * df$n / tot[df$w]
                    ylab <- "% wierszem"
                } else if (st$pc == "col") {
                    tot <- tapply(df$n, df$k, sum)
                    df$val <- 100 * df$n / tot[df$k]
                    ylab <- "% kolumną"
                } else {
                    df$val <- df$n; ylab <- "Liczność"
                }
                p <- ggplot2::ggplot(df, ggplot2::aes(x = w, y = val, fill = k)) +
                    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
                    ggplot2::labs(x = st$xlab, y = ylab, fill = st$fill)
            } else {
                # mozaika: szerokość kolumny ~ liczebność brzegowa wiersza,
                # podział wewnątrz ~ rozkład warunkowy — własna geometria (geom_rect)
                rw <- rowSums(tab) / sum(tab)
                xr <- cumsum(c(0, rw))
                rects <- do.call(rbind, lapply(seq_len(nrow(tab)), function(i) {
                    cw <- tab[i, ] / sum(tab[i, ])
                    yr <- cumsum(c(0, cw))
                    data.frame(xmin = xr[i], xmax = xr[i + 1],
                               ymin = yr[-length(yr)], ymax = yr[-1],
                               w = rownames(tab)[i], k = colnames(tab), stringsAsFactors = FALSE)
                }))
                mid <- (xr[-length(xr)] + xr[-1]) / 2
                p <- ggplot2::ggplot(rects) +
                    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                                    fill = k), colour = "white", linewidth = 0.6) +
                    ggplot2::scale_x_continuous(breaks = mid, labels = rownames(tab)) +
                    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
                    ggplot2::labs(x = st$xlab, y = "Udział w wierszu", fill = st$fill)
            }
            print(p + ggplot2::theme_minimal())
            TRUE
        }
    )
)
