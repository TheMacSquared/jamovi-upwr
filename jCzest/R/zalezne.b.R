#' @importFrom jmvcore .
zalezneClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "zalezneClass",
    inherit = zalezneBase,
    private = list(

        .pairsBuilt = FALSE,
        # opis zastosowanych metod (jmvcore::metodyNew, wspólny mechanizm jUPWR) — zbierany po drodze, renderowany na końcu .run
        .metody = NULL,

        # kolumny tabeli par zależą od poziomów pomiarów — jak w tabeli krzyżowej
        # budujemy je z .init ORAZ z .run (w GUI dane bywają dostępne dopiero tam)
        .buildPairCols = function(lv, vars) {
            if (isTRUE(private$.pairsBuilt) || length(lv) == 0) return(invisible(FALSE))
            t <- self$results$pairs
            # nazwy pomiarow jako naglowki (wiersze = 1. pomiar, kolumny = 2. pomiar),
            # zeby orientacji tabeli nie trzeba bylo tlumaczyc w nocie
            t$addColumn(name = "row", title = vars[1], type = "text")
            for (l in lv) t$addColumn(name = paste0("c_", l), title = l, type = "text",
                                      superTitle = vars[2])
            t$addColumn(name = "total", title = "Ogółem", type = "text")
            private$.pairsBuilt <- TRUE
            invisible(TRUE)
        },

        .init = function() {
            o <- self$options
            if (length(o$vars) < 2) return()
            v <- try(self$data[[o$vars[1]]], silent = TRUE)
            if (inherits(v, "try-error") || is.null(v)) return()
            private$.buildPairCols(levels(v), o$vars)
        },

        #' Kodowanie 0/1: PIERWSZY poziom = 1 („wystąpiło"), pozostałe = 0.
        #' Konwencja spójna z testem zgodności, gdzie też testujemy pierwszy poziom.
        .binary = function(vars) {
            d <- lapply(vars, function(v) {
                x <- self$data[[v]]
                if (!is.factor(x)) x <- factor(x)
                lv <- levels(x)
                if (length(lv) != 2) return(NULL)
                as.integer(x == lv[1])
            })
            if (any(vapply(d, is.null, logical(1)))) return(NULL)
            m <- do.call(cbind, d)
            colnames(m) <- vars
            m
        },

        .run = function() {
            o <- self$options
            vars <- o$vars
            if (length(vars) < 2) return()

            private$.metody <- jmvcore::metodyNew()
            if (length(vars) == 2) private$.runMcnemar(vars) else private$.runCochran(vars)
            private$.metody$render(self$results$metody)
        },

        # --- dwa pomiary: McNemar ---
        .runMcnemar = function(vars) {
            o <- self$options
            cnt <- if (optNonEmpty(o$counts)) jmvcore::toNumeric(self$data[[o$counts]]) else NULL
            tab <- pairedTable(self$data[[vars[1]]], self$data[[vars[2]]], cnt)
            t <- self$results$tests

            if (!all(dim(tab) == c(2, 2))) {
                t$setNote("d", "Test McNemara wymaga dwóch pomiarów o tych samych dwóch kategoriach.")
                return()
            }
            private$.buildPairCols(rownames(tab), vars)
            m <- private$.metody
            m$add("Dane", "Dwa pomiary tych samych jednostek: pierwszy „%s”, drugi „%s”; jednostka = wiersz arkusza; N = %s par kompletnych.",
                  vars[1], vars[2], format(sum(tab), big.mark = " "))
            m$addIf(optNonEmpty(o$counts), "Dane", "Dane zagregowane: liczności par z kolumny „%s”.", o$counts)
            m$add("Dane", "Kategoria traktowana jako „wystąpiło”: „%s” (pierwsza alfabetycznie) — jej dotyczą udziały i wykres.",
                  rownames(tab)[1])
            m$addIf(o$table, "Dane", "Tabela par: wiersze = „%s”, kolumny = „%s”; przekątna = pary zgodne; brzegi = liczność (udział %%).",
                    vars[1], vars[2])
            private$.fillPairs(tab, vars)
            # przy DWOCH pomiarach udzialy sa juz brzegami tabeli par — osobna
            # tabela powtarzalaby te same liczby, wiec zostaje tylko stan wykresu
            self$results$marg$setVisible(FALSE)
            private$.fillMarg(stats::setNames(c(sum(tab[1, ]), sum(tab[, 1])), vars),
                              sum(tab), rownames(tab)[1])

            mc <- mcnemar(tab, correct = isTRUE(o$corr))
            lab <- if (isTRUE(o$corr)) "McNemar (z poprawką ciągłości)" else "McNemar"
            m$add("Testy", "Test McNemara na parach niezgodnych b i c: χ² = (|b − c|%s)² / (b + c), df = 1.",
                  if (isTRUE(o$corr)) " − 1" else "")
            m$add("Testy", "Warunek stosowalności sprawdzany automatycznie: b + c ≥ 25 — tu %s.",
                  if (mc$discordant >= 25) "spełniony" else "niespełniony (ostrzeżenie nad wynikami)")
            # OR par niezgodnych to miara dla TEJ SAMEJ pary pomiarow co test,
            # wiec idzie w wiersz testu — nie do osobnej tabeli z jednym wierszem
            or <- if (isTRUE(o$effSize)) mcnemarOR(tab, level = o$ciWidth / 100)
                  else list(est = NULL, lower = NULL, upper = NULL)
            if (is.na(mc$stat)) {
                t$setNote("z", "Brak par niezgodnych — pomiary są identyczne, więc testu nie da się policzyć.")
            } else {
                t$addRow(rowKey = "mc", values = list(test = lab, stat = mc$stat, df = mc$df, p = mc$p,
                                                      or = or$est, lower = or$lower, upper = or$upper))
                m$addIf(o$effSize, "Wielkość efektu", paste(
                    "OR par niezgodnych = liczba par „%s → %s” / liczba par „%s → %s”; 1 = brak zmiany;",
                    "przedział ufności %g%% metodą Walda na skali logarytmicznej."),
                    rownames(tab)[1], rownames(tab)[2], rownames(tab)[2], rownames(tab)[1], o$ciWidth)
            }

            if (isTRUE(o$exact)) {
                ex <- mcnemarExact(tab)
                if (!is.null(ex) && !is.na(ex$p)) {
                    t$addRow(rowKey = "ex", values = list(
                        test = "Dokładny test dwumianowy", stat = NA_real_, df = NA_integer_, p = ex$p))
                    m$add("Testy", "Dokładny test dwumianowy: b ~ Bin(b + c, ½), p dwustronne.")
                }
            }
            private$.describePlot(rownames(tab)[1])

            # warunek stosowalności: przybliżenie χ² wymaga dość par niezgodnych
            a <- checkMcnemar(tab)
            if (!is.null(a) && !isTRUE(a$ok) && a$discordant > 0)
                self$results$insert(1, jmvcore::Notice$new(
                    self$options, name = ".assumption", type = jmvcore::NoticeType$WARNING,
                    content = sprintf(paste(
                        "Pary niezgodne: %d (zwyczajowo wymaga się co najmniej 25),",
                        "więc przybliżenie χ² jest zawodne. Użyj dokładnego testu dwumianowego."),
                        a$discordant)))
        },

        # --- trzy i więcej pomiarów: Q Cochrana ---
        .runCochran = function(vars) {
            o <- self$options
            t <- self$results$tests
            m <- private$.binary(vars)
            if (is.null(m)) {
                t$setNote("b", "Q Cochrana wymaga, żeby każdy pomiar miał dokładnie 2 kategorie.")
                return()
            }
            if (optNonEmpty(o$counts))
                t$setNote("c", "Kolumna liczności działa tylko przy dwóch pomiarach (McNemar).")

            q <- cochranQ(m)
            if (is.null(q)) { t$setNote("n", "Brak kompletnych obserwacji."); return() }
            md <- private$.metody
            lv1 <- levels(factor(self$data[[vars[1]]]))[1]
            md$add("Dane", "%d pomiary tych samych jednostek: %s; jednostka = wiersz arkusza; N = %d jednostek bez braków.",
                   length(vars), jmvcore::metodyCyt(vars), q$n)
            md$add("Dane", "Kategoria traktowana jako „wystąpiło”: „%s” (pierwsza alfabetycznie w pierwszym pomiarze) — jej dotyczą udziały i wykres.",
                   lv1)
            md$add("Testy", "Trzy i więcej pomiarów → Q Cochrana, df = k − 1 = %d (dla k = 2 równoważne testowi McNemara).",
                   q$df)

            if (is.na(q$stat)) {
                t$setNote("z", paste("Żadna jednostka nie różnicuje pomiarów (wszędzie same",
                                     "wystąpienia albo same braki), więc Q jest nieokreślone."))
            } else {
                t$addRow(rowKey = "q", values = list(test = "Q Cochrana", stat = q$stat,
                                                     df = q$df, p = q$p))
                t$setNote("n", sprintf("N = %d, k = %d.", q$n, length(vars)))
            }

            private$.fillMarg(stats::setNames(q$props * q$n, vars), q$n, lv1)
            private$.describePlot(lv1)

            # OR jest miara dla PARY pomiarow, wiec przy k >= 3 nie ma jednej
            # wartosci — chowamy kolumny w tabeli testow i podajemy OR osobno
            # dla kazdej pary w post-hoc
            for (col in c("or", "lower", "upper")) t$getColumn(col)$setVisible(FALSE)

            if (isTRUE(o$posthoc)) {
                pw <- pairwiseMcnemar(m, vars)
                ph <- self$results$posthoc
                if (!is.null(pw)) {
                    for (i in seq_len(nrow(pw)))
                        ph$addRow(rowKey = i, values = list(
                            g1 = pw$g1[i], g2 = pw$g2[i], disc = pw$disc[i],
                            stat = pw$stat[i], p = pw$p[i],
                            or = pw$or[i], lower = pw$lower[i], upper = pw$upper[i]))
                    md$add("Post-hoc", paste(
                        "Dla każdej pary pomiarów test McNemara (%d porównań), p skorygowane metodą Holma;",
                        "OR par niezgodnych z 95%% przedziałem ufności, 1 = brak zmiany."), nrow(pw))
                }
            } else if (isTRUE(o$effSize)) {
                self$results$tests$setNote("es", paste(
                    "Wielkość efektu dla trzech i więcej pomiarów jest określona parami —",
                    "włącz „Porównania par pomiarów” w sekcji Zaawansowane."))
            }
        },

        .fillMarg = function(counts, n, level = NULL) {
            if (isTRUE(self$options$plot) && n > 0)
                self$results$plot$setState(list(
                    vars = names(counts), prop = as.numeric(counts) / n,
                    n = n, level = level))
            if (!isTRUE(self$options$props)) return()
            t <- self$results$marg
            for (i in seq_along(counts))
                t$addRow(rowKey = names(counts)[i], values = list(
                    var = names(counts)[i], n = counts[[i]],
                    prop = if (n > 0) counts[[i]] / n else NA_real_))
            # ktorej kategorii dotycza udzialy, mowi opis metod (sekcja Dane)
        },

        .describePlot = function(level) {
            if (!isTRUE(self$options$plot)) return()
            private$.metody$add("Wykres", paste(
                "Udział kategorii „%s” w kolejnych pomiarach; linia łączy słupki,",
                "bo to te same jednostki mierzone kilka razy."), level)
        },

        .plot = function(image, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            df <- data.frame(pomiar = factor(st$vars, levels = st$vars),
                             udzial = st$prop, stringsAsFactors = FALSE)
            lab <- if (is.null(st$level)) "Udział" else sprintf("Udział kategorii „%s”", st$level)
            p <- ggplot2::ggplot(df, ggplot2::aes(x = pomiar, y = udzial, group = 1)) +
                ggplot2::geom_col(width = 0.6, fill = "grey60") +
                # linia laczy slupki, bo to TE SAME jednostki mierzone kilka razy —
                # bez niej wykres wyglada jak porownanie niezaleznych grup
                ggplot2::geom_line(colour = "grey25", linewidth = 0.7) +
                ggplot2::geom_point(size = 2.6, colour = "grey25") +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 100 * udzial)),
                                   vjust = -1.1, size = 3.6) +
                ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                            limits = c(0, min(1, max(df$udzial) * 1.25 + 0.05))) +
                ggplot2::labs(x = NULL, y = lab,
                              caption = sprintf("N = %d", st$n)) +
                ggplot2::theme_minimal()
            print(p)
            TRUE
        },

        .fillPairs = function(tab, vars) {
            if (!isTRUE(self$options$table)) return()
            t <- self$results$pairs
            lv <- colnames(tab)
            n <- sum(tab)

            # liczba bez zbednych zer; caloscia steruje .b.R, bo kolumny sa tekstowe
            num <- function(x) if (abs(x - round(x)) < 1e-9) format(round(x)) else sprintf("%.1f", x)
            # brzeg = liczność z udziałem w nawiasie, zeby rozklady obu pomiarow
            # byly widoczne wprost w tabeli, a nie w nocie
            marg <- function(x) sprintf("%s (%.1f%%)", num(x), 100 * x / n)

            for (i in seq_len(nrow(tab))) {
                vals <- list(row = rownames(tab)[i], total = marg(sum(tab[i, ])))
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- num(tab[i, j])
                t$addRow(rowKey = rownames(tab)[i], values = vals)
            }
            vals <- list(row = "Ogółem", total = num(n))
            for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- marg(sum(tab[, j]))
            t$addRow(rowKey = ".total", values = vals)
        }
    )
)
