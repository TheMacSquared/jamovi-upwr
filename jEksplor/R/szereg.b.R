#' @importFrom jmvcore .
szeregClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "szeregClass",
    inherit = szeregBase,
    private = list(
        .run = function() {
            o <- self$options
            if (!optNonEmpty(o$var)) return()
            x <- jmvcore::toNumeric(self$data[[o$var]]); x <- x[!is.na(x)]
            ct <- self$results$classes; st <- self$results$stats
            if (length(x) < 2) { ct$setNote("n", "Za mało obserwacji (n < 2)."); return() }
            start <- if (isTRUE(o$startAuto)) NULL else o$start
            if (!is.null(start) && start > min(x)) { ct$setNote("s", sprintf("Początek pierwszej klasy (%g) musi być ≤ minimum (%g).", start, min(x))); return() }
            br <- classBreaks(x, o$method, o$nClasses, o$width, start)
            if (is.null(br)) { ct$setNote("h", "Szerokość klasy musi być dodatnia."); return() }
            tab <- classTable(x, br$breaks); k <- nrow(tab)

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienna „%s”; N = %d (braki pominięte); minimum %s, maksimum %s.", o$var, length(x), format(signif(min(x), 6)), format(signif(max(x), 6)))
            m$add("Klasy", "%s: k = %d klas o szerokości h = %s, od %s; przedziały lewostronnie domknięte [a; b), ostatni [a; b].",
                  switch(o$method, sturges = "Reguła Sturgesa k = ⌈log₂ n + 1⌉", count = "Zadana liczba klas", width = "Zadana szerokość klasy"),
                  k, format(signif(br$h, 6)), if (is.null(start)) "minimum" else format(signif(start, 6)))
            m$add("Klasy", "Środek klasy = (a + b)/2; częstość = n_i / N; skumulowane = suma do danej klasy włącznie.")
            m$add("Statystyki z szeregu", "Średnia = Σ środek·n_i / N; wariancja = Σ (środek − średnia)²·n_i / (N − 1).")
            m$add("Statystyki z szeregu", "Dominanta interpolowana w klasie modalnej: x₀ + h·(n_D − n_{D−1}) / ((n_D − n_{D−1}) + (n_D − n_{D+1})); mediana interpolowana w klasie zawierającej N/2: x₀ + h·(N/2 − cum_{poprz}) / n_Me.")
            m$addIf(o$compare, "Statystyki z szeregu", "Kolumna „z danych”: te same miary policzone z surowych obserwacji (mediana i dominanta jak w „Zmienne ilościowe”).")
            m$addIf(o$plot, "Wykres", "Histogram klas: słupki między granicami klas, wysokość = liczność.")
            m$addIf(o$ogive, "Wykres", "Krzywa częstości skumulowanych (ogiwa): punkty w górnych granicach klas.")
            m$render(self$results$metody)

            for (i in seq_len(k))
                ct$addRow(rowKey = i, values = list(klasa = fmtClass(tab$lower[i], tab$upper[i], i == k), mid = tab$mid[i], n = tab$n[i],
                    pct = tab$pct[i], cumN = tab$cumN[i], cumPct = tab$cumPct[i]))
            ct$addRow(rowKey = "sum", values = list(klasa = "Razem", n = sum(tab$n), pct = 100))
            ct$addFormat(rowKey = "sum", col = 1, jmvcore::Cell.BEGIN_END_GROUP)

            g <- groupedStats(tab); raw <- descStats(x)
            rows <- list(list("mean", "Średnia", g$mean, raw$mean), list("median", "Mediana", g$median, raw$median),
                         list("mode", "Dominanta", g$mode, raw$mode), list("var", "Wariancja", g$var, raw$variance), list("sd", "Odchylenie standardowe", g$sd, raw$sd))
            for (r in rows) st$addRow(rowKey = r[[1]], values = list(stat = r[[2]], grouped = r[[3]], exact = r[[4]]))
            st$setNote("cls", sprintf("Klasa modalna: %s; klasa mediany: %s.", fmtClass(tab$lower[g$modalClass], tab$upper[g$modalClass], g$modalClass == k),
                                      fmtClass(tab$lower[g$medianClass], tab$upper[g$medianClass], g$medianClass == k)))
            if (is.na(g$mode)) st$setNote("md", "Dominanta z szeregu nieokreślona (sąsiednie klasy równie liczne).")
            self$results$plot$setState(list(tab = tab, label = o$var)); self$results$ogive$setState(list(tab = tab, label = o$var))
        },
        .histPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); classHistPlot(s$tab, s$label, ggtheme, theme) },
        .ogivePlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); ogivePlot(s$tab, s$label, ggtheme, theme) }
    )
)
