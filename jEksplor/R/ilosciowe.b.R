#' @importFrom jmvcore .
ilosciowe_statDefs <- function(o) {
    # ordered list of statistics: key, column title, option enabling it, description sentence
    defs <- list(
        list("n", "N", o$n, NULL), list("missing", "Braki", o$missing, NULL),
        list("mean", "Średnia", o$mean, NULL), list("median", "Mediana", o$median, "mediana = kwantyl 0.5"),
        list("q1", "Q1", o$quart, "kwartyle Q1 i Q3 = kwantyle 0.25 i 0.75 (definicja typu 7 jak w R: interpolacja liniowa)"), list("q3", "Q3", o$quart, NULL),
        list("sd", "SD", o$sd, "SD = odchylenie standardowe z próby (dzielnik n − 1)"), list("min", "Min", o$min, NULL), list("max", "Max", o$max, NULL),
        list("v", "V (%)", o$v, "V = SD / średnia · 100%"),
        list("mode", "Dominanta", o$mode, "dominanta = wartość najczęstsza (brak, gdy każda wartość występuje raz)"),
        list("sum", "Suma", o$sum, NULL),
        list("gmean", "Śr. geometryczna", o$gmean, "średnia geometryczna i harmoniczna tylko dla wartości dodatnich"),
        list("hmean", "Śr. harmoniczna", o$hmean, NULL),
        list("tmean", sprintf("Śr. ucięta (%g%%)", 100 * o$trimProp), o$tmean, sprintf("średnia ucięta: bez %g%% najmniejszych i %g%% największych wartości; winsoryzowana: te wartości zastąpione skrajnymi pozostawionymi", 100 * o$trimProp, 100 * o$trimProp)),
        list("wmean", sprintf("Śr. winsoryzowana (%g%%)", 100 * o$trimProp), o$wmean, NULL),
        list("variance", "Wariancja", o$variance, NULL), list("range", "Rozstęp", o$range, NULL), list("iqr", "IQR", o$iqr, "IQR = Q3 − Q1"),
        list("meanDev", "Odch. przeciętne", o$meanDev, "odchylenie przeciętne = średnia |x − średnia|"),
        list("mad", "MAD", o$mad, "MAD = mediana |x − mediana| (bez stałej 1.4826)"),
        list("qdev", "Odch. ćwiartkowe", o$qdev, "odchylenie ćwiartkowe Q = (Q3 − Q1)/2"),
        list("vq", "V_Q (%)", o$vq, "V_Q = Q / mediana · 100%"),
        list("typLo", "Typowy obszar: od", o$typical, "typowy obszar zmienności: średnia − SD < x < średnia + SD"), list("typHi", "Typowy obszar: do", o$typical, NULL),
        list("skew", "Skośność", o$skew, "skośność G1 i kurtoza G2 (nadwyżkowa) z poprawką na próbę, z błędem standardowym — jak w SPSS i jamovi"), list("seSkew", "SE skośności", o$skew, NULL),
        list("kurt", "Kurtoza", o$kurt, NULL), list("seKurt", "SE kurtozy", o$kurt, NULL),
        list("skewPearson", "Skośność Pearsona", o$skewPearson, "skośność Pearsona = 3 (średnia − mediana) / SD"),
        list("skewQuart", "Skośność kwartylowa", o$skewQuart, "skośność kwartylowa (Bowleya) = (Q3 + Q1 − 2 · mediana) / (Q3 − Q1)"))
    if (isTRUE(o$pc)) for (p in parsePercentiles(o$pcValues))
        defs[[length(defs) + 1]] <- list(paste0("pc", p), sprintf("P%g", p), TRUE, NULL)
    if (isTRUE(o$pcEqGr) && o$pcNEqGr >= 2) for (i in seq_len(o$pcNEqGr - 1))
        defs[[length(defs) + 1]] <- list(paste0("cut", i), sprintf("%d/%d", i, o$pcNEqGr), TRUE, NULL)
    Filter(function(d) isTRUE(d[[3]]), defs)
}

ilosciowe_extraStats <- function(x, o) {
    out <- list()
    if (isTRUE(o$pc)) for (p in parsePercentiles(o$pcValues)) out[[paste0("pc", p)]] <- unname(stats::quantile(x, p / 100, type = 7))
    if (isTRUE(o$pcEqGr) && o$pcNEqGr >= 2) for (i in seq_len(o$pcNEqGr - 1))
        out[[paste0("cut", i)]] <- unname(stats::quantile(x, i / o$pcNEqGr, type = 7))
    out
}

iloscioweClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "iloscioweClass",
    inherit = iloscioweBase,
    private = list(
        .built = FALSE,
        .groupKeys = function() {
            # combinations of split-by levels present in the data (in .init the
            # data may have no rows: then the declared factor levels are used)
            o <- self$options
            if (length(o$splitBy) == 0) return(NULL)
            lv <- lapply(o$splitBy, function(v) {
                col <- self$data[[v]]; l <- levels(col)
                if (is.null(l) || length(l) == 0) l <- levels(droplevels(factor(col[!is.na(col)])))
                l
            })
            # last split variable varies fastest, so the first one forms the outer blocks
            g <- expand.grid(rev(lv), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)[, rev(seq_along(lv)), drop = FALSE]
            names(g) <- o$splitBy
            g
        },
        .buildTable = function() {
            if (isTRUE(private$.built)) return()
            o <- self$options; t <- self$results$desc
            defs <- ilosciowe_statDefs(o); groups <- private$.groupKeys()
            if (o$layout == "rows") {
                t$addColumn(name = "var", title = "", type = "text", combineBelow = TRUE)
                for (v in o$splitBy) t$addColumn(name = paste0("g_", v), title = v, type = "text", combineBelow = TRUE)
                for (d in defs) t$addColumn(name = d[[1]], title = d[[2]], type = if (d[[1]] %in% c("n", "missing")) "integer" else "number")
            } else {
                t$addColumn(name = "stat", title = "", type = "text")
                for (v in o$vars) {
                    if (is.null(groups)) t$addColumn(name = paste0("v_", v), title = v, type = "number")
                    else for (i in seq_len(nrow(groups)))
                        t$addColumn(name = paste0("v_", v, "_", i), title = paste(unlist(groups[i, ]), collapse = " / "), type = "number", superTitle = v)
                }
                for (d in defs) t$addRow(rowKey = d[[1]], values = list(stat = d[[2]]))
            }
            private$.built <- TRUE
        },
        .init = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            private$.buildTable()
            for (v in o$vars) {
                for (arr in c("hist", "box", "qq", "ecdf", "lorenz")) {
                    self$results[[arr]]$addItem(key = v); self$results[[arr]]$get(key = v)$setTitle(v)
                }
                self$results$extreme$addItem(key = v); self$results$extreme$get(key = v)$setTitle(paste("Wartości skrajne:", v))
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0) return()
            private$.buildTable()
            defs <- ilosciowe_statDefs(o); keys <- vapply(defs, function(d) d[[1]], "")
            t <- self$results$desc
            split <- length(o$splitBy) > 0
            gAll <- if (split) interaction(lapply(o$splitBy, function(v) factor(self$data[[v]])), sep = " / ", lex.order = TRUE) else NULL
            groups <- private$.groupKeys()
            groupLabel <- function(i) if (is.null(groups)) "" else paste(unlist(groups[i, ]), collapse = " / ")

            m <- jmvcore::metodyNew()
            m$add("Dane", "Zmienne: %s%s; braki pomijane osobno dla każdej zmiennej%s.", jmvcore::metodyCyt(o$vars),
                  if (split) sprintf("; podział według: %s", jmvcore::metodyCyt(o$splitBy)) else "",
                  if (split) " (obserwacja z brakiem w zmiennej grupującej pominięta)" else "")
            m$add("Dane", "Układ tabeli: %s.", if (o$layout == "rows") "zmienne w wierszach, statystyki w kolumnach" else "statystyki w wierszach, zmienne w kolumnach")
            for (d in defs) if (!is.null(d[[4]])) m$add("Statystyki", paste0(sub("^(.)", "\\U\\1", d[[4]], perl = TRUE), "."))
            m$addIf(o$pc, "Statystyki", "Percentyle P%s (kwantyle typu 7).", paste(parsePercentiles(o$pcValues), collapse = ", P"))
            m$addIf(o$pcEqGr, "Statystyki", "Punkty podziału na %d równoliczne grupy (kwantyle typu 7).", o$pcNEqGr)
            m$addIf(o$extreme, "Statystyki", "Wartości skrajne: %d najmniejszych i %d największych z numerem wiersza w arkuszu (bez podziału na grupy).", o$extremeN, o$extremeN)
            m$addIf(o$gini, "Koncentracja", "Współczynnik Giniego G = 2 Σ i·x(i) / (n Σ x) − (n + 1)/n na wartościach uporządkowanych rosnąco; wymaga wartości nieujemnych; 0 = równy rozkład, 1 = pełna koncentracja.")
            m$addIf(o$lorenz, "Koncentracja", "Krzywa Lorenza: skumulowany udział sumy wobec skumulowanego udziału jednostek.")
            m$addIf(o$sw, "Założenia", "Shapiro-Wilk (od 3 do 5000 obserwacji).")
            m$addIf(o$lillie, "Założenia", "Lilliefors: statystyka Kołmogorowa-Smirnowa wobec rozkładu normalnego z estymowanymi parametrami, p z przybliżenia Dallala-Wilkinsona (n ≥ 5).")
            m$addIf(o$ad, "Założenia", "Anderson-Darling z estymowanymi parametrami, p z przybliżenia Stephensa (n ≥ 8).")
            m$addIf(o$qq, "Założenia", "Wykres Q-Q: kwantyle zmiennej wobec kwantyli rozkładu normalnego%s.", if (split) ", osobno w grupach" else "")
            m$addIf(o$hist, "Wykres", "Histogram (30 przedziałów)%s%s.", if (o$dens) " z krzywą gęstości jądrowej" else "", if (split) ", panele = grupy" else "")
            m$addIf(o$box, "Wykres", "Wykres pudełkowy: pudełko = kwartyle, linia = mediana, wąsy do 1.5 IQR, punkty poza = obserwacje odstające%s%s%s.",
                    if (o$violin) "; skrzypce = gęstość" else "", if (o$dot) "; punkty = obserwacje" else "", if (o$boxMean) "; romb = średnia" else "")
            m$addIf(o$ecdf, "Wykres", "Dystrybuanta empiryczna: udział obserwacji ≤ x%s.", if (split) ", linie = grupy" else "")
            m$render(self$results$metody)

            fillRow <- function(v, gi, x, missing) {
                st <- if (length(x)) c(descStats(x, o$trimProp), ilosciowe_extraStats(x, o)) else list()
                st$n <- length(x); st$missing <- missing
                vals <- st[intersect(keys, names(st))]
                if (o$layout == "rows") {
                    vals$var <- v
                    if (split) for (s in o$splitBy) vals[[paste0("g_", s)]] <- groups[gi, s]
                    t$addRow(rowKey = paste(v, gi), values = vals)
                } else {
                    col <- if (split) paste0("v_", v, "_", gi) else paste0("v_", v)
                    for (k in names(vals)) t$setCell(col = col, value = vals[[k]], rowKey = k)
                }
                st
            }
            plotOn <- c(hist = isTRUE(o$hist), box = isTRUE(o$box), qq = isTRUE(o$qq), ecdf = isTRUE(o$ecdf), lorenz = isTRUE(o$lorenz))
            for (v in o$vars) {
                raw <- jmvcore::toNumeric(self$data[[v]])
                if (split) {
                    for (gi in seq_len(nrow(groups))) {
                        lab <- groupLabel(gi); inG <- !is.na(gAll) & as.character(gAll) == lab
                        x <- raw[inG]; fillRow(v, gi, x[!is.na(x)], sum(is.na(x)))
                        private$.fillNorm(v, lab, x[!is.na(x)]); private$.fillConc(v, lab, x[!is.na(x)])
                    }
                    ok <- !is.na(raw) & !is.na(gAll)
                    d <- data.frame(y = raw[ok], group = factor(as.character(gAll[ok]), levels = vapply(seq_len(nrow(groups)), groupLabel, "")))
                } else {
                    x <- raw[!is.na(raw)]; fillRow(v, 1, x, sum(is.na(raw)))
                    private$.fillNorm(v, "", x); private$.fillConc(v, "", x)
                    d <- data.frame(y = x, group = factor(v))
                }
                private$.fillExtreme(v, raw)
                for (arr in c("hist", "box", "qq", "ecdf", "lorenz")) if (isTRUE(plotOn[[arr]]))
                    self$results[[arr]]$get(key = v)$setState(list(d = d, label = v))
            }
            if (isTRUE(o$gini)) self$results$conc$setNote("g", "G tylko dla wartości nieujemnych.")
        },
        .fillNorm = function(v, lab, x) {
            o <- self$options
            if (!(isTRUE(o$sw) || isTRUE(o$lillie) || isTRUE(o$ad))) return()
            sw <- if (isTRUE(o$sw)) shapiroTest(x) else list(stat = NULL, p = NULL)
            li <- if (isTRUE(o$lillie)) lillieTest(x) else list(stat = NULL, p = NULL)
            ad <- if (isTRUE(o$ad)) adTest(x) else list(stat = NULL, p = NULL)
            self$results$norm$addRow(rowKey = paste(v, lab), values = list(var = v, group = lab,
                w = sw$stat, pw = sw$p, d = li$stat, pd = li$p, a = ad$stat, pa = ad$p))
        },
        .fillConc = function(v, lab, x) {
            if (!isTRUE(self$options$gini)) return()
            self$results$conc$addRow(rowKey = paste(v, lab), values = list(var = v, group = lab, gini = gini(x), n = length(x)))
        },
        .fillExtreme = function(v, raw) {
            o <- self$options
            if (!isTRUE(o$extreme)) return()
            t <- self$results$extreme$get(key = v)
            idx <- which(!is.na(raw)); ord <- idx[order(raw[idx])]
            k <- min(o$extremeN, length(ord))
            for (i in seq_len(k)) t$addRow(rowKey = paste("lo", i), values = list(type = "Najmniejsze", place = i, row = ord[i], value = raw[ord[i]]))
            hi <- rev(ord)
            for (i in seq_len(k)) t$addRow(rowKey = paste("hi", i), values = list(type = "Największe", place = i, row = hi[i], value = raw[hi[i]]))
        },
        .histPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); histPlot(s$d, s$label, isTRUE(self$options$dens), ggtheme, theme) },
        .boxPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE)
            o <- self$options; boxPlot(s$d, s$label, isTRUE(o$violin), isTRUE(o$dot), isTRUE(o$boxMean), ggtheme, theme) },
        .qqPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); qqPlotDesc(s$d, s$label, ggtheme, theme) },
        .ecdfPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE); ecdfPlot(s$d, s$label, ggtheme, theme) },
        .lorenzPlot = function(image, ggtheme, theme, ...) { s <- image$state; if (is.null(s)) return(FALSE)
            if (!all(s$d$y >= 0)) return(FALSE); lorenzPlot(s$d, s$label, ggtheme, theme) }
    )
)
