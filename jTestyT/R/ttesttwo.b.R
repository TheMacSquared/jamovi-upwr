#' @importFrom jmvcore .
ttesttwoClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ttesttwoClass",
    inherit = ttesttwoBase,
    private = list(
        .init = function() {
            for (v in self$options$vars) {
                self$results$plots$addItem(key = v); self$results$qq$addItem(key = v)
                self$results$plots$get(key = v)$setTitle(v); self$results$qq$get(key = v)$setTitle(v)
            }
        },
        .run = function() {
            o <- self$options
            if (length(o$vars) == 0 || !optNonEmpty(o$group)) return()
            gAll <- factor(self$data[[o$group]])
            tt <- self$results$ttest
            if (nlevels(droplevels(gAll[!is.na(gAll)])) != 2) {
                tt$setNote("g", "Zmienna grupująca musi mieć dokładnie 2 poziomy (odfiltruj pozostałe)."); return()
            }
            m <- jmvcore::metodyNew()
            lvAll <- levels(droplevels(gAll[!is.na(gAll)]))
            m$add("Dane", "Zmienne zależne: %s; zmienna grupująca „%s” z poziomami „%s” i „%s”; braki pomijane osobno dla każdej zmiennej.",
                  jmvcore::metodyCyt(o$vars), o$group, lvAll[1], lvAll[2])
            m$addIf(o$student, "Testy", "t Studenta dla prób niezależnych (wspólna wariancja), df = n₁ + n₂ − 2.")
            m$addIf(o$welch, "Testy", "t Welcha (osobne wariancje, df Welcha-Satterthwaite’a).")
            metodyWspolne(m, o, "two", sprintf("Różnica = „%s” − „%s”", lvAll[1], lvAll[2]), homog = isTRUE(o$homog))
            m$render(self$results$metody)
            for (v in o$vars) {
                y <- jmvcore::toNumeric(self$data[[v]]); ok <- !is.na(y) & !is.na(gAll)
                y <- y[ok]; g <- droplevels(gAll[ok]); lv <- levels(g)
                if (nlevels(g) != 2 || any(table(g) < 2)) { tt$setNote(paste0("n", v), sprintf("%s: za mało obserwacji w grupie.", v)); next }
                if (isTRUE(o$student)) addTestRow(tt, paste(v, "t"), v, twoSampleT(y, g, FALSE, o$hypothesis))
                if (isTRUE(o$welch)) addTestRow(tt, paste(v, "tw"), v, twoSampleT(y, g, TRUE, o$hypothesis))
                if (isTRUE(o$nonpar)) addTestRow(tt, paste(v, "mw"), v, mannWhitney(y, g, o$hypothesis))
                x1 <- y[g == lv[1]]; x2 <- y[g == lv[2]]
                if (isTRUE(o$desc)) for (l in lv)
                    self$results$desc$addRow(rowKey = paste(v, l), values = c(list(var = v, group = l), descRow(y[g == l])))
                if (isTRUE(o$norm)) for (l in lv)
                    self$results$norm$addRow(rowKey = paste(v, l), values = c(list(var = v, group = l), shapiroRow(y[g == l])))
                if (isTRUE(o$homog)) self$results$homog$addRow(rowKey = v, values = c(list(var = v), leveneTwo(y, g)))
                self$results$plots$get(key = v)$setState(list(groups = stats::setNames(list(x1, x2), lv), ylab = v))
                self$results$qq$get(key = v)$setState(list(x = c(x1 - mean(x1), x2 - mean(x2)), label = paste(v, "(reszty w grupach)")))
            }
            # kierunek roznicy musi byc widoczny bez opisu metod — zostaje jedno zdanie
            tt$setNote("h", sprintf("Różnica = %s − %s.", lvAll[1], lvAll[2]))
        },
        .plot = function(image, ggtheme, theme, ...) boxPlotTests(image, ggtheme, theme),
        .qq = function(image, ggtheme, theme, ...) qqPlotResid(image, ggtheme, theme)
    )
)
