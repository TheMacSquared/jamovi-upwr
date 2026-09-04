#' @importFrom jmvcore .
zalezneClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "zalezneClass",
    inherit = zalezneBase,
    private = list(

        .pairsBuilt = FALSE,

        # kolumny tabeli par zależą od poziomów pomiarów — jak w tabeli krzyżowej
        # budujemy je z .init ORAZ z .run (w GUI dane bywają dostępne dopiero tam)
        .buildPairCols = function(lv) {
            if (isTRUE(private$.pairsBuilt) || length(lv) == 0) return(invisible(FALSE))
            t <- self$results$pairs
            t$addColumn(name = "row", title = "", type = "text")
            for (l in lv) t$addColumn(name = paste0("c_", l), title = l, type = "number")
            private$.pairsBuilt <- TRUE
            invisible(TRUE)
        },

        .init = function() {
            o <- self$options
            if (length(o$vars) < 2) return()
            v <- try(self$data[[o$vars[1]]], silent = TRUE)
            if (inherits(v, "try-error") || is.null(v)) return()
            private$.buildPairCols(levels(v))
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

            if (length(vars) == 2) private$.runMcnemar(vars) else private$.runCochran(vars)
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
            private$.buildPairCols(rownames(tab))
            private$.fillPairs(tab)
            private$.fillMarg(stats::setNames(c(sum(tab[1, ]), sum(tab[, 1])), vars), sum(tab), rownames(tab)[1])

            m <- mcnemar(tab, correct = isTRUE(o$corr))
            lab <- if (isTRUE(o$corr)) "McNemar (z poprawką ciągłości)" else "McNemar"
            if (is.na(m$stat)) {
                t$setNote("z", "Brak par niezgodnych — pomiary są identyczne, więc testu nie da się policzyć.")
            } else {
                t$addRow(rowKey = "mc", values = list(test = lab, stat = m$stat, df = m$df, p = m$p))
                t$setNote("disc", sprintf("Test opiera się na %d parach niezgodnych (%d i %d).",
                                          m$discordant, m$b, m$c))
            }

            if (isTRUE(o$exact)) {
                ex <- mcnemarExact(tab)
                if (!is.null(ex) && !is.na(ex$p))
                    t$addRow(rowKey = "ex", values = list(
                        test = "Dokładny test dwumianowy", stat = NA_real_, df = NA_integer_, p = ex$p))
            }

            # warunek stosowalności: przybliżenie χ² wymaga dość par niezgodnych
            a <- checkMcnemar(tab)
            if (!is.null(a) && !isTRUE(a$ok) && a$discordant > 0)
                self$results$insert(1, jmvcore::Notice$new(
                    self$options, name = ".assumption", type = jmvcore::NoticeType$WARNING,
                    content = sprintf(paste(
                        "Par niezgodnych jest tylko %d (zwyczajowo wymaga się co najmniej 25),",
                        "więc przybliżenie χ² jest zawodne. Użyj dokładnego testu dwumianowego."),
                        a$discordant)))

            if (isTRUE(o$effSize)) {
                or <- mcnemarOR(tab, level = o$ciWidth / 100)
                e <- self$results$effsize
                e$addRow(rowKey = "or", values = list(measure = "OR par niezgodnych",
                                                      value = or$est, lower = or$lower, upper = or$upper))
                e$setNote("or", sprintf(
                    "Iloraz liczby par „%s → %s” do par „%s → %s”; 1 oznacza brak zmiany.",
                    rownames(tab)[1], rownames(tab)[2], rownames(tab)[2], rownames(tab)[1]))
            }
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

            if (is.na(q$stat)) {
                t$setNote("z", paste("Żadna jednostka nie różnicuje pomiarów (wszędzie same",
                                     "wystąpienia albo same braki), więc Q jest nieokreślone."))
            } else {
                t$addRow(rowKey = "q", values = list(test = "Q Cochrana", stat = q$stat,
                                                     df = q$df, p = q$p))
                t$setNote("n", sprintf("N = %d jednostek, %d pomiarów.", q$n, length(vars)))
            }

            lv1 <- levels(factor(self$data[[vars[1]]]))[1]
            private$.fillMarg(stats::setNames(q$props * q$n, vars), q$n, lv1)

            if (isTRUE(o$effSize))
                self$results$effsize$setNote("na", paste(
                    "OR par niezgodnych jest określony tylko dla dwóch pomiarów;",
                    "przy większej liczbie pomiarów porównuj pary w sekcji post-hoc."))

            if (isTRUE(o$posthoc)) {
                pw <- pairwiseMcnemar(m, vars)
                ph <- self$results$posthoc
                if (!is.null(pw)) for (i in seq_len(nrow(pw)))
                    ph$addRow(rowKey = i, values = list(g1 = pw$g1[i], g2 = pw$g2[i],
                                                        disc = pw$disc[i], stat = pw$stat[i], p = pw$p[i]))
            }
        },

        .fillMarg = function(counts, n, level = NULL) {
            if (!isTRUE(self$options$props)) return()
            t <- self$results$marg
            for (i in seq_along(counts))
                t$addRow(rowKey = names(counts)[i], values = list(
                    var = names(counts)[i], n = counts[[i]],
                    prop = if (n > 0) counts[[i]] / n else NA_real_))
            # nazwanie poziomu wprost: przy „tak/nie" pierwsza alfabetycznie jest
            # kategoria „nie", co bez tej noty odwraca odczyt tabeli
            t$setNote("lv", if (is.null(level))
                "Udział pierwszej kategorii (alfabetycznie) w każdym pomiarze."
                else sprintf("Udział kategorii „%s” (pierwszej alfabetycznie) w każdym pomiarze.", level))
        },

        .fillPairs = function(tab) {
            if (!isTRUE(self$options$table)) return()
            t <- self$results$pairs
            lv <- colnames(tab)
            for (i in seq_len(nrow(tab))) {
                vals <- list(row = rownames(tab)[i])
                for (j in seq_along(lv)) vals[[paste0("c_", lv[j])]] <- tab[i, j]
                t$addRow(rowKey = rownames(tab)[i], values = vals)
            }
            t$setNote("p", "Wiersze — pomiar pierwszy, kolumny — drugi; przekątna to pary zgodne.")
        }
    )
)
