# Opis zastosowanych metod — wspólny mechanizm jUPWR (dodatek forka, jak themes.R).
#
# Każda analiza jUPWR ma na końcu .r.yaml element `metody` (type: Html,
# visible: (metody)) sterowany checkboxem `metody` z .a.yaml. Element zbiera
# krótkie punkty: CO policzono, z JAKIMI parametrami i JAK przedstawiono
# (kierunek porównania, poziom referencyjny, mianownik procentów…).
# NIE zawiera wyników — te są w tabelach. Zastępuje rozbudowane noty pod
# tabelami: nota ma zostać tylko tam, gdzie bez niej tabeli nie da się
# odczytać (błąd, brak wiersza, N).
#
# Użycie w .b.R modułu:
#   private$.metody <- jmvcore::metodyNew()
#   private$.metody$add("Dane", "Wiersze: „%s”, kolumny: „%s”.", o$rows, o$cols)
#   private$.metody$render(self$results$metody)

#' Akumulator opisu zastosowanych metod (jUPWR)
#'
#' `add(sekcja, fmt, ...)` działa jak sprintf; argumenty tekstowe są
#' escapowane do HTML (nazwy zmiennych i poziomów mogą zawierać < > &),
#' a sam `fmt` jest zaufany i może zawierać znaczniki (<i>, <sub>…) — dlatego
#' znaki < > w treści `fmt` pisz jako &lt; &gt;.
#' Sekcje wyświetlane są w kolejności z `order` (nieznane trafiają przed
#' ostatnią, czyli przed „Wykres”), niezależnie od kolejności dopisywania —
#' .b.R może dopisywać punkty w kolejności obliczeń.
#'
#' @param order kolejność sekcji w wyjściowym HTML
#' @return lista funkcji: add, addIf, html, render, length
#' @export
metodyNew <- function(order = c("Dane", "Model", "Testy", "Wielkość efektu",
                                "Post-hoc", "Wykres")) {
    items <- list()
    esc <- function(x) htmlEscape(as.character(x))
    self <- list()

    self$add <- function(section, fmt, ...) {
        args <- lapply(list(...), function(a)
            if (is.character(a) || is.factor(a)) esc(a) else a)
        text <- if (length(args) == 0) fmt else do.call(sprintf, c(list(fmt), args))
        items[[length(items) + 1]] <<- list(section = section, text = text)
        invisible(self)
    }

    # `addIf(cond, …)`: skrót, żeby .b.R nie puchło od if-ów przy opcjach Bool
    self$addIf <- function(cond, section, fmt, ...) {
        if (isTRUE(cond)) self$add(section, fmt, ...)
        invisible(self)
    }

    self$html <- function() {
        if (length(items) == 0) return("")
        sections <- unique(vapply(items, function(i) i$section, character(1)))
        pos <- match(sections, order)
        pos[is.na(pos)] <- length(order) - 0.5
        sections <- sections[base::order(pos, seq_along(sections))]
        out <- '<div class="jupwr-metody" style="font-size: 90%; line-height: 1.45; max-width: 62em">'
        for (s in sections) {
            texts <- vapply(Filter(function(i) identical(i$section, s), items),
                            function(i) i$text, character(1))
            out <- paste0(out,
                '<p style="margin: 0.5em 0 0.15em 0"><b>', esc(s), '</b></p>',
                '<ul style="margin: 0 0 0 1.3em; padding: 0">',
                paste0("<li>", texts, "</li>", collapse = ""),
                "</ul>")
        }
        paste0(out, "</div>")
    }

    self$render <- function(element) {
        element$setContent(self$html())
        invisible(self)
    }

    self$length <- function() length(items)
    self
}

#' Lista wartości w cudzysłowach drukarskich (jUPWR)
#'
#' Do wypisywania nazw zmiennych/poziomów w jednym zdaniu opisu metod.
#'
#' @param x wektor tekstowy
#' @return jeden łańcuch: „a”, „b”, …
#' @export
metodyCyt <- function(x) paste0("„", x, "”", collapse = ", ")
