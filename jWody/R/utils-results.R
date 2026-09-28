hydroRun <- function(name, data, options) {
    o <- hydroOptions(options$resolution, options$kind, options$yearStart,
                      options$completeness, options$dateStart, options$dateEnd)
    groups <- hydroPrepare(data[[options$date]], jmvcore::toNumeric(data[[options$value]]),
        if (!is.null(options$station) && nzchar(options$station)) data[[options$station]] else NULL,
        o, audit = name == "hydrocheck")
    results <- lapply(groups, function(g) {
        d <- g$series
        switch(name,
            hydrocheck = {
                m <- hydroAggregate(d, o, "monthly"); m$scale <- "miesiąc"
                a <- hydroAggregate(d, o, "annual"); a$scale <- "rok"
                details <- rbind(m, a)
                details$accepted <- ifelse(details$accepted, "tak", "nie")
                list(summary = g$quality, details = details, series = d, monthly = m)
            },
            hydroregime = hydroRegime(d, o, options$aggregation, options$refStart, options$refEnd),
            hydroflow = hydroFlow(d, o, options$area),
            hydrotrend = hydroTrend(d, o, options$trendMode, options$inferIndependent, options$pettitt),
            hydrolow = hydroLow(d, o, options$thresholdMode, options$exceedance, options$threshold,
                options$minDuration, options$poolDays, options$refStart, options$refEnd))
    })
    bind <- function(key) {
        pieces <- lapply(results, function(r) r[[key]])
        do.call(rbind, pieces)
    }
    list(summary = bind("summary"), details = bind("details"),
        state = list(name = name, unit = hydroUnit(o$kind), kind = o$kind,
            summary = bind("summary"), details = bind("details"), series = bind("series"),
            monthly = bind("monthly"), fdc = bind("fdc"), acf = bind("acf")))
}

hydroFill <- function(table, data) {
    columns <- vapply(table$columns, function(x) x$name, character(1))
    if (!nrow(data)) return(invisible(NULL))
    for (i in seq_len(nrow(data))) table$addRow(rowKey = i, values = as.list(data[i, intersect(columns, names(data)), drop = FALSE]))
}

hydroNotes <- function(name, options) {
    unit <- hydroUnit(options$kind)
    common <- sprintf("Jednostka: %s. Rok hydrologiczny zaczyna się w miesiącu %d i jest oznaczony rokiem zakończenia. Minimalna kompletność: %g%% pełnego okresu kalendarzowego. Luki pozostają brakami; bez interpolacji. Daty są sortowane. Agregaty opadu to sumy, pozostałych wielkości — średnie (miesięczne średnie ważone liczbą dni przy agregacji rocznej).", unit, options$yearStart, options$completeness)
    specific <- switch(name,
        hydrocheck = "Duplikaty są liczone jako nadmiarowe wiersze; wszystkie pomiary z powtórzonej daty wyłączono z kompletności. Błędne daty nie trafiają na oś czasu. Braki jawne, brakujące daty i konflikty mogą mieć różne przyczyny. Ujemny stan/rzędna mogą być poprawne; dla opadu/przepływu/głębokości wymagają sprawdzenia. Bez automatycznego usuwania odstających obserwacji.",
        hydroregime = sprintf("Anomalia = agregat minus średnia przyjętych agregatów odniesienia; dla miesięcy porównujemy ten sam miesiąc. Okres odniesienia: %s–%s; dla agregatów rocznych liczy się rok zakończenia. Przy niepełnych sumach opadu nie stosujemy przeskalowania do pełnego okresu.", if (options$refStart == 0) "początek danych" else options$refStart, if (options$refEnd == 0) "koniec danych" else options$refEnd),
        hydroflow = "NQ/SQ/WQ = minimum/średnia/maksimum dobowe w roku. NNQ i WWQ = skrajne wartości przyjętych lat; SNQ/SSQ/SWQ = średnie ich NQ/SQ/WQ (lata mają równe wagi). Q90/Q95 = kwantyle 0,10/0,05 (R, typ 7) pomiarów z przyjętych lat; oznaczają przewyższenie przez 90/95% czasu. Punkty krzywej: ranga/(n+1). Objętość = suma obserwowanych Q × 86400; warstwa = objętość/(1000 × km²). Przy kompletności <100% objętość i warstwa obejmują tylko zmierzone dni.",
        hydrotrend = paste("MK z poprawką na remisy i ciągłość; sezonowy MK sumuje statystyki i wariancje miesięcy, bez kowariancji między sezonami. Sen = mediana nachyleń względem rzeczywistego czasu (jednostka/rok), sezonowo wyłącznie w tym samym miesiącu. PU 95%: przybliżenie rangowe normalne. r lag 1/12 obliczamy parami na regularnej osi reszt po odjęciu trendu Sena i median sezonowych; nie jest to formalny test niezależności.",
            if (options$inferIndependent) "Użytkownik włączył wnioskowanie zakładające niezależność. p i PU NIE są skorygowane o autokorelację. Przy zależności czasowej mogą być zbyt optymistyczne." else "Wnioskowanie wyłączone: p i PU nie są obliczane bez jawnego przyjęcia niezależności.",
            "Pettitt (opcjonalnie, tylko rocznie): orientacyjny punkt zmiany po wskazanym roku, przybliżone p, wiarygodniejsze dla p ≤ 0,5. Remisy i zależność czasowa ograniczają interpretację. Punkt zmiany nie identyfikuje przyczyny. Wzrost głębokości zwierciadła oznacza obniżanie zwierciadła."),
        hydrolow = sprintf("Niżówka: Q ściśle poniżej progu. Próg: %s; przewyższenie %g%%, próg podany %g m³/s. Qp: kwantyl R typu 7 z przyjętych miesięcy okresu odniesienia %s–%s; minimum 10 pomiarów (osobno w miesiącach dla progu sezonowego). Łączenie przez ≤%d znanych dni nad progiem; nigdy przez luki. Minimum %d dni pod progiem liczone po łączeniu. Rozpiętość uwzględnia przerwy, deficyt tylko dni pod progiem. Zdarzenia przy lukach/granicach są oznaczane jako ucięte. W tabeli rocznej dni i deficyt dzielimy na lata, liczbę zdarzeń przypisujemy do roku początku; odrzucone lata mają braki zamiast zer.", options$thresholdMode, options$exceedance, options$threshold, if (options$refStart == 0) "początek danych" else options$refStart, if (options$refEnd == 0) "koniec danych" else options$refEnd, options$poolDays, options$minDuration))
    paste(common, specific)
}

hydroPlot <- function(s, secondary, ggtheme) {
    if (is.null(s)) return(FALSE)
    name <- s$name
    if (name == "hydrocheck" && !secondary || name == "hydrolow" && !secondary) {
        d <- s$series
        plot <- ggplot2::ggplot(d, ggplot2::aes(x = date, y = value))
        plot <- plot + ggplot2::geom_line(colour = "#32678c", na.rm = TRUE)
        if (name == "hydrolow") plot <- plot +
            ggplot2::geom_line(ggplot2::aes(y = threshold), colour = "#9b343e", linetype = "dashed") +
            ggplot2::geom_point(data = d[d$event > 0 & !is.na(d$value) & d$value < d$threshold, ], colour = "#9b343e", size = .7)
        plot <- plot + ggplot2::labs(x = "Data", y = s$unit, caption = if (name == "hydrolow") "Linia przerywana: próg; czerwone punkty: dni przyjętych niżówek" else NULL)
    } else if (name == "hydrocheck" || name == "hydroregime" && secondary) {
        d <- s$monthly
        d$year <- base::format(d$date, "%Y"); d$month <- as.integer(base::format(d$date, "%m"))
        d$z <- if (name == "hydrocheck") d$coverage else d$value
        plot <- ggplot2::ggplot(d, ggplot2::aes(x = month, y = year, fill = z))
        plot <- plot + ggplot2::geom_tile() + ggplot2::scale_x_continuous(breaks = 1:12) +
            ggplot2::scale_fill_gradient(low = "#edf3f8", high = "#32678c", na.value = "#b6b6b6") +
            ggplot2::labs(x = "Miesiąc", y = "Rok", fill = if (name == "hydrocheck") "%" else s$unit)
    } else if (name == "hydroflow" && !secondary) {
        d <- s$fdc
        if (!nrow(d)) return(FALSE)
        plot <- ggplot2::ggplot(d, ggplot2::aes(x = exceedance, y = value))
        plot <- plot + ggplot2::geom_line(colour = "#32678c") +
            ggplot2::labs(x = "Czas przewyższenia [%]", y = "Przepływ [m³/s]")
    } else if (name == "hydrotrend" && secondary) {
        d <- s$acf
        plot <- ggplot2::ggplot(d, ggplot2::aes(x = lag, y = value))
        plot <- plot + ggplot2::geom_col(fill = "#32678c", na.rm = TRUE) +
            ggplot2::geom_hline(yintercept = 0) + ggplot2::coord_cartesian(ylim = c(-1, 1)) +
            ggplot2::labs(x = "Opóźnienie [agregaty]", y = "Korelacja reszt (pary dostępne)")
    } else {
        d <- if (name == "hydroregime") s$summary else s$details
        if (name == "hydrolow") d$value <- d$deficit
        plot <- ggplot2::ggplot(d, ggplot2::aes(x = date, y = value))
        plot <- plot + ggplot2::geom_line(colour = "#32678c", na.rm = TRUE) +
            ggplot2::geom_point(colour = "#32678c", size = 1, na.rm = TRUE)
        if (name == "hydrotrend") plot <- plot + ggplot2::geom_line(ggplot2::aes(y = fitted), colour = "#9b343e", linetype = "dashed", na.rm = TRUE)
        if (name == "hydroflow") plot <- plot +
            ggplot2::geom_line(ggplot2::aes(y = minimum), colour = "#529a78", na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = maximum), colour = "#9b343e", na.rm = TRUE) +
            ggplot2::labs(caption = "Zielona: NQ; niebieska: SQ; czerwona: WQ")
        plot <- plot + ggplot2::labs(x = "Początek okresu", y = if (name == "hydrolow") "Deficyt wykrytych niżówek [m³]" else s$unit)
    }
    plot <- plot + ggplot2::facet_wrap(~station, scales = "free_y") + ggtheme
    print(plot)
    TRUE
}
