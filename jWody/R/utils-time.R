# Strict ISO dates avoid locale-dependent parsing and Excel serial ambiguity.
hydroDates <- function(x) {
    if (inherits(x, "Date")) return(x)
    z <- as.character(x)
    good <- !is.na(z) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z)
    out <- as.Date(rep(NA_character_, length(z)))
    out[good] <- suppressWarnings(as.Date(z[good], format = "%Y-%m-%d"))
    good <- good & !is.na(out) & base::format(out, "%Y-%m-%d") == z
    out[is.na(good) | !good] <- NA
    out
}

hydroYear <- function(date, start = 11L) {
    y <- as.integer(base::format(date, "%Y"))
    m <- as.integer(base::format(date, "%m"))
    y + as.integer(start > 1L & m >= start)
}

hydroBounds <- function(year, start) {
    begin <- as.Date(sprintf("%04d-%02d-01", year - as.integer(start > 1), start))
    end <- as.Date(sprintf("%04d-%02d-01", year + as.integer(start == 1), start)) - 1
    list(start = begin, end = end)
}

hydroOptions <- function(resolution = "daily", kind = "flow", yearStart = 11,
                         completeness = 100, dateStart = "", dateEnd = "") {
    if (!resolution %in% c("daily", "monthly")) stop("Nieznana rozdzielczość.")
    if (!kind %in% c("flow", "rain", "stage", "depth", "level")) stop("Nieznana wielkość.")
    if (!is.finite(yearStart) || yearStart != as.integer(yearStart) || yearStart < 1 || yearStart > 12)
        stop("Początek roku musi być miesiącem 1–12.")
    if (!is.finite(completeness) || completeness <= 0 || completeness > 100)
        stop("Kompletność musi należeć do (0, 100].")
    for (s in c(dateStart, dateEnd)) if (nzchar(s) && is.na(hydroDates(s))) stop("Błędna granica okresu: użyj RRRR-MM-DD.")
    if (nzchar(dateStart) && nzchar(dateEnd) && hydroDates(dateStart) > hydroDates(dateEnd)) stop("Początek okresu jest po końcu.")
    list(resolution = resolution, kind = kind, yearStart = yearStart,
         completeness = completeness, dateStart = dateStart, dateEnd = dateEnd)
}

hydroPrepare <- function(date, value, station = NULL, o = hydroOptions(), audit = FALSE) {
    if (length(date) != length(value) || !length(value)) stop("Brak danych lub niezgodne długości kolumn.")
    if (length(value) > 200000L) stop("Limit wersji 0.1: 200 000 wierszy; wybierz krótszy szereg.")
    if (is.null(station)) station <- rep("Szereg", length(value))
    station <- as.character(station)
    if (length(station) != length(value) || anyNA(station) || any(!nzchar(trimws(station))))
        stop("Uzupełnij identyfikator stacji w każdym wierszu.")
    if (!is.numeric(value)) stop("Wartości pomiarów muszą być liczbami.")
    if (any(!is.finite(value) & !is.na(value))) stop("Szereg zawiera nieskończone wartości.")
    d <- data.frame(date = hydroDates(date), value = value, station = station)
    if (nzchar(o$dateStart)) d <- d[is.na(d$date) | d$date >= hydroDates(o$dateStart), ]
    if (nzchar(o$dateEnd)) d <- d[is.na(d$date) | d$date <= hydroDates(o$dateEnd), ]
    if (!nrow(d)) stop("Brak danych w wybranym okresie.")
    if (length(unique(d$station)) > 50L) stop("Limit wersji 0.1: 50 stacji w jednej analizie.")
    if (o$resolution == "monthly" && any(base::format(d$date, "%d") != "01", na.rm = TRUE))
        stop("Dane miesięczne: data musi wskazywać pierwszy dzień miesiąca.")
    groups <- split(d, d$station)
    lapply(groups, function(g) {
        invalid <- sum(is.na(g$date))
        gvalid <- g[!is.na(g$date), ]
        dup <- duplicated(gvalid$date) | duplicated(gvalid$date, fromLast = TRUE)
        duplicates <- sum(duplicated(gvalid$date))
        if (!audit && invalid) stop(paste0(g$station[1], ": błędne daty; uruchom Kontrolę szeregu."))
        if (!audit && duplicates) stop(paste0(g$station[1], ": powtórzone daty; rozstrzygnij duplikaty."))
        if (!audit && o$kind %in% c("flow", "rain", "depth") && any(g$value < 0, na.rm = TRUE))
            stop("Wartości ujemne nie odpowiadają wybranej wielkości. Sprawdź kody braków i rodzaj pomiaru.")
        if (!nrow(gvalid)) stop(paste0(g$station[1], ": brak poprawnych dat."))
        # Conflicting timestamps are unavailable, not averaged or silently selected.
        gvalid$value[dup] <- NA_real_
        gvalid <- gvalid[!duplicated(gvalid$date), ]
        gvalid <- gvalid[order(gvalid$date), ]
        first <- if (nzchar(o$dateStart)) hydroDates(o$dateStart) else min(gvalid$date)
        last <- if (nzchar(o$dateEnd)) hydroDates(o$dateEnd) else max(gvalid$date)
        if (o$resolution == "monthly") {
            first <- as.Date(base::format(first, "%Y-%m-01"))
            last <- as.Date(base::format(last, "%Y-%m-01"))
        }
        if (as.numeric(last - first) > 200000) stop("Zakres dat jest zbyt długi; sprawdź daty.")
        grid <- seq(first, last, by = if (o$resolution == "daily") "day" else "month")
        m <- match(grid, gvalid$date)
        series <- data.frame(date = grid, value = gvalid$value[m], station = g$station[1])
        rr <- rle(is.na(series$value))
        longest <- if (any(rr$values)) max(rr$lengths[rr$values]) else 0L
        quality <- data.frame(station = g$station[1], start = as.character(first), end = as.character(last),
            rows = nrow(g), observed = sum(!is.na(series$value)), missing = sum(is.na(g$value)),
            absent = sum(is.na(m)), duplicates = duplicates, invalid = invalid,
            negative = sum(g$value < 0, na.rm = TRUE), longest = longest,
            coverage = 100 * mean(!is.na(series$value)))
        list(series = series, quality = quality)
    })
}

hydroUnit <- function(kind) switch(kind, flow = "m³/s", rain = "mm", stage = "cm", depth = "m p.p.t.", level = "m n.p.m.")

hydroReference <- function(d, start = 0, end = 0) {
    if (start < 0 || end < 0 || (start > 0 && end > 0 && start > end)) stop("Błędny okres odniesienia.")
    year <- as.integer(base::format(d$date, "%Y"))
    d[(start == 0 | year >= start) & (end == 0 | year <= end), , drop = FALSE]
}
