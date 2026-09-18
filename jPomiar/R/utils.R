# Computational helpers use standard uncertainties, never expanded uncertainties.
measurementScalar <- function(x, label, lower = -Inf, upper = Inf, positive = FALSE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        x < lower || x > upper || (positive && x <= 0))
        stop(sprintf("%s: podaj skończoną liczbę z dopuszczalnego zakresu.", label), call. = FALSE)
    x
}

measurementSeries <- function(x) {
    if (!is.numeric(x)) stop("Wybierz liczbową kolumnę pomiarów.", call. = FALSE)
    missing <- sum(is.na(x))
    index <- which(!is.na(x))
    x <- x[index]
    if (any(!is.finite(x))) stop("Pomiary zawierają wartości nieskończone.", call. = FALSE)
    if (length(x) < 2L) stop("Potrzebne są co najmniej dwa niebrakujące pomiary.", call. = FALSE)
    s <- stats::sd(x)
    m <- mean(x)
    if (!all(is.finite(c(m, s)))) stop("Wartości są zbyt duże do obliczeń. Zmień jednostkę.", call. = FALSE)
    list(n = length(x), missing = missing, mean = m, sd = s,
         se = s / sqrt(length(x)), x = x, index = index)
}

measurementSummary <- function(value, variance, k) {
    measurementScalar(value, "Wynik")
    measurementScalar(variance, "Wariancja wyniku", lower = 0)
    measurementScalar(k, "Mnożnik k", positive = TRUE)
    uc <- sqrt(variance)
    expanded <- k * uc
    limits <- c(value - expanded, value + expanded)
    if (!all(is.finite(c(expanded, limits))))
        stop("Wynik przekracza zakres obliczeń. Zmień jednostkę.", call. = FALSE)
    list(estimate = value, uc = uc, k = k, expanded = expanded,
         lower = limits[1], upper = limits[2])
}

measurementBudget <- function(value, correction = 0, uA = NULL,
                              calibrationU = NULL, calibrationK = 2,
                              resolution = NULL, k = 2) {
    measurementScalar(value, "Wartość pomiaru")
    measurementScalar(correction, "Poprawka")
    components <- data.frame(source = character(), method = character(), u = numeric())
    if (!is.null(uA)) {
        measurementScalar(uA, "Niepewność typu A", lower = 0)
        components <- rbind(components, data.frame(source = "Powtarzalność średniej", method = "A", u = uA))
    }
    if (!is.null(calibrationU)) {
        measurementScalar(calibrationU, "U wzorcowania", lower = 0)
        measurementScalar(calibrationK, "k wzorcowania", positive = TRUE)
        components <- rbind(components, data.frame(source = "Wzorcowanie", method = "B", u = calibrationU / calibrationK))
    }
    if (!is.null(resolution)) {
        measurementScalar(resolution, "Krok wskazania", lower = 0)
        components <- rbind(components, data.frame(source = "Rozdzielczość", method = "B", u = resolution / sqrt(12)))
    }
    if (!nrow(components)) stop("Włącz co najmniej jeden składnik budżetu i podaj jego wartość.", call. = FALSE)
    components$variance <- components$u^2
    v <- sum(components$variance)
    summary <- measurementSummary(value + correction, v, k)
    components$share <- if (v > 0) 100 * components$variance / v else rep(NA_real_, nrow(components))
    list(summary = summary, components = components)
}

measurementPropagation <- function(model, x, y, ux, uy, rho = 0, k = 2) {
    measurementScalar(x, "x")
    measurementScalar(y, "y")
    measurementScalar(ux, "u(x)", lower = 0)
    measurementScalar(uy, "u(y)", lower = 0)
    measurementScalar(rho, "Korelacja", lower = -1, upper = 1)
    if (!model %in% c("sum", "difference", "product", "ratio"))
        stop("Nieznany wzór propagacji.", call. = FALSE)
    if (model == "ratio" && y == 0) stop("Iloraz wymaga mianownika y różnego od zera.", call. = FALSE)
    spec <- switch(model,
        sum = list(value = x + y, c = c(1, 1), formula = "z = x + y"),
        difference = list(value = x - y, c = c(1, -1), formula = "z = x − y"),
        product = list(value = x * y, c = c(y, x), formula = "z = x × y"),
        ratio = list(value = x / y, c = c(1 / y, -(x / y) / y), formula = "z = x / y"))
    if (!all(is.finite(c(spec$value, spec$c))))
        stop("Wynik lub pochodne przekraczają zakres obliczeń. Zmień jednostki.", call. = FALSE)
    scaled <- spec$c * c(ux, uy)
    terms <- c(scaled^2, 2 * rho * scaled[1] * scaled[2])
    if (!all(is.finite(terms))) stop("Wariancja przekracza zakres obliczeń. Zmień jednostki.", call. = FALSE)
    # A sum of non-negative terms is stable at perfect correlation/cancellation.
    variance <- (scaled[1] + rho * scaled[2])^2 + (1 - rho) * (1 + rho) * scaled[2]^2
    summary <- measurementSummary(spec$value, variance, k)
    notes <- character()
    if (model %in% c("product", "ratio"))
        notes <- c(notes, "Propagacja pierwszego rzędu (linearyzacja): wynik jest przybliżony. Przy dużych niepewnościach potrzebna jest ocena nieliniowości, np. metodą Monte Carlo.")
    if (model == "ratio" && uy >= 0.1 * abs(y) && uy > 0)
        notes <- c(notes, "Niepewność mianownika jest duża względem jego wartości (u(y)/|y| ≥ 0,1). Przybliżenie liniowe może być niedokładne; próg 0,1 jest wskazówką dydaktyczną, nie gwarancją poprawności poniżej progu.")
    if (model %in% c("product", "ratio") && variance == 0 && (ux > 0 || uy > 0))
        notes <- c(notes, "Zerowa wariancja w przybliżeniu liniowym nie dowodzi braku niepewności: pominięte wyrazy wyższego rzędu mogą mieć znaczenie.")
    list(summary = summary, inputs = data.frame(input = c("x", "y"), value = c(x, y), u = c(ux, uy), c = spec$c),
         components = data.frame(source = c("x: c_x² u(x)²", "y: c_y² u(y)²", "Kowariancja: 2 c_x c_y ρ u(x) u(y)"), variance = terms),
         formula = spec$formula, rho = rho, notes = notes)
}

measurementRows <- function(table, data) {
    for (i in seq_len(nrow(data))) table$addRow(rowKey = i, values = as.list(data[i, , drop = FALSE]))
}

measurementCoverageText <- function() {
    paste0("<p>U = k · u_c. Granice wynik ± U nie są automatycznie przedziałem ufności 95%. ",
           "Interpretacja probabilistyczna wymaga założeń o rozkładzie i stopniach swobody. ",
           "Wartość k = 2 jest mnożnikiem, nie zadanym poziomem ufności.</p>")
}

measurementVariancePlot <- function(state, ggtheme, theme) {
    if (is.null(state)) return(FALSE)
    d <- state
    d$source <- factor(d$source, levels = rev(d$source))
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = source, y = variance))
    plot <- plot + ggplot2::geom_col(fill = "#32678c") + ggplot2::geom_hline(yintercept = 0) +
        ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Wkład do wariancji wyniku") + ggtheme
    print(plot)
    TRUE
}
