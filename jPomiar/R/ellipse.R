# Ellipses are computed in the original coordinate units, without standardization.
measurementEllipse <- function(x, y, kind = "standard", level = 95) {
    if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y))
        stop("Wybierz dwie liczbowe kolumny o tej samej długości.", call. = FALSE)
    measurementScalar(level, "Poziom", lower = 50, upper = 99.9)
    if (!kind %in% c("standard", "scatter", "mean")) stop("Nieznany rodzaj elipsy.", call. = FALSE)
    if (any(is.infinite(x)) || any(is.infinite(y))) stop("Współrzędne zawierają nieskończoności.", call. = FALSE)
    complete <- stats::complete.cases(x, y)
    d <- cbind(x = x[complete], y = y[complete])
    n <- nrow(d)
    if (n < 2) stop("Potrzebne są co najmniej dwie kompletne pary pomiarów.", call. = FALSE)
    center <- colMeans(d)
    covariance <- stats::cov(d)
    if (!all(is.finite(c(center, covariance)))) stop("Zmień jednostki: zakres obliczeń został przekroczony.", call. = FALSE)
    eig <- eigen(covariance, symmetric = TRUE)
    values <- pmax(eig$values, 0)
    if (!all(is.finite(values)) || !is.finite(sum(values))) stop("Wariancja przekracza zakres obliczeń.", call. = FALSE)
    tolerance <- max(values) * 1e-12
    singular <- min(values) <= tolerance
    isotropic <- abs(diff(values)) <= tolerance
    angles <- (atan2(eig$vectors[2, ], eig$vectors[1, ]) * 180 / pi) %% 180
    if (isotropic) angles[] <- NA_real_
    correlation <- if (all(diag(covariance) > 0)) stats::cor(d)[1, 2] else NA_real_
    notes <- character()
    if (singular) notes <- c(notes, "Macierz jest osobliwa lub niemal osobliwa. Elipsa opisowa może zdegenerować się do odcinka lub punktu; obszar Hotellinga jest niedostępny.")
    if (isotropic) notes <- c(notes, "Brak wyróżnionego kierunku osi: jednakowe wariancje główne (koło lub punkt). Kąty pozostają nieokreślone.")
    if (is.na(correlation)) notes <- c(notes, "Korelacja jest nieokreślona, gdy przynajmniej jedna współrzędna jest stała.")
    available <- TRUE
    scale <- switch(kind, standard = 1, scatter = sqrt(stats::qchisq(level / 100, df = 2)), mean = {
        if (n <= 2 || singular) {
            available <- FALSE
            notes <- c(notes, "Obszar ufności średniej wymaga n > 2 i nieosobliwej macierzy kowariancji.")
            NA_real_
        } else sqrt(2 * (n - 1) / (n * (n - 2)) * stats::qf(level / 100, 2, n - 2))
    })
    radii <- sqrt(values) * scale
    if (available && any(!is.finite(radii))) stop("Półosie przekraczają zakres obliczeń.", call. = FALSE)
    label <- switch(kind, standard = "Elipsa standardowa (1 SD)",
                    scatter = sprintf("Rozrzut %g%% (przybliżenie)", level),
                    mean = sprintf("Ufność średniej %g%% (Hotelling)", level))
    points <- NULL
    segments <- NULL
    if (available) {
        theta <- seq(0, 2 * pi, length.out = 361)
        boundary <- sweep(eig$vectors %*% (radii * rbind(cos(theta), sin(theta))), 1, center, "+")
        if (!all(is.finite(boundary))) stop("Elipsa przekracza zakres obliczeń. Zmień jednostki.", call. = FALSE)
        points <- data.frame(x = boundary[1, ], y = boundary[2, ])
        ends <- eig$vectors %*% diag(radii, 2)
        segments <- data.frame(x = center[1] - ends[1, ], y = center[2] - ends[2, ],
                               xend = center[1] + ends[1, ], yend = center[2] + ends[2, ])
    }
    list(n = n, missing = sum(!complete), center = center, covariance = covariance,
         meanCovariance = covariance / n, r = correlation,
         principal = data.frame(axis = c("PC1", "PC2"), eigenvalue = values, sd = sqrt(values),
             share = if (sum(values) > 0) 100 * values / sum(values) else c(NA_real_, NA_real_), angle = angles),
         radii = radii, angle = angles[1], available = available, label = label,
         points = points, segments = segments, observations = as.data.frame(d), notes = notes)
}

measurementEllipsePlot <- function(state, ggtheme, theme) {
    if (is.null(state)) return(FALSE)
    r <- state$result
    plot <- ggplot2::ggplot(r$observations, ggplot2::aes(x = x, y = y))
    plot <- plot + ggplot2::geom_point(colour = "#718897", alpha = 0.65) +
        ggplot2::geom_point(data = data.frame(x = r$center[1], y = r$center[2]),
                           colour = "#ae6030", shape = 4, size = 3, stroke = 1.2) +
        ggplot2::coord_equal() + ggplot2::labs(x = state$xLabel, y = state$yLabel,
            title = if (r$available) r$label else "Obszar ufności niedostępny",
            caption = "Punkty: pomiary; krzyżyk: średnie położenie\nObie współrzędne muszą mieć tę samą jednostkę.") + ggtheme
    if (r$available) plot <- plot + ggplot2::geom_path(data = r$points, colour = "#32678c", linewidth = 0.8)
    if (r$available && state$showAxes && !is.na(r$angle)) plot <- plot +
        ggplot2::geom_segment(data = r$segments,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            inherit.aes = FALSE, linetype = "dashed", colour = "#ae6030")
    print(plot)
    TRUE
}
