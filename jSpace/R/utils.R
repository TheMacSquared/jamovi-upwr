# Shared helpers for jSpace analyses: plot builders and geo utilities.

# --- world borders backdrop -------------------------------------------------

# Country borders (Natural Earth 1:110m) shipped as an sf object inside the
# module — read with readRDS so runtime needs no rnaturalearth and no GDAL
# vector driver. Cached per session.
.graniceEnv <- new.env(parent = emptyenv())

granicePanstw <- function() {
    if (is.null(.graniceEnv$kraje)) {
        path <- system.file("extdata", "kraje_ne110.rds", package = "jSpace")
        .graniceEnv$kraje <- readRDS(path)
    }
    .graniceEnv$kraje
}

# --- TLE / orbit helpers ----------------------------------------------------

# Greenwich Mean Sidereal Time [deg] for a UTC datetime string (IAU 1982).
# Used for the textbook TEME -> geographic conversion below; asteRisk's own
# TEMEtoLATLON needs the asteRiskData package (Earth orientation tables),
# which we deliberately do not bundle. GMST-only rotation is accurate to a
# small fraction of a degree — fully adequate for a teaching ground track.
gmstStopnie <- function(dateTime) {
    t <- as.POSIXct(dateTime, tz = "UTC")
    jd <- as.numeric(t) / 86400 + 2440587.5
    d <- jd - 2451545
    tc <- d / 36525
    gmst <- 280.46061837 + 360.98564736629 * d +
        0.000387933 * tc^2 - tc^3 / 38710000
    gmst %% 360
}

# TEME position [km] + UTC datetime -> geographic lat/lon [deg] and
# altitude above the mean Earth radius [km] (spherical approximation)
temeNaGeo <- function(pos, dateTime) {
    r <- sqrt(sum(pos^2))
    lat <- asin(pos[3] / r) * 180 / pi
    lonInercjalna <- atan2(pos[2], pos[1]) * 180 / pi
    lon <- lonInercjalna - gmstStopnie(dateTime)
    lon <- ((lon + 180) %% 360) - 180
    c(lat = lat, lon = lon, wysokosc = r - 6371)
}

# reads the example TLE file shipped with the module: list of 3-line sets
wczytajPrzykladyTLE <- function() {
    path <- system.file("extdata", "tle_przyklady.txt", package = "jSpace")
    lines <- readLines(path, warn = FALSE)
    n <- length(lines) %/% 3
    sats <- lapply(seq_len(n), function(i) lines[(3 * i - 2):(3 * i)])
    names(sats) <- vapply(sats, function(s) trimws(s[1]), character(1))
    sats
}

# --- plot builders ----------------------------------------------------------

buildGroupBoxPlot <- function(label, x, grp, plotMeans, ggtheme, theme) {
    df <- data.frame(x = x, grp = grp)
    means <- aggregate(x ~ grp, df, mean)

    plot <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = x)) +
        ggplot2::geom_boxplot(fill = theme$fill[2], color = theme$color[1],
                              outlier.alpha = 0.4) +
        ggplot2::labs(x = NULL, y = label) +
        ggtheme +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 30, hjust = 1))

    if (plotMeans)
        plot <- plot + ggplot2::geom_point(
            data = means, ggplot2::aes(x = grp, y = x),
            shape = 23, size = 3, fill = theme$color[2],
            color = theme$color[1])

    plot
}

# base map with country borders, optionally cropped to a lon/lat range;
# land gets a soft tint of the theme ink so the backdrop stays neutral in
# both light and dark jUPWR themes and does not fight the accent color
buildMapaBazowa <- function(zakres = NULL, theme) {
    kraje <- granicePanstw()
    lad <- grDevices::adjustcolor(theme$color[1], alpha.f = 0.12)
    granica <- grDevices::adjustcolor(theme$color[1], alpha.f = 0.45)
    plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = kraje, fill = lad,
                         color = granica, linewidth = 0.2)
    if (!is.null(zakres))
        plot <- plot + ggplot2::coord_sf(
            xlim = zakres$lon, ylim = zakres$lat, expand = FALSE)
    plot
}
