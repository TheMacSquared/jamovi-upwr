getGlobalTheme <- function(name, palette) {

    ggtheme <- getGGTheme(name, scale = 'discrete', palette)
    theme <- getTheme(name, palette)

    return(list(ggtheme=ggtheme, theme=theme))
}

getGGTheme <- function(name, scale, palette) {

    if (requireNamespace('ggplot2')) {

        base_size <- 16

        if (name == 'hadley')
            ggtheme <- jmvcore::theme_hadley(base_size, scale, palette)
        else if (name == 'minimal')
            ggtheme <- jmvcore::theme_min(base_size, scale, palette)
        else if (name == 'iheartspss')
            ggtheme <- jmvcore::theme_spss(base_size, scale, palette)
        else if (name == 'bw')
            ggtheme <- jmvcore::theme_blackwhite(base_size, scale, palette)
        else if (name == 'presentation')
            ggtheme <- jmvcore::theme_presentation(base_size, scale, palette)
        else if (name == 'grid')
            ggtheme <- jmvcore::theme_grid(base_size, scale, palette)
        else if (name == 'jupwrJasny')
            ggtheme <- jmvcore::theme_jupwr_jasny(base_size, scale, palette)
        else if (name == 'jupwrCiemny')
            ggtheme <- jmvcore::theme_jupwr_ciemny(base_size, scale, palette)
        else
            ggtheme <- jmvcore::theme_default(base_size, scale, palette)

    } else {

        ggtheme <- NULL

    }

    return(ggtheme)
}


getTheme = function(name = 'default', palette = 'jmv') {
    theme <- list()

    if (name == 'iheartspss') {
        theme[['color']] <- c('#333333', '#333333')
        theme[['fill']] <- c('#F0F0F0', '#d3ce97')
        theme[['palette']] <- palette
    } else if (name %in% names(jupwrNames)) {
        # analyses draw reference lines and point outlines with color[1] and
        # fill points with fill[1], so these have to follow the variant --
        # otherwise the dark variant draws near-black marks on a dark panel
        v <- jupwrVariants[[jupwrNames[[name]]]]
        theme[['color']] <- c(v$ink_soft, jmvcore::colorPalette(1, palette, 'color'))
        theme[['fill']] <- c(v$panel, jmvcore::colorPalette(1, palette, 'fill'))
        theme[['palette']] <- palette
    } else {
        theme[['color']] <- c('#333333', jmvcore::colorPalette(1, palette, 'color'))
        theme[['fill']] <- c('#FFFFFF', jmvcore::colorPalette(1, palette, 'fill'))
        theme[['palette']] <- palette
    }

    if (name == 'bw')
        theme[['bw']] <- TRUE
    else
        theme[['bw']] <- FALSE

    # smooth ramps for continuous aesthetics (heat maps, gradients); analyses
    # can use these with scale_fill_gradientn() / scale_colour_gradientn()
    theme[['gradient']] <- gradientPalette(palette, 'sequential')
    theme[['divergent']] <- gradientPalette(palette, 'diverging')

    return(theme)
}

#' Creates the hadley jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the hadley jmv ggplot2 theme
#' @export
theme_hadley <- function(base_size = 16, scale = 'none', palette = 'jmv') {
    theme <- list(baseTheme(base_size))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the default jmv ggplot2 theme
#'
#' Follows the APA 7 figure conventions: pure black axes and lettering on a
#' white field, no grid, ticks pointing outward and one single type size
#' throughout the figure.
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the default jmv ggplot2 theme
#' @export
theme_default <- function(base_size = 16, scale = 'none', palette = 'jmv') {

    # APA asks for black lettering at a single size across the whole figure
    ink <- '#000000'
    size <- base_size * 0.8

    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill='transparent', color=NA),
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(linewidth=.6, colour=ink),
            axis.ticks = ggplot2::element_line(linewidth=.6, colour=ink),
            axis.ticks.length = ggplot2::unit(4, 'pt'),
            axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 0, 0, 0), colour=ink, size=size, lineheight=0.85),
            axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0, 5, 0, 0), colour=ink, size=size),
            axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10, 0, 0, 0), colour=ink, size=size),
            axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0, 10, 0, 0), colour=ink, size=size, angle=90),
            legend.key = ggplot2::element_blank(),
            legend.background = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(colour=ink, size=size),
            legend.title = ggplot2::element_text(colour=ink, size=size),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA),
            strip.text.x = ggplot2::element_text(colour=ink, size=size),
            strip.text.y = ggplot2::element_text(colour=ink, size=size),
            plot.title = ggplot2::element_text(
                margin=ggplot2::margin(0, 0, 15, 0), colour=ink,
                size=size, hjust=0, face='bold'))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the spss jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the spss jmv ggplot2 theme
#' @export
theme_spss <- function(base_size = 16, scale = 'none', palette = 'jmv') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.border = ggplot2::element_rect(colour="#333333", fill=NA, size=0.5),
            panel.background = ggplot2::element_rect(fill='#F0F0F0'),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the minimal jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the minimal jmv ggplot2 theme
#' @export
theme_min <- function(base_size = 16, scale = 'none', palette = 'jmv') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background= ggplot2::element_rect(fill='transparent', color=NA),
            axis.line = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = '#E8E8E8'),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the black & white jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the black & white jmv ggplot2 theme
#' @export
theme_blackwhite <- function(base_size = 16, scale = 'none', palette = 'Greys') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill='transparent', color=NA),
            panel.border = ggplot2::element_rect(colour='#333333', fill=NA, linewidth=0.5),
            panel.grid.major = ggplot2::element_line(colour='#D9D9D9', linewidth=0.3),
            panel.grid.minor = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_line(colour='#333333', linewidth=0.3),
            legend.key = ggplot2::element_rect(fill='transparent', color=NA),
            strip.background = ggplot2::element_rect(fill='transparent', colour='#333333'))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the presentation jmv ggplot2 theme
#'
#' Like the default theme, but with larger type and heavier lines so that
#' plots stay readable when projected or pasted onto a slide.
#'
#' @param base_size Font size (scaled up internally)
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the presentation jmv ggplot2 theme
#' @export
theme_presentation <- function(base_size = 16, scale = 'none', palette = 'jmv') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size * 1.35),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill='transparent', color=NA),
            axis.line = ggplot2::element_line(linewidth=.8, colour='#333333'),
            axis.ticks = ggplot2::element_line(linewidth=.8, colour='#333333'),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the grid jmv ggplot2 theme
#'
#' White panel with both major and minor grid lines, for plots that are meant
#' to be read off rather than only looked at.
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the grid jmv ggplot2 theme
#' @export
theme_grid <- function(base_size = 16, scale = 'none', palette = 'jmv') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill='transparent', color=NA),
            axis.line = ggplot2::element_line(linewidth=.5, colour='#333333'),
            panel.grid.major = ggplot2::element_line(colour='#D0D0D0', linewidth=0.35),
            panel.grid.minor = ggplot2::element_line(colour='#E8E8E8', linewidth=0.25),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

# --- jUPWR lecture-script styling ---------------------------------------------
# Ported from the standalone lecture-note theme so that plots produced in jamovi
# match the lecture notes. Each variant carries its own ink, panel and rule
# colours; the matching palettes live in jupwrPalettes below.

jupwrVariants <- list(
    ciemny = list(
        bg = '#0f1419', panel = '#141a21', panel_alt = '#1a2028',
        ink = '#e8e4da', ink_soft = '#b5b1a7', ink_faded = '#a09b90',
        rule = '#3a4252', rule_soft = '#242b36', accent = '#d76473',
        small_text = 0.95),
    jasny = list(
        # white ground so exported figures sit neutrally on any page
        bg = '#ffffff', panel = '#ffffff', panel_alt = '#f7ece8',
        ink = '#2a1f24', ink_soft = '#5a4850', ink_faded = '#9a8790',
        rule = '#ead8d3', rule_soft = '#f4e6e1', accent = '#9c3b4a'))

# main       -- 6 colours, accent first; separated by lightness and a
#               yellow/blue axis rather than a red/green pair, so the series
#               stay apart under protanopia and deuteranopia
# para       -- accent against a second colour of equal weight, used for n = 2
# rozbiezna  -- bipolar, for values with a natural zero (r, residuals)
# ciepla     -- ordinal, reads by lightness
jupwrPalettes <- list(
    ciemny = list(
        main = c('#d76473', '#d4a858', '#4a7fb5', '#6ac4b8', '#8fd6a0', '#c39ae0'),
        para = c('#d76473', '#d4a858'),
        rozbiezna = c('#e8737f', '#b0505f', '#6b3a45', '#2a3140', '#3a5f85', '#4a8fc7', '#7fbde8'),
        ciepla = c('#3a1a20', '#7d2f3c', '#b84050', '#d76473', '#d4a858', '#f0dcae')),
    jasny = list(
        main = c('#9c3b4a', '#d99a5b', '#3f6f9e', '#7a9b8e', '#b183a8', '#c2571f'),
        para = c('#9c3b4a', '#d99a5b'),
        rozbiezna = c('#9c3b4a', '#c0737e', '#e0b2b8', '#f7f0ec', '#b3c6d8', '#6f97b8', '#3f6f9e'),
        ciepla = c('#6e2632', '#9c3b4a', '#c85264', '#d99a5b', '#eec79a', '#fbe3e5')))

# a plain greyscale set, so that the black & white theme has a palette of the
# same kind as the others to pair with
greyscaleColors <- c('#1a1a1a', '#3d3d3a', '#8a8780', '#b8b5ad', '#d9d5cc', '#e8e5de')

# theme and palette names map onto the same variant
jupwrNames <- c(jupwrJasny = 'jasny', jupwrCiemny = 'ciemny')

# fonts shipped with the distribution, keyed by the family name they report
jupwrFontFiles <- c(
    'Source Serif 4' = 'SourceSerif4-Regular.ttf',
    'Atkinson Hyperlegible' = 'AtkinsonHyperlegible-Regular.ttf',
    'JetBrains Mono' = 'JetBrainsMono-Regular.ttf')

jupwrFontState <- new.env(parent = emptyenv())

#' Locate the directory holding the bundled fonts
#'
#' Looks at JUPWR_FONTS_PATH, then at <JAMOVI_HOME>/fonts, then walks up from
#' the installed jmvcore (modules/base/R/jmvcore -> the installation root), so
#' that the Docker image and the macOS and Windows packages all resolve.
#'
#' @return the path, or NULL when no font directory is present
jupwrFontsDir <- function() {

    root <- tryCatch(
        dirname(dirname(dirname(dirname(system.file(package='jmvcore'))))),
        error=function(e) '')

    candidates <- c(
        Sys.getenv('JUPWR_FONTS_PATH', ''),
        file.path(Sys.getenv('JAMOVI_HOME', ''), 'fonts'),
        file.path(root, 'fonts'))

    candidates <- candidates[nzchar(candidates)]
    found <- candidates[dir.exists(candidates)]

    if (length(found) > 0)
        found[1]
    else
        NULL
}

#' Register the bundled fonts with systemfonts
#'
#' They are added as local fonts rather than installed system wide, which is
#' the one mechanism that works the same way on Linux, macOS and Windows.
#' Runs once per session.
jupwrRegisterFonts <- function() {

    if (isTRUE(jupwrFontState$registered))
        return(invisible(FALSE))
    jupwrFontState$registered <- TRUE

    if ( ! requireNamespace('systemfonts', quietly=TRUE))
        return(invisible(FALSE))

    dir <- jupwrFontsDir()
    if (is.null(dir))
        return(invisible(FALSE))

    files <- list.files(dir, pattern='[.](ttf|otf|ttc)$', full.names=TRUE)
    if (length(files) == 0)
        return(invisible(FALSE))

    ok <- tryCatch({ systemfonts::add_fonts(files); TRUE },
                   error=function(e) FALSE)

    invisible(ok)
}

#' Resolve the lecture-note fonts, falling back to the generic families
#'
#' A family counts as available when it is either shipped with the
#' distribution or already installed on the machine; otherwise the theme takes
#' 'serif' / 'sans' / 'mono' so that plots still render.
#'
#' @return a list with the title, text and mono family names
jupwrFamilies <- function() {

    if ( ! is.null(jupwrFontState$families))
        return(jupwrFontState$families)

    jupwrRegisterFonts()
    dir <- jupwrFontsDir()

    installed <- character(0)
    if (requireNamespace('systemfonts', quietly=TRUE))
        installed <- tryCatch(systemfonts::system_fonts()$family,
                              error=function(e) character(0))

    pick <- function(preferred, fallback) {
        bundled <- ! is.null(dir) && jupwrFontFiles[[preferred]] %in% list.files(dir)
        if (bundled || preferred %in% installed) preferred else fallback
    }

    families <- list(
        title = pick('Source Serif 4', 'serif'),
        text  = pick('Atkinson Hyperlegible', 'sans'),
        mono  = pick('JetBrains Mono', 'mono'))

    jupwrFontState$families <- families
    families
}

# Built on theme_minimal() rather than on baseTheme(), to stay faithful to the
# lecture-note styling; everything baseTheme() would set is overridden anyway.
buildJupwrTheme <- function(variant, base_size, scale, palette) {

    v <- jupwrVariants[[variant]]
    fam <- jupwrFamilies()
    half <- base_size / 2
    small <- if (is.null(v$small_text)) 0.85 else v$small_text
    gridLine <- ggplot2::element_line(colour=v$rule_soft, linewidth=0.35)

    theme <- list(
        ggplot2::theme_minimal(base_size=base_size, base_family=fam$text) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill=v$bg, colour=NA),
            panel.background = ggplot2::element_rect(fill=v$panel, colour=NA),
            panel.border = ggplot2::element_blank(),
            panel.grid.major.x = gridLine,
            panel.grid.major.y = gridLine,
            panel.grid.minor = ggplot2::element_blank(),
            axis.line.x = ggplot2::element_line(colour=v$rule, linewidth=0.5),
            axis.line.y = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_line(colour=v$rule, linewidth=0.4),
            axis.ticks.length = ggplot2::unit(base_size / 3, 'pt'),
            axis.text = ggplot2::element_text(colour=v$ink_faded, size=ggplot2::rel(small), family=fam$mono),
            axis.title = ggplot2::element_text(colour=v$ink_soft, size=ggplot2::rel(0.9)),
            axis.title.x = ggplot2::element_text(margin=ggplot2::margin(t=half), hjust=0),
            axis.title.y = ggplot2::element_text(margin=ggplot2::margin(r=half), hjust=1, angle=90),
            plot.title = ggplot2::element_text(family=fam$title, face='bold', colour=v$ink,
                                               size=ggplot2::rel(1.5), hjust=0,
                                               margin=ggplot2::margin(b=half * 0.6)),
            plot.subtitle = ggplot2::element_text(family=fam$title, colour=v$ink_soft,
                                                  size=ggplot2::rel(1.0), hjust=0, lineheight=1.3,
                                                  face='italic', margin=ggplot2::margin(b=base_size)),
            plot.caption = ggplot2::element_text(family=fam$mono, colour=v$ink_faded,
                                                 size=ggplot2::rel(0.72), hjust=0,
                                                 margin=ggplot2::margin(t=base_size)),
            plot.title.position = 'plot',
            plot.caption.position = 'plot',
            plot.margin = ggplot2::margin(half, half, half, half),
            legend.position = 'top',
            legend.justification = 'left',
            legend.title = ggplot2::element_text(colour=v$ink_soft, size=ggplot2::rel(small)),
            legend.text = ggplot2::element_text(colour=v$ink_soft, size=ggplot2::rel(small)),
            legend.key = ggplot2::element_blank(),
            legend.background = ggplot2::element_blank(),
            legend.margin = ggplot2::margin(0, 0, half, 0),
            strip.background = ggplot2::element_rect(fill=v$panel_alt, colour=NA),
            strip.text = ggplot2::element_text(family=fam$title, face='bold', colour=v$ink,
                                               size=ggplot2::rel(0.9),
                                               margin=ggplot2::margin(half * 0.6, half * 0.6,
                                                                      half * 0.6, half * 0.6)),
            panel.spacing = ggplot2::unit(base_size, 'pt')))

    if (scale != 'none')
        theme <- c(theme, ggPalette(palette, scale))

    return(theme)
}

#' Creates the jUPWR lecture-note ggplot2 theme (dark)
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the dark lecture-note ggplot2 theme
#' @export
theme_jupwr_ciemny <- function(base_size = 16, scale = 'none', palette = 'jupwrCiemny')
    buildJupwrTheme('ciemny', base_size, scale, palette)

#' Creates the jUPWR lecture-note ggplot2 theme (light)
#'
#' @param base_size Font size
#' @param scale 'none', 'discrete' or 'continuous'
#' @param palette Color palette name
#'
#' @return the light lecture-note ggplot2 theme
#' @export
theme_jupwr_jasny <- function(base_size = 16, scale = 'none', palette = 'jupwrJasny')
    buildJupwrTheme('jasny', base_size, scale, palette)

seqPalettes <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')

otherPalettes <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1',
                 'Pastel2', 'Set1', 'Set2', 'Set3')

divPalettes <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                 'RdYlGn', 'Spectral')

# Okabe & Ito's qualitative palette -- distinguishable under the common forms
# of colour vision deficiency
okabeItoColors <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2',
                    '#D55E00', '#CC79A7', '#000000')

# anchor points of the viridis colour map; perceptually uniform, colour vision
# deficiency friendly and legible when printed in greyscale
viridisColors <- c('#440154', '#482878', '#3E4A89', '#31688E', '#26828E',
                   '#1F9E89', '#35B779', '#6DCD59', '#B4DE2C', '#FDE725')

#' @importFrom grDevices col2rgb rgb
#' @importFrom stats approx
interp <- function(n, pal, begin=0.0, end=1.0) {

    palette <- RColorBrewer::brewer.pal(9, pal)
    rgb <- col2rgb(palette)

    or <- rgb['red',]
    og <- rgb['green',]
    ob <- rgb['blue',]

    ox <- seq(0, 1, length.out=9)
    if (n == 1)
        nx <- (end + begin) / 2
    else
        nx <- seq(begin, end, length.out=n)

    r <- approx(ox, or, nx)$y
    g <- approx(ox, og, nx)$y
    b <- approx(ox, ob, nx)$y

    rgb(r, g, b, maxColorValue=255)
}

brighten <- function(colours, amount) {
    colours <- col2rgb(colours)
    colours <- colours * amount
    colours <- pmin(colours, 255)
    rgb(colours['red',],
        colours['green',],
        colours['blue',],
        maxColorValue=255)
}

lighten <- function(colours, amount) {
    colours <- col2rgb(colours)
    rgb((1 - amount) * colours['red',] + 255 * amount,
        (1 - amount) * colours['green',] + 255 * amount,
        (1 - amount) * colours['blue',]  + 255 * amount,
        maxColorValue=255)
}

#' A function that creates a color palette
#'
#' @param n Number of colors needed
#' @param pal Color palette name
#' @param type 'fill' or 'color'
#'
#' @return a vector of hex color codes
#' @importFrom grDevices hcl
#' @export
colorPalette <- function(n = 5, pal = 'jmv', type='fill') {

    # extract colors belonging to palette name
    if (pal %in% seqPalettes) {

        if (type == 'fill')
            cols <- interp(n, pal, 0.1, 0.6)
        else
            cols <- interp(n, pal, 0.4, 0.9)

    } else if (pal %in% otherPalettes) {

        cols <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
        if (type == 'fill')
            cols <- lighten(cols, .4)
        else
            cols <- lighten(cols, .1)

    } else if (pal == 'spss') {

        cols <- c('#3e58ac', '#2eb848', '#d3ce97', '#7c287d', '#fbf873', '#f8981d', '#248bac', '#a21619')
        if (n == 1) {
            cols <- cols[3]
        } else {
            if (type == 'fill')
                cols <- lighten(cols, .4)
            else
                cols <- lighten(cols, .1)
        }

    } else if (pal %in% names(jupwrNames)) {

        p <- jupwrPalettes[[jupwrNames[[pal]]]]
        if (n == 1)
            cols <- p$main[1]
        else if (n == 2)
            cols <- p$para
        else
            cols <- p$main

        # the palettes are designed as-is, so only lift the fills enough to
        # keep a box or bar lighter than its own outline
        if (type == 'fill')
            cols <- lighten(cols, .25)

    } else if (pal == 'greyscale') {

        cols <- greyscaleColors
        if (n == 2)
            cols <- cols[c(1, 4)]

        if (type == 'fill')
            cols <- lighten(cols, .25)

    } else if (pal == 'okabeito') {

        cols <- okabeItoColors

        if (type == 'fill')
            cols <- lighten(cols, .4)
        else
            cols <- lighten(cols, .1)

    } else if (pal == 'viridis') {

        ramp <- grDevices::colorRampPalette(viridisColors)
        if (n == 1)
            cols <- ramp(3)[2]
        else
            cols <- ramp(n)

        if (type == 'fill')
            cols <- lighten(cols, .3)

    } else if (pal == 'hadley') {

        ggColors <- function(n) {
            hues <- seq(15, 375, length = n + 1)
            hcl(h = hues, l = 65, c = 100)[1:n]
        }

        cols <- ggColors(n)

        if (type == 'fill')
            cols <- lighten(cols, .4)
        else
            cols <- lighten(cols, .1)

    } else {

        cols <- c('#6B9DE8', '#9F9F9F', '#E6AC40', '#399B3F', '#CE3D3D', '#3E6DA9')
        if (n == 2)
            cols <- cols[c(1,3)]

        if (type == 'fill')
            cols <- lighten(cols, .4)
        else
            cols <- lighten(cols, .1)
    }

    # add colors if palette needs more colors
    if (n > length(cols))
        cols <- grDevices::colorRampPalette(cols)(n)

    return(cols[1:n])
}

#' A function that creates a smooth colour ramp for continuous scales
#'
#' Discrete palettes only cover grouping aesthetics; heat maps and other
#' gradients need a continuous ramp. Sequential and diverging palettes are
#' ramped through their own colours, qualitative palettes are ramped from
#' (or through) their leading colours.
#'
#' @param pal Color palette name
#' @param type 'sequential' or 'diverging'
#' @param n Number of colors in the ramp
#'
#' @return a vector of hex color codes
#' @export
gradientPalette <- function(pal = 'jmv', type = 'sequential', n = 256) {

    if (pal %in% seqPalettes) {

        # a sequential palette has no natural midpoint, so both types are
        # served by the same light -> dark ramp
        anchors <- RColorBrewer::brewer.pal(9, pal)

    } else if (pal %in% divPalettes) {

        # brewer lists the diverging palettes warm end first; reverse them so
        # that the ramp always runs low -> midpoint -> high
        anchors <- rev(RColorBrewer::brewer.pal(11, pal))
        if (type == 'sequential')
            anchors <- anchors[6:11]

    } else if (pal == 'viridis') {

        anchors <- viridisColors

    } else if (pal %in% names(jupwrNames)) {

        p <- jupwrPalettes[[jupwrNames[[pal]]]]
        # both are listed high-to-low in the lecture notes, so reverse them to
        # run low -> high like every other ramp here
        if (type == 'diverging')
            anchors <- rev(p$rozbiezna)
        else
            anchors <- rev(p$ciepla)

    } else {

        # qualitative palette -- build the ramp from its leading colours
        cols <- colorPalette(2, pal, 'color')
        if (type == 'diverging')
            anchors <- c(cols[2], '#E6E6E6', cols[1])
        else
            anchors <- c(lighten(cols[1], .85), cols[1])
    }

    grDevices::colorRampPalette(anchors)(n)
}

ggPalette <- function(pal = 'jmv', scale = 'discrete') {

    if (scale == 'continuous') {
        cols <- gradientPalette(pal, 'sequential')
        return(
            list(ggplot2::scale_fill_gradientn(colours=cols),
                 ggplot2::scale_colour_gradientn(colours=cols))
        )
    }

    fill <- function(n) colorPalette(n, pal=pal, 'fill')
    color <- function(n) colorPalette(n, pal=pal, 'color')

    return(
        list(ggplot2::discrete_scale("fill", "jmv", fill),
             ggplot2::discrete_scale("colour", "jmv", color))
    )
}

baseTheme <- function(base_size = 16) {
    ggplot2::`%+replace%`(
        ggplot2::theme_gray(base_size = base_size),
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill='transparent', color=NA),
            panel.background = ggplot2::element_rect(fill='#E8E8E8', color=NA),
            plot.margin = ggplot2::margin(15, 15, 15, 15),
            axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 0, 0, 0), colour='#333333', lineheight=0.85),
            axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0, 5, 0, 0), colour='#333333'),
            axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10, 0, 0, 0), colour='#333333'),
            axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0, 10, 0, 0), colour='#333333', angle = 90),
            plot.title = ggplot2::element_text(margin=ggplot2::margin(0, 0, 15, 0), colour='#333333'),
            legend.background = ggplot2::element_rect("transparent", color=NA),
            legend.key = ggplot2::element_rect(fill='#E8E8E8', color=NA),
            legend.title = ggplot2::element_text(colour='#333333'),
            legend.text = ggplot2::element_text(colour='#333333'),
            strip.text.x = ggplot2::element_text(colour='#333333'),
            strip.text.y = ggplot2::element_text(colour='#333333')))
}

#' Wrap long axis / facet / legend labels for plots
#'
#' Breaks labels at spaces, hyphens and underscores so that no line exceeds
#' \code{width} characters; a single word longer than \code{width} is broken
#' hard with a hyphen. If the result still has more than \code{maxLines}
#' lines, it is truncated with an ellipsis. Only intended for plot labels --
#' tables keep the full label.
#'
#' @param x character vector (or factor) of labels
#' @param width maximum characters per line
#' @param maxLines maximum number of lines before truncating
#'
#' @return character vector with embedded newlines
#' @export
wrapLabels <- function(x, width = 12, maxLines = 3) {
    wrapOne <- function(label) {
        if (is.na(label) || nchar(label) <= width)
            return(label)
        # split on spaces / hyphens / underscores, keeping the hyphen
        tokens <- unlist(strsplit(label, '(?<=-)|[ _]', perl = TRUE))
        tokens <- tokens[nzchar(tokens)]
        # hard-break any token longer than width
        pieces <- character(0)
        for (tok in tokens) {
            while (nchar(tok) > width) {
                pieces <- c(pieces, paste0(substr(tok, 1, width - 1), '-'))
                tok <- substr(tok, width, nchar(tok))
            }
            pieces <- c(pieces, tok)
        }
        # greedy line fill
        lines <- character(0)
        current <- ''
        for (piece in pieces) {
            sep <- if (nzchar(current) && !endsWith(current, '-')) ' ' else ''
            candidate <- paste0(current, sep, piece)
            if (nzchar(current) && nchar(candidate) > width) {
                lines <- c(lines, current)
                current <- piece
            } else {
                current <- candidate
            }
        }
        lines <- c(lines, current)
        if (length(lines) > maxLines) {
            lines <- lines[seq_len(maxLines)]
            lines[maxLines] <- paste0(substr(lines[maxLines], 1, width - 1), '…')
        }
        paste(lines, collapse = '\n')
    }
    labels <- as.character(x)
    vapply(labels, wrapOne, character(1), USE.NAMES = FALSE)
}
