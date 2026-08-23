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

seqPalettes <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')

otherPalettes <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1',
                 'Pastel2', 'Set1', 'Set2', 'Set3')

divPalettes <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                 'RdYlGn', 'Spectral')

# UPWr house colours; the burgundy and the gold are taken from the jUPWR app
# icon, the remaining hues are chosen for separability
upwrColors <- c('#832034', '#E6AC41', '#3E6DA9', '#4C8C5A', '#5B4A82', '#6E6E6E')

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

    } else if (pal == 'upwr') {

        cols <- upwrColors

        if (type == 'fill')
            cols <- lighten(cols, .4)
        else
            cols <- lighten(cols, .1)

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
