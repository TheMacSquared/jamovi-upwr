#' @importFrom jmvcore .
rcbdClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "rcbdClass",
    inherit = rcbdBase,
    private = list(
        .init = function() initDesignAnalysis(self, "rcbd"),
        .run = function() runDesignAnalysis(self, "rcbd"),
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .residPlot = function(image, ggtheme, theme, ...) residPlot(image, ggtheme, theme)
    )
)
