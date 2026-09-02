#' @importFrom jmvcore .
latinClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "latinClass",
    inherit = latinBase,
    private = list(
        .init = function() initDesignAnalysis(self, "latin"),
        .run = function() runDesignAnalysis(self, "latin"),
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .residPlot = function(image, ggtheme, theme, ...) residPlot(image, ggtheme, theme)
    )
)
