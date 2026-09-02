#' @importFrom jmvcore .
splitplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "splitplotClass",
    inherit = splitplotBase,
    private = list(
        .init = function() initDesignAnalysis(self, "splitplot"),
        .run = function() runDesignAnalysis(self, "splitplot"),
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .residPlot = function(image, ggtheme, theme, ...) residPlot(image, ggtheme, theme)
    )
)
