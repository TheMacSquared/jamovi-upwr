#' @importFrom jmvcore .
crdClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "crdClass",
    inherit = crdBase,
    private = list(
        .init = function() initDesignAnalysis(self, "crd"),
        .run = function() runDesignAnalysis(self, "crd"),
        .meansPlot = function(image, ggtheme, theme, ...) meansPlot(image, ggtheme, theme),
        .interactionPlot = function(image, ggtheme, theme, ...) interactionPlot(image, ggtheme, theme),
        .residPlot = function(image, ggtheme, theme, ...) residPlot(image, ggtheme, theme)
    )
)
