#' @importFrom jmvcore .
hydroflowClass <- R6::R6Class("hydroflowClass",
    inherit = hydroflowBase,
    private = list(
        .run = function() {
            if (is.null(self$options$date) || !nzchar(self$options$date) ||
                is.null(self$options$value) || !nzchar(self$options$value)) return()
            result <- hydroRun("hydroflow", self$data, self$options)
            hydroFill(self$results$summary, result$summary)
            hydroFill(self$results$details, result$details)
            note <- hydroNotes("hydroflow", self$options)
            self$results$summary$setNote("method", note)
            md <- jmvcore::metodyNew()
            md$add("Metoda i ograniczenia", "%s", note)
            md$render(self$results$metody)
            if (self$options$showPlot) {
                self$results$plot$setState(result$state)
                self$results$plot2$setState(result$state)
            }
        },
        .plot = function(image, ggtheme, theme, ...) hydroPlot(image$state, FALSE, ggtheme),
        .plot2 = function(image, ggtheme, theme, ...) hydroPlot(image$state, TRUE, ggtheme)
    )
)
