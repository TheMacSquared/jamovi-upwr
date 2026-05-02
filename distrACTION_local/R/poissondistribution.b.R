PoissonDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "PoissonDistributionClass",
  inherit = PoissonDistributionBase,
  private = list(

    .run = function() {

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      QuantileFunctionType <- self$options$QuantileFunctionType
      DistributionFunctionType <- self$options$DistributionFunctionType
      XValue <- self$options$x1
      Quantile <- self$options$p
      XValue2 <- self$options$x2
      DP1 <- self$options$dp1  # lambda

      LowerTail <- 0
      UpperTail <- qpois(0.999, DP1)
      if (UpperTail < 5) UpperTail <- 5
      N <- UpperTail + 1
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      InputLabel1 <- paste("\u03BB = ", DP1, sep = "")
      DistributionFunctionTypeLabel <- ""
      QuantileFunctionTypeLabel <- ""
      if (DistributionFunctionType == "lower")
        DistributionFunctionTypeLabel <- "Tryb: P(X \u2264 x1)"
      if (DistributionFunctionType == "higher")
        DistributionFunctionTypeLabel <- "Tryb: P(X \u2265 x1)"
      if (DistributionFunctionType == "interval")
        DistributionFunctionTypeLabel <- paste("Tryb: x2 = ", XValue2, sep = "")
      if (DistributionFunctionType == "is")
        DistributionFunctionTypeLabel <- "Tryb: P(X = x1)"
      if (QuantileFunctionType == "cumulative")
        QuantileFunctionTypeLabel <- "tryb kumulatywny"
      if (QuantileFunctionType == "central")
        QuantileFunctionTypeLabel <- "tryb centralny"

      Inputs <- self$results$Inputs
      Inputs$setRow(rowNo = 1, values = list(
        ParametersColumn = InputLabel1,
        DistributionFunctionColumn = paste("x1 = ", XValue, sep = ""),
        QuantileFunctionColumn = paste("p = ", Quantile, sep = "")))

      x <- seq(LowerTail, UpperTail, by = 1)
      Density <- dpois(x, DP1)

      if(DistributionFunction == "TRUE"){
        if (DistributionFunctionType == "is"){
          DistributionResult <- dpois(XValue, DP1)
        } else {
          DistributionResult1 <- ppois(floor(XValue), DP1)
          if (DistributionFunctionType == "interval" || DistributionFunctionType == "higher")
            DistributionResult1 <- ppois(ceiling(XValue) - 1, DP1)
          DistributionResult <- DistributionResult1
          if (DistributionFunctionType == "interval"){
            DistributionResult2 <- ppois(floor(XValue2), DP1)
            DistributionResult <- DistributionResult2 - DistributionResult1}
          if (DistributionFunctionType == "higher")
            DistributionResult <- 1 - DistributionResult}}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qpois(Quantile, DP1)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qpois(LowerQuantile, DP1)
          QuantileResult2 <- qpois(HigherQuantile, DP1)}}

      OutputLabel11 <- ""
      OutputLabel12 <- ""
      OutputLabel22 <- ""

      if(DistributionFunction == "TRUE"){
        DistributionResult <- round(DistributionResult, digits = 3)
        OutputLabel11 <- paste("P = ", DistributionResult, sep = "")}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          OutputLabel12 <- paste("x1 = ", QuantileResult, sep = "")
        if (QuantileFunctionType == "central") {
          OutputLabel12 <- paste("x1 = ", QuantileResult, sep = "")
          OutputLabel22 <- paste("x2 = ", QuantileResult2, sep = "")}}

      Outputs <- self$results$Outputs
      Outputs$setRow(rowNo = 1, values = list(
        DistributionResultColumn = OutputLabel11,
        QuantileResultColumn = OutputLabel12))
      if((QuantileFunction == "TRUE") & (QuantileFunctionType == "central"))
        Outputs$addRow(rowKey = 2, values = list(
          DistributionResultColumn = "",
          QuantileResultColumn = OutputLabel22))

      ShowMean <- self$options$showMean
      ShowVariance <- self$options$showVariance
      MomentsTable <- self$results$MomentsTable
      if (ShowMean || ShowVariance) {
        MomentsTable$setVisible(visible = TRUE)
        if (ShowMean) {
          MomentsTable$addRow(rowKey = "mean", values = list(
            MomentColumn = "E[X]",
            FormulaColumn = "E[X] = λ",
            ValueColumn = as.character(round(DP1, 4))
          ))
        }
        if (ShowVariance) {
          MomentsTable$addRow(rowKey = "var", values = list(
            MomentColumn = "Var[X]",
            FormulaColumn = "Var[X] = λ",
            ValueColumn = as.character(round(DP1, 4))
          ))
        }
      } else {
        MomentsTable$setVisible(visible = FALSE)
      }

      Datas <- data.frame(x, Density)
      colnames(Datas) <- Columnames

      BarColors <- rep("grey", length(x))
      if (DistributionFunction == "TRUE") {
        if (DistributionFunctionType == "is")
          BarColors[x == XValue] <- "#e0bc6b"
        if (DistributionFunctionType == "lower")
          BarColors[x <= XValue] <- "#e0bc6b"
        if (DistributionFunctionType == "higher")
          BarColors[x >= XValue] <- "#e0bc6b"
        if (DistributionFunctionType == "interval")
          BarColors[x >= XValue & x <= XValue2] <- "#e0bc6b"
      }

      QuantileAlphaLow <- 1
      QuantileAlphaHigh <- 1
      QuantileLabel <- "Kwantyl"
      Textsize <- 16
      HigherSegment <- NA
      LowerSegment <- NA
      HigherSegmentLength <- NA
      LowerSegmentLength <- NA

      if(QuantileFunction == "TRUE"){
        if(QuantileFunctionType == "cumulative"){
          HigherSegment <- qpois(Quantile, DP1)
          HigherSegmentLength <- max(Datas$Prob) * 0.8
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qpois(LowerQuantile, DP1)
          LowerSegmentLength <- max(Datas$Prob) * 0.8
          HigherSegment <- qpois(HigherQuantile, DP1)
          HigherSegmentLength <- max(Datas$Prob) * 0.8}}

      PlotDataset <- list(
        Datas = Datas,
        BarColors = BarColors,
        HigherSegment = HigherSegment,
        LowerSegment = LowerSegment,
        HigherSegmentLength = HigherSegmentLength,
        LowerSegmentLength = LowerSegmentLength,
        QuantileAlphaLow = QuantileAlphaLow,
        QuantileAlphaHigh = QuantileAlphaHigh,
        QuantileLabel = QuantileLabel,
        Textsize = Textsize)

      image <- self$results$plot
      image$setState(PlotDataset)

      if(((DistributionFunction == "TRUE") & (DistributionFunctionType == "interval")) & (XValue >= XValue2)){
        Inputs$setError("x2 musi być większe od x1.")
        Outputs$setVisible(visible = FALSE)}},

    .plot = function(image, ...) {
      state <- image$state
      Datas <- state$Datas
      BarColors <- state$BarColors

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      Color <- c("#e0bc6b", "#7b9ee6", "#9f9f9f")
      Linewidth <- 1
      TypeOfLine <- "dashed"

      Plot <- ggplot(Datas, aes(x = X, y = Prob)) +
        geom_col(fill = BarColors, color = "black", width = 0.7) +
        ggplot2::xlab("") + ggplot2::ylab("") +
        scale_x_continuous(breaks = Datas$X) +
        theme_classic() +
        theme(legend.title = element_blank())

      if (QuantileFunction == "TRUE") {
        Plot <- Plot +
          geom_segment(aes(
            x = state$LowerSegment, y = 0,
            xend = state$LowerSegment, yend = state$LowerSegmentLength,
            linetype = state$QuantileLabel),
            colour = Color[2], size = Linewidth,
            alpha = state$QuantileAlphaLow) +
          geom_segment(aes(
            x = state$HigherSegment, y = 0,
            xend = state$HigherSegment, yend = state$HigherSegmentLength,
            linetype = state$QuantileLabel),
            colour = Color[2], size = Linewidth,
            alpha = state$QuantileAlphaHigh) +
          scale_linetype_manual(values = TypeOfLine)
      }

      Plot <- Plot +
        theme(legend.text = element_text(size = state$Textsize))

      print(Plot)
      TRUE}))
