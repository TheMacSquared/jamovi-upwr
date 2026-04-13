GeometricDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "GeometricDistributionClass",
  inherit = GeometricDistributionBase,
  private = list(

    .run = function() {

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      QuantileFunctionType <- self$options$QuantileFunctionType
      DistributionFunctionType <- self$options$DistributionFunctionType
      XValue <- self$options$x1
      Quantile <- self$options$p
      XValue2 <- self$options$x2
      DP1 <- self$options$dp1  # probability of success

      LowerTail <- 0
      UpperTail <- qgeom(0.999, DP1)
      if (UpperTail < 5) UpperTail <- 5
      N <- UpperTail + 1
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      InputLabel1 <- paste("Probability = ", DP1, sep = "")
      DistributionFunctionTypeLabel <- ""
      QuantileFunctionTypeLabel <- ""
      if (DistributionFunctionType == "lower")
        DistributionFunctionTypeLabel <- "Mode: P(X ≤ x1)"
      if (DistributionFunctionType == "higher")
        DistributionFunctionTypeLabel <- "Mode: P(X ≥ x1)"
      if (DistributionFunctionType == "interval")
        DistributionFunctionTypeLabel <- paste("Mode: x2 = ", XValue2, sep = "")
      if (DistributionFunctionType == "is")
        DistributionFunctionTypeLabel <- "Mode: P(X = x1)"
      if (QuantileFunctionType == "cumulative")
        QuantileFunctionTypeLabel <- "cumulative mode"
      if (QuantileFunctionType == "central")
        QuantileFunctionTypeLabel <- "central mode"

      Inputs <- self$results$Inputs
      Inputs$setRow(rowNo = 1, values = list(
        ParametersColumn = InputLabel1,
        DistributionFunctionColumn = paste("x1 = ", XValue, sep = ""),
        QuantileFunctionColumn = paste("p = ", Quantile, sep = "")))

      # Discrete: integer sequence
      x <- seq(LowerTail, UpperTail, by = 1)
      Density <- dgeom(x, DP1)

      if(DistributionFunction == "TRUE"){
        if (DistributionFunctionType == "is"){
          DistributionResult <- dgeom(XValue, DP1)
        } else {
          DistributionResult1 <- pgeom(XValue, DP1)
          DistributionResult <- DistributionResult1
          if (DistributionFunctionType == "interval"){
            DistributionResult2 <- pgeom(XValue2, DP1)
            DistributionResult <- DistributionResult2 - DistributionResult1 + dgeom(XValue, DP1)}
          if (DistributionFunctionType == "higher")
            DistributionResult <- 1 - DistributionResult + dgeom(XValue, DP1)}}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qgeom(Quantile, DP1)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qgeom(LowerQuantile, DP1)
          QuantileResult2 <- qgeom(HigherQuantile, DP1)}}

      OutputLabel11 <- ""
      OutputLabel12 <- ""
      OutputLabel22 <- ""

      if(DistributionFunction == "TRUE"){
        DistributionResult <- round(DistributionResult, digits = 3)
        OutputLabel11 <- paste("P = ", DistributionResult, sep = "")}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative") {
          OutputLabel12 <- paste("x1 = ", QuantileResult, sep = "")}
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

      # Plot data for discrete distribution
      Datas <- data.frame(x, Density)
      colnames(Datas) <- Columnames

      # Color vector for bars
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
      QuantileLabel <- "Quantile"
      Textsize <- 16
      HigherSegment <- NA
      LowerSegment <- NA
      HigherSegmentLength <- NA
      LowerSegmentLength <- NA

      if(QuantileFunction == "TRUE"){
        if(QuantileFunctionType == "cumulative"){
          HigherSegment <- qgeom(Quantile, DP1)
          HigherSegmentLength <- max(Datas$Prob) * 0.8
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qgeom(LowerQuantile, DP1)
          LowerSegmentLength <- max(Datas$Prob) * 0.8
          HigherSegment <- qgeom(HigherQuantile, DP1)
          HigherSegmentLength <- max(Datas$Prob) * 0.8}}

      # Pack data for plot
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
        Inputs$setError("x2 must be greater than x1.")
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
