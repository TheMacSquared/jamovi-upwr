ExponentialDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "ExponentialDistributionClass",
  inherit = ExponentialDistributionBase,
  private = list(

    .run = function() {

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      QuantileFunctionType <- self$options$QuantileFunctionType
      DistributionFunctionType <- self$options$DistributionFunctionType
      XValue <- self$options$x1
      Quantile <- self$options$p
      XValue2 <- self$options$x2
      DP1 <- self$options$dp1  # rate

      LowerTail <- 0
      UpperTail <- qexp(0.999, DP1)
      N <- 1000
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      InputLabel1 <- paste("Rate (λ) = ", DP1, sep = "")
      DistributionFunctionTypeLabel <- ""
      QuantileFunctionTypeLabel <- ""
      if (DistributionFunctionType == "lower")
        DistributionFunctionTypeLabel <- "Mode: P(X ≤ x1)"
      if (DistributionFunctionType == "interval")
        DistributionFunctionTypeLabel <- paste("Mode: x2 = ", XValue2, sep = "")
      if (DistributionFunctionType == "higher")
        DistributionFunctionTypeLabel <- "Mode: P(X ≥ x1)"
      if (QuantileFunctionType == "cumulative")
        QuantileFunctionTypeLabel <- "cumulative mode"
      if (QuantileFunctionType == "central")
        QuantileFunctionTypeLabel <- "central mode"

      Inputs <- self$results$Inputs
      Inputs$setRow(rowNo = 1, values = list(
        ParametersColumn = InputLabel1,
        DistributionFunctionColumn = paste("x1 = ", XValue, sep = ""),
        QuantileFunctionColumn = paste("p = ", Quantile, sep = "")))

      x <- seq(LowerTail, UpperTail, length = N)
      Density <- dexp(x, DP1)

      if(DistributionFunction == "TRUE"){
        DistributionResult1 <- pexp(XValue, DP1)
        DistributionResult <- DistributionResult1
        if (DistributionFunctionType == "interval"){
          DistributionResult2 <- pexp(XValue2, DP1)
          DistributionResult <- DistributionResult2 - DistributionResult1}}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qexp(Quantile, DP1)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qexp(LowerQuantile, DP1)
          QuantileResult2 <- qexp(HigherQuantile, DP1)}}

      OutputLabel11 <- ""
      OutputLabel12 <- ""
      OutputLabel22 <- ""

      if(DistributionFunction == "TRUE"){
        if(DistributionFunctionType == "higher")
          DistributionResult <- 1 - DistributionResult
        DistributionResult <- round(DistributionResult, digits = 3)
        OutputLabel11 <- paste("P = ", DistributionResult, sep = "")}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative") {
          QuantileResult <- round(QuantileResult, digits = 3)
          OutputLabel12 <- paste("x1 = ", QuantileResult, sep = "")}
        if (QuantileFunctionType == "central") {
          QuantileResult <- round(QuantileResult, digits = 3)
          OutputLabel12 <- paste("x1 = ", QuantileResult, sep = "")
          QuantileResult2 <- round(QuantileResult2, digits = 3)
          OutputLabel22 <- paste("x2 = ", QuantileResult2, sep = "")}}

      Outputs <- self$results$Outputs
      Outputs$setRow(rowNo = 1, values = list(
        DistributionResultColumn = OutputLabel11,
        QuantileResultColumn = OutputLabel12))
      if((QuantileFunction == "TRUE") & (QuantileFunctionType == "central"))
        Outputs$addRow(rowKey = 2, values = list(
          DistributionResultColumn = "",
          QuantileResultColumn = OutputLabel22))

      Datas <- data.frame(x, Density)
      colnames(Datas) <- Columnames
      MainCurveData <- as.data.frame(Datas)

      if (DistributionFunction == "TRUE") {
        if (DistributionFunctionType == "lower") {
          MainCurveData$Prob[MainCurveData$X > XValue] <- NA
          MainCurveData$X[MainCurveData$X > XValue] <- NA}
        if (DistributionFunctionType == "higher") {
          MainCurveData$Prob[MainCurveData$X < XValue] <- NA
          MainCurveData$X[MainCurveData$X < XValue] <- NA}
        if (DistributionFunctionType == "interval") {
          MainCurveData$Prob[MainCurveData$X < XValue] <- NA
          MainCurveData$X[MainCurveData$X < XValue] <- NA
          MainCurveData$Prob[MainCurveData$X > XValue2] <- NA
          MainCurveData$X[MainCurveData$X > XValue2] <- NA}}

      QuantileAlphaLow <- 1
      QuantileAlphaHigh <- 1
      QuantileLabel <- "Quantile"
      Textsize <- 16
      AxisSegments <- pretty(c(LowerTail, UpperTail), n = 8)
      HigherSegment <- NA
      LowerSegment <- NA
      HigherSegmentLength <- NA
      LowerSegmentLength <- NA

      if(QuantileFunction == "TRUE"){
        if(QuantileFunctionType == "cumulative"){
          HigherSegment <- qexp(Quantile, DP1)
          HigherSegmentLength <- dexp(HigherSegment, DP1)
          if((HigherSegmentLength * 18) < (max(Datas$Prob)))
            HigherSegmentLength <- ((max(Datas$Prob)) / 18)
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qexp(LowerQuantile, DP1)
          LowerSegmentLength <- dexp(LowerSegment, DP1)
          HigherSegment <- qexp(HigherQuantile, DP1)
          HigherSegmentLength <- dexp(HigherSegment, DP1)
          if((LowerSegmentLength * 18) < (max(Datas$Prob))){
            LowerSegmentLength <- ((max(Datas$Prob)) / 18)
            HigherSegmentLength <- ((max(Datas$Prob)) / 18)}}}

      Dataset <- cbind(Datas, MainCurveData[, 2], MainCurveData)
      Dataset[, 4:5] <- NA
      Dataset[1, 4] <- HigherSegment
      Dataset[2, 4] <- LowerSegment
      Dataset[3, 4] <- HigherSegmentLength
      Dataset[4, 4] <- LowerSegmentLength
      Dataset[5, 4] <- QuantileAlphaLow
      Dataset[6, 4] <- QuantileAlphaHigh
      Dataset[7, 4] <- QuantileLabel
      Dataset[8, 4] <- Textsize
      Dataset[1:(length(AxisSegments)), 5] <- AxisSegments
      image <- self$results$plot
      image$setState(Dataset)

      if(((DistributionFunction == "TRUE") & (DistributionFunctionType == "interval")) & (XValue >= XValue2)){
        Inputs$setError("x2 must be greater than x1.")
        Outputs$setVisible(visible = FALSE)}
      if(XValue < 0 & DistributionFunction == "TRUE"){
        Inputs$setError("x1 must be >= 0 for exponential distribution.")
        Outputs$setVisible(visible = FALSE)}},

    .plot = function(image, ...) {
      Dataset <- image$state
      PlotData <- Dataset[, 1:3]
      colnames(PlotData) <- c("X", "Prob", "CurveProb")
      HigherSegment <- as.numeric(Dataset[1, 4])
      LowerSegment <- as.numeric(Dataset[2, 4])
      HigherSegmentLength <- as.numeric(Dataset[3, 4])
      LowerSegmentLength <- as.numeric(Dataset[4, 4])
      QuantileAlphaLow <- as.numeric(Dataset[5, 4])
      QuantileAlphaHigh <- as.numeric(Dataset[6, 4])
      QuantileLabel <- Dataset[7, 4]
      Textsize <- Dataset[8, 4]
      AxisSegments <- as.numeric(Dataset[, 5])
      AxisSegments <- na.omit(AxisSegments)

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction

      Pointsize <- 0.000001
      TypeOfLine <- "dashed"
      Linewidth <- 1
      Color <- c("#e0bc6b", "#7b9ee6", "#9f9f9f")

      Plot <- ggplot(PlotData, mapping = aes(x = PlotData$X, y = PlotData$Prob)) +
        ggplot2::xlab("") + ggplot2::ylab("") +
        scale_x_continuous(breaks = AxisSegments)

      if (DistributionFunction == "TRUE")
        Plot <- Plot +
          geom_area(PlotData, mapping = aes(x = PlotData$X, y = PlotData$CurveProb, fill = " P (Area)")) +
          scale_fill_manual(values = Color)

      if (QuantileFunction == "TRUE")
        Plot <- Plot +
          geom_segment(aes(x = LowerSegment, y = 0, xend = LowerSegment, yend = LowerSegmentLength, linetype = QuantileLabel), colour = Color[2], size = Linewidth, alpha = QuantileAlphaLow) +
          geom_segment(aes(x = HigherSegment, y = 0, xend = HigherSegment, yend = HigherSegmentLength, linetype = QuantileLabel), colour = Color[2], size = Linewidth, alpha = QuantileAlphaHigh) +
          scale_linetype_manual(values = TypeOfLine)

      Plot <- Plot +
        geom_point(size = Pointsize, color = Color[1]) +
        geom_line() +
        theme_classic() +
        theme(legend.text = element_text(size = Textsize)) +
        theme(legend.title = element_blank())

      print(Plot)
      TRUE}))
