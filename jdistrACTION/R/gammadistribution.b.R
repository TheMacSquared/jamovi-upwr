GammaDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "GammaDistributionClass",
  inherit = GammaDistributionBase,
  private = list(

    .run = function() {

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      QuantileFunctionType <- self$options$QuantileFunctionType
      DistributionFunctionType <- self$options$DistributionFunctionType
      XValue <- self$options$x1
      Quantile <- self$options$p
      XValue2 <- self$options$x2
      DP1 <- self$options$dp1  # shape (alpha)
      DP2 <- self$options$dp2  # rate (lambda)

      UpperTail <- qgamma(0.999, DP1, DP2)
      if (DistributionFunction == "TRUE" && is.finite(XValue))
        UpperTail <- max(UpperTail, XValue)
      if (DistributionFunction == "TRUE" && DistributionFunctionType == "interval" && is.finite(XValue2))
        UpperTail <- max(UpperTail, XValue2)
      # density is unbounded at 0 for shape < 1, start the grid at a positive epsilon
      LowerTail <- 0
      if (DP1 < 1)
        LowerTail <- UpperTail / 10000
      N <- 1000
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      InputLabel1 <- paste("Kształt (α) = ", DP1, ", Intensywność (λ) = ", DP2,
                           ", Skala (θ = 1/λ) = ", round(1/DP2, 4), sep = "")
      DistributionFunctionTypeLabel <- ""
      QuantileFunctionTypeLabel <- ""
      if (DistributionFunctionType == "lower")
        DistributionFunctionTypeLabel <- "Tryb: P(X ≤ x1)"
      if (DistributionFunctionType == "interval")
        DistributionFunctionTypeLabel <- paste("Tryb: x2 = ", XValue2, sep = "")
      if (DistributionFunctionType == "higher")
        DistributionFunctionTypeLabel <- "Tryb: P(X ≥ x1)"
      if (QuantileFunctionType == "cumulative")
        QuantileFunctionTypeLabel <- "tryb kumulatywny"
      if (QuantileFunctionType == "central")
        QuantileFunctionTypeLabel <- "tryb centralny"

      Inputs <- self$results$Inputs
      Inputs$setRow(rowNo = 1, values = list(
        ParametersColumn = InputLabel1,
        DistributionFunctionColumn = paste("x1 = ", XValue, sep = ""),
        QuantileFunctionColumn = paste("p = ", Quantile, sep = "")))

      x <- seq(LowerTail, UpperTail, length = N)
      Density <- dgamma(x, DP1, DP2)
      Density[!is.finite(Density)] <- NA

      if(DistributionFunction == "TRUE"){
        if (DistributionFunctionType == "lower")
          DistributionResult <- pgamma(XValue, DP1, DP2)
        # upper tail computed directly for numerical stability
        if (DistributionFunctionType == "higher")
          DistributionResult <- pgamma(XValue, DP1, DP2, lower.tail = FALSE)
        if (DistributionFunctionType == "interval")
          DistributionResult <- pgamma(XValue2, DP1, DP2) - pgamma(XValue, DP1, DP2)}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qgamma(Quantile, DP1, DP2)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qgamma(LowerQuantile, DP1, DP2)
          QuantileResult2 <- qgamma(HigherQuantile, DP1, DP2)}}

      OutputLabel11 <- ""
      OutputLabel12 <- ""
      OutputLabel22 <- ""

      if(DistributionFunction == "TRUE"){
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

      ShowMean <- self$options$showMean
      ShowVariance <- self$options$showVariance
      MomentsTable <- self$results$MomentsTable
      if (ShowMean || ShowVariance) {
        MomentsTable$setVisible(visible = TRUE)
        if (ShowMean) {
          MomentsTable$addRow(rowKey = "mean", values = list(
            MomentColumn = "E[X]",
            FormulaColumn = "E[X] = α/λ",
            ValueColumn = as.character(round(DP1/DP2, 4))
          ))
        }
        if (ShowVariance) {
          MomentsTable$addRow(rowKey = "var", values = list(
            MomentColumn = "Var[X]",
            FormulaColumn = "Var[X] = α/λ²",
            ValueColumn = as.character(round(DP1/DP2^2, 4))
          ))
        }
      } else {
        MomentsTable$setVisible(visible = FALSE)
      }

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
      QuantileLabel <- "Kwantyl"
      Textsize <- 16
      AxisSegments <- pretty(c(0, UpperTail), n = 8)
      HigherSegment <- NA
      LowerSegment <- NA
      HigherSegmentLength <- NA
      LowerSegmentLength <- NA

      if(QuantileFunction == "TRUE"){
        if(QuantileFunctionType == "cumulative"){
          HigherSegment <- qgamma(Quantile, DP1, DP2)
          HigherSegmentLength <- dgamma(HigherSegment, DP1, DP2)
          if(!is.finite(HigherSegmentLength) || (HigherSegmentLength * 18) < (max(Datas$Prob, na.rm = TRUE)))
            HigherSegmentLength <- ((max(Datas$Prob, na.rm = TRUE)) / 18)
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qgamma(LowerQuantile, DP1, DP2)
          LowerSegmentLength <- dgamma(LowerSegment, DP1, DP2)
          HigherSegment <- qgamma(HigherQuantile, DP1, DP2)
          HigherSegmentLength <- dgamma(HigherSegment, DP1, DP2)
          if(!is.finite(LowerSegmentLength) || (LowerSegmentLength * 18) < (max(Datas$Prob, na.rm = TRUE))){
            LowerSegmentLength <- ((max(Datas$Prob, na.rm = TRUE)) / 18)
            HigherSegmentLength <- ((max(Datas$Prob, na.rm = TRUE)) / 18)}}}

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
        Inputs$setError("x2 musi być większe od x1.")
        Outputs$setVisible(visible = FALSE)}
      if(XValue < 0 & DistributionFunction == "TRUE"){
        Inputs$setError("x1 musi być ≥ 0 dla rozkładu gamma.")
        Outputs$setVisible(visible = FALSE)}},

    .plot = function(image, ggtheme, theme, ...) {
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
      Textsize <- as.numeric(Dataset[8, 4])
      AxisSegments <- as.numeric(Dataset[, 5])
      AxisSegments <- na.omit(AxisSegments)

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction

      Pointsize <- 0.000001
      TypeOfLine <- "dashed"
      Linewidth <- 1
      # Colours from the palette selected in the app: pal[1] highlights the
      # probability area / bars, pal[2] draws the quantile lines
      pal <- jmvcore::colorPalette(2, theme$palette, 'fill')
      Color <- c(pal[1], pal[2], '#9f9f9f')

      Plot <- ggplot(PlotData, mapping = aes(x = PlotData$X, y = PlotData$Prob)) +
        # Theme (and base scales) selected in the app; manual scales below override
        ggtheme +
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
        theme(legend.text = element_text(size = Textsize)) +
        theme(legend.title = element_blank())

      print(Plot)
      TRUE}))
