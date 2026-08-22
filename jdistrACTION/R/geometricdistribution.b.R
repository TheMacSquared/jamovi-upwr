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
      Convention <- self$options$RandomVariable

      # X counts failures before the first success (support 0, 1, ... — the R
      # convention, historical default here) or trials up to the first success
      # (support 1, 2, ...); both map onto dgeom/pgeom/qgeom via the offset
      Offset <- 0
      if (Convention == "trials") Offset <- 1
      SupportStart <- Offset

      dGeom <- function(k)
        ifelse(k == floor(k) & k >= SupportStart, dgeom(k - Offset, DP1), 0)
      pGeom <- function(k)  # P(X <= k) for any real k
        pgeom(floor(k) - Offset, DP1)
      qGeom <- function(q)
        qgeom(q, DP1) + Offset

      LowerTail <- SupportStart
      UpperTail <- qGeom(0.999)
      if ((UpperTail - SupportStart) < 5) UpperTail <- SupportStart + 5
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      ConventionLabel <- "X = liczba porażek przed pierwszym sukcesem"
      if (Convention == "trials")
        ConventionLabel <- "X = numer próby pierwszego sukcesu"
      InputLabel1 <- paste("Prawdopodb. = ", DP1, "; ", ConventionLabel, sep = "")
      DistributionFunctionTypeLabel <- ""
      QuantileFunctionTypeLabel <- ""
      if (DistributionFunctionType == "lower")
        DistributionFunctionTypeLabel <- "Tryb: P(X ≤ x1)"
      if (DistributionFunctionType == "higher")
        DistributionFunctionTypeLabel <- "Tryb: P(X ≥ x1)"
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

      # Discrete: integer sequence
      x <- seq(LowerTail, UpperTail, by = 1)
      Density <- dGeom(x)

      if(DistributionFunction == "TRUE"){
        if (DistributionFunctionType == "is"){
          DistributionResult <- dGeom(XValue)
        } else {
          DistributionResult1 <- pGeom(XValue)
          if (DistributionFunctionType == "interval" || DistributionFunctionType == "higher")
            DistributionResult1 <- pGeom(ceiling(XValue) - 1)
          DistributionResult <- DistributionResult1
          if (DistributionFunctionType == "interval"){
            DistributionResult2 <- pGeom(XValue2)
            DistributionResult <- DistributionResult2 - DistributionResult1}
          if (DistributionFunctionType == "higher")
            DistributionResult <- 1 - DistributionResult}}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qGeom(Quantile)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qGeom(LowerQuantile)
          QuantileResult2 <- qGeom(HigherQuantile)}}

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

      ShowMean <- self$options$showMean
      ShowVariance <- self$options$showVariance
      MomentsTable <- self$results$MomentsTable
      if (ShowMean || ShowVariance) {
        MomentsTable$setVisible(visible = TRUE)
        if (ShowMean) {
          if (Convention == "trials") {
            MeanFormula <- "E[X] = 1 / p"
            MeanValue <- 1 / DP1
          } else {
            MeanFormula <- "E[X] = (1 − p) / p"
            MeanValue <- (1 - DP1) / DP1
          }
          MomentsTable$addRow(rowKey = "mean", values = list(
            MomentColumn = "E[X]",
            FormulaColumn = MeanFormula,
            ValueColumn = as.character(round(MeanValue, 4))
          ))
        }
        if (ShowVariance) {
          MomentsTable$addRow(rowKey = "var", values = list(
            MomentColumn = "Var[X]",
            FormulaColumn = "Var[X] = (1 − p) / p²",
            ValueColumn = as.character(round((1 - DP1) / DP1^2, 4))
          ))
        }
      } else {
        MomentsTable$setVisible(visible = FALSE)
      }

      # Plot data for discrete distribution
      Datas <- data.frame(x, Density)
      colnames(Datas) <- Columnames

      # Color vector for bars
      BarColors <- rep("base", length(x))
      if (DistributionFunction == "TRUE") {
        if (DistributionFunctionType == "is")
          BarColors[x == XValue] <- "highlight"
        if (DistributionFunctionType == "lower")
          BarColors[x <= XValue] <- "highlight"
        if (DistributionFunctionType == "higher")
          BarColors[x >= XValue] <- "highlight"
        if (DistributionFunctionType == "interval")
          BarColors[x >= XValue & x <= XValue2] <- "highlight"
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
          HigherSegment <- qGeom(Quantile)
          HigherSegmentLength <- max(Datas$Prob) * 0.8
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qGeom(LowerQuantile)
          LowerSegmentLength <- max(Datas$Prob) * 0.8
          HigherSegment <- qGeom(HigherQuantile)
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
        Inputs$setError("x2 musi być większe od x1.")
        Outputs$setVisible(visible = FALSE)}},

    .plot = function(image, ggtheme, theme, ...) {
      state <- image$state
      Datas <- state$Datas
      BarColors <- state$BarColors

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      # Colours from the palette selected in the app: pal[1] highlights the
      # probability area / bars, pal[2] draws the quantile lines
      pal <- jmvcore::colorPalette(2, theme$palette, 'fill')
      Color <- c(pal[1], pal[2], '#9f9f9f')
      # Highlighted bars take the palette colour, the rest stay neutral grey
      BarFill <- ifelse(BarColors == "highlight", pal[1], Color[3])
      Linewidth <- 1
      TypeOfLine <- "dashed"

      Plot <- ggplot(Datas, aes(x = X, y = Prob)) +
        geom_col(fill = BarFill, color = theme$color[1], width = 0.7) +
        ggplot2::xlab("") + ggplot2::ylab("") +
        scale_x_continuous(breaks = Datas$X) +
        ggtheme +
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
        theme(legend.text = element_text(size = as.numeric(state$Textsize)))

      print(Plot)
      TRUE}))
