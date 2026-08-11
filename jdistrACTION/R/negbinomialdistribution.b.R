NegBinomialDistributionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "NegBinomialDistributionClass",
  inherit = NegBinomialDistributionBase,
  private = list(

    .run = function() {

      DistributionFunction <- self$options$DistributionFunction
      QuantileFunction <- self$options$QuantileFunction
      QuantileFunctionType <- self$options$QuantileFunctionType
      DistributionFunctionType <- self$options$DistributionFunctionType
      XValue <- self$options$x1
      Quantile <- self$options$p
      XValue2 <- self$options$x2
      DP1 <- self$options$dp1  # number of successes (r)
      DP2 <- self$options$dp2  # probability of success (p)
      Convention <- self$options$RandomVariable

      Inputs <- self$results$Inputs
      Outputs <- self$results$Outputs

      if (DP1 != round(DP1)) {
        Inputs$setError("Liczba sukcesów r musi być liczbą całkowitą.")
        Outputs$setVisible(visible = FALSE)
        return()
      }

      # X counts trials up to the r-th success (support r, r+1, ...) or, in the
      # R convention, failures before the r-th success (support 0, 1, ...);
      # both map onto dnbinom/pnbinom/qnbinom via the offset below
      Offset <- 0
      if (Convention == "trials") Offset <- DP1
      SupportStart <- Offset

      dNB <- function(k)
        ifelse(k == floor(k) & k >= SupportStart,
               dnbinom(k - Offset, size = DP1, prob = DP2), 0)
      pNB <- function(k)  # P(X <= k) for any real k
        pnbinom(floor(k) - Offset, size = DP1, prob = DP2)
      sNB <- function(k)  # P(X > k) for integer k, stable in the right tail
        pnbinom(k - Offset, size = DP1, prob = DP2, lower.tail = FALSE)
      qNB <- function(q)
        qnbinom(q, size = DP1, prob = DP2) + Offset

      UpperTail <- qNB(0.999)
      if ((UpperTail - SupportStart) < 5) UpperTail <- SupportStart + 5
      Columnames <- c("X", "Prob")

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "central") {
          LowerQuantile <- ((1 - Quantile) / 2)
          HigherQuantile <- LowerQuantile + Quantile}}

      ConventionLabel <- "X = numer próby r-tego sukcesu"
      if (Convention == "failures")
        ConventionLabel <- "X = liczba porażek przed r-tym sukcesem"
      InputLabel1 <- paste("r = ", DP1, ", p = ", DP2, "; ", ConventionLabel, sep = "")

      Inputs$setRow(rowNo = 1, values = list(
        ParametersColumn = InputLabel1,
        DistributionFunctionColumn = paste("x1 = ", XValue, sep = ""),
        QuantileFunctionColumn = paste("p = ", Quantile, sep = "")))

      # Discrete: integer sequence over the support
      x <- seq(SupportStart, UpperTail, by = 1)
      Density <- dNB(x)

      if(DistributionFunction == "TRUE"){
        if (DistributionFunctionType == "is")
          DistributionResult <- dNB(XValue)
        if (DistributionFunctionType == "lower")
          DistributionResult <- pNB(XValue)
        if (DistributionFunctionType == "higher")
          DistributionResult <- sNB(ceiling(XValue) - 1)
        if (DistributionFunctionType == "interval")
          DistributionResult <- pNB(XValue2) - pNB(ceiling(XValue) - 1)}

      if(QuantileFunction == "TRUE"){
        if (QuantileFunctionType == "cumulative")
          QuantileResult <- qNB(Quantile)
        if (QuantileFunctionType == "central"){
          QuantileResult <- qNB(LowerQuantile)
          QuantileResult2 <- qNB(HigherQuantile)}}

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
            MeanFormula <- "E[X] = r/p"
            MeanValue <- DP1 / DP2
          } else {
            MeanFormula <- "E[X] = r(1 − p)/p"
            MeanValue <- DP1 * (1 - DP2) / DP2
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
            FormulaColumn = "Var[X] = r(1 − p)/p²",
            ValueColumn = as.character(round(DP1 * (1 - DP2) / DP2^2, 4))
          ))
        }
      } else {
        MomentsTable$setVisible(visible = FALSE)
      }

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
      QuantileLabel <- "Kwantyl"
      Textsize <- 16
      HigherSegment <- NA
      LowerSegment <- NA
      HigherSegmentLength <- NA
      LowerSegmentLength <- NA

      if(QuantileFunction == "TRUE"){
        if(QuantileFunctionType == "cumulative"){
          HigherSegment <- qNB(Quantile)
          HigherSegmentLength <- max(Datas$Prob) * 0.8
          LowerSegment <- HigherSegment
          LowerSegmentLength <- HigherSegmentLength}
        if(QuantileFunctionType == "central"){
          LowerSegment <- qNB(LowerQuantile)
          LowerSegmentLength <- max(Datas$Prob) * 0.8
          HigherSegment <- qNB(HigherQuantile)
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
        theme(legend.text = element_text(size = as.numeric(state$Textsize)))

      print(Plot)
      TRUE}))
