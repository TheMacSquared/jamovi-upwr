lifetimeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "lifetimeClass",
  inherit = lifetimeBase,
  private = list(

    .par = function() {
      dist <- self$options$distribution
      par <- switch(dist,
        exponential = list(rate = self$options$expRate),
        gamma       = list(shape = self$options$gammaShape,
                           rate = self$options$gammaRate),
        weibull     = list(shape = self$options$weibullShape,
                           scale = self$options$weibullScale))
      list(dist = dist, par = par)
    },

    .run = function() {

      mp <- private$.par()
      dist <- mp$dist
      par <- mp$par
      tUser <- self$options$t

      atTable <- self$results$atTable

      ft <- riskLtDensity(tUser, dist, par)
      Ft <- riskLtCdf(tUser, dist, par)
      Rt <- riskLtReliability(tUser, dist, par)
      ht <- riskLtHazard(tUser, dist, par)

      atTable$setRow(rowNo = 1, values = list(
        tcol = tUser,
        ft = if (is.finite(ft)) ft else NA,
        Ft = Ft,
        Rt = Rt,
        ht = if (is.finite(ht)) ht else NA))
      if (!is.finite(ht) || !is.finite(ft))
        atTable$setNote("hazard0",
          "Dla kształtu < 1 gęstość i intensywność uszkodzeń są nieskończone w t = 0.")

      summaryTable <- self$results$summaryTable

      if (self$options$showMTTF) {
        mttfFormula <- switch(dist,
          exponential = "MTTF = 1/λ",
          gamma       = "MTTF = α/λ",
          weibull     = "MTTF = η·Γ(1 + 1/β)")
        summaryTable$addRow(rowKey = "mttf", values = list(
          quantity = "MTTF",
          formula = mttfFormula,
          value = as.character(round(riskLtMTTF(dist, par), 4))))
      }

      if (self$options$showMedian) {
        medianFormula <- switch(dist,
          exponential = "t₀.₅ = ln(2)/λ",
          gamma       = "t₀.₅: F(t₀.₅) = 0.5",
          weibull     = "t₀.₅ = η·(ln 2)^(1/β)")
        summaryTable$addRow(rowKey = "median", values = list(
          quantity = "Mediana czasu życia",
          formula = medianFormula,
          value = as.character(round(riskLtMedian(dist, par), 4))))
      }

      shape <- switch(dist, exponential = 1, gamma = par$shape, weibull = par$shape)
      shapeSymbol <- switch(dist, exponential = "", gamma = "α", weibull = "β")
      characterLabel <- switch(riskLtHazardCharacter(dist, par),
        constant   = "stała",
        increasing = paste("rosnąca (", shapeSymbol, " > 1)", sep = ""),
        decreasing = paste("malejąca (", shapeSymbol, " < 1)", sep = ""))
      if (dist == "exponential")
        characterLabel <- "stała: h(t) = λ (brak pamięci)"
      summaryTable$addRow(rowKey = "hazardChar", values = list(
        quantity = "Charakter h(t)",
        formula = "h(t) = f(t)/R(t)",
        value = characterLabel))

      # common grid for the three plots; positive epsilon start when the
      # density/hazard is unbounded at 0 (shape < 1)
      tMax <- max(riskLtQuantile(0.995, dist, par), tUser * 1.1)
      tMin <- 0
      if (shape < 1)
        tMin <- tMax / 10000
      grid <- seq(tMin, tMax, length.out = 400)

      RCurve <- riskLtReliability(grid, dist, par)
      hCurve <- riskLtHazard(grid, dist, par)
      fCurve <- riskLtDensity(grid, dist, par)
      hCurve[!is.finite(hCurve) | RCurve < 1e-9] <- NA
      fCurve[!is.finite(fCurve)] <- NA

      self$results$plotR$setState(data.frame(x = grid, y = RCurve, tUser = tUser))
      self$results$plotH$setState(data.frame(x = grid, y = hCurve, tUser = tUser))
      self$results$plotF$setState(data.frame(x = grid, y = fCurve, tUser = tUser))
    },

    .plotCurve = function(image, ylab) {
      state <- image$state
      if (is.null(state))
        return(FALSE)
      Color <- c("#e0bc6b", "#7b9ee6", "#9f9f9f")
      tUser <- state$tUser[1]

      Plot <- ggplot(state, aes(x = x, y = y)) +
        geom_line(colour = "black", linewidth = 0.8) +
        geom_vline(xintercept = tUser, colour = Color[2], linetype = "dashed") +
        ggplot2::xlab("t") + ggplot2::ylab(ylab) +
        theme_classic() +
        theme(text = element_text(size = 14))

      print(Plot)
      TRUE
    },

    .plotR = function(image, ...)
      private$.plotCurve(image, "R(t)"),

    .plotH = function(image, ...)
      private$.plotCurve(image, "h(t)"),

    .plotF = function(image, ...)
      private$.plotCurve(image, "f(t)")))
