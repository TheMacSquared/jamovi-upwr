lifetimeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "lifetimeClass",
  inherit = lifetimeBase,
  private = list(

    .modelLabels = c(exponential = "Wykładniczy",
                     gamma = "Gamma",
                     weibull = "Weibulla"),

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
      if (self$options$mode == "data") {
        private$.runData()
        return()
      }

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

    .runData = function() {
      dataCounts <- self$results$dataCounts
      timeVarName <- self$options$timeVar
      if (is.null(timeVarName))
        return()

      tRaw <- jmvcore::toNumeric(self$data[[timeVarName]])
      statusVarName <- self$options$statusVar
      if (is.null(statusVarName)) {
        status <- rep(1L, length(tRaw))
      } else {
        sv <- as.factor(self$data[[statusVarName]])
        lev <- self$options$failureLevel
        # match the UI auto-selection: the event is conventionally the
        # second level (0/1, nie/tak)
        if (is.null(lev))
          lev <- levels(sv)[min(2, nlevels(sv))]
        if (!is.null(self$options$failureLevel) && !(lev %in% levels(sv)))
          stop(sprintf("Nie można wykonać obliczenia: poziom «%s» zmiennej «%s» nie istnieje.", lev, statusVarName), call. = FALSE)

        status <- as.integer(as.character(sv) == lev)
        status[is.na(sv)] <- NA_integer_
      }

      # optional grouping: every group (e.g. device type) gets its own
      # Kaplan-Meier estimate and model fits
      groupVarName <- self$options$groupVar
      if (is.null(groupVarName)) {
        gRaw <- factor(rep("", length(tRaw)))
      } else {
        gRaw <- as.factor(self$data[[groupVarName]])
      }

      keep <- !is.na(tRaw) & !is.na(status) & !is.na(gRaw)
      t <- tRaw[keep]
      status <- status[keep]
      g <- droplevels(gRaw[keep])

      if (length(t) == 0) {
        dataCounts$setError("Brak kompletnych obserwacji (czas i status).")
        return()
      }
      if (any(t < 0)) {
        dataCounts$setError("Czas życia nie może być ujemny.")
        return()
      }
      if (any(t == 0 & status == 1)) {
        dataCounts$setError("Awaria w czasie t = 0 nie jest dopuszczalna w modelach ciągłych.")
        return()
      }
      # a censored observation at t = 0 carries no information
      zeroCens <- t == 0
      if (any(zeroCens)) {
        t <- t[!zeroCens]
        status <- status[!zeroCens]
        g <- droplevels(g[!zeroCens])
        dataCounts$setNote("zeroCens",
          paste("Usunięto", sum(zeroCens), "obserwacji cenzurowanych w t = 0 (brak informacji)."))
      }
      if (sum(status) == 0) {
        dataCounts$setError("Wszystkie obserwacje są cenzurowane — nie można dopasować modeli.")
        return()
      }

      fitTable <- self$results$fitTable
      paramTable <- self$results$paramTable
      dataAtTable <- self$results$dataAtTable
      tUser <- self$options$t
      grouped <- !is.null(groupVarName)

      paramLabels <- list(
        exponential = c(rate = "Intensywność (λ)"),
        gamma = c(shape = "Kształt (α)", rate = "Intensywność (λ)"),
        weibull = c(shape = "Kształt (β)", scale = "Skala (η)"))

      grid <- seq(max(t) / 400, max(max(t), tUser) * 1.02, length.out = 400)
      kmSteps <- NULL
      curves <- NULL
      hazCurves <- NULL
      bestNotes <- character(0)
      allCensored <- character(0)
      anySingular <- FALSE

      for (gr in levels(g)) {
        sel <- g == gr
        tg <- t[sel]
        sg <- status[sel]
        key <- function(x) paste(gr, x, sep = "|")

        km <- riskKaplanMeier(tg, sg)
        dataCounts$addRow(rowKey = gr, values = list(
          group = gr, n = length(tg), events = sum(sg), censored = sum(1L - sg),
          kmMedian = if (is.finite(km$median)) km$median else NA))
        if (sum(sg) == 0) {
          allCensored <- c(allCensored, gr)
          next
        }

        fits <- list()
        for (dist in c("exponential", "gamma", "weibull")) {
          fit <- riskLtFit(tg, sg, dist)
          fits[[dist]] <- fit
          label <- private$.modelLabels[[dist]]
          if (!fit$ok) {
            fitTable$addRow(rowKey = key(dist), values = list(
              group = gr, model = paste(label, "— brak zbieżności"),
              logLik = NA, aic = NA, bic = NA))
            next
          }
          fitTable$addRow(rowKey = key(dist), values = list(
            group = gr, model = label,
            logLik = fit$logLik, aic = fit$AIC, bic = fit$BIC))
          for (pn in names(fit$par)) {
            paramTable$addRow(rowKey = key(paste(dist, pn)), values = list(
              group = gr,
              model = label,
              param = paramLabels[[dist]][[pn]],
              est = fit$par[[pn]],
              lower = if (fit$singular) NA else fit$lower[[pn]],
              upper = if (fit$singular) NA else fit$upper[[pn]]))
          }
          if (fit$singular)
            anySingular <- TRUE
        }

        okFits <- Filter(function(f) f$ok, fits)
        bestDist <- NULL
        if (length(okFits) > 0) {
          aics <- vapply(okFits, function(f) f$AIC, 0)
          bestDist <- names(okFits)[which.min(aics)]
          bestNotes <- c(bestNotes, if (grouped)
            paste(gr, ": ", private$.modelLabels[[bestDist]], sep = "")
            else private$.modelLabels[[bestDist]])
        }

        # KM estimate of R(t*): the step value at the largest event time <= t*
        kmAt <- 1
        if (any(km$time <= tUser))
          kmAt <- km$surv[max(which(km$time <= tUser))]
        dataAtTable$addRow(rowKey = key("km"), values = list(
          group = gr, model = "Kaplan–Meier", rt = kmAt, mttf = NA,
          median = if (is.finite(km$median)) km$median else NA))
        for (dist in names(okFits)) {
          par <- okFits[[dist]]$par
          dataAtTable$addRow(rowKey = key(dist), values = list(
            group = gr,
            model = private$.modelLabels[[dist]],
            rt = riskLtReliability(tUser, dist, par),
            mttf = riskLtMTTF(dist, par),
            median = riskLtMedian(dist, par)))
        }

        # plot states: without groups all models are drawn; with groups only
        # the lowest-AIC model of each group, so the picture stays readable
        kmSteps <- rbind(kmSteps, data.frame(
          group = gr, time = c(0, km$time), surv = c(1, km$surv)))
        drawn <- if (grouped) bestDist else names(okFits)
        for (dist in drawn) {
          par <- okFits[[dist]]$par
          R <- riskLtReliability(grid, dist, par)
          h <- riskLtHazard(grid, dist, par)
          h[!is.finite(h) | R < 1e-9] <- NA
          lab <- if (grouped) gr else private$.modelLabels[[dist]]
          curves <- rbind(curves, data.frame(model = lab, x = grid, y = R))
          hazCurves <- rbind(hazCurves, data.frame(model = lab, x = grid, y = h))
        }
      }

      if (length(bestNotes) > 0)
        fitTable$setNote("aic", paste(
          if (grouped) "Najniższe AIC w grupach — " else "Najniższe AIC: model ",
          paste(bestNotes, collapse = "; "),
          ". Porównanie ma charakter opisowy — nie wskazuje modelu „prawdziwego”.",
          sep = ""))
      if (length(allCensored) > 0)
        fitTable$setNote("censored", paste(
          "Bez dopasowania (wszystkie obserwacje cenzurowane): ",
          paste(allCensored, collapse = ", "), ".", sep = ""))
      if (anySingular)
        paramTable$setNote("singular",
          "Osobliwa macierz Hessego — przedziały ufności niedostępne dla części parametrów.")
      self$results$kmPlot$setState(list(km = kmSteps, curves = curves,
                                        tUser = tUser, grouped = grouped))
      self$results$hazPlot$setState(list(curves = hazCurves, tUser = tUser,
                                         grouped = grouped))
    },

    .plotCurve = function(image, ggtheme, theme, ylab) {
      state <- image$state
      if (is.null(state))
        return(FALSE)
      tUser <- state$tUser[1]

      Plot <- ggplot(state, aes(x = x, y = y)) +
        geom_line(colour = theme$color[2], linewidth = 0.8) +
        geom_vline(xintercept = tUser, colour = "grey60", linetype = "dashed") +
        ggplot2::xlab("t") + ggplot2::ylab(ylab) +
        ggtheme +
        theme(text = element_text(size = 14))

      print(Plot)
      TRUE
    },

    .plotR = function(image, ggtheme, theme, ...)
      private$.plotCurve(image, ggtheme, theme, "R(t)"),

    .plotH = function(image, ggtheme, theme, ...)
      private$.plotCurve(image, ggtheme, theme, "h(t)"),

    .plotF = function(image, ggtheme, theme, ...)
      private$.plotCurve(image, ggtheme, theme, "f(t)"),

    .plotKM = function(image, ggtheme, theme, ...) {
      state <- image$state
      if (is.null(state))
        return(FALSE)
      if (isTRUE(state$grouped)) {
        # one colour per group: KM steps solid, lowest-AIC model dashed
        Plot <- ggplot() +
          geom_step(data = state$km, aes(x = time, y = surv, colour = group),
                    linewidth = 0.9) +
          geom_vline(xintercept = state$tUser, colour = "grey60", linetype = "dotted") +
          ggplot2::xlab("t") + ggplot2::ylab("R(t)") +
          ggtheme +
          theme(text = element_text(size = 14), legend.title = element_blank())
        if (!is.null(state$curves))
          Plot <- Plot +
            geom_line(data = state$curves, aes(x = x, y = y, colour = model),
                      linewidth = 0.8, linetype = "dashed")
      } else {
        Plot <- ggplot() +
          geom_step(data = state$km, aes(x = time, y = surv),
                    colour = theme$color[1], linewidth = 0.9) +
          geom_vline(xintercept = state$tUser, colour = "grey60", linetype = "dotted") +
          ggplot2::xlab("t") + ggplot2::ylab("R(t)") +
          ggtheme +
          theme(text = element_text(size = 14), legend.title = element_blank())
        if (!is.null(state$curves))
          Plot <- Plot +
            geom_line(data = state$curves, aes(x = x, y = y, colour = model),
                      linewidth = 0.8)
      }
      # discrete colour scale for fitted models comes from ggtheme (palette)

      print(Plot)
      TRUE
    },

    .plotHaz = function(image, ggtheme, theme, ...) {
      state <- image$state
      if (is.null(state) || is.null(state$curves))
        return(FALSE)
      Plot <- ggplot(state$curves, aes(x = x, y = y, colour = model)) +
        geom_line(linewidth = 0.8) +
        geom_vline(xintercept = state$tUser, colour = "grey60", linetype = "dotted") +
        ggplot2::xlab("t") + ggplot2::ylab("h(t)") +
        ggtheme +
        theme(text = element_text(size = 14), legend.title = element_blank())

      print(Plot)
      TRUE
    }))
