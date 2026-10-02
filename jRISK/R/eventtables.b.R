eventtablesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "eventtablesClass",
  inherit = eventtablesBase,
  private = list(

    .run = function() {

      varA <- self$options$varA
      varB <- self$options$varB
      if (is.null(varA) || is.null(varB))
        return()

      countsTable <- self$results$countsTable

      fa <- as.factor(self$data[[varA]])
      fb <- as.factor(self$data[[varB]])
      levA <- self$options$levelA
      if (is.null(levA)) levA <- levels(fa)[1]
      levB <- self$options$levelB
      if (is.null(levB)) levB <- levels(fb)[1]

      if (!is.null(self$options$levelA) && !(levA %in% levels(fa)))
        stop(sprintf("Nie można wykonać obliczenia: poziom «%s» zmiennej «%s» nie istnieje.", levA, varA), call. = FALSE)

      if (!is.null(self$options$levelB) && !(levB %in% levels(fb)))
        stop(sprintf("Nie można wykonać obliczenia: poziom «%s» zmiennej «%s» nie istnieje.", levB, varB), call. = FALSE)

      # each row counts once, or as many times as the count variable / the
      # jamovi data weights say (aggregated table: one row per cell)
      cw <- riskCaseWeights(self$data, self$options$countVar)
      if (any(cw$w < 0, na.rm = TRUE)) {
        countsTable$setError(sprintf("Nie można wykonać obliczenia: liczności w «%s» nie mogą być ujemne.", cw$name))
        return()
      }
      keep <- !is.na(fa) & !is.na(fb) & !is.na(cw$w)
      A <- as.character(fa[keep]) == levA
      B <- as.character(fb[keep]) == levB
      w <- cw$w[keep]
      n <- sum(w)
      if (n == 0) {
        countsTable$setError("Brak kompletnych obserwacji dla obu zmiennych.")
        return()
      }
      if (cw$source == "counts")
        countsTable$setNote("counts", sprintf("Liczności z kolumny «%s».", cw$name))
      else if (cw$source == "weights")
        countsTable$setNote("counts", sprintf("Dane ważone zmienną «%s».", cw$name))
      if (any(w != round(w)))
        countsTable$setNote("round",
          "Liczności niecałkowite: tabela pokazuje wartości zaokrąglone, prawdopodobieństwa liczone są dokładnie.")

      labA <- paste("A: ", varA, " = ", levA, sep = "")
      labNotA <- "nie-A"
      nAB <- sum(w[A & B])
      nAnB <- sum(w[A & !B])
      nnAB <- sum(w[!A & B])
      nnAnB <- sum(w[!A & !B])

      countsTable$getColumn("b")$setTitle(paste("B: ", varB, " = ", levB, sep = ""))
      countsTable$getColumn("notb")$setTitle("nie-B")
      countsTable$setRow(rowNo = 1, values = list(
        rowLabel = labA, b = round(nAB), notb = round(nAnB), total = round(nAB + nAnB)))
      countsTable$setRow(rowNo = 2, values = list(
        rowLabel = labNotA, b = round(nnAB), notb = round(nnAnB), total = round(nnAB + nnAnB)))
      countsTable$setRow(rowNo = 3, values = list(
        rowLabel = "Suma", b = round(nAB + nnAB), notb = round(nAnB + nnAnB), total = round(n)))

      pA <- (nAB + nAnB) / n
      pB <- (nAB + nnAB) / n
      pAB <- nAB / n

      probTable <- self$results$probTable
      probTable$addRow(rowKey = "pa", values = list(
        quantity = "P(A)", formula = "n(A)/n", value = pA))
      probTable$addRow(rowKey = "pb", values = list(
        quantity = "P(B)", formula = "n(B)/n", value = pB))
      probTable$addRow(rowKey = "pab", values = list(
        quantity = "P(A ∩ B)", formula = "n(A ∩ B)/n", value = pAB))
      probTable$addRow(rowKey = "paub", values = list(
        quantity = "P(A ∪ B)", formula = "P(A) + P(B) − P(A ∩ B)",
        value = pA + pB - pAB))

      if (self$options$showConditional) {
        pAgB <- if (pB > 0) pAB / pB else NA
        pBgA <- if (pA > 0) pAB / pA else NA
        probTable$addRow(rowKey = "pagb", values = list(
          quantity = "P(A | B)", formula = "P(A ∩ B)/P(B)", value = pAgB))
        probTable$addRow(rowKey = "pbga", values = list(
          quantity = "P(B | A)", formula = "P(A ∩ B)/P(A)", value = pBgA))
        probTable$addRow(rowKey = "product", values = list(
          quantity = "Reguła iloczynu", formula = "P(A ∩ B) = P(A | B)·P(B)",
          value = if (is.na(pAgB)) NA else pAgB * pB))
        if (pB == 0 || pA == 0)
          probTable$setNote("undef",
            "P(A) = 0 lub P(B) = 0 — odpowiednie prawdopodobieństwa warunkowe są niezdefiniowane.")
      }

      if (self$options$showDetector) {
        detectorTable <- self$results$detectorTable
        sens <- if (pA > 0) nAB / (nAB + nAnB) else NA
        spec <- if (pA < 1) nnAnB / (nnAB + nnAnB) else NA
        ppv <- if (pB > 0) nAB / (nAB + nnAB) else NA
        npv <- if (pB < 1) nnAnB / (nAnB + nnAnB) else NA
        detectorTable$addRow(rowKey = "sens", values = list(
          quantity = "Czułość", formula = "P(B | A)", value = sens))
        detectorTable$addRow(rowKey = "spec", values = list(
          quantity = "Swoistość", formula = "P(nie-B | nie-A)", value = spec))
        detectorTable$addRow(rowKey = "ppv", values = list(
          quantity = "PPV", formula = "P(A | B)", value = ppv))
        detectorTable$addRow(rowKey = "npv", values = list(
          quantity = "NPV", formula = "P(nie-A | nie-B)", value = npv))
        detectorTable$setNote("conv",
          "Interpretacja: A = stan rzeczywisty (np. awaria), B = alarm/wynik testu.")
      }

      if (self$options$showNaturalFreq) {
        freqTable <- self$results$freqTable
        s <- 1000 / n
        freqTable$getColumn("b")$setTitle(paste("B: ", varB, " = ", levB, sep = ""))
        freqTable$setRow(rowNo = 1, values = list(
          rowLabel = labA, b = round(nAB * s), notb = round(nAnB * s),
          total = round((nAB + nAnB) * s)))
        freqTable$setRow(rowNo = 2, values = list(
          rowLabel = labNotA, b = round(nnAB * s), notb = round(nnAnB * s),
          total = round((nnAB + nnAnB) * s)))
        freqTable$setRow(rowNo = 3, values = list(
          rowLabel = "Suma", b = round((nAB + nnAB) * s),
          notb = round((nAnB + nnAnB) * s), total = 1000))
      }

      self$results$treePlot$setState(list(
        n = n, nA = nAB + nAnB, nnA = nnAB + nnAnB,
        nAB = nAB, nAnB = nAnB, nnAB = nnAB, nnAnB = nnAnB,
        labA = levA, labB = levB))
    },

    .plotTree = function(image, ggtheme, theme, ...) {
      s <- image$state
      if (is.null(s))
        return(FALSE)
      fmt <- function(v) format(round(v, 2), big.mark = " ")
      s[c("n", "nA", "nnA", "nAB", "nAnB", "nnAB", "nnAnB")] <-
        lapply(s[c("n", "nA", "nnA", "nAB", "nAnB", "nnAB", "nnAnB")], fmt)
      nodes <- data.frame(
        x = c(0, 1, 1, 2, 2, 2, 2),
        y = c(0, 1, -1, 1.5, 0.5, -0.5, -1.5),
        label = c(
          paste("n =", s$n),
          paste("A\n", s$nA), paste("nie-A\n", s$nnA),
          paste("A ∩ B\n", s$nAB), paste("A ∩ nie-B\n", s$nAnB),
          paste("nie-A ∩ B\n", s$nnAB), paste("nie-A ∩ nie-B\n", s$nnAnB)))
      edges <- data.frame(
        x = c(0, 0, 1, 1, 1, 1),
        y = c(0, 0, 1, 1, -1, -1),
        xend = c(1, 1, 2, 2, 2, 2),
        yend = c(1, -1, 1.5, 0.5, -0.5, -1.5))

      Plot <- ggplot() +
        geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
                     colour = "grey60") +
        geom_label(data = nodes, aes(x = x, y = y, label = label),
                   fill = theme$fill[2], colour = theme$color[1], label.size = 0.3, size = 3.6,
                   lineheight = 0.9) +
        theme_void() +
        coord_cartesian(xlim = c(-0.3, 2.4))

      print(Plot)
      TRUE
    }))
