relsystemClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "relsystemClass",
  inherit = relsystemBase,
  private = list(

    .run = function() {

      structure <- self$options$structure
      inputsTable <- self$results$inputsTable
      resultTable <- self$results$resultTable

      nOpt <- self$options$nComponents
      kOpt <- self$options$kValue
      mOpt <- self$options$nBlocks
      npbOpt <- self$options$componentsPerBlock

      if (nOpt != round(nOpt) || kOpt != round(kOpt) ||
          mOpt != round(mOpt) || npbOpt != round(npbOpt)) {
        inputsTable$setError("Liczby elementów, bloków i k muszą być całkowite.")
        return()
      }

      n <- round(nOpt)
      k <- round(kOpt)
      m <- round(mOpt)
      npb <- round(npbOpt)

      if (structure %in% c("seriesParallel", "parallelSeries")) {
        n <- m * npb
        if (n > 8) {
          inputsTable$setError("Iloczyn m × (elementy w bloku) nie może przekraczać 8.")
          return()
        }
      }
      if (structure == "bridge")
        n <- 5
      if (structure == "koutofn" && k > n) {
        inputsTable$setError("k nie może być większe od n.")
        return()
      }

      if (self$options$sameReliability) {
        r <- rep(self$options$componentReliability, n)
      } else {
        rAll <- c(self$options$r1, self$options$r2, self$options$r3,
                  self$options$r4, self$options$r5, self$options$r6,
                  self$options$r7, self$options$r8)
        r <- rAll[seq_len(n)]
      }

      phi <- switch(structure,
        series         = riskPhiSeries(n),
        parallel       = riskPhiParallel(n),
        koutofn        = riskPhiKofN(n, k),
        seriesParallel = riskPhiSeriesParallel(m, npb),
        parallelSeries = riskPhiParallelSeries(m, npb),
        bridge         = riskPhiBridge())

      Rsys <- riskSystemReliability(phi, r)

      structureLabel <- switch(structure,
        series         = "Szeregowa",
        parallel       = "Równoległa",
        koutofn        = paste(k, "-z-", n, sep = ""),
        seriesParallel = paste("Szeregowo-równoległa (", m, " bloki po ", npb, ")", sep = ""),
        parallelSeries = paste("Równoległo-szeregowa (", m, " gałęzie po ", npb, ")", sep = ""),
        bridge         = "Mostek (5 elementów)")

      inputsTable$setRow(rowNo = 1, values = list(
        structureCol = structureLabel,
        nCol = paste("n = ", n, sep = ""),
        relCol = paste("r = (", paste(format(r, digits = 3), collapse = ", "), ")", sep = "")))

      resultTable$setRow(rowNo = 1, values = list(rel = Rsys, fail = 1 - Rsys))
      resultTable$setNote("assumptions",
        "Założenia: awarie elementów są niezależne, a wszystkie niezawodności odnoszą się do tego samego czasu misji.")

      if (self$options$showStructureFun)
        self$results$structureFun$setContent(
          private$.structureText(structure, n, k, m, npb, r, Rsys))

      if (self$options$showPathsCuts) {
        pathsTable <- self$results$pathsTable
        paths <- riskMinimalPaths(phi, n)
        cuts <- riskMinimalCuts(phi, n)
        rowNo <- 0
        for (s in paths) {
          rowNo <- rowNo + 1
          pathsTable$addRow(rowKey = rowNo, values = list(
            type = "ścieżka minimalna",
            set = paste("{", paste(s, collapse = ", "), "}", sep = "")))
        }
        for (s in cuts) {
          rowNo <- rowNo + 1
          pathsTable$addRow(rowKey = rowNo, values = list(
            type = "przekrój minimalny",
            set = paste("{", paste(s, collapse = ", "), "}", sep = "")))
        }
      }

      if (self$options$showCoherence) {
        coherenceTable <- self$results$coherenceTable
        co <- riskCoherence(phi, n)
        coherenceTable$addRow(rowKey = "mono", values = list(
          quantity = "Monotoniczność funkcji struktury",
          value = if (co$monotone) "tak" else "nie"))
        coherenceTable$addRow(rowKey = "rel", values = list(
          quantity = "Wszystkie elementy istotne",
          value = if (all(co$relevant)) "tak"
                  else paste("nie (nieistotne: ",
                             paste(which(!co$relevant), collapse = ", "), ")", sep = "")))
        coherenceTable$addRow(rowKey = "coh", values = list(
          quantity = "System koherentny",
          value = if (co$coherent) "tak" else "nie"))
      }

      if (self$options$showStateTable) {
        stateTable <- self$results$stateTable
        st <- riskStateTable(phi, r)
        for (i in seq_len(nrow(st)))
          stateTable$addRow(rowKey = i, values = list(
            state = st$state[i], phi = st$phi[i], prob = st$prob[i]))
      }

      layout <- riskDiagramLayout(structure, n, m = m, npb = npb, r = r)
      self$results$diagram$setState(layout)
    },

    .structureText = function(structure, n, k, m, npb, r, Rsys) {
      idx <- seq_len(n)
      xs <- paste("x", idx, sep = "")
      rs <- format(r, digits = 3)
      RsysTxt <- format(round(Rsys, 5), nsmall = 5)

      if (structure == "series") {
        phiTxt <- paste("φ(x) =", paste(xs, collapse = " · "))
        formulaTxt <- paste("R = r1 · … · r", n, " = ",
                            paste(rs, collapse = " · "), " = ", RsysTxt, sep = "")
      } else if (structure == "parallel") {
        phiTxt <- paste("φ(x) = 1 − ",
                        paste("(1 − ", xs, ")", sep = "", collapse = ""), sep = "")
        formulaTxt <- paste("R = 1 − ",
                            paste("(1 − ", rs, ")", sep = "", collapse = ""),
                            " = ", RsysTxt, sep = "")
      } else if (structure == "koutofn") {
        phiTxt <- paste("φ(x) = 1, gdy x1 + … + x", n, " ≥ ", k, "; 0 w przeciwnym razie", sep = "")
        if (length(unique(r)) == 1) {
          formulaTxt <- paste("R = Σ(j=", k, "…", n, ") C(", n, ", j) · r^j · (1 − r)^(", n,
                              " − j), r = ", rs[1], "  →  R = ", RsysTxt, sep = "")
        } else {
          formulaTxt <- paste("R obliczone przez enumerację 2^", n,
                              " stanów (elementy różne) = ", RsysTxt, sep = "")
        }
      } else if (structure == "seriesParallel") {
        blocks <- split(idx, rep(seq_len(m), each = npb))
        blockTxt <- vapply(blocks, function(b)
          paste("[1 − ", paste("(1 − x", b, ")", sep = "", collapse = ""), "]", sep = ""), "")
        phiTxt <- paste("φ(x) =", paste(blockTxt, collapse = " · "))
        blockVal <- vapply(blocks, function(b) 1 - prod(1 - r[b]), 0)
        formulaTxt <- paste("R = ", paste(format(blockVal, digits = 5), collapse = " · "),
                            " = ", RsysTxt, sep = "")
      } else if (structure == "parallelSeries") {
        branches <- split(idx, rep(seq_len(m), each = npb))
        branchTxt <- vapply(branches, function(b)
          paste("[1 − ", paste("x", b, sep = "", collapse = "·"), "]", sep = ""), "")
        phiTxt <- paste("φ(x) = 1 −", paste(branchTxt, collapse = " · "))
        branchVal <- vapply(branches, function(b) prod(r[b]), 0)
        formulaTxt <- paste("R = 1 − ",
                            paste("(1 − ", format(branchVal, digits = 5), ")",
                                  sep = "", collapse = ""),
                            " = ", RsysTxt, sep = "")
      } else {  # bridge
        phiTxt <- paste(
          "φ(x) = 1, gdy działa któraś ze ścieżek minimalnych:",
          "{1, 2}, {4, 5}, {1, 3, 5}, {2, 3, 4}",
          "(e1: S–A, e2: A–T, e3: A–B, e4: S–B, e5: B–T)", sep = "\n")
        b3work <- (1 - (1 - r[1]) * (1 - r[4])) * (1 - (1 - r[2]) * (1 - r[5]))
        b3fail <- 1 - (1 - r[1] * r[2]) * (1 - r[4] * r[5])
        formulaTxt <- paste(
          "Dekompozycja względem elementu 3 (poprzeczka):\n",
          "R = r3 · R(3 sprawny) + (1 − r3) · R(3 uszkodzony)\n",
          "  = ", rs[3], " · ", format(round(b3work, 5), nsmall = 5),
          " + ", format(round(1 - r[3], 5)), " · ", format(round(b3fail, 5), nsmall = 5),
          " = ", RsysTxt, sep = "")
      }

      paste(phiTxt, formulaTxt, sep = "\n")
    },

    .plotDiagram = function(image, ...) {
      layout <- image$state
      if (is.null(layout))
        return(FALSE)
      boxes <- layout$boxes
      edges <- layout$edges
      w <- layout$boxW / 2
      h <- layout$boxH / 2

      Plot <- ggplot() +
        geom_segment(data = edges,
                     aes(x = x, y = y, xend = xend, yend = yend),
                     colour = "#9f9f9f", linewidth = 0.7) +
        geom_rect(data = boxes,
                  aes(xmin = x - w, xmax = x + w, ymin = y - h, ymax = y + h),
                  fill = "#e0bc6b", colour = "black") +
        geom_text(data = boxes, aes(x = x, y = y, label = label),
                  size = 3.6, lineheight = 0.9) +
        theme_void() +
        coord_fixed(clip = "off")

      print(Plot)
      TRUE
    }))
