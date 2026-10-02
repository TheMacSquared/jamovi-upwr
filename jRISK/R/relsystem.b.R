relsystemClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "relsystemClass",
  inherit = relsystemBase,
  private = list(

    .run = function() {

      if (self$options$mode == "data") {
        private$.runData()
        return()
      }

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

      cc <- private$.commonCause(phi, r, as.character(seq_len(n)),
                                 function(rr) riskSystemReliability(phi, rr))
      if (is.null(cc))
        return()

      inputsTable$setRow(rowNo = 1, values = list(
        structureCol = paste(structureLabel, cc$structureSuffix, sep = ""),
        nCol = paste("n = ", n, sep = ""),
        relCol = paste("r = (", paste(format(r, digits = 3), collapse = ", "), ")", sep = "")))

      Rsys <- Rsys * cc$factor
      resultTable$setRow(rowNo = 1, values = list(rel = Rsys, fail = 1 - Rsys))
      resultTable$setNote("assumptions", cc$note)

      private$.extras(cc$phi, cc$r, cc$labels, cc$relFun)

      layout <- riskDiagramLayout(structure, n, m = m, npb = npb, r = r)
      self$results$diagram$setState(private$.diagramCommonCause(layout))
    },

    .runData = function() {
      inputsTable <- self$results$inputsTable
      resultTable <- self$results$resultTable

      relVar <- self$options$relVar
      if (is.null(relVar))
        return()

      r <- jmvcore::toNumeric(self$data[[relVar]])
      labelVar <- self$options$labelVar
      labels <- if (is.null(labelVar)) as.character(seq_along(r))
                else as.character(self$data[[labelVar]])
      groupVar <- self$options$groupVar
      group <- if (is.null(groupVar)) rep("system", length(r))
               else as.character(self$data[[groupVar]])

      keep <- !is.na(r) & !is.na(labels) & !is.na(group)
      r <- r[keep]
      labels <- labels[keep]
      group <- group[keep]

      if (length(r) == 0) {
        inputsTable$setError("Brak kompletnych wierszy komponentów.")
        return()
      }
      if (any(r < 0 | r > 1)) {
        inputsTable$setError("Niezawodności komponentów muszą być w przedziale [0, 1].")
        return()
      }

      innerGate <- self$options$innerGate
      outerGate <- self$options$outerGate
      gateLabel <- c(series = "szeregowo", parallel = "równolegle")

      # components ordered by group (order of first appearance in the data)
      group <- factor(group, levels = unique(group))
      ord <- order(as.integer(group))
      r <- r[ord]
      labels <- labels[ord]
      group <- group[ord]
      groupSizes <- as.integer(table(group))
      n <- length(r)

      k <- NULL
      if (innerGate == "koutofn") {
        k <- self$options$kValue
        if (k != round(k)) {
          inputsTable$setError("k musi być liczbą całkowitą.")
          return()
        }
        if (any(groupSizes < k)) {
          inputsTable$setError(paste(
            "Bramka k-z-n wymaga co najmniej k = ", k,
            " elementów w każdym podsystemie; za małe: ",
            paste(levels(group)[groupSizes < k], collapse = ", "), ".", sep = ""))
          return()
        }
        gateLabel <- c(gateLabel, koutofn = paste("co najmniej", k, "sprawne"))
      }

      relFun <- function(rr)
        riskTwoLevelReliability(rr, groupSizes, innerGate, outerGate, k)
      Rsys <- relFun(r)

      # enumeration-based extras only for small systems
      phi <- if (n <= 8) riskPhiTwoLevel(groupSizes, innerGate, outerGate, k) else NULL
      cc <- private$.commonCause(phi, r, labels, relFun)
      if (is.null(cc))
        return()

      structureLabel <- paste(
        "Dwupoziomowa: w podsystemie ", gateLabel[[innerGate]],
        ", podsystemy ", gateLabel[[outerGate]], cc$structureSuffix, sep = "")
      inputsTable$setRow(rowNo = 1, values = list(
        structureCol = structureLabel,
        nCol = paste("n = ", n, " (grupy: ",
                     paste(levels(group), " [", groupSizes, "]",
                           sep = "", collapse = ", "), ")", sep = ""),
        relCol = paste("r = (", paste(format(r, digits = 3), collapse = ", "), ")", sep = "")))

      Rsys <- Rsys * cc$factor
      resultTable$setRow(rowNo = 1, values = list(rel = Rsys, fail = 1 - Rsys))
      resultTable$setNote("assumptions", cc$note)

      if (n > 8 && (self$options$showPathsCuts || self$options$showStateTable ||
                    self$options$showCoherence))
        resultTable$setNote("enumLimit",
          "Ścieżki/przekroje, tabela stanów i koherentność są wyznaczane dla systemów o maksymalnie 8 komponentach.")

      private$.extras(cc$phi, cc$r, cc$labels, cc$relFun)

      # a k-out-of-n group is drawn as a parallel block; with groups in
      # parallel that picture would hide the k requirement, so it is skipped
      if (innerGate == "koutofn" && outerGate == "parallel" && length(groupSizes) > 1) {
        self$results$diagram$setVisible(FALSE)
        return()
      }
      drawInner <- if (innerGate == "koutofn") "parallel" else innerGate
      layout <- riskDiagramLayoutTwoLevel(groupSizes, drawInner, outerGate,
                                          r = r,
                                          labels = jmvcore::wrapLabels(labels, width = 14))
      self$results$diagram$setState(private$.diagramCommonCause(layout))
    },

    # optional common cause: one extra element in series with the system,
    # reliability 1 - q; returns the structure, reliabilities and labels
    # extended by that element (phi NULL when enumeration is too large)
    .commonCause = function(phi, r, labels, relFun) {
      n <- length(r)
      base <- "Założenia: awarie elementów są niezależne, a wszystkie niezawodności odnoszą się do tego samego czasu misji."
      if (!self$options$commonCause)
        return(list(phi = phi, r = r, labels = labels, relFun = relFun,
                    factor = 1, structureSuffix = "", note = base))
      q <- self$options$ccfProb
      if (is.na(q) || q < 0 || q > 1) {
        self$results$inputsTable$setError("Prawdopodobieństwo wspólnej przyczyny q musi być w przedziale [0, 1].")
        return(NULL)
      }
      list(
        phi = if (is.null(phi)) NULL else riskPhiWithCommonCause(phi, n),
        r = c(r, 1 - q),
        labels = c(labels, "CCF"),
        relFun = function(rr) relFun(rr[seq_len(n)]) * rr[n + 1],
        factor = 1 - q,
        structureSuffix = paste(", + wspólna przyczyna CCF (q = ", format(q), ")", sep = ""),
        note = paste("Założenia: wspólna przyczyna CCF (q = ", format(q),
                     ") wyłącza cały system; poza nią awarie elementów są niezależne, ",
                     "a wszystkie niezawodności odnoszą się do tego samego czasu misji.", sep = ""))
    },

    .diagramCommonCause = function(layout) {
      if (!self$options$commonCause)
        return(layout)
      riskDiagramAppendSeries(layout,
        paste("CCF\n", format(1 - self$options$ccfProb, digits = 3), sep = ""))
    },

    # paths/cuts, state table, coherence (enumeration, phi non-NULL) and
    # Birnbaum importance (closed form via relFun, any size)
    .extras = function(phi, r, labels, relFun) {
      n <- length(r)
      fmtSet <- function(s) paste("{", paste(labels[s], collapse = ", "), "}", sep = "")

      if (!is.null(phi)) {
        if (self$options$showPathsCuts) {
          pathsTable <- self$results$pathsTable
          rowNo <- 0
          for (s in riskMinimalPaths(phi, n)) {
            rowNo <- rowNo + 1
            pathsTable$addRow(rowKey = rowNo, values = list(
              type = "ścieżka minimalna", set = fmtSet(s)))
          }
          for (s in riskMinimalCuts(phi, n)) {
            rowNo <- rowNo + 1
            pathsTable$addRow(rowKey = rowNo, values = list(
              type = "przekrój minimalny", set = fmtSet(s)))
          }
        }
        if (self$options$showStateTable) {
          stateTable <- self$results$stateTable
          st <- riskStateTable(phi, r)
          for (i in seq_len(nrow(st)))
            stateTable$addRow(rowKey = i, values = list(
              state = st$state[i], phi = st$phi[i], prob = st$prob[i]))
        }
        if (self$options$showCoherence) {
          coh <- riskCoherence(phi, n)
          yesNo <- function(b) if (b) "tak" else "nie"
          irrelevant <- labels[!coh$relevant]
          coherenceTable <- self$results$coherenceTable
          coherenceTable$addRow(rowKey = "mono", values = list(
            property = "φ niemalejąca względem każdego elementu",
            value = yesNo(coh$monotone)))
          coherenceTable$addRow(rowKey = "rel", values = list(
            property = "Każdy element istotny",
            value = if (length(irrelevant) == 0) "tak"
                    else paste("nie (nieistotne: ", paste(irrelevant, collapse = ", "), ")", sep = "")))
          coherenceTable$addRow(rowKey = "coh", values = list(
            property = "System koherentny", value = yesNo(coh$coherent)))
        }
      }

      if (self$options$showImportance) {
        importanceTable <- self$results$importanceTable
        B <- riskBirnbaum(relFun, r)
        for (j in order(B, decreasing = TRUE))
          importanceTable$addRow(rowKey = j, values = list(
            component = labels[j], rj = r[j], birnbaum = B[j]))
      }
    },

    .plotDiagram = function(image, ggtheme, theme, ...) {
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
                     colour = "grey60", linewidth = 0.7) +
        geom_rect(data = boxes,
                  aes(xmin = x - w, xmax = x + w, ymin = y - h, ymax = y + h),
                  fill = theme$fill[2], colour = theme$color[1]) +
        geom_text(data = boxes, aes(x = x, y = y, label = label),
                  size = 3.6, lineheight = 0.9, colour = theme$color[1]) +
        theme_void() +
        coord_fixed(clip = "off")

      print(Plot)
      TRUE
    }))
