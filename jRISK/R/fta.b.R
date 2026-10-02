ftaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "ftaClass",
  inherit = ftaBase,
  private = list(

    .run = function() {

      probVar <- self$options$probVar
      if (is.null(probVar))
        return()

      topTable <- self$results$topTable

      probs <- jmvcore::toNumeric(self$data[[probVar]])
      labelVar <- self$options$labelVar
      labels <- if (is.null(labelVar)) paste("E", seq_along(probs), sep = "")
                else as.character(self$data[[labelVar]])
      branchVar <- self$options$branchVar
      branch <- if (is.null(branchVar)) labels
                else as.character(self$data[[branchVar]])

      keep <- !is.na(probs) & !is.na(labels) & !is.na(branch)
      probs <- probs[keep]
      labels <- labels[keep]
      branch <- branch[keep]

      if (length(probs) == 0) {
        topTable$setError("Brak kompletnych wierszy zdarzeń (prawdopodobieństwo, etykieta, gałąź).")
        return()
      }
      if (any(probs < 0 | probs > 1)) {
        topTable$setError("Prawdopodobieństwa zdarzeń muszą być w przedziale [0, 1].")
        return()
      }
      shared <- FALSE
      if (anyDuplicated(labels)) {
        dup <- unique(labels[duplicated(labels)])
        if (!self$options$sharedEvents) {
          topTable$setError(paste(
            "Powtórzona etykieta zdarzenia (", paste(dup, collapse = ", "),
            ") — możliwa wspólna przyczyna lub podwójne liczenie; ",
            "naiwny rachunek przy założeniu niezależności byłby błędny. ",
            "Jeśli to to samo zdarzenie, zaznacz „Powtórzona etykieta = to samo zdarzenie”.",
            sep = ""))
          return()
        }
        # a shared event must carry one probability wherever it appears
        uLab <- unique(labels)
        ev <- match(labels, uLab)
        pEv <- probs[!duplicated(labels)]
        if (any(abs(probs - pEv[ev]) > 1e-12)) {
          topTable$setError(paste(
            "Powtórzone zdarzenie ma różne prawdopodobieństwa w różnych wierszach: ",
            paste(uLab[tapply(probs, ev, function(p) diff(range(p)) > 1e-12)], collapse = ", "),
            ".", sep = ""))
          return()
        }
        if (length(pEv) > 16) {
          topTable$setError("Rachunek dokładny z powtórzonymi zdarzeniami obsługuje maksymalnie 16 różnych zdarzeń.")
          return()
        }
        shared <- TRUE
      }

      innerGate <- self$options$innerGate
      topGate <- self$options$topGate
      gateLabel <- c(and = "AND", or = "OR")

      # keep the branch order as it appears in the data
      branch <- factor(branch, levels = unique(branch))
      pTop <- if (shared) riskFtaTopProbShared(pEv, ev, as.integer(branch), innerGate, topGate)
              else riskFtaTopProb(probs, branch, innerGate, topGate)

      topTable$setRow(rowNo = 1, values = list(
        structure = paste("gałęzie: ", gateLabel[[innerGate]],
                          ", top: ", gateLabel[[topGate]], sep = ""),
        ptop = pTop))
      if (shared)
        topTable$setNote("assumptions", paste(
          "Założenia: różne zdarzenia bazowe są niezależne; powtórzone (",
          paste(dup, collapse = ", "),
          ") to jedno zdarzenie w kilku gałęziach, P(TOP) dokładnie przez wyliczenie stanów; ",
          "wszystkie odnoszą się do tego samego horyzontu.", sep = ""))
      else
        topTable$setNote("assumptions",
          "Założenia: zdarzenia bazowe są niezależne i różne; wszystkie odnoszą się do tego samego horyzontu.")

      branchTable <- self$results$branchTable
      byBranch <- split(seq_along(probs), branch)
      for (b in names(byBranch)) {
        i <- byBranch[[b]]
        branchTable$addRow(rowKey = b, values = list(
          branch = b,
          gate = gateLabel[[innerGate]],
          events = paste(labels[i], collapse = ", "),
          prob = if (shared) riskFtaBranchProb(pEv[unique(ev[i])], innerGate)
                 else riskFtaBranchProb(probs[i], innerGate)))
      }

      # from here on work on distinct events: with shared events the leaves
      # map onto them through ev, otherwise every leaf is its own event
      if (!shared) {
        ev <- seq_along(probs)
        pEv <- probs
      }
      evLabels <- labels[!duplicated(ev)]
      occLeaves <- riskFtaOccurrence(as.integer(branch), innerGate, topGate)
      occ <- function(x) occLeaves(x[ev])

      if (self$options$showCuts) {
        cutsTable <- self$results$cutsTable
        nEv <- length(pEv)
        if (nEv > 12) {
          cutsTable$setNote("limit",
            "Minimalne przekroje wyznaczane są dla maksymalnie 12 zdarzeń bazowych.")
        } else {
          cuts <- riskMinimalPaths(occ, nEv)
          for (ci in seq_along(cuts)) {
            s <- cuts[[ci]]
            cutsTable$addRow(rowKey = ci, values = list(
              cut = paste("{", paste(evLabels[s], collapse = ", "), "}", sep = ""),
              prob = prod(pEv[s])))
          }
        }
      }

      if (self$options$showImportance) {
        importanceTable <- self$results$importanceTable
        imp <- if (shared) riskFtaImportanceShared(pEv, ev, as.integer(branch), innerGate, topGate)
               else riskFtaImportance(probs, branch, innerGate, topGate)
        ord <- order(imp, decreasing = TRUE)
        for (i in ord)
          importanceTable$addRow(rowKey = i, values = list(
            event = evLabels[i], prob = pEv[i], drop = imp[i]))
      }

      self$results$diagram$setState(list(
        labels = labels, probs = probs, branch = as.character(branch),
        innerGate = gateLabel[[innerGate]], topGate = gateLabel[[topGate]],
        pTop = pTop))
    },

    .plotTree = function(image, ggtheme, theme, ...) {
      s <- image$state
      if (is.null(s))
        return(FALSE)
      # Palette fills: leaves (1st), top event (2nd); gates keep neutral fill
      Fill <- jmvcore::colorPalette(2, theme$palette, 'fill')

      branches <- unique(s$branch)
      nB <- length(branches)
      # leaves laid out left to right, grouped by branch
      leafX <- seq_along(s$labels)
      branchX <- vapply(branches, function(b) mean(leafX[s$branch == b]), 0)
      topX <- mean(leafX)

      leaves <- data.frame(
        x = leafX, y = 0,
        label = paste(jmvcore::wrapLabels(s$labels, width = 14), "\n",
                      format(s$probs, digits = 3), sep = ""))
      gates <- data.frame(
        x = branchX, y = 1.2,
        label = paste(jmvcore::wrapLabels(branches, width = 14), "\n[", s$innerGate, "]", sep = ""))
      top <- data.frame(
        x = topX, y = 2.4,
        label = paste("TOP [", s$topGate, "]\nP = ",
                      format(signif(s$pTop, 3), scientific = FALSE), sep = ""))

      edges <- rbind(
        data.frame(x = leafX, y = 0.25,
                   xend = branchX[match(s$branch, branches)], yend = 0.95),
        data.frame(x = branchX, y = 1.45, xend = topX, yend = 2.15))

      Plot <- ggplot() +
        geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
                     colour = "grey60") +
        geom_label(data = leaves, aes(x = x, y = y, label = label),
                   fill = Fill[1], colour = theme$color[1], label.size = 0.3, size = 3.2,
                   lineheight = 0.9) +
        geom_label(data = gates, aes(x = x, y = y, label = label),
                   fill = theme$fill[1], colour = theme$color[1], label.size = 0.3, size = 3.4,
                   lineheight = 0.9) +
        geom_label(data = top, aes(x = x, y = y, label = label),
                   fill = Fill[2], colour = theme$color[1], label.size = 0.4, size = 3.6,
                   lineheight = 0.9) +
        theme_void() +
        coord_cartesian(xlim = c(0.4, max(leafX) + 0.6),
                        ylim = c(-0.4, 2.8))

      print(Plot)
      TRUE
    }))
