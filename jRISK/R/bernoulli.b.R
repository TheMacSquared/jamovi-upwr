bernoulliClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "bernoulliClass",
  inherit = bernoulliBase,
  private = list(

    .run = function() {

      outcomeVar <- self$options$outcomeVar
      if (is.null(outcomeVar))
        return()

      summaryTable <- self$results$summaryTable

      fo <- as.factor(self$data[[outcomeVar]])
      lev <- self$options$successLevel
      # match the UI auto-selection: the event is conventionally the
      # second level (0/1, nie/tak)
      if (is.null(lev)) lev <- levels(fo)[min(2, nlevels(fo))]

      orderVar <- self$options$orderVar
      if (is.null(orderVar)) {
        ord <- seq_along(fo)
        summaryTable$setNote("order",
          "Nie wskazano porządku prób — przyjęto kolejność wierszy arkusza.")
      } else {
        ord <- jmvcore::toNumeric(self$data[[orderVar]])
      }

      keep <- !is.na(fo) & !is.na(ord)
      x <- as.integer(as.character(fo[keep]) == lev)
      x <- x[order(ord[keep])]
      n <- length(x)
      if (n == 0) {
        summaryTable$setError("Brak kompletnych obserwacji.")
        return()
      }

      k <- sum(x)
      phat <- k / n
      # Wilson 95% interval
      z <- qnorm(0.975)
      centre <- (phat + z^2 / (2 * n)) / (1 + z^2 / n)
      half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)

      summaryTable$setRow(rowNo = 1, values = list(
        n = n, successes = k, phat = phat,
        lower = max(0, centre - half), upper = min(1, centre + half)))
      summaryTable$setNote("diag",
        "Wykres częstości skumulowanej ilustruje stabilizację p̂; nie dowodzi niezależności prób ani stałości p.")

      self$results$runPlot$setState(data.frame(
        trial = seq_len(n), prop = cumsum(x) / seq_len(n), phat = phat))
    },

    .plotRun = function(image, ...) {
      state <- image$state
      if (is.null(state))
        return(FALSE)
      Color <- c("#e0bc6b", "#7b9ee6", "#9f9f9f")

      Plot <- ggplot(state, aes(x = trial, y = prop)) +
        geom_hline(yintercept = state$phat[1], colour = Color[2],
                   linetype = "dashed") +
        geom_line(colour = "black", linewidth = 0.7) +
        geom_point(size = 0.8, colour = Color[1]) +
        ggplot2::xlab("Numer próby") +
        ggplot2::ylab("Skumulowana częstość sukcesów") +
        coord_cartesian(ylim = c(0, 1)) +
        theme_classic() +
        theme(text = element_text(size = 14))

      print(Plot)
      TRUE
    }))
