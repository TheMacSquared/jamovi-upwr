#' @importFrom jmvcore .
satclassifyClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "satclassifyClass",
    inherit = satclassifyBase,
    private = list(
        .run = function() {
            if (is.null(self$options$klasa) ||
                    length(self$options$predyktory) < 1)
                return()

            klasa <- as.factor(self$data[[self$options$klasa]])
            pred <- self$data[self$options$predyktory]
            for (nm in names(pred))
                pred[[nm]] <- jmvcore::toNumeric(pred[[nm]])

            ok <- !is.na(klasa) & stats::complete.cases(pred)
            klasa <- droplevels(klasa[ok])
            pred <- pred[ok, , drop = FALSE]
            n <- length(klasa)

            if (n < 20 || nlevels(klasa) < 2) {
                self$results$podzial$setNote("err", paste(
                    "Potrzeba co najmniej 20 kompletnych obserwacji",
                    "i 2 klas."))
                return()
            }

            set.seed(self$options$ziarno)
            idxTren <- sample(n, round(n * self$options$procTren / 100))
            trenX <- pred[idxTren, , drop = FALSE]
            trenY <- klasa[idxTren]
            testX <- pred[-idxTren, , drop = FALSE]
            testY <- klasa[-idxTren]

            self$results$podzial$setRow(rowNo = 1, values = list(
                zbior = "Treningowy", n = length(trenY)))
            self$results$podzial$setRow(rowNo = 2, values = list(
                zbior = "Testowy", n = length(testY)))

            metoda <- self$options$metoda
            if (metoda == "knn") {
                # k-NN works on distances: standardize with training
                # parameters, apply the same scaling to the test set
                srednie <- vapply(trenX, mean, numeric(1))
                odch <- vapply(trenX, sd, numeric(1))
                odch[odch == 0] <- 1
                trenZ <- scale(trenX, center = srednie, scale = odch)
                testZ <- scale(testX, center = srednie, scale = odch)
                predykcja <- class::knn(trenZ, testZ, trenY,
                                        k = self$options$k)
                model <- list(srednie = srednie, odch = odch,
                              trenZ = trenZ, trenY = trenY)
            } else {
                dtren <- data.frame(.klasa = trenY, trenX,
                                    check.names = FALSE)
                drzewo <- rpart::rpart(.klasa ~ ., data = dtren,
                                       method = "class")
                dtest <- data.frame(testX, check.names = FALSE)
                predykcja <- predict(drzewo, dtest, type = "class")
                self$results$reguly$setContent(paste(
                    utils::capture.output(print(drzewo)), collapse = "\n"))
                model <- list(drzewo = drzewo)
            }

            poziomy <- levels(klasa)
            cm <- table(prawdziwa = testY,
                        przewidziana = factor(predykcja, levels = poziomy))

            # confusion matrix: one column per predicted class, added
            # dynamically since the classes come from the data
            mac <- self$results$macierz
            for (p in poziomy)
                mac$addColumn(name = paste0("p_", p),
                              title = p, type = 'integer',
                              superTitle = 'Klasa przewidziana')
            for (i in seq_along(poziomy)) {
                wartosci <- list(prawdziwa = poziomy[i])
                for (j in seq_along(poziomy))
                    wartosci[[paste0("p_", poziomy[j])]] <- cm[i, j]
                mac$addRow(rowKey = poziomy[i], values = wartosci)
            }

            trafnosc <- sum(diag(cm)) / sum(cm)
            mac$setNote("acc", sprintf(
                "Trafnosc (accuracy): %.1f%% (%d z %d poprawnie)",
                100 * trafnosc, sum(diag(cm)), sum(cm)))

            for (p in poziomy) {
                nTest <- sum(cm[p, ])
                nPred <- sum(cm[, p])
                self$results$miary$addRow(rowKey = p, values = list(
                    klasa = p,
                    n = nTest,
                    czulosc = if (nTest > 0) cm[p, p] / nTest else NA,
                    precyzja = if (nPred > 0) cm[p, p] / nPred else NA))
            }
            self$results$miary$setNote("def", paste(
                "Czulosc: odsetek obserwacji danej klasy rozpoznanych",
                "poprawnie. Precyzja: odsetek trafnych wsrod",
                "przewidzianych jako dana klasa."))

            if (self$options$granica && length(self$options$predyktory) >= 2)
                self$results$granicaPlot$setState(list(
                    trenX = trenX[, 1:2],
                    trenY = as.character(trenY),
                    testX = testX[, 1:2],
                    testY = as.character(testY),
                    nazwy = names(pred)[1:2],
                    metoda = metoda))
        },

        .granicaPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state

            # decision regions on a 2D grid using only the first two
            # predictors (refit on 2D so the picture matches the axes)
            x1 <- s$trenX[[1]]; x2 <- s$trenX[[2]]
            siatka <- expand.grid(
                x1 = seq(min(x1), max(x1), length.out = 150),
                x2 = seq(min(x2), max(x2), length.out = 150))

            if (s$metoda == "knn") {
                m1 <- mean(x1); s1 <- max(sd(x1), 1e-9)
                m2 <- mean(x2); s2 <- max(sd(x2), 1e-9)
                trenZ <- cbind((x1 - m1) / s1, (x2 - m2) / s2)
                siatkaZ <- cbind((siatka$x1 - m1) / s1,
                                 (siatka$x2 - m2) / s2)
                siatka$klasa <- as.character(
                    class::knn(trenZ, siatkaZ, s$trenY, k = self$options$k))
            } else {
                d <- data.frame(.klasa = factor(s$trenY), x1 = x1, x2 = x2)
                drzewo <- rpart::rpart(.klasa ~ x1 + x2, data = d,
                                       method = "class")
                siatka$klasa <- as.character(
                    predict(drzewo, siatka, type = "class"))
            }

            dTest <- data.frame(x1 = s$testX[[1]], x2 = s$testX[[2]],
                                klasa = s$testY)

            plot <- ggplot2::ggplot() +
                ggplot2::geom_raster(
                    data = siatka,
                    ggplot2::aes(x = x1, y = x2, fill = klasa),
                    alpha = 0.35) +
                ggplot2::geom_point(
                    data = dTest,
                    ggplot2::aes(x = x1, y = x2, color = klasa),
                    size = 1.8) +
                ggplot2::labs(x = s$nazwy[1], y = s$nazwy[2],
                              fill = "Region decyzyjny",
                              color = "Klasa (test)") +
                ggtheme
            plot
        }
    )
)
