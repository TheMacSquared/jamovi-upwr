#' @importFrom jmvcore .
planClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "planClass",
    inherit = planBase,
    private = list(
        .run = function() {
            o <- self$options
            design <- o$design
            df <- planDesign(design, o$nTreat, o$nRep, o$nSub, o$labelsA, o$labelsB, o$seed)
            tab <- self$results$plan
            hasBlock <- !all(is.na(df$block))
            hasB <- !all(is.na(df$B))
            tab$getColumn("block")$setVisible(hasBlock)
            tab$getColumn("B")$setVisible(hasB)
            for (i in seq_len(nrow(df))) {
                tab$addRow(rowKey = i, values = list(plot = df$plot[i],
                    block = if (hasBlock) df$block[i] else NA,
                    row = df$row[i], col = df$col[i], A = df$A[i],
                    B = if (hasB) df$B[i] else ""))
            }
            desc <- switch(design,
                crd = sprintf(paste0("<p>Układ całkowicie losowy: liczba obiektów %d × liczba powtórzeń %d ",
                    "= liczba poletek %d. Obiekty rozlosowano w całym polu bez ograniczeń.</p>"),
                    o$nTreat, o$nRep, nrow(df)),
                rcbd = sprintf(paste0("<p>Układ losowanych bloków: liczba obiektów %d, liczba bloków %d ",
                    "= liczba poletek %d. Każdy obiekt występuje raz w każdym bloku; ",
                    "losowanie osobno w obrębie bloku.</p>"), o$nTreat, o$nRep, nrow(df)),
                latin = sprintf(paste0("<p>Kwadrat łaciński %d × %d: każdy obiekt raz ",
                    "w każdym wierszu i w każdej kolumnie (kontrola dwóch kierunków ",
                    "zmienności). Wylosowano permutacje wierszy, kolumn i obiektów.</p>"),
                    o$nTreat, o$nTreat),
                splitplot = sprintf(paste0("<p>Split-plot: liczba bloków %d; w każdym bloku ",
                    "obiekty czynnika A (liczba poziomów: %d) rozlosowano na dużych poletkach, ",
                    "a w obrębie każdego dużego poletka poziomy czynnika B (liczba poziomów: %d) ",
                    "na małych poletkach; liczba małych poletek: %d.</p>"), o$nRep, o$nTreat, o$nSub, nrow(df)))
            self$results$info$setContent(paste0(desc,
                sprintf("<p>Ziarno losowania: %d (ten sam plan przy tym samym ziarnie).</p>", o$seed)))
            self$results$map$setState(list(plan = df, design = design,
                rowLabel = if (hasBlock) "Blok" else "Wiersz",
                aLabel = "Obiekt A"))
        },
        .planPlot = function(image, ggtheme, theme, ...) planPlot(image, ggtheme, theme)
    )
)
