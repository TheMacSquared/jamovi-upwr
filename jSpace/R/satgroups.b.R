#' @importFrom jmvcore .
satgroupsClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "satgroupsClass",
    inherit = satgroupsBase,
    private = list(
        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            dep <- self$options$dep
            x <- jmvcore::toNumeric(self$data[[dep]])
            grp <- as.factor(self$data[[self$options$group]])

            validIdx <- !is.na(x) & !is.na(grp)
            x <- x[validIdx]
            grp <- droplevels(grp[validIdx])

            if (length(x) < 3 || nlevels(grp) < 2) {
                self$results$desc$setNote("err",
                    "Potrzebne co najmniej 2 grupy i 3 obserwacje.")
                return()
            }

            for (g in levels(grp)) {
                xg <- x[grp == g]
                self$results$desc$addRow(rowKey = g, values = list(
                    grp = g,
                    n = length(xg),
                    mean = mean(xg),
                    sd = if (length(xg) > 1) sd(xg) else NA,
                    median = median(xg),
                    iqr = IQR(xg)
                ))
            }

            if (self$options$testAnova) {
                fit <- aov(x ~ grp)
                sm <- summary(fit)[[1]]
                ss <- sm[["Sum Sq"]]
                eta2 <- ss[1] / sum(ss)
                self$results$tests$addRow(rowKey = "anova", values = list(
                    test = "ANOVA (F)",
                    stat = sm[["F value"]][1],
                    df1 = sm[["Df"]][1],
                    df2 = sm[["Df"]][2],
                    p = sm[["Pr(>F)"]][1],
                    es = eta2
                ))
            }

            if (self$options$testKruskal) {
                kw <- kruskal.test(x ~ grp)
                n <- length(x)
                # epsilon squared: H / ((n^2 - 1) / (n + 1))
                eps2 <- unname(kw$statistic) / ((n^2 - 1) / (n + 1))
                self$results$tests$addRow(rowKey = "kruskal", values = list(
                    test = "Kruskal-Wallis (chi-kwadrat)",
                    stat = unname(kw$statistic),
                    df1 = unname(kw$parameter),
                    df2 = NA,
                    p = kw$p.value,
                    es = eps2
                ))
            }

            if (self$options$testAnova || self$options$testKruskal)
                self$results$tests$setNote("es",
                    "Wielkosc efektu: eta-kwadrat (ANOVA), epsilon-kwadrat (Kruskal-Wallis)")

            self$results$plot$setState(list(
                label = dep,
                x = x,
                grp = as.character(grp)
            ))
        },
        .boxPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            s <- image$state
            buildGroupBoxPlot(s$label, s$x, s$grp,
                              plotMeans = self$options$plotMeans,
                              ggtheme = ggtheme, theme = theme)
        }
    )
)
