# Helpers writing the "Wyniki" table. Results are stored as numbers, not as
# text ("P = 0.632"): the number format and the decimal symbol chosen in the
# app apply to number cells only, and a rounded text would hide small
# probabilities ("P = 0").

# A whole number within the integer range becomes an integer, which is shown
# as it is (quantiles of discrete distributions: 7, not 7.000)
wholeAsInteger <- function(x) {
    if (is.finite(x) && x == round(x) && abs(x) <= .Machine$integer.max)
        as.integer(x)
    else
        x
}

# Writes the probability and the quantile(s). `q2` (upper bound of the
# central interval) goes to a second row. Arguments left NULL are skipped.
# The table of a distribution with a single quantile has no label column.
fillOutputs <- function(outputs, prob = NULL, q1 = NULL, q2 = NULL, discrete = FALSE) {
    cols <- names(outputs$columns)
    conv <- if (discrete) wholeAsInteger else identity
    first <- list()
    if (!is.null(prob))
        first$DistributionResultColumn <- prob
    if (!is.null(q1)) {
        first$QuantileResultColumn <- conv(q1)
        if ("QuantileLabelColumn" %in% cols)
            first$QuantileLabelColumn <- "x1"
    }
    if (length(first) > 0)
        outputs$setRow(rowNo = 1, values = first)
    if (!is.null(q2)) {
        second <- list(DistributionResultColumn = "",
                       QuantileLabelColumn = "x2",
                       QuantileResultColumn = conv(q2))
        # the event of the first row has no counterpart here
        for (col in intersect(c("EmpiricalResultColumn", "EmpiricalCountColumn"), cols))
            second[[col]] <- ""
        outputs$addRow(rowKey = 2, values = second)
    }
}

# Writes the relative frequency of the event in the data next to its
# probability, with the count ("12 z 30") in a column of its own; `emp` is
# the result of empEventFreq(). NULL hides both columns.
fillOutputsEmpirical <- function(outputs, emp = NULL) {
    if (is.null(emp)) {
        outputs$getColumn("EmpiricalResultColumn")$setVisible(FALSE)
        outputs$getColumn("EmpiricalCountColumn")$setVisible(FALSE)
    } else {
        outputs$setRow(rowNo = 1, values = list(
            EmpiricalResultColumn = emp$freq,
            EmpiricalCountColumn = emp$count))
    }
}
