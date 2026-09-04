# Wspolny mechanizm opisu metod jUPWR (R/metody.R)
context('metody')

test_that("argumenty tekstowe sa escapowane, fmt nie", {
    m <- metodyNew()
    m$add("Dane", "Wiersze: „%s”, <i>N</i> = %d.", "a<b", 10L)
    h <- m$html()
    expect_true(grepl("a&lt;b", h, fixed = TRUE))
    expect_true(grepl("<i>N</i> = 10", h, fixed = TRUE))
    expect_equal(m$length(), 1)
})

test_that("addIf pomija, sekcje ida w kanonicznej kolejnosci, nieznane przed Wykres", {
    m <- metodyNew()
    m$add("Wykres", "w")
    m$add("Post-hoc", "p")
    m$addIf(FALSE, "Testy", "nie ma")
    m$add("Testy", "t")
    m$add("Miary 2×2", "x")
    m$add("Dane", "d")
    h <- m$html()
    pos <- function(s) regexpr(paste0("<b>", s, "</b>"), h, fixed = TRUE)
    expect_true(pos("Dane") < pos("Testy") && pos("Testy") < pos("Post-hoc"))
    expect_true(pos("Post-hoc") < pos("Miary 2×2") && pos("Miary 2×2") < pos("Wykres"))
    expect_false(grepl("nie ma", h))
})

test_that("pusty opis daje pusty lancuch, metodyCyt cytuje liste", {
    expect_identical(metodyNew()$html(), "")
    expect_identical(metodyCyt(c("a", "b")), "„a”, „b”")
})
