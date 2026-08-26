# Unit tests for the shared helpers (utils.R). They run against the compiled
# package, mirroring the jRISK test layout.

skip_if_not_installed("jSpace")

test_that("gmstStopnie matches the J2000 reference value", {
  # by definition GMST at JD 2451545.0 (2000-01-01 12:00 UTC) is 280.46062 deg
  expect_equal(jSpace::gmstStopnie("2000-01-01 12:00:00"),
               280.46061837 %% 360, tolerance = 1e-6)
})

test_that("temeNaGeo returns sane geographic coordinates", {
  # a point on the inertial x axis lies on the equator
  g <- jSpace::temeNaGeo(c(7000, 0, 0), "2026-01-01 00:00:00")
  expect_equal(unname(g["lat"]), 0, tolerance = 1e-9)
  expect_equal(unname(g["wysokosc"]), 7000 - 6371, tolerance = 1e-9)
  expect_true(g["lon"] >= -180 && g["lon"] <= 180)

  # a point on the z axis is the north pole
  g <- jSpace::temeNaGeo(c(0, 0, 7000), "2026-01-01 00:00:00")
  expect_equal(unname(g["lat"]), 90, tolerance = 1e-9)
})

test_that("granicePanstw loads the bundled country borders", {
  kraje <- jSpace::granicePanstw()
  expect_s3_class(kraje, "sf")
  expect_true(nrow(kraje) > 150)
  expect_true(all(c("panstwo", "iso3", "kontynent") %in% names(kraje)))
  expect_true("Poland" %in% kraje$panstwo)
})

test_that("wczytajPrzykladyTLE returns complete TLE sets", {
  sats <- jSpace::wczytajPrzykladyTLE()
  expect_equal(length(sats), 8)
  expect_true("ISS (ZARYA)" %in% names(sats))
  for (s in sats) {
    expect_equal(length(s), 3)
    expect_match(s[2], "^1 ")
    expect_match(s[3], "^2 ")
  }
})

test_that("plot builders return valid ggplot objects", {
  motyw <- list(color = c("#333333", "#1272bd"),
                fill = c("#FFFFFF", "#dddddd"))
  p1 <- jSpace::buildGroupBoxPlot(
      "y", rnorm(30), rep(c("A", "B", "C"), 10),
      plotMeans = TRUE, ggtheme = ggplot2::theme_minimal(), theme = motyw)
  expect_s3_class(p1, "ggplot")
  expect_silent(ggplot2::ggplot_build(p1))

  p2 <- jSpace::buildMapaBazowa(
      list(lon = c(-25, 45), lat = c(34, 72)), theme = motyw)
  expect_s3_class(p2, "ggplot")
  expect_silent(ggplot2::ggplot_build(p2))
})
