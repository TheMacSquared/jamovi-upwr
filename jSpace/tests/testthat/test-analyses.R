# Integration tests calling the compiled analyses directly (jmc-generated
# wrappers return analysis$results). They run only when the compiled jSpace
# package is installed. Datasets come from the module's data/ directory.

skip_if_not_installed("jSpace")

daneSciezka <- function(plik) {
  # data/ CSVs are packaged at the top level of the installed module dir
  # when built by jmc; during plain R CMD check they sit in the source tree
  kandydaci <- c(
    system.file("data", plik, package = "jSpace"),
    file.path("..", "..", "data", plik))
  for (k in kandydaci)
    if (nzchar(k) && file.exists(k))
      return(k)
  skip(paste("brak pliku danych:", plik))
}

test_that("satgroups: descriptives and tests on the satellite data", {
  d <- read.csv(daneSciezka("satelity.csv"))
  d$konstelacja <- factor(d$konstelacja)
  res <- jSpace::satgroups(data = d, dep = "wysokosc_km",
                           group = "konstelacja", testKruskal = TRUE)

  desc <- res$desc$asDF
  expect_equal(nrow(desc), nlevels(d$konstelacja))
  expect_equal(sum(desc$n), nrow(d))

  gps <- desc[desc$grp == "GPS", ]
  expect_equal(gps$mean, mean(d$wysokosc_km[d$konstelacja == "GPS"]),
               tolerance = 1e-9)

  testy <- res$tests$asDF
  expect_equal(nrow(testy), 2)
  expect_equal(testy$df1[1], nlevels(d$konstelacja) - 1)
  expect_true(all(testy$p < 0.001))
  expect_true(all(testy$es > 0 & testy$es <= 1))
})

test_that("tleorbit: ISS propagation gives a sane LEO orbit", {
  res <- jSpace::tleorbit(satelita = "iss", czasProp = 3, krok = 2)

  el <- res$elementy$asDF
  okres <- el$wartosc[el$parametr == "Okres orbitalny"]
  expect_true(okres > 88 && okres < 96)

  st <- res$staty$asDF
  wys <- st[st$wielkosc == "Wysokosc [km]", ]
  expect_true(wys$min > 350 && wys$max < 500)
  pred <- st[st$wielkosc == "Predkosc [km/s]", ]
  expect_true(pred$mean > 7 && pred$mean < 8)
})

test_that("tleorbit: GEO satellite runs through the SDP4 branch", {
  res <- jSpace::tleorbit(satelita = "goes16", czasProp = 6, krok = 10)
  st <- res$staty$asDF
  wys <- st[st$wielkosc == "Wysokosc [km]", ]
  expect_true(abs(wys$mean - 35786) < 50)
})

test_that("tleorbit: custom TLE input works and bad input is caught", {
  sats <- jSpace::wczytajPrzykladyTLE()
  iss <- sats[["ISS (ZARYA)"]]
  res <- jSpace::tleorbit(zrodlo = "wlasne", tle1 = iss[2], tle2 = iss[3],
                          czasProp = 1, krok = 5)
  expect_true(nrow(res$elementy$asDF) == 5)

  res <- jSpace::tleorbit(zrodlo = "wlasne", tle1 = "za krotkie",
                          tle2 = "tez", czasProp = 1, krok = 5)
  expect_equal(nrow(res$elementy$asDF), 0)
})

test_that("geomap: spatial join, regional stats and correlation", {
  d <- read.csv(daneSciezka("stacje_pomiarowe.csv"))
  res <- jSpace::geomap(data = d, lon = "lon", lat = "lat",
                        wartosc = "temperatura", grupa = NULL,
                        korelacja = TRUE)

  reg <- res$regiony$asDF
  expect_equal(sum(reg$n), nrow(d))
  expect_true("Poland" %in% reg$panstwo)
  # 4 Polish cities x 3 stations, minus whatever jitter pushed out
  expect_true(reg$n[reg$panstwo == "Poland"] >= 9)

  kor <- res$kor$asDF
  expect_true(kor$r < -0.9)   # temperature falls with latitude by design
  expect_true(kor$p < 1e-6)
  expect_equal(kor$df, sum(!is.na(d$temperatura)) - 2)
})

test_that("rasterstats: matches terra::global on the bundled raster", {
  res <- jSpace::rasterstats(xKol = NULL, yKol = NULL, wartoscKol = NULL,
                             prog = 0.4)
  st <- res$staty$asDF

  r <- terra::rast(system.file("extdata", "ndvi_wroclaw.tif",
                               package = "jSpace"))
  w <- terra::values(r)[, 1]
  expect_equal(st$n, length(w))
  expect_equal(st$mean, mean(w), tolerance = 1e-9)
  expect_equal(st$sd, sd(w), tolerance = 1e-9)
  expect_equal(st$median, median(w), tolerance = 1e-9)

  pr <- res$progTab$asDF
  expect_equal(pr$n, sum(w > 0.4))
  expect_equal(pr$procent, sum(w > 0.4) / length(w), tolerance = 1e-9)
})

test_that("rasterstats: x/y/value spreadsheet columns give the same stats", {
  d <- read.csv(daneSciezka("ndvi_tabela.csv"))
  res <- jSpace::rasterstats(data = d, zrodlo = "kolumny",
                             xKol = "x", yKol = "y", wartoscKol = "ndvi")
  st <- res$staty$asDF
  expect_equal(st$n, nrow(d))
  expect_equal(st$mean, mean(d$ndvi), tolerance = 1e-6)
})

test_that("satclassify: kNN on the Landsat samples is accurate and stable", {
  d <- read.csv(daneSciezka("landsat_probki.csv"))
  d$klasa <- factor(d$klasa)
  res <- jSpace::satclassify(
      data = d, klasa = "klasa",
      predyktory = c("zielony", "czerwony", "nir1", "nir2"),
      metoda = "knn", k = 5, ziarno = 1)

  pod <- res$podzial$asDF
  expect_equal(sum(pod$n), nrow(d))

  mac <- res$macierz$asDF
  nTest <- sum(mac[, -1])
  expect_equal(nTest, pod$n[pod$zbior == "Testowy"])
  trafne <- sum(diag(as.matrix(mac[, -1])))
  expect_true(trafne / nTest > 0.8)

  # deterministic given the same seed
  res2 <- jSpace::satclassify(
      data = d, klasa = "klasa",
      predyktory = c("zielony", "czerwony", "nir1", "nir2"),
      metoda = "knn", k = 5, ziarno = 1)
  expect_equal(res2$macierz$asDF, mac)
})

test_that("satclassify: decision tree fills the rules output", {
  d <- read.csv(daneSciezka("landsat_probki.csv"))
  d$klasa <- factor(d$klasa)
  res <- jSpace::satclassify(data = d, klasa = "klasa",
                             predyktory = c("ndvi", "zielony"),
                             metoda = "drzewo")
  expect_match(res$reguly$content, "node")
  mac <- res$macierz$asDF
  trafne <- sum(diag(as.matrix(mac[, -1])))
  expect_true(trafne / sum(mac[, -1]) > 0.5)
})
