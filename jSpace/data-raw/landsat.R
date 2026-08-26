# Generates data/landsat_probki.csv from the UCI Statlog (Landsat Satellite)
# dataset: 3x3 pixel neighbourhoods from Landsat MSS imagery, 4 spectral bands
# each. We keep the 4 bands of the CENTRAL pixel (attributes 17-20), compute
# NDVI and translate the land-cover classes to Polish. Deterministic subsample
# (fixed seed) of 1500 rows.
# Source: Srinivasan, A. (1993). Statlog (Landsat Satellite). UCI Machine
# Learning Repository, doi:10.24432/C55887. License: CC BY 4.0.

src <- "sat.trn"
if (!file.exists(src))
    download.file(paste0("https://archive.ics.uci.edu/ml/",
                         "machine-learning-databases/statlog/satimage/sat.trn"),
                  src, quiet = TRUE)

d <- read.table(src)

# central pixel of the 3x3 neighbourhood: attributes 17-20
# bands: 0.55-0.61um (green), 0.61-0.68um (red), 0.72-0.90um (NIR),
# 0.80-1.10um (NIR)
d <- data.frame(
    zielony = d[[17]],
    czerwony = d[[18]],
    nir1 = d[[19]],
    nir2 = d[[20]],
    klasa = d[[37]]
)

d$ndvi <- round((d$nir1 - d$czerwony) / (d$nir1 + d$czerwony), 3)

klasy <- c(`1` = "gleba czerwona",
           `2` = "uprawa bawelny",
           `3` = "gleba szara",
           `4` = "gleba szara wilgotna",
           `5` = "roslinnosc na sciernisku",
           `7` = "gleba szara bardzo wilgotna")
d$klasa <- klasy[as.character(d$klasa)]

set.seed(2026)
d <- d[sort(sample(nrow(d), 1500)), c("zielony", "czerwony", "nir1", "nir2",
                                      "ndvi", "klasa")]

write.csv(d, file.path("..", "data", "landsat_probki.csv"),
          row.names = FALSE, quote = TRUE)
cat(sprintf("landsat_probki.csv: %d wierszy\n", nrow(d)))
print(table(d$klasa))
