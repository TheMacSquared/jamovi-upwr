# Generates inst/extdata/ndvi_wroclaw.tif and data/ndvi_tabela.csv — a
# SIMULATED NDVI raster of the Wroclaw area (200x200 cells, EPSG:4326).
# Synthetic on purpose: real Sentinel-2 downloads require Copernicus Data
# Space authentication, and a simulated field keeps the module fully
# reproducible from this script alone. The spatial structure mimics reality:
# a smooth vegetation field, a low-NDVI urban core, and a river band.
# Deterministic (fixed seed).

library(terra)

set.seed(2026)
n <- 200
lonZakres <- c(16.85, 17.25)
latZakres <- c(50.98, 51.24)

# smooth random field: coarse Gaussian noise upscaled with bilinear smoothing
gruby <- rast(nrows = 20, ncols = 20, xmin = lonZakres[1], xmax = lonZakres[2],
              ymin = latZakres[1], ymax = latZakres[2], crs = "EPSG:4326")
values(gruby) <- rnorm(ncell(gruby), 0, 0.12)
pole <- rast(nrows = n, ncols = n, extent = ext(gruby), crs = "EPSG:4326")
pole <- resample(gruby, pole, method = "cubicspline")

xy <- xyFromCell(pole, seq_len(ncell(pole)))
lon <- xy[, 1]; lat <- xy[, 2]

# base vegetation level + smooth field
ndvi <- 0.55 + values(pole)[, 1]

# urban core of Wroclaw (centre ~17.03E, 51.11N): NDVI drops towards centre
dMiasto <- sqrt(((lon - 17.03) * cos(51.11 * pi / 180))^2 + (lat - 51.11)^2)
ndvi <- ndvi - 0.38 * exp(-(dMiasto / 0.045)^2)

# Odra river: gentle SE-NW band through the city, water has NDVI near zero
dRzeka <- abs((lat - 51.11) - 0.55 * (lon - 17.03))
ndvi <- ifelse(dRzeka < 0.006, pmin(ndvi, 0.03 + rnorm(length(ndvi), 0, 0.02)),
               ndvi)

ndvi <- pmax(-0.1, pmin(0.95, ndvi + rnorm(length(ndvi), 0, 0.02)))

r <- rast(nrows = n, ncols = n, xmin = lonZakres[1], xmax = lonZakres[2],
          ymin = latZakres[1], ymax = latZakres[2], crs = "EPSG:4326",
          vals = round(ndvi, 3))
names(r) <- "ndvi"

outTif <- file.path("..", "inst", "extdata", "ndvi_wroclaw.tif")
writeRaster(r, outTif, overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"), datatype = "FLT4S")

tab <- as.data.frame(r, xy = TRUE)
names(tab) <- c("x", "y", "ndvi")
tab$x <- round(tab$x, 5)
tab$y <- round(tab$y, 5)
write.csv(tab, file.path("..", "data", "ndvi_tabela.csv"),
          row.names = FALSE, quote = FALSE)

cat(sprintf("ndvi_wroclaw.tif: %d x %d, %.0f kB; ndvi_tabela.csv: %d wierszy\n",
            nrow(r), ncol(r), file.size(outTif) / 1024, nrow(tab)))
