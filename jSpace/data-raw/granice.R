# Generates inst/extdata/kraje_ne110.rds — country borders used as the map
# backdrop in jSpace analyses. Authoring-time only: rnaturalearth is NOT a
# runtime dependency; analyses read the .rds with readRDS(), so no GDAL
# vector driver is involved at runtime.
# Source: Natural Earth 1:110m Admin 0 countries (public domain).

library(rnaturalearth)

kraje <- ne_countries(scale = 110, returnclass = "sf")
kraje <- kraje[, c("name", "iso_a3", "continent", "geometry")]
names(kraje)[1:3] <- c("panstwo", "iso3", "kontynent")

# Do NOT st_make_valid here: the planar repair shreds polygons crossing the
# antimeridian (Russia, Fiji) into horizontal bands on the map. The polygons
# are only invalid on the S2 sphere, so runtime spatial joins simply switch
# to planar predicates (sf_use_s2(FALSE) in geomap.b.R).

out <- file.path("..", "inst", "extdata", "kraje_ne110.rds")
saveRDS(kraje, out, compress = "xz")
cat(sprintf("kraje_ne110.rds: %d panstw, %.0f kB\n",
            nrow(kraje), file.size(out) / 1024))
