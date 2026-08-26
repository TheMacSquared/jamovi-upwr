# Generates data/stacje_pomiarowe.csv — synthetic teaching dataset: ground
# measurement stations around European cities. City coordinates are public
# facts (cross-checked with Natural Earth populated places, public domain);
# the yearly-mean measurements are simulated with a fixed seed so that
# temperature correlates with latitude (teaching: correlation/regression on
# geographic data). 3 stations per city, jittered around the city centre.

miasta <- read.csv(text = 'miasto,kraj,lon,lat
Lizbona,Portugalia,-9.14,38.72
Madryt,Hiszpania,-3.70,40.42
Barcelona,Hiszpania,2.17,41.39
Sewilla,Hiszpania,-5.98,37.39
Porto,Portugalia,-8.61,41.15
Paryz,Francja,2.35,48.86
Marsylia,Francja,5.37,43.30
Lyon,Francja,4.84,45.76
Tuluza,Francja,1.44,43.60
Londyn,Wielka Brytania,-0.13,51.51
Manchester,Wielka Brytania,-2.24,53.48
Edynburg,Wielka Brytania,-3.19,55.95
Dublin,Irlandia,-6.26,53.35
Bruksela,Belgia,4.35,50.85
Amsterdam,Holandia,4.90,52.37
Berlin,Niemcy,13.40,52.52
Monachium,Niemcy,11.58,48.14
Hamburg,Niemcy,9.99,53.55
Kolonia,Niemcy,6.96,50.94
Zurych,Szwajcaria,8.54,47.37
Wieden,Austria,16.37,48.21
Praga,Czechy,14.44,50.08
Warszawa,Polska,21.01,52.23
Krakow,Polska,19.94,50.06
Wroclaw,Polska,17.03,51.11
Gdansk,Polska,18.65,54.35
Budapeszt,Wegry,19.04,47.50
Bukareszt,Rumunia,26.10,44.43
Sofia,Bulgaria,23.32,42.70
Belgrad,Serbia,20.46,44.79
Zagrzeb,Chorwacja,15.98,45.81
Rzym,Wlochy,12.50,41.90
Mediolan,Wlochy,9.19,45.46
Neapol,Wlochy,14.27,40.85
Ateny,Grecja,23.73,37.98
Saloniki,Grecja,22.94,40.64
Sztokholm,Szwecja,18.07,59.33
Goteborg,Szwecja,11.97,57.71
Oslo,Norwegia,10.75,59.91
Kopenhaga,Dania,12.57,55.68
Helsinki,Finlandia,24.94,60.17
Tallinn,Estonia,24.75,59.44
Ryga,Lotwa,24.11,56.95
Wilno,Litwa,25.28,54.69
Kijow,Ukraina,30.52,50.45
Lwow,Ukraina,24.03,49.84
Stambul,Turcja,28.98,41.01
Ankara,Turcja,32.86,39.93
Reykjavik,Islandia,-21.94,64.15
Trondheim,Norwegia,10.40,63.43',
    stringsAsFactors = FALSE)

set.seed(2026)
naStacje <- 3
n <- nrow(miasta) * naStacje

d <- miasta[rep(seq_len(nrow(miasta)), each = naStacje), ]
d$stacja <- paste0(d$miasto, "-", rep(seq_len(naStacje), nrow(miasta)))
d$lon <- round(d$lon + runif(n, -0.25, 0.25), 3)
d$lat <- round(d$lat + runif(n, -0.2, 0.2), 3)

# yearly mean temperature falls with latitude (~0.6 deg C per degree lat)
d$temperatura <- round(38 - 0.6 * d$lat + rnorm(n, 0, 1.3), 1)
# aerosol optical depth: mild south-north gradient plus noise
d$aod <- round(pmax(0.03, 0.35 - 0.004 * d$lat + rnorm(n, 0, 0.05)), 3)
# mean NDVI: peaks at temperate latitudes, lower in far south/north
d$ndvi_sredni <- round(pmax(0.05, pmin(0.85,
    0.75 - 0.0022 * (d$lat - 50)^2 + rnorm(n, 0, 0.06))), 3)

d <- d[, c("stacja", "miasto", "kraj", "lon", "lat",
           "temperatura", "aod", "ndvi_sredni")]
write.csv(d, file.path("..", "data", "stacje_pomiarowe.csv"),
          row.names = FALSE, quote = TRUE)
cat(sprintf("stacje_pomiarowe.csv: %d stacji w %d miastach\n",
            nrow(d), nrow(miasta)))
