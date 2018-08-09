# Filename: 01-mlr.R (2018-08-09)
#
# TO DO: Introduce RQGIS
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. RESPONSE-PREDICTOR MATRIX
# 3. MLR BUILDING BLOCKS (SPATIAL CV)
# 4. PREDICTIVE MAPPING
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("raster")
library("dplyr")
library("mlr")
library("vegan")

# attach data
random_points = readRDS("data/random_points.rds")
# site-species community matrix (plant species of Mt. Mongón)
data("comm", package = "RQGIS")
# raster stack (terrain attributes)
files = file.path("data", paste0(c("dem", "ndvi", "cslope", "carea"), ".tif"))
ta = stack(files)
ta$carea = log10(ta$carea)


# first create a response - the floristic gradient
pa = decostand(comm, "pa")
set.seed(25072018)
nmds = metaMDS(comm = pa, k = 4, try = 500)
nmds$stress
elev = dplyr::filter(random_points, id %in% rownames(pa)) %>%
  dplyr::pull(dem)
# rotating NMDS in accordance with altitude (proxy for humidity)
rotnmds = MDSrotate(nmds, elev)
# extracting the first two axes
sc = scores(rotnmds, choices = 1:2)
par(mfrow = c(1, 2))
# plotting the first axis against altitude
plot(y = sc[, 1], x = elev, xlab = "elevation in m",
     ylab = "First NMDS axis", cex.lab = 0.8, cex.axis = 0.8)
# extract the coordinates into a separate dataframe
coords = sf::st_coordinates(rp) %>%
  as.data.frame %>%
  rename(x = X, y = Y)
# only keep response and predictors which should be used for the modeling
rp = dplyr::select(rp, -id, -spri) %>%
  st_set_geometry(NULL)
