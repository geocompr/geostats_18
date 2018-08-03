# Filename: intro_rqgis.R (2017-08-08)
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
# 2. INTERSECTION USING (R)QGIS
# 3. YOUR TURN
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
# just in case let's install the developer version
devtools::install_github("jannes-m/RQGIS")
library("RQGIS")
library("sf")
library("raster")
library("mapview")

# create two polygons for a toy example
coords_1 <-  
  matrix(data = c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
         ncol = 2, byrow = TRUE)
coords_2 <-
  matrix(data = c(-0.5, -0.5, 0.5, -0.5, 0.5, 0.5, 
                  -0.5, 0.5, -0.5, -0.5),
         ncol = 2, byrow = TRUE)

# create the first polygon
poly_1 <- st_polygon(list((coords_1))) 
class(poly_1)
# convert it into a simple feature collection 
poly_1 <- st_sfc(poly_1)
# you could als add a a coordinate reference
# poly_1 <- st_sfc(poly_1, crs = 4326)
class(poly_1)
# finally, convert it into an sf-object
poly_1 <- st_sf(geometry = poly_1)
# you could also add attribute data
# st_sf(data.frame(id = 1, name = "poly_1"), geometry = poly_1)

# create a second polygon
poly_2 <- st_polygon(list((coords_2))) %>%
  st_sfc %>%
  st_sf(geometry = .)
# visualize it
plot(poly_1, xlim = c(-1, 1), ylim = c(-1, 1))
plot(poly_2, add = TRUE)

#**********************************************************
# 2 INTERSECTION USING RQGIS-------------------------------
#**********************************************************

# set_env tries to find automatically your QGIS installation (this might take a while)
# But it caches its output so it only takes long for one time.
set_env()
# You can also indicate the path to your QGIS installation (much faster), in my
# case:
# set_env("C:/OSGeo4W64/", dev = TRUE)

# open_app establishes the Python tunnel
open_app()
# you don't have to run it explicitly, all subsequent RQGIS functions will check
# if a Python tunnel was established, and if not it will open one

# find_algorithms lets you find out about the available geoalgorithms
algs <- find_algorithms()
length(algs)
# in my case, I have 940 geoalgorithms at my disposal
tail(algs)

# you can also use regular expressions with find_algorithms We are looking for a
# function that does an intersection, so maybe the term intersection will also
# appear in its name and/or short descriptions
# which function might do this for us
find_algorithms("intersec")
open_help("qgis:intersection")
get_usage("qgis:intersection")
# using R named arguments#
int <- run_qgis("qgis:intersection", INPUT = poly_1, INPUT2 = poly_2,
                OUTPUT = "out.shp",
                load_output = TRUE)

# we could also use a parameter-argument list to specify QGIS parameters
# get_args_man collects all function parameters and corresponding default values
params <- get_args_man("qgis:intersection")
# we can also use a path to a spatial object
st_write(poly_1, file.path(tempdir(), "poly_1.shp"))
# You can also geopackage GPKG
# st_write(poly_1, file.path(tempdir(), "poly_1.gpkg"))
params$INPUT <- file.path(tempdir(), "poly_1.shp")
# params$INPUT <- file.path(tempdir(), "poly_1.gpkg")
params$INPUT2 <- poly_2
params$OUTPUT <- "out.shp"
int <- run_qgis("qgis:intersection", params = params, load_output = TRUE)
# visualize it
plot(poly_1, xlim = c(-1, 1), ylim = c(-1, 1))
plot(poly_2, add = TRUE)
plot(int, col = "lightblue", add = TRUE)

#**********************************************************
# 3 YOUR TURN----------------------------------------------
#**********************************************************

# 3.1 SAGA wetness index===================================
#**********************************************************

# Calculate the SAGA wetness index of `data(dem)` using RQGIS. If you are faster
# than the others or if you have trouble using SAGA, calculate the slope, the
# aspect (and the curvatures) of `data(dem)` using GRASS through RQGIS.



# 3.2 Intersecting using sf, SAGA and GRASS================
#**********************************************************

# If you are faster than the others, calculate the intersection of poly_1 and
# poly_2 with the help of sf, SAGA and/or GRASS (hint: overlay and
# open_help).

