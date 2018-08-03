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
# 3. INTERSECTION USING sf, SAGA, GRASS
# 4. SAGA WETNESS INDEX
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RQGIS")
library("sf")
library("raster")
library("mapview")
library("dplyr")

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

# first of all, we need to find out which function might do this for us
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
params$INPUT <- file.path(tempdir(), "poly_1.shp")
params$INPUT2 <- poly_2
params$OUTPUT <- "out.shp"
int <- run_qgis("qgis:intersection", params = params, load_output = TRUE)
# visualize it
plot(poly_1, xlim = c(-1, 1), ylim = c(-1, 1))
plot(poly_2, add = TRUE)
plot(int, col = "blue", add = TRUE)

#**********************************************************
# 3 SAGA WETNESS INDEX-------------------------------------
#**********************************************************

# 3.1 Using RQGIS==========================================
#**********************************************************
# load a raster
data(dem, package = "RQGIS")

find_algorithms("wetness")
alg <- "saga:sagawetnessindex"
get_usage(alg)
get_args_man(alg)
twi <- run_qgis(alg, DEM = dem, TWI = "twi.tif", load_output = TRUE)
plot(twi, col = RColorBrewer::brewer.pal(n = 9, name = "Blues"))
# or using mapview
# proj4string(twi) = paste0("+proj=utm +zone=17 +south +ellps=WGS84 +towgs84=",
#                           "0,0,0,0,0,0,0 +units=m +no_defs")
mapview(twi, col.regions = RColorBrewer::brewer.pal(n = 9, "Blues"),
        at = seq(cellStats(twi, "min") - 0.01, cellStats(twi, "max") + 0.01,
                 length.out = 9))

# 3.2 Using RSAGA==========================================
#**********************************************************
library("RSAGA")
# adding SAGA 2.3.2 to PATH -> will not work since unsupported by RSAGA
# some SAGA > 2.2.2 versions might sometimes work with RSAGA
# Sys.setenv(PATH = paste0(Sys.getenv("PATH"),  "C:\\OSGeo4W64\\apps\\saga-ltr;"))
# adding SAGA 2.1.2 to PATH (supported by SAGA)
Sys.setenv(PATH = paste0(Sys.getenv("PATH"),  "C:\\OSGeo4W64\\apps\\saga;"))
# check if rsaga.env can find the corresponding SAGA installation
rsaga.env()
rsaga.get.libraries()
rsaga.get.modules(libs = "ta_hydrology")
rsaga.get.usage(lib = "ta_hydrology", module = "SAGA Wetness Index")
raster::writeRaster(dem, filename = file.path(tempdir(), "dem.sdat"), 
                    format = "SAGA")
params = list(DEM = file.path(tempdir(), "dem.sgrd"),
              TWI = file.path(tempdir(), "twi.sdat"))
rsaga.geoprocessor(lib = "ta_hydrology", module = "SAGA Wetness Index", 
                   param = params)
# shortcut version
# rsaga.wetness.index(in.dem = file.path(tempdir(), "dem.sgrd"), 
#                     out.wetness.index = file.path(tempdir(), "twi"))
twi_saga <- raster(file.path(tempdir(), "twi.sdat"))

#**********************************************************
# 4 INTERSECTION USING sf, SAGA, GRASS---------------------
#**********************************************************

# 4.1 Using sf=============================================
#**********************************************************
# sf uses geos, a geospatial library, in the background
plot(poly_1, xlim = c(-1, 1), ylim = c(-1, 1))
plot(poly_2, add = TRUE)
plot(sf::st_intersection(poly_1, poly_2), add = TRUE, col = "red")
# rgeos::gIntersection has been slower compared to e.g., SAGA's intersection
# algorithm. However, Edzer has added a spatial index to geometry functions
# (e.g., st_intersection), so maybe sf is now as fast or even faster than
# SAGA... 
# for more information on spatial indexes, visit:
# browseURL("http://r-spatial.org//r/2017/06/22/spatial-index.html")

# 4.2 Using SAGA through RQGIS=============================
#**********************************************************
find_algorithms("intersect")
# sometimes the help page is not that helpful...
# open_help("saga:intersect")
get_usage("saga:intersect")
# get_args_man("saga:intersect")
int_2 <- run_qgis("saga:intersect", A = poly_1, B = poly_2, 
                  RESULT = "out_saga.shp", load_output = TRUE)

# 4.3 Using GRASS through RQGIS============================
#**********************************************************
find_algorithms("overlay")
get_usage("grass7:v.overlay")
# to find out the defaults, use get_args_man
# get_args_man("grass7:v.overlay")
int_3 <- run_qgis("grass7:v.overlay", ainput = poly_1, binput = poly_2,
                  output = "out_grass.shp", load_output = TRUE)

# 4.4 Using GRASS through rgrass7==========================
#**********************************************************
library("rgrass7")

# assign a CRS and add some attribute data, otherwise writeVECT will complain 
# about an unknown data type
st_crs(poly_1) <- 4326
poly_1$id <- 1
# slightly different syntax
poly_2 <- sf::st_set_crs(poly_2, 4326) %>%
  mutate(id = 1)

# use link2GI interactively to initialize GRASS choose a GRASS 7 version, e.g.,
# 7.2.1 (only necessary if you have installed more than one GRASS installation)
link2GI::linkGRASS7(st_union(poly_1, poly_2), ver_select = TRUE)

# # or manually
# # indicate path to the GRASS installation on your computer
# grass_path <- "C:/OSGeo4W64/apps/grass/grass-7.2.1"
# # next line of code only necessary if we want to use GRASS as installed by
# # OSGeo4W. Among others, open_app adds some paths to PATH, which are also needed
# # for running GRASS.
# # link2GI::getparams_GRASS4W() # would do more or less the same but is less handy
# try(RQGIS::open_app())
# initGRASS(gisBase = grass_path,
#           # home parameter necessary under UNIX-based systems
# #          home = tempdir(),
#           gisDbase = tempdir(),
#           location = "user1",
#           mapset = "PERMANENT", override = TRUE)
# # default region settings
# # system("C:/OSGeo4W64/apps/grass/grass-7.2.1/bin/g.region.exe -dp")
# # next we need to define the extent, the projection, and possible the resolution
# execGRASS("g.proj", flags = c("c", "quiet"),
#           proj4 = sf::st_crs(poly_1)$proj4string)
# b_box <-  st_bbox(st_union(poly_1, poly_2))
# execGRASS("g.region", flags = c("quiet"),
#           n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]),
#           e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]),
#           res = "1")

# let's have a look at the help of v.overlay via rgrass7
execGRASS("g.manual", entry = "v.overlay")
# RQGIS::open_help("grass7:v.overlay")
writeVECT(as(poly_1, "Spatial"), vname = "poly_1")
writeVECT(as(poly_2, "Spatial"), vname = "poly_2")
execGRASS("v.overlay", ainput = "poly_1", binput = "poly_2",
          output = "out_grass", operator = "and", flag = "overwrite")
out_grass <- readVECT("out_grass")
plot(st_geometry(poly_1), xlim = c(-1, 1), ylim = c(-1, 1))
plot(st_geometry(poly_2), add = TRUE)
plot(out_grass, add = TRUE, col = "lightblue")

#**********************************************************
# 5 VIEWSHED ANALYSIS--------------------------------------
#**********************************************************
data("dem")
data("random_points")

find_algorithms("viewshed")
alg <- "grass7:r.viewshed"
get_usage(alg)
open_help(alg)
# let's find out about the default values
get_args_man(alg)
point <- random_points[sample(1:nrow(random_points), 1), ]
coord <- paste(sf::st_coordinates(point), collapse = ",")
out <- run_qgis(alg, input = dem, coordinates = coord,
                output = "out.tif", load_output = TRUE)

# under Linux you might not get an output (I think we should file a bug...)
# so let's use rgrass7
# library("rgrass7")
# link2GI::linkGRASS7(dem, ver_select = TRUE)
# writeRAST(as(dem, "SpatialGridDataFrame"), "dem")
# writeVECT(as(random_points, "Spatial"), vname = "points")
# execGRASS("r.viewshed", input = "dem", coordinates = sf::st_coordinates(point), 
#           output = "view")
# out <- raster(readRAST("view"))

hs <- hillShade(terrain(dem), terrain(dem, "aspect"), 40, 270)
plot(hs, col = gray(0:100 / 100), legend = FALSE)
plot(dem, add = TRUE, alpha = 0.5, legend = FALSE)
plot(point, add = TRUE, col = "red", pch = 16)
plot(out, add = TRUE, col = "lightgray", legend = FALSE)
plot(point, add = TRUE, col = "red", pch = 16)

# or using mapview
mapview(out, col = "white", map.type = "Esri.WorldImagery") +
  mapview(point)
