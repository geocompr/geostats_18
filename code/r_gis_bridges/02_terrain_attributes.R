# Filename: terrain_attributes.R (2018-08-09)
#
# TO DO: Create predictor rasters (terrain attributes) and extract values to
#        points
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. PREDICTOR RETRIEVAL
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RQGIS")
library("sf")
library("raster")
library("dplyr")

# attach data
data(random_points, package = "RQGIS")
data("dem", "ndvi", "study_area", package = "RQGIS")

#**********************************************************
# 2 RQGIS VARIABLE RETRIEVAL-------------------------------
#**********************************************************

# plot the randomly sampled points on a hillshade
hs = hillShade(terrain(dem), terrain(dem, "aspect"), 40, 270)
plot(hs, col = gray(0:100 / 100), legend = FALSE)
plot(dem, add = TRUE, alpha = 0.5)
plot(st_geometry(d), add = TRUE)

# Remove sinks, i.e. geometric artifacts, from the DEM

# have a look at how to use the sinkremoval algorithm
get_usage("saga:sinkremoval")
# have a look at the default values
get_args_man("saga:sinkremoval")
# define input DEM, method and output file
# METHOD 1: Fill sinks
# run the algorithm
run_qgis("saga:sinkremoval", DEM = dem, METHOD = 1,
         DEM_PREPROC = file.path("data/sdem.tif"))

# compute catchment slope and catchment area using sagawetnessindex
get_usage("saga:sagawetnessindex")
# just have a look at the possible options
get_options("saga:sagawetnessindex", qgis_env = qgis_env)
# AREA_TYPE 0 = absolute catchment area
# SLOPE_TYPE 1 = catchment slope
# have a look at the default values
get_args_man("saga:sagawetnessindex")
# compute catchment slope (instead of local slope (default))
# The catchment slope output grid of the Catchment Area (Parallel) module is
# computed like this: for each cell, the local slope is calculated using the
# approach of Zevenbergen & Thorne. These slope values are accumulated
# downslope. Finally, for each cell the accumulated slope values are divided by
# the derived catchment area of the cell. The unit of the grid are radians
run_qgis("saga:sagawetnessindex",
         DEM = dem,
         SLOPE_TYPE = 1,
         AREA_TYPE = 1,
         SLOPE = "data/cslope.tif",
         AREA = "data/carea.tif"
         # if this not works under Windows, replace .tif by .sdat
         # SLOPE = "data/cslope.sdat",
         # AREA = "data/carea.sdat"
         )
# ok, let's also save dem and ndvi
# export altitudes and NDVI as GTiff-rasterfiles
# for (i in c("dem", "ndvi")) {
#   writeRaster(get(i),
#               file.path("code/r_gis_bridges/images/", i),
#               format = "GTiff",
#               prj = TRUE,
#               overwrite = TRUE)
# }


# stack containing the predictor rasters, let's call it terrain attributes (ta)
ta = stack(dem, ndvi, "code/r_gis_bridges/images/cslope.tif",
           "code/r_gis_bridges/images/carea.tif")

# in case something didn't work for you, load the rasters from disk
# files = file.path("code/r_gis_bridges/images",
#                   paste0(c("dem", "ndvi", "cslope", "carea"), ".tif"))
# ta = stack(files)
hist(ta)
# just in case, let's normalize carea ndvi would be also a suitable candidate,
# however, log-transformation would be useless since NDVI contains negative
# values
ta$carea = log10(ta$carea)
hist(ta)

# extract values to points
random_points[, names(ta)] = extract(ta, random_points)
# let's have a look at the output
random_points
# let's save it
# saveRDS(random_points, "code/r_gis_bridges/images/random_points.rds")
