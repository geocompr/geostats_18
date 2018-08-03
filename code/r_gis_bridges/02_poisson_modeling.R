# Filename: poisson_modeling.R (2017-08-15)
#
# TO DO: Retrieving predictors and Poisson modeling
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. PREDICTOR RETRIEVAL
# 3. LITTLE DATA EXPLORATION
# 4. POISSON MODELING & PREDICTIVE MAPPING
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RQGIS")
library("sf")
library("raster")
library("reshape2")
library("lattice")
library("latticeExtra")
library("rgdal")
library("gstat")
library("dplyr")
library("mapview")

# define directories
dir_main <- "D:/uni/fsu/teaching/misc/geostats_rqgis"
dir_data <- file.path(dir_main, "data")
dir_ima <- file.path(dir_main, "images")

# attach data
data(random_points, package = "RQGIS")
d <- random_points
data("dem", package = "RQGIS")
data("ndvi", package = "RQGIS")
# study area mask
study_mask <- st_read(file.path(dir_data, "mask.shp"))
plot(st_geometry(study_mask))

#**********************************************************
# 2 RQGIS VARIABLE RETRIEVAL-------------------------------
#**********************************************************

# plot the randomly sampled points on a hillshade
hs <- hillShade(terrain(dem), terrain(dem, "aspect"), 40, 270)
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
         DEM_PREPROC = file.path(dir_data, "sdem.asc"))

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
run_qgis("saga:sagawetnessindex", DEM = dem, SLOPE_TYPE = 1, AREA_TYPE = 1,
         SLOPE = file.path(dir_data, "cslope.asc"),
         AREA = file.path(dir_data, "carea.asc"))

# create a dem containing the second order orthogonal altitude
dem <- dem / 1000  # convert altitude from m to km
# dem2 <- dem^2  # simply the square of altitude -> highly collinear
my_poly <- poly(values(dem), degree = 2)
dem1 <- dem2 <- dem
values(dem1) <- my_poly[, 1]
values(dem2) <- my_poly[, 2]

# export altitudes and NDVI as ASCII-rasterfiles
for (i in c("dem1", "dem2", "ndvi")) {
  writeRaster(get(i), 
              file.path(dir_data, paste0(i, ".asc")), 
              format = "ascii", prj = TRUE, 
              overwrite = TRUE)  
}


# extract values to points using local interpolation unfortunately,
# RSAGA::pick.from.ascii.grids does not support sf so far. Hence, we need to just
# keep the attribute values of random_points by getting rid off the sticky
# geometry column
vals <- random_points %>% 
  as_tibble %>%
  select(-geometry)

# You may have to install RSAGA from my github account:
# devtools::install_github("jannes-m/RSAGA")
vals <- RSAGA::pick.from.ascii.grids(
  data = vals,
  X.name = "x", Y.name = "y",
  file = file.path(dir_data, c("dem1", "dem2", "carea", "cslope", "ndvi")))
head(vals)

# just in case RSAGA::pick.from.ascii.grid is not working
# carea <- raster(file.path(dir_data, "carea.asc"))
# cslope <- raster(file.path(dir_data, "cslope.asc"))
# vals[, c("dem1", "dem2", "carea", "cslope", "ndvi")] <-
#   lapply(c(dem1, dem2, carea, cslope, ndvi), function(x) {
#     extract(x, vals[, c("x", "y")], method = "bilinear")
# })

#**********************************************************
# 3 LITTLE DATA EXPLORATION--------------------------------
#**********************************************************

# Have a look at the distribution
histogram(~ spri + dem1 + dem2 + cslope + ndvi + carea,
          data = vals,
          scales = list(x = list(relation = "free"),
                        y = list(relation = "free")),
          breaks = NULL)
# browseURL(past0("http://r.789695.n4.nabble.com/", 
#                 "lattice-histogram-for-multiple-variables-adjusting", 
#                 "-x-axis-td889887.html"))

# let's do some data transformations
# catchment area is highly skewed to the right
my_trafo <- function(x) {
  # convert radians to degrees
  x$cslope <- x$cslope * 180 / pi
  # log-transform size of catchment area since it is extremely skewed
  x$log_carea <- log(x$carea / 1e+06)
  x
}
# apply the transformation function
vals <- my_trafo(vals)
head(vals, 3)

histogram(~ spri + dem1 + dem2 + cslope + ndvi + log_carea,
          data =vals,
          scales = list(x = list(relation = "free"),
                        y = list(relation = "free")),
          breaks = NULL)

# save vals
# save(vals, file = file.path(dir_ima, "vals.Rdata"))
# load(file.path(dir_ima, "vals.Rdata"))

par(mfrow = c(2, 3))
for (i in c("spri", "dem1", "dem2", "cslope", "ndvi", "log_carea")) {
  boxplot(vals[, i], main = i)
  # dotchart(vals[, i], main = i)
}

# plot each predictor against the response
tmp <- dplyr::select(vals, -dplyr::one_of("id", "x", "y", "carea", "dem2"))
tmp <- reshape2::melt(tmp, id.vars = "spri")
xyplot(spri ~ value | variable, data = tmp,
       scales = list(x = list(relation = "free"),
                     # suppress ticks on top
                     tck = c(1, 0),         
                     # plots y-axes labels on the left side
                     alternating = c(1, 1), 
                     draw = TRUE),
       panel = function(x, y){
         # plot the points
         panel.points(x, y, pch = 16, col = "salmon")
         # line smoother to aid visual inspection
         panel.loess(x, y, span = 0.9, lwd = 2, col = "gray")
         })

#**********************************************************
# 4 MODELING AND VALIDATION--------------------------------
#**********************************************************

# 4.1 Modeling=============================================
#**********************************************************
load(file.path(dir_ima, "vals.Rdata"))
fit <- glm(spri ~ dem1 + dem2 + cslope + ndvi + log_carea, 
           data = vals,
           family = "poisson")
fit
summary(fit)

# 4.2 Model validation/inspection==========================
#**********************************************************

# We are using Pearson residuals, i.e. each residual is divided by the square 
# root of the variance. This corresponds more or less to a normalization. This 
# is useful since larger fitted values with Poisson distributions are allowed 
# to have more variation. Therefore, while we still want to see small residuals
# yi − μi for small values of μi, residuals are allowed to be larger for larger
# μi (Zuur et al. 2009).
resids <- residuals(fit, type = "pearson")
# or doing it manually
(vals$spri - fitted(fit)) / sqrt(fitted(fit))
# Pearson residuals are defined as observed values minus the expected values,
# divided by the square root of the variance (which are the expected values
# again as this is a Poisson GLM) (Zuur et al. 2016).

# Spatial autocorrelation (= independence assumption)
# Model the residual semi-variogram
svgm <- variogram(resids ~ 1, location=~ x + y, data = vals)
# plot(svgm, xlab = "distance (m)", ylab = "Semivariance", col = "black",
#      plot.numbers = TRUE)
# remember the rule of thumb that you should consider at least 30 pairs of
# points per lag -> so here the first lag has too few observations

# so let's specifiy the distance between observations
svgm <- variogram(resids ~ 1, location=~ x + y, width = 100, data = vals)
plot(svgm, xlab = "distance (m)", ylab = "Semivariance", col = "black",
     plot.numbers = TRUE)
# so, this is not really an indication for autocorrelation well, we could use
# INLA to test if a model incorporating spatial autocorrelation is better (in
# terms of AIC/DIC) than one without spatial autocorrelation
# Or we could use spatial cross-validation to check if model predictions are 
# independent of 5-10 different spatial configurations
# Unfortunately, we have no time to show these approaches, so we leave this as
# an exercise for you.

# Now, we made sure that spatial autocorrelation is not a major issue here,
# let's go on with the model validation, i.e. plot the residuals:
# 1. alone
# 2. against the fitted values
# 3. against all covariates in the model
# 4. against all covariates not in the model
# Finally, check for overdispersion since we are using a Poisson model

# plot the residuals
dev.off()
plot(resids)
# plot the residuals against the fitted values
plot(resids, fitted(fit))
# fitted(fit) == predict(fit, type = "response")
# Plot residuals versus each covariate in the model, each covariate not in the
# model
tmp <- vals
tmp$resid <- resids
tmp <- dplyr::select(tmp, -id, -carea, -spri)
tmp <- reshape2::melt(tmp, id.vars = "resid")
xyplot(resid ~ value | variable, data = tmp,
       scales = list(x = list(relation = "free"),
                     # suppress ticks on top
                     tck = c(1, 0),         
                     # plots y-axes labels on the left side
                     alternating = c(1, 1), 
                     draw = TRUE),
       panel = function(x, y) {
         # plot the points
         panel.points(x, y, pch = 16, col = "salmon")
         # line smoother to aid visual inspection
         panel.loess(x, y, span = 0.9, lwd = 2, col = "gray")
       }) 
dev.off()
# looks okayish ndvi smells a bit like homogeneity trouble but since this is
# only due to 3 points and we can explain it with too few observations for
# positive NDVI values (-> more variance), we move on.
# Also it seems unnecessary to include a non-linear NDVI effect.

# check for overdispersion Overdispersion means the variance is larger than the 
# mean. It can be caused by an outlier, a missing covariate, missing 
# interaction, wrong link-function, non-linear patterns that are modelled as 
# linear, zero-inflation, dependency, or a large variance. Overdispersion is
# calculated as the sum of the squared Pearson residuals divided by sample size
# minus the number of regression parameters. If it is > 1.5 you should think of
# some kind of action to correct it.
sum(resids^2) / (nrow(vals) - (length(coef(fit))))  # 1.28
# for further discussion on overdispersion, see Zuur et al. (2009: 224 ff.)

# 4.3 Spatial prediction===================================
#**********************************************************
# using the RSAGA package
# You may have to install RSAGA from my github account:
# devtools::install_github("jannes-m/RSAGA")
# multi.local.function(
#   path = dir_ima,
#   in.grids = c("dem1","dem2", "cslope", "ndvi", "carea"),
#   out.varnames = "pred",
#   fit = fit,
#   fun = grid.predict,
#   trafo = my_trafo,
#   control.predict = list(type = "response"))
# pred_1 <- raster(file.path(dir_ima, "pred.asc"))

# using the raster package
# writeRaster(log_carea, file.path(dir_ima, "log_carea.asc"), 
#             prj = TRUE, format = "ascii", overwrite = TRUE)
# writeRaster(cslope, file.path(dir_ima, "cslope.asc"), 
#             prj = TRUE, format = "ascii", overwrite = TRUE)
# log_carea <- raster(file.path(dir_ima, "log_carea.asc"))
# cslope <- raster(file.path(dir_ima, "cslope.asc"))
# s <- stack(dem1, dem2, log_carea, cslope, ndvi)
# pred_2 <- predict(s, fit, fun = predict, type = "response")

# load all raster datasets into R
ind <- grep(".asc$", dir(dir_data), value = TRUE)
for (i in ind) {
  r_name <- gsub(".asc", "", i)
  print(r_name)  # print the name of the raster objects
  tmp <- raster(file.path(dir_data, i), values = TRUE)
  # make sure that all rasters have the same extent
  tmp <- crop(tmp, dem)
  assign(r_name, tmp)
}
# we have to apply the same transformations to the raster data as we have done
# for our input data
log_carea <- log(carea / 1e+06)
cslope <- cslope * 180 / pi

# doing the prediction manually
coefs <- coef(fit)
# In a Poisson model, we use following predictor function:
# expected_value = exp(alpha + beta_1 * predictor_1 + beta_2 * predictor_2...)
# which is the same as: log(expected_value) = alpha + beta_1 * predictor_1...
# because the mean (expected value) is modelled as an exponential model, it is 
# always positive

# so applying our Poisson predictor function to our rasters:
pred_3 <- exp(coefs[1] + coefs[2] * dem1 + coefs[3] * dem2 + coefs[4] * cslope +
              coefs[5] * ndvi + coefs[6] * log_carea)

# create a really beautiful prediction map
dem <- mask(dem, as(study_mask, "Spatial"))
hill <- hillShade(terrain(dem), terrain(dem, "aspect"), 40, 270)
# just use our study area
pred_1 <- mask(pred_3, as(study_mask, "Spatial"))

plot(pred_1, col = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
     xlim = c(795000, 797500), ylim = c(8932000, 8935000))
# specify a color 
pal <- RColorBrewer::brewer.pal(9, "YlOrRd")
cuts <- seq(0, 30, 4)
# projecting 2 points from WGS84 to UTM (we need them as axis labels)
p <-  data.frame("x" = c(-78.3), "y" = c(-9.633333333333))
p <- SpatialPoints(cbind(p$x, p$y),
                   proj4string = CRS("+proj=longlat +ellps=WGS84"))
p_2 <- spTransform(p, CRS(proj4string(dem)))
# make the plot
p_1 <- 
  spplot(pred_1, col.regions = pal, alpha.regions = 0.8,
         xlim = c(795000, 797500), ylim = c(8932000, 8935000),
         # use the WGS84-coordinates as axis labels
         scales = list(x = list(relation = "same",
                                at = coordinates(p_2)[1],
                                labels = paste(abs(round(coordinates(p)[1], 2)),
                                               "W"),
                                cex = 0.7),
                       y = list(relation = "same",
                                at = coordinates(p_2)[2],
                                labels = paste(abs(round(coordinates(p)[2], 2)),
                                               "S"),
                                cex = 0.7),
                       # ticks on top are suppressed
                       tck = c(1, 0),               
                       # plots axes labels only in row and column 1 and 4
                       alternating = c(1, 0, 0, 1), 
                       draw = TRUE),
         # the legend should be placed beneath the plot
         colorkey = list(space = "bottom",
                         # labels specifies what to plot
                         # at specifies where to plot the labels
                         labels = list(labels = c(0, cuts[-1]),
                                       at = cuts, cex = 0.7),
                         # at is needed again, to indicate where the 
                         # colors change (must be of length 1 more than 
                         # the col vector!!!)
                         at = cuts,
                         # width and heigt are relative to the plot
                         width = 1, height = 0.5,
                         # draw a box and ticks around the legend
                         axis.line = list(col = "black")),
         # let's add our study area border and a scale
         sp.layout = list(
           list("sp.lines", as(study_mask, "Spatial"), first = FALSE, lwd = 2),
           list("SpatialPolygonsRescale", layout.scale.bar(), 
                offset = c(796750, 8932250), 
                scale = 500, fill = c("transparent","black")),
           list("sp.text", c(796750, 8932150), "0", cex = 0.7),
           list("sp.text", c(797250, 8932150), "500 m", cex = 0.7)
         ),
         # at COMMAND AGAIN NECESSARY AS WE PLOT A CONTINOUS VARIABLE!!!
         at = cuts, pretty = TRUE) +
  # finally, put the hillshade underneath our prediction plot
  as.layer(spplot(hill, col.regions = gray(0:100 / 100)), under = TRUE)
# display the plot
p_1

# Use mapview for interactive viewing
mapviewGetOption("basemaps")
mapview(pred_1, col.regions = pal, at = cuts,
        map.type = "Esri.WorldImagery") +
  mapview(study_mask, col.regions = "white", lwd = 3, alpha.regions = 0)

# save everything in an image
save.image(file = file.path(dir_ima, "02_poisson.Rdata"))
  