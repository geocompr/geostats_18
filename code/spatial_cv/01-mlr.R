# Filename: 01-mlr.R (2018-08-09)
#
# TO DO: Spatial CV and predictive mapping with mlr
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. RESPONSE: FLORISTIC GRADIENT
# 3. SPATIAL CV
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
library("parallelMap")

# attach data
random_points = readRDS("images/r_gis_bridges/random_points.rds")
# site-species community matrix (plant species of Mt. Mongón)
data("comm", package = "RQGIS")
data("dem", "study_area", package = "RQGIS")
# raster stack (environmental predictors)
files = file.path("data", paste0(c("dem", "ndvi", "cslope", "carea"), ".tif"))
ep = stack(files)
ep$carea = log10(ep$carea)

#**********************************************************
# 2 RESPONSE: FLORISTIC GRADIENT---------------------------
#**********************************************************

# first create a response - the floristic gradient
pa = decostand(comm, "pa")
set.seed(25072018)
nmds = metaMDS(comm = pa, k = 4, try = 500)
# for a deeper look at NMDS, please refer to:
# browseURL("https://geocompr.robinlovelace.net/eco.html#nmds")
nmds$stress
# note that the rownames of pa (comm) correspond to the id-column of
# random_points, see also ?random_points
elev = dplyr::filter(random_points, id %in% rownames(pa)) %>%
  dplyr::pull(dem)
# rotating NMDS in accordance with altitude (proxy for humidity)
rotnmds = MDSrotate(nmds, elev)
# extracting the first two axes
sc = scores(rotnmds, choices = 1:2)
# plotting the first axis against altitude
plot(y = sc[, 1], x = elev, xlab = "elevation in m",
     ylab = "First NMDS axis", cex.lab = 0.8, cex.axis = 0.8)
# add sc to random_points, i.e. complete our response predictor (rp) matrix
sc = data.frame(id = as.numeric(rownames(sc)), sc = sc[, 1])
rp = inner_join(random_points, sc, by = "id")

#**********************************************************
# 3 SPATIAL CV---------------------------------------------
#**********************************************************

# for an in-depth explanation of spatial CV, check out:
browseURL("https://geocompr.robinlovelace.net/spatial-cv.html")

# 3.1 Response-predictor matrix and coordinates============
#**********************************************************
# extract the coordinates into a separate dataframe
coords = sf::st_coordinates(rp) %>%
  as.data.frame %>%
  rename(x = X, y = Y)
# only keep response and predictors which should be used for the modeling
rp = dplyr::select(rp, -id, -spri) %>%
  st_set_geometry(NULL)

# 3.2 MLR building blocks==================================
#**********************************************************

# 3.3.1 Spatial CV#########################################
#**********************************************************
# create task
task = makeRegrTask(data = rp, target = "sc", coordinates = coords)
# learner
lrn_rf = makeLearner(cl = "regr.ranger", predict.type = "response")
# performance estimation level with 5 spatial partitions and 100 repetitions
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
# five spatially disjoint partitions in the tune level (one repetition)
tune_level = makeResampleDesc(method = "SpCV", iters = 5)
# random search with 50 iterations
ctrl = makeTuneControlRandom(maxit = 50)
# specifying the search space
ps = makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(rp) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)
# wrap it all up
wrapped_lrn_rf = makeTuneWrapper(learner = lrn_rf,
                                   # inner loop (tunning level)
                                   resampling = tune_level,
                                   # hyperparameter seach space
                                   par.set = ps,
                                   # random search
                                   control = ctrl,
                                   show.info = TRUE,
                                   # performance measure
                                   measures = mlr::rmse)
# to make sure that the modeling goes on even if one model fails
configureMlr(on.learner.error = "warn", on.error.dump = TRUE)


if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore",
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams",
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 2),
                mc.set.seed = TRUE)
}

if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams",
                      cpus =  round(parallel::detectCores() / 2))
}

# run the spatial cross-validation
set.seed(12345)
result = mlr::resample(learner = wrapped_lrn_rf,
                       task = task,
                       resampling = perf_level,
                       extract = getTuneResult,
                       measures = mlr::rmse)
# stop parallelization
parallelStop()
# save your result, e.g.:
saveRDS(result, "images/spatial_cv/rf_sp_sp_50it.rds")
result = readRDS("images/spatial_cv/rf_sp_sp_50it.rds")

# 3.3.2. Non-spaital doing the same non-spatially
# create task
task_nsp = makeRegrTask(data = rp, target = "sc")
# performance estimation level with 5 spatial partitions and 100 repetitions
perf_level_nsp = makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
# five spatially disjoint partitions in the tune level (one repetition)
tune_level_nsp = makeResampleDesc(method = "CV", iters = 5)
# random search with 50 iterations
ctrl = makeTuneControlRandom(maxit = 50)
# specifying the search space
ps = makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(rp) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)
# wrap it all up
wrapped_lrn_rf = makeTuneWrapper(learner = lrn_rf,
                                 # inner loop (tunning level)
                                 resampling = tune_level_nsp,
                                 # hyperparameter seach space
                                 par.set = ps,
                                 # random search
                                 control = ctrl,
                                 show.info = TRUE,
                                 # performance measure
                                 measures = mlr::rmse)
# to make sure that the modeling goes on even if one model fails
configureMlr(on.learner.error = "warn", on.error.dump = TRUE)


if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
  parallelStart(mode = "multicore",
                # parallelize the hyperparameter tuning level
                level = "mlr.tuneParams",
                # just use half of the available cores
                cpus = round(parallel::detectCores() / 2),
                mc.set.seed = TRUE)
}

if (Sys.info()["sysname"] == "Windows") {
  parallelStartSocket(level = "mlr.tuneParams",
                      cpus =  round(parallel::detectCores() / 2))
}

# run the spatial cross-validation
set.seed(12346)
result_nsp = mlr::resample(learner = wrapped_lrn_rf,
                       task = task_nsp,
                       resampling = perf_level_nsp,
                       extract = getTuneResult,
                       measures = mlr::rmse)
# stop parallelization
parallelStop()
# save your result, e.g.:
saveRDS(result_nsp, "images/spatial_cv/rf_nsp_nsp_50it.rds")
result_nsp = readRDS("images/spatial_cv/rf_nsp_nsp_50it.rds")

# Difference between the mean (-> heavily influenced by outliers)
# still, this is a strong indication that nsp cv produced over-optimistic
# results
result$aggr
result_nsp$aggr
# Visualize difference between spatial and non-spatial CV
boxplot(result$measures.test$rmse,
        result_nsp$measures.test$rmse,
        col = c("lightblue2", "mistyrose2"),
        names = c("spatial CV", "conventional CV"),
        ylab = "RMSE")

#**********************************************************
# 4 PREDICTIVE MAPPING-------------------------------------
#**********************************************************

# for an in-depth explanation on the predictive mapping procedure, check out:
browseURL("https://geocompr.robinlovelace.net/eco.html")

# 4.1 Tuning, modeling and prediction======================
#**********************************************************
# create task
task = makeRegrTask(data = rp, target = "sc", coordinates = coords)
# learner
lrn_rf = makeLearner(cl = "regr.ranger", predict.type = "response")
# spatial partitioning
perf_level = makeResampleDesc("SpCV", iters = 5)
# specifying random search
ctrl = makeTuneControlRandom(maxit = 50L)
# specifying the search space
ps = makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(rp) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)
# hyperparamter tuning
set.seed(02082018)
tune = tuneParams(learner = lrn_rf,
                  task = task,
                  resampling = perf_level,
                  par.set = ps,
                  control = ctrl,
                  measures = mlr::rmse)
saveRDS(tune, "images/spatial_cv/rf_tune_50it.rds")
# learning using the best hyperparameter combination
lrn_rf = makeLearner(cl = "regr.ranger",
                     predict.type = "response",
                     mtry = tune$x$mtry,
                     sample.fraction = tune$x$sample.fraction,
                     min.node.size = tune$x$min.node.size)
# doing the same more elegantly using setHyperPars()
# lrn_rf = setHyperPars(makeLearner("regr.ranger", predict.type = "response"),
#                       par.vals = tune$x)
# train model
model_rf = train(lrn_rf, task)
# convert raster stack into a dataframe
new_data = as.data.frame(as.matrix(ep))
# apply the model to the dataframe
pred_rf = predict(model_rf, newdata = new_data)
# put the predicted values into a raster
pred = dem
# replace altitudinal values by rf-prediction values
pred[] = pred_rf$data$response

# 4.2 Prediction map=======================================
#**********************************************************
library("latticeExtra")
library("grid")

# create a color palette
blue = rgb(0, 0, 146, maxColorValue = 255)
lightblue = rgb(0, 129, 255, maxColorValue = 255)
turquoise = rgb(0, 233, 255, maxColorValue = 255)
green = rgb(142, 255, 11, maxColorValue = 255)
yellow = rgb(245, 255, 8, maxColorValue = 255)
orange = rgb(255, 173, 0, maxColorValue = 255)
lightred = rgb(255, 67, 0, maxColorValue = 255)
red = rgb(170, 0, 0, maxColorValue = 255)
pal = colorRampPalette(c(blue, lightblue, turquoise, green, yellow,
                         orange, lightred, red))

# restrict the prediction to the study area
pred = mask(pred, study_area) %>%
  trim
# create a hillshade
hs = hillShade(terrain(dem), terrain(dem, "aspect")) %>%
  mask(., study_area)
spplot(extend(pred, 2), col.regions = pal(50), alpha.regions = 0.7,
       scales = list(draw = TRUE,
                     tck = c(1, 0),
                     cex = 0.8),
       colorkey = list(space = "right", width = 0.5, height = 0.5,
                       axis.line = list(col = "black")),
       sp.layout = list(
         # list("sp.points", as(random_points, "Spatial"), pch = 16,
         #      col = "black", cex = 0.8, first = FALSE),
         list("sp.polygons", as(study_area, "Spatial"),
              col = "black", first = FALSE, lwd = 3)
       )
) +
  latticeExtra::as.layer(spplot(hs, col.regions = gray(0:100 / 100)),
                         under = TRUE)
