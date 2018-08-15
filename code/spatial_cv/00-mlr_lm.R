# Filename: 00-mlr_lm.R (2018-08-15)
#
# TO DO: Introduce mlr building blocks and spatial cv with a simple lm
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. MLR BUILDING BLOCKS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library(mlr)
library(dplyr)
library(sf)
library(lattice)

#**********************************************************
# 2 MLR BUILDING BLOCKS------------------------------------
#**********************************************************

rp = readRDS("code/spatial_cv/images/rp.rds")
# extract the coordinates into a separate dataframe
coords = sf::st_coordinates(rp) %>%
  as.data.frame %>%
  rename(x = X, y = Y)
# only keep response and predictors which should be used for the modeling
rp = dplyr::select(rp, -id, -spri) %>%
  st_set_geometry(NULL)

# first have a look at the data
d = reshape2::melt(rp, id.vars = "sc")
xyplot(sc ~ value | variable, data = d, pch = 21, fill = "lightblue",
       col = "black", ylab = "response (sc)", xlab = "predictors",
       scales = list(x = "free",
                     tck = c(1, 0),
                     alternating = c(1, 0)),
       strip = strip.custom(bg = c("white"),
                            par.strip.text = list(cex = 1.2)),
       panel = function(x, y, ...) {
         panel.points(x, y, ...)
         panel.loess(x, y, col = "salmon", span = 0.5)
       })

# create a task
task = makeRegrTask(data = rp, target = "sc",
                    coordinates = coords)
# run listLearners to find out which models could be thrown at our task
lrns = listLearners(task, warn.missing.packages = FALSE)
dplyr::select(lrns, class, name, short.name, package)

lrn = makeLearner(cl = "regr.lm", predict.type = "response")
# simple lm of the stats package
getLearnerPackages(lrn)
helpLearner(lrn)
# so the model being fitted is simply a lm
getLearnerModel(train(lrn, task))

# performance level
perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
cv_sp = mlr::resample(learner = lrn,
                      task = task,
                      resampling = perf_level,
                      measures = mlr::rmse)
boxplot(cv_sp$measures.test$rmse)

# we can run the same using a conventional cross-validation
# task_nsp = makeRegrTask(data = rp, target = "sc")
# perf_level_nsp = makeResampleDesc(method = "RepCV", folds = 5, reps = 100)
# cv_nsp = mlr::resample(learner = lrn,
#                       task = task_nsp,
#                       resampling = perf_level_nsp,
#                       measures = mlr::rmse)
# boxplot(cv_sp$measures.test$rmse, cv_nsp$measures.test$rmse,
#         col = c("lightblue2", "mistyrose2"),
#         names = c("spatial CV", "conventional CV"),
#         ylab = "RMSE")
