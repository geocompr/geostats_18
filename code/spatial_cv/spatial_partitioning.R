# Filename: spatial_partitioning.R (2018-08-14)
#
# TO DO: Show difference between conventional and spatial partitioning
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library(grid)
library(gridExtra)
library(lattice)
library(latticeExtra)
library(sperrorest)
library(reshape2)
library(sf)

# attach data
data("dem", "random_points", package = "RQGIS")

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# non-spatial partitioning
random_points[, c("x", "y")] = st_coordinates(random_points)
rp = sf::st_set_geometry(random_points, NULL)
resamp_nsp = sperrorest::partition_cv(rp, nfold = 5, repetition = 1)
# plot(resamp_nsp, data = lsl)
# spatial partitioning
resamp_sp = sperrorest::partition_kmeans(rp, nfold = 5, repetition = 1)
# plot(resamp_sp, data = lsl)

# extract spatial partitioning points
spp = rp[, c("x", "y")]
spp[, paste("fold", 1:5)] = lapply(resamp_sp$`1`, function(x) {
  spp[x$test, "fold"] = 1
  spp[is.na(spp$fold), "fold"] = 0
  spp$fold = as.logical(spp$fold)
  spp$fold
})
# melt the dataframe
spp = reshape2::melt(spp, id.vars = c("x", "y"))
names(spp) = c("x", "y", "fold", "value")
p_1 = xyplot(y ~ x | fold, col = c("salmon", "lightblue"), groups = value,
             data = spp, layout = c(5, 1),
       asp = "iso", pch = 16, xlab = "", ylab = "", cex = 1.25,
       scales = list(tck = 0,
                     alternating = 0),
       strip = strip.custom(bg = c("white"),
                            par.strip.text = list(cex = 1.2)),
       key = list(space = "bottom", columns = 2,
                  text = list(c("training data", "test data"), cex = 1),
                  points = list(col = c("salmon", "lightblue"), pch = 16)))
png("pres/spatial_cv/img/spatial_partitioning.png", res = 300,
    width = 25, height = 10,
    units = "cm")
print(p_1)
dev.off()


# extract random partitioning points
rpp = rp[, c("x", "y")]
rpp[, paste("fold", 1:5)] = lapply(resamp_nsp$`1`, function(x) {
  rpp[x$test, "fold"] = 1
  rpp[is.na(rpp$fold), "fold"] = 0
  rpp$fold = as.logical(rpp$fold)
  rpp$fold
})
# melt the dataframe
rpp = reshape2::melt(rpp, id.vars = c("x", "y"))
names(rpp) = c("x", "y", "fold", "value")
p_2 = xyplot(y ~ x | fold, col = c("salmon", "lightblue"), groups = value,
             data = rpp, layout = c(5, 1),
             asp = "iso", pch = 16, xlab = "", ylab = "", cex = 1.25,
             scales = list(tck = 0,
                           alternating = 0),
             strip = strip.custom(bg = c("white"),
                                  par.strip.text = list(cex = 1.2)),
             key = list(space = "bottom", columns = 2,
                        text = list(c("training data", "test data"), cex = 1),
                        points = list(col = c("salmon", "lightblue"), pch = 16)))
png("pres/spatial_cv/img/random_partitioning.png", res = 300,
    width = 25, height = 10,
    units = "cm")
print(p_2)
dev.off()
