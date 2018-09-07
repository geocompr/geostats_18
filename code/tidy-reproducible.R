# aim: demonstrate dplyr code

library(sf)
library(raster)
library(tidyverse)
library(spData)

# system commands
system("ls")
ls 
ls -hal
pwd
dir_old = setwd("~/Desktop")


# subsetting rows
w1 = slice(world, 1)
w2 = slice(world, 2)
rbind(w1, w2)
bind_rows(w1, w2)
