library(ncdf4)
library(MASS)

# may need to rename the following object "urban"
load(url('https://www.dropbox.com/s/m1ixr8ulfzympe3/cesmdata.RData?dl=1'))
# Tu: urban screen-height temperature
# TH: atmospheric potential tempetature at reference height
# Q: specific humidity at reference height
# P: pressture at reference height
# Tref: atmospheric tempetature at reference height
# Ld: longwave down
# Sd: shortwave down
# LiqPrecip: rain
# SolidPrecip: SNOW
# PCO2: CO2 pressure

library(tidyverse)
library(R.matlab)
library(purrr)
library(devtools)
library(abind)

# squeeze lat/lon into a single margin and make into a 3d array
# dims: [gridcell, time, variable]
urban <- map(urban, ~matrix(.x, ncol = dim(.x)[3])) %>%
  abind(urban, along = 3)

# add lat/lon as names to each gridcell
latlon <- readMat("data/latlon.mat")
dimnames(urban)[[1]] <- paste0(latlon[["lat2d"]], ", ", latlon[["lon2d"]])

# save processed data
devtools::use_data(urban)
