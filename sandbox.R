library(tidyverse)
library(R.matlab)
library(purrr)
library(lubridate)
library(devtools)
library(abind)

# load data
load("data/urban.rda")
readMat("")

# spool out into matrices and squeeze lat/lon
mat_temp <- urban[["Ld"]]
Ld <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["LiqPrecip"]]
LiqPrecip <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["P"]]
P <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["PCO2"]]
PCO2 <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["Q"]]
Q <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["Sd"]]
Sd <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["SolidPrecip"]]
SolidPrecip <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["T"]]
T_ <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["TH"]]
TH <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))
mat_temp <- urban[["Tu"]]
Tu <- array(mat_temp, dim = c(dim(mat_temp)[1] * dim(mat_temp)[2], dim(mat_temp)[3]))

urban_array <- array(c(Ld, LiqPrecip, P, PCO2, Q, Sd, SolidPrecip, T_, TH, Tu), dim = c(55296, 1152, 10))

urban <- map(urban, ~matrix(.x, ncol = dim(.x)[3]))
urban <- abind(urban, along = 3)







# functions
slice_time <- function(x, start_time, end_time, ref_start = "2005-01", ref_end = "2100-12") {
  start_period <- as.period(ymd(paste0(start_time, "-01")) - ymd(paste0(ref_start, "-01")))
  start_ind <- round(day(start_period)/31) + 1
  end_period <- as.period(ymd(paste0(end_time, "-01")) - ymd(paste0(ref_start, "-01")))
  end_ind <- round(day(end_period)/31) + 1
  return(map(x, ~ .x[ , , start_ind:end_ind]))
}

deseason_ts <- function(timeseries) {
  len <- length(timeseries)
  st <- rep(NA, len)
  for (i in 1:12) {
    st[i, 1] <- mean(timeseries[seq(from = i, to = len - 12 + i, by = 12)], na.rm = T)
  }
  st <- rep(st, len/12)
  ds_Ts <- timeseries - st
}


a <- slice_time(d, start_time = "2005-01", end_time = "2005-01")



Tu <- d[["Tu"]]
g1 <- Tu[1, 1, ]


g <- list
for (i in 1:dim())

listed_matrix_to_df <- function(x) {

}


# Data Representation
m <- array(c(1:12), dim = c(2, 3, 2))

h <- list(m, m)

g <- array(unlist(h), dim = c(2, 3, 2, 2))
# margins: lon, lat, time, variable

g <- array(unlist(d), dim = c(288, 192, 1152, 10))

# subset data by looking only at first 20 years
g <- map(d, .x[ , , 1:240])


map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)


