# Calculate cross-validated RMSE based on the model prediction #
# 
library(abind)
library(plyr)
library(purrr)
library(tidyverse)

#=================#
# Load training data
load("data/urban.rda")
load("data/cesm_urban_UV.RData")
# Data Processing
urban[ , , "LiqPrecip"] <- log(urban[ , , "LiqPrecip"] + 0.001)
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
urban_cube <- abind(urban_UV[is_urban, , ], urban[is_urban, , c("T", "Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "Tu")])

# Add month variable
d <- t(array(rep(rep(1:12, times = 96), times = dim(urban)[1]),
             dim = c(1152, sum(is_urban))))
urban_cube <- abind(d, urban_cube)
dimnames(urban_cube)[[3]][1:4] <- c("month","U","V", "Tref")
urban_list <- alply(urban_cube, 1)

# Tools for CV
library(cvTools)
get_oos_rmse <- function(x, formula, K, R) {
  call <- call("lm", formula = formula)
  folds <- cvFolds(nrow(x), K = K, R = R)
  return(mean(cvTool(call, data = x, y = x$Tu, folds = folds)))
}

# Model 2: full variables plus all two-way interactions
m2_m <- map_dbl(urban_list, ~get_oos_rmse(as.data.frame(.x),
                                          Tu ~ U + V +Tref + Ld + LiqPrecip + P + PCO2 + Q + Sd + as.factor(month) +
                                            as.factor(month):U + as.factor(month):V + as.factor(month):Tref + 
                                            as.factor(month):Ld + as.factor(month): LiqPrecip +
                                            as.factor(month):P + as.factor(month):PCO2 + as.factor(month):Q +
                                            as.factor(month):Sd,
                                          K = 10, R = 1))
library(stringr)
latlon <- dimnames(urban)[[1]] %>%
  str_split(", ") %>%
  unlist()
lat <- as.integer(latlon[seq(1, length(latlon), 2)])
lon <- as.integer(latlon[seq(2, length(latlon), 2)])
lat <- lat[is_urban]
lon <- lon[is_urban]
lon[lon > 180] <- lon[lon > 180] - 360
map_df_m2 <- data.frame(lat = lat,
                        lon = lon,
                        RMSE = c(m2_m))
# Plot RMSE map
library(ggplot2)
map.world <- map_data(map = "world")
map.world$long <- map.world$long - 180
ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = map_df_m2, aes(x = lon, y = lat, fill = RMSE, colour = RMSE), size = 1) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(0, 1.5),
                       labels = c("0", ".5", "1", "1.5"),
                       breaks = c(0, .5, 1, 1.5)) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 1.5),
                        labels = c("0", ".5", "1", "1.5"),
                        breaks = c(0, .5, 1, 1.5))
ggsave("Map_cvRMSE.pdf")