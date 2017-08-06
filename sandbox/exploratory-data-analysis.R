library(GGally)
library(tidyverse)
library(purrr)
library(broom)
library(abind)
library(plyr)

# load data
load(url("https://www.dropbox.com/s/v0cal2s54mizlfo/urban.rda?dl=1"))

# find urban gridcells
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)


#================#
# Study 3 cells only
# make plots for 3 random urban gridcells
set.seed(304)
n <- 3
rand_cells <- sample(1:sum(is_urban), size = n)

for (i in 1:n) {
  g <- as.data.frame(urban[which(is_urban)[rand_cells[i]], , ])
  png(file = paste0("cell", rand_cells[i], ".png"), width = 1300, height = 1200)
  print(ggpairs(g))
  dev.off()
}

# slice off cells of interest and scale vars
urban_subset <- urban[which(is_urban)[rand_cells], , ] %>%
  alply(1)
urban_subset <- map(urban_subset, ~apply(.x, MARGIN = 2, scale))

# fit models
m <- map(urban_subset, ~lm(Tu ~ ., data = as.data.frame(.x)))


#================#
# Fit all models

# Data processing

# add season
d <- t(array(rep(rep(c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1), times = 96), times = dim(urban)[1]),
           dim = c(1152, length(urban_list))))
u2 <- abind(urban, d, along = 3)

all_jans <- seq(1, dim(urban)[2], by = 12)
urban_list <- urban[is_urban,
                    all_jans,
                    c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "T", "Tu")] %>%
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))

# Fit Models
model_list <- map(urban_list, ~lm(Tu ~ . + season:Ld + season:Q, data = as.data.frame(.x)))

# Extract coefficients
coef_list <- map(model_list, ~tidy(.x)$estimate)
coef_df <- data.frame(matrix(unlist(coef_list), nrow = length(urban_list)))
names(coef_df) <- c("intercept", names(coef(m1)[-1]))
ggpairs(coef_df)

# k-means clustering
df <- data.frame(k = 2:10, withinss = rep(NA, 9))
for (i in 1:9) {
  df[i, "withinss"] <- kmeans(coef_df, centers = i + 1, iter.max = 15)$tot.withinss
}
ggplot(df, aes(x = k, y = withinss)) +
  geom_point() +
  geom_line()

# hierarchical clustering
subsamp <- as.data.frame(scale(coef_df)) %>%
  dist()
hc <- hclust(subsamp)
plot(hc, xlab = "", ylab = "", sub = "", main = "Complete Linkage", cex = .6)







# cross-validation
k <- 5
u <- urban_subset[[1]]
for (i in 1:k) {
  test <- u
}


#====================#
# RMSE map
#====================#
# This requires running part three of model-selection.R to
# create map_m1_df and map_m2_df

library(ggplot2)
map.world <- map_data(map = "world")

map.world$long <- map.world$long - 180
ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = map_df_m1, aes(x = lon, y = lat, fill = RMSE, colour = RMSE), size = 1) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(0, 1.5),
                       labels = c("0", ".5", "1", "1.5"),
                       breaks = c(0, .5, 1, 1.5)) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 1.5),
                        labels = c("0", ".5", "1", "1.5"),
                        breaks = c(0, .5, 1, 1.5))

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


#====================#
# Grouping Models
#====================#
# This requires the fitting full model from predict.R
time_fit <- data.frame(fit = m_full_list[[1]]$fitted.values,
                       obs = urban_list[[1]][ , "Tu"])


# Extract coefficients
library(broom)
coef_list <- purrr::map(m_full_list, ~tidy(.x)$estimate)
is_full <- unlist(lapply(coef_list, length)) == 84
coef_list <- coef_list[is_full]
coef_df <- matrix(unlist(coef_list), nrow = sum(is_full), byrow = TRUE) %>%
  scale() %>%
  as.data.frame()
names(coef_df) <- c("intercept", names(coef(m1)[-1]))
#ggpairs(coef_df)

# k-means clustering
df <- data.frame(k = 2:10, withinss = rep(NA, 9))
for (i in 1:9) {
  df[i, "withinss"] <- kmeans(coef_df, centers = i + 1, iter.max = 15)$tot.withinss
}
ggplot(df, aes(x = k, y = withinss)) +
  geom_point() +
  geom_line() +
  theme_minimal()

# hierarchical clustering
subsamp <- dist(coef_df)
hc <- hclust(subsamp)
plot(hc, xlab = "", ylab = "", sub = "", main = "Complete Linkage", cex = .6)


