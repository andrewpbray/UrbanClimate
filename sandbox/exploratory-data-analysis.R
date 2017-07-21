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


# make map of urban areas in North America
library(ggmap)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

north_am <- c(left = -128, bottom = 12, right = -65, top = 55)
world <- c(left = -180, bottom = -90, right = 180, top = 90)
map <- get_openstreetmap(world)
p1 <- ggmap(map) +
  theme_minimal()

df <- data.frame(lat = c(35, 20), lon = c(-85, -100), val = c(5, 10))
p1 + geom_tile(data = map_df, aes(x = lon, y = lat, fill = m1))

is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
is_urban_mat <- matrix(is_urban, nrow = 288)

map.world <- map_data(map = "world")

map.world$long <- map.world$long - 180
ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("lon") #+
  geom_tile(data = map_df, aes(x = lon, y = lat, fill = m1))

ggplot(map_df, aes(x = lon, y = lat, fill = m1)) +
  geom_tile() + theme_minimal()

#

library(raster)
library(rgeos)

## get SpatialPolygnsDataFrame map of the states
m <- getData("GADM", country="United States", level=1)
m <- m[!m$NAME_1 %in% c("Alaska","Hawaii"),] # sorry Alaska and Hawaii

## here I modified your code to make a raster object
r <- raster(nrow=30, ncol=30,
            xmn=bbox(m)["x","min"], xmx=bbox(m)["x","max"],
            ymn=bbox(m)["y","min"], ymx=bbox(m)["y","max"],
            crs=proj4string(m))
xyz <- rasterToPoints(r)
r[] <- sin(xyz[,"y"]*pi/180) + cos(xyz[,"x"]*pi/180)

## Option A) mask raster using polygon
newr <- mask(r, m)
plot(newr, col=cm.colors(60), axes=FALSE)
plot(m, add=TRUE)
box(col="white")


