library(GGally)
library(tidyverse)
library(purrr)
library(broom)

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
all_jans <- seq(1, dim(urban)[2], by = 12)
urban_list <- urban[is_urban,
                    all_jans,
                    c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "T", "Tu")] %>%
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))

# Fit Models
model_list <- map(urban_list, ~lm(Tu ~ ., data = as.data.frame(.x)))

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
subsamp <- sample_n(as.data.frame(scale(coef_df)), size = 200) %>%
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
map <- get_stamenmap(north_am, zoom = 5, maptype = "terrain-background")
p1 <- ggmap(map) +
  theme_map()

is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
is_urban_mat <- matrix(is_urban, nrow = 288)




#

df <- as.data.frame(urban_subset[[1]])
ggplot(df, aes(x = TH, y = Tu)) +
  geom_point() +
  geom_smooth(method = "lm")
df_res <- data.frame(res = m[[1]]$res, time_ind = 1:dim(urban_subset[[1]])[1])
ggplot(df_res, aes(x = time_ind, y = res)) +
  geom_point()

models <- apply(urban_subset, MARGIN = 1, FUN = SLR, var = "TH")


