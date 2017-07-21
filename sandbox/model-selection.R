#=================#
# Model Selection
#=================#

#=================#
# Data Processing
urban[ , , "LiqPrecip"] <- log(urban[ , , "LiqPrecip"] + 0.001)
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
urban_list <- urban[is_urban, , c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "T", "Tu")] %>%
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))

# Tools for CV
library(cvTools)
get_oos_rmse <- function(x, formula, K, R) {
  call <- call("lm", formula = formula)
  folds <- cvFolds(nrow(x), K = K, R = R)
  return(mean(cvTool(call, data = x, y = x$Tu, folds = folds)))
}

#=================#
# Part One: No seasonality component

# Model 1: full variables
m1 <- map_dbl(urban_list, ~get_oos_rmse(as.data.frame(.x), Tu ~ ., K = 10, R = 2))

# Model 2: Tu ~ t
m2 <- map_dbl(urban_list, ~get_oos_rmse(as.data.frame(.x), Tu ~ `T`, K = 10, R = 2))

# Compare models
df <- data.frame(m1, m2)
ggplot(df, aes(x = m1, y = m2)) +
  geom_point(alpha = .3) +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, col = "goldenrod") +
  xlab("Full model") +
  ylab("Simple model (only T)")
mean(df$m1 > df$m2)

# Where did Model 2 ourperform?
m2_wins <- df$m1 > df$m2
m2_wins_mat <- is_urban
m2_wins_mat[is_urban] <- is_urban[is_urban] + m2_wins
m2_wins_mat <- matrix(m2_wins_mat, nrow = 288)
image(m2_wins_mat)


#=================#
# Part Two: Only January
#=================#

# Data Processing
urban_jans <- urban[is_urban,
                    seq(1, 1152, 12),
                    c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "Tu")] %>%
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))

# Model 1: full variables
m1_j <- map_dbl(urban_jans, ~get_oos_rmse(as.data.frame(.x), Tu ~ ., K = 10, R = 2))

# Model 2: Tu ~ t
m2_j <- map_dbl(urban_jans, ~get_oos_rmse(as.data.frame(.x), Tu ~ `T`, K = 10, R = 2))

# Compare models
df <- data.frame(m1_j, m2_j)
ggplot(df, aes(x = m1_j, y = m2_j)) +
  geom_point(alpha = .3) +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, col = "goldenrod") +
  xlab("Full model") +
  ylab("Simple model (only T)")
mean(df$m1_j > df$m2_j)

# Where did Model 2 ourperform?
m2_wins <- df$m1_j > df$m2_j
m2_wins_mat <- is_urban
m2_wins_mat[is_urban] <- is_urban[is_urban] + m2_wins
m2_wins_mat <- matrix(m2_wins_mat, nrow = 288)
image(m2_wins_mat)

#=================#
# Part Three: With montly dummy var
#=================#
# NB: this data does not get scaled
d <- t(array(rep(rep(1:12, times = 96), times = dim(urban)[1]),
             dim = c(1152, length(urban_list))))

urban_cube <- abind(d, urban[is_urban, , c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "Tu")])
dimnames(urban_cube)[[3]][1] <- "month"
# urban_cube[, , "month"] <- as.factor(urban_cube[, , "month"])
urban_list <- alply(urban_cube, 1)

# Model 1: full variables
m1_m <- map_dbl(urban_list, ~get_oos_rmse(as.data.frame(.x),
                                          Tu ~ Ld + LiqPrecip + P + PCO2 + Q + Sd + as.factor(month),
                                          K = 10, R = 1))

# Model 2: full variables plus all two-way interactions
m2_m <- map_dbl(urban_list, ~get_oos_rmse(as.data.frame(.x),
                                          Tu ~ Ld + LiqPrecip + P + PCO2 + Q + Sd + as.factor(month) +
                                            as.factor(month):Ld + as.factor(month): LiqPrecip + as.factor(month):P +
                                            as.factor(month):PCO2 + as.factor(month):Q + as.factor(month):Sd,
                                          K = 10, R = 1))


# Plot global RMSE
m2_m_rmse <- is_urban
m2_m_rmse[is_urban] <- m2_m
m2_m_rmse <- matrix(m2_m_rmse, nrow = 288)

library(lattice)
levelplot(m2_m_rmse)

library(stringr)

latlon <- dimnames(urban)[[1]] %>%
  str_split(", ") %>%
  unlist()
lat <- as.integer(latlon[seq(1, length(latlon), 2)])
lon <- as.integer(latlon[seq(2, length(latlon), 2)])
lon[lon > 180] <- lon[lon > 180] - 360
m1_m_rmse[m1_m_rmse == 0] <- NA
m2_m_rmse[m2_m_rmse == 0] <- NA
map_df <- data.frame(lat = lat,
                     lon = lon,
                     m1 = c(m1_m_rmsee),
                     m2 = c(m2_m_rmsee))


#=================#
# Part Four: With seasonality dummy vars
#=================#
# Create variable
d <- t(array(rep(rep(c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1), times = 96), times = dim(urban)[1]),
             dim = c(1152, length(urban_list))))
urban2 <- abind(urban, d, along = 3)

urban_list <- urban2[is_urban, , c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "T", "Tu")] %>%
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))

# Fit Models
model_list_2 <- map(urban_list,
                  ~cv.lm(data = as.data.frame(.x), form.lm = formula(Tu ~ .), m = 5, printit = FALSE))


#=================#
# Old code

# # cv with caret (i think it's in-sample RMSE)
# library(caret)
# m <- train(Tu ~ .,
#            data = u,
#            method = "lm",
#            trControl = trainControl(method = "repeatedcv",
#                                     number = 10,
#                                     repeats = 50))
#
# model_list_1 <- map(urban_list[1:100],
#                     ~train(Tu ~ .,
#                            data = .x,
#                            method = "lm",
#                            trControl = trainControl(method = "cv", number = 5, savePredictions=TRUE)))
#
#
# # homegrown cv
#
# folds <- createFolds(u$Tu, k = 10)
#
# get_cv <- function() {
#   train_df <- u[-folds[[1]], ]
#   test_df <- u[folds[[1]], ]
#   m1 <- lm(Tu ~ ., data = train_df)
#   y_hat <- predict(m1, new_data = test_df)
#   test$"Tu" - y_hat
}

