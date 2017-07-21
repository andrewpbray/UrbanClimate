#=================#
# Prediction
#=================#
# Use data from runxx to get predictions of Tu using emulator

#=================#
# Fit Model

# Load training data
load("data/urban.rda")

# Data Processing
urban[ , , "LiqPrecip"] <- log(urban[ , , "LiqPrecip"] + 0.001)
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)

# Add month variable
d <- t(array(rep(rep(1:12, times = 96), times = dim(urban)[1]),
             dim = c(1152, sum(is_urban))))
urban_cube <- abind(d, urban[is_urban, , c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "Tu")])
dimnames(urban_cube)[[3]][1] <- "month"
urban_list <- alply(urban_cube, 1)

# Fit model: full variables plus all two-way interactions
m_full_list <- purrr::map(urban_list, ~lm(Tu ~ Ld + LiqPrecip + P + PCO2 + Q + Sd + as.factor(month) +
                                     as.factor(month):Ld + as.factor(month): LiqPrecip +
                                     as.factor(month):P + as.factor(month):PCO2 + as.factor(month):Q +
                                     as.factor(month):Sd, data = as.data.frame(.x)))

#=================#
# Make predictions

# Load testing data (note that it has only 95 years of data and no CO2)
urban_train <- urban # don't overwrite training data
load("data/cesm_r1.RData")

# Data Processing
urban[ , , "LiqPrecip"] <- log(urban[ , , "LiqPrecip"] + 0.001)
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)

# Add month variable
d <- t(array(rep(rep(1:12, times = 95), times = dim(urban)[1]),
             dim = c(1140, sum(is_urban))))
urban_cube <- abind(d, urban[is_urban, , c("Ld", "LiqPrecip", "P", "Q", "Sd", "Tu")])
dimnames(urban_cube)[[3]][1] <- "month"
urban_list <- alply(urban_cube, 1)

# grab PCO2 from original run
dimnames(urban)[[3]] <- c("Tref", "Q", "P", "Ld", "Sd", "LiqPrecip")

is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
urban_list <- urban[is_urban, , c("Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd", "T", "Tu")] %>%k
  alply(1) %>%
  map(~apply(.x, MARGIN = 2, scale))


