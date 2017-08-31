#=================#
# Prediction
#=================#
# Use data from runxx to get predictions of Tu using emulator
# This script is for the use in Cheyenne/Yellowstone

library(abind, lib.loc='~leizhao/lib')
library(plyr)
library(tidyr,lib.loc='~leizhao/lib')
library(readr,lib.loc='~leizhao/lib')
library(purrr, lib.loc='~leizhao/lib')
library(tidyverse, lib.loc='~leizhao/lib')

#=================#
wd<-getwd()  #directory where the script is in on Cheyenne/Yellowstone
cat(paste("********* working directory:",wd,"******************"),sep='\n')
# Fit Model

# Load training data
cat("************** loading training data ******************",sep='\n')
load("cesm_urban.rda")
load("cesm_urban_UV.RData")
cat("************** Done: training data loaded ******************",sep='\n')
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

# Fit model: full variables plus all two-way interactions
cat("************** model fitting ******************",sep='\n')
m_full_list <- purrr::map(urban_list, ~lm(Tu ~ U + V +Tref + Ld + LiqPrecip + P + PCO2 + Q + Sd + as.factor(month) +
                                            as.factor(month):U + as.factor(month):V + as.factor(month):Tref + 
                                            as.factor(month):Ld + as.factor(month): LiqPrecip +
                                            as.factor(month):P + as.factor(month):PCO2 + as.factor(month):Q +
                                            as.factor(month):Sd, data = as.data.frame(.x)))
cat("************** Done: model fitted ******************",sep='\n')

#=================#
# Make predictions

# Load testing data (note that it has only 95 years of data and no CO2)
urban_train <- urban # don't overwrite training data
ESMlist <- read.table("CMIP5ESMlist.txt",colClasses = "character")
for (ESM in ESMlist) {
  cat("******************************************",sep='\n')
  cat(paste("*********Predicting on",ESM,"****************"),sep='\n')
  ESMdata <- paste0(wd, "/", ESM, "/", ESM, "_r1.RData")
  cat(paste("*********Loading urban matrix data in",ESMdata,"******************"),sep='\n')
  load(ESMdata)
  # Data Processing
  # Add month variable
  d <- t(array(rep(rep(1:12, times = 95), times = dim(urban)[1]),
               dim = c(1140, sum(is_urban))))
  urban_cube <- abind(d, urban[is_urban, , ])
  
  # grab PCO2 from original run
  urban_cube <- abind(urban_train[is_urban, 13:1152, "PCO2"], urban_cube)
  dimnames(urban_cube)[[3]][1:2] <- c("PCO2", "month")
  
  # prepare as newdata
  urban_cube <- urban_cube[ , , c("month", "U", "V", "Tref", "Ld", "LiqPrecip", "P", "PCO2", "Q", "Sd")]
  urban_cube[ , , "LiqPrecip"] <- log(urban_cube[ , , "LiqPrecip"] + 0.001)
  newdata <- urban_cube %>%
    alply(1, as.data.frame)
  
  # predict
  pred_list <- purrr::map2(.x = m_full_list, .y = newdata, ~predict(.x, newdata = .y))
  pred_mat <- unlist(pred_list) %>%
    matrix(ncol = 1140, byrow = TRUE)
  
  # output
  save(pred_mat,file=paste0('Tu_pred_',ESM,'.RData'))
  # memory collection
  rm(urban, urban_cube, newdata, pred_list, pred_mat)
  gc()
}


#================#
# Computed weighted average time series
#load("data/area_u.Rdata")
#weights <- area_u[is_urban]
#weights <- weights/sum(weights)
#weight_mat <- matrix(rep(weights, 1140), ncol = 1140)

#pred_Tu <- colSums(weight_mat * pred_mat)

# output
#devtools::use_data(pred_Tu, overwrite = TRUE)

#df <- data.frame(pred_Tu, year = as.factor(rep(1:95, each = 12) + 2005))
#df2 <- df %>%
#  group_by(year) %>%
#  summarize(avg_T = mean(pred_Tu))

#ggplot(df2, aes(x = year, y = avg_T)) +
#  geom_point() +
#  theme_minimal()




