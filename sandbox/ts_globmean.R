#=================#
# Global time series and uncertainty analysis
#=================#
# Calculate the multi-model annual global mean and uncertainty range

library(ggplot2)
ESMlist <- read.table("CMIP5ESMlist.txt",colClasses = "character")
#================#
GlobTu_CMIP5 <- data.frame(matrix(NA, nrow = 1140, ncol = length(ESMlist)))
colnames(GlobTu_CMIP5) <- ESMlist
# Computed weighted average time series
load("data/area_u.Rdata")
load("data/is_urban.RData")
weights <- area_u[is_urban]
weights <- weights/sum(weights)
weight_mat <- matrix(rep(weights, 1140), ncol = 1140)
for (ESM in ESMlist) {
  load(paste0("data/Tu_pred_",ESM,".RData"))
  pred_Tu <- colSums(weight_mat * pred_mat)
  #plot(pred_Tu,type='l')
  GlobTu_CMIP5[,ESM]<-pred_Tu
}
# load global land mean T in CMIP5
load("data/LandMeanT_CMIP5.RData")
# calc. annual values and stats.
AnnualGlobTu_CMIP5 <- data.frame(matrix(NA, nrow = 95, ncol = length(ESMlist)))
AnnualGlobLandMeanT_CMIP5 <- data.frame(matrix(NA, nrow = 95, ncol = length(ESMlist)))
AnnualUrbLandMeanT_CMIP5 <- data.frame(matrix(NA, nrow = 95, ncol = length(ESMlist)))
colnames(AnnualGlobTu_CMIP5) <- ESMlist
colnames(AnnualGlobLandMeanT_CMIP5) <- ESMlist
colnames(AnnualUrbLandMeanT_CMIP5) <- ESMlist
for (yr in 1:95) {
  print(yr)
  AnnualGlobTu_CMIP5[yr,]<-colMeans(GlobTu_CMIP5[(yr*12-11):(yr*12),])
  AnnualGlobLandMeanT_CMIP5[yr,]<-colMeans(GlobLandMeanT_CMIP5[(yr*12-11):(yr*12),])
  AnnualUrbLandMeanT_CMIP5[yr,]<-colMeans(UrbLandMeanT_CMIP5[(yr*12-11):(yr*12),])
}
nESM<-length(ESMlist)
# data of urban grid cells with urban
df_urban<-data.frame(matrix(NA, nrow = 95, ncol = 5))
colnames(df_urban)<-c("Mean","Lower","Upper","Year","Group")
df_urban[,"Year"]<-2006:2100
df_urban[,"Mean"]<-apply(AnnualGlobTu_CMIP5,1,mean)
se<-apply(AnnualGlobTu_CMIP5,1,function(x) sd(x)/sqrt(nESM))
df_urban[,"Lower"]<-df_urban[,"Mean"]-2*se
df_urban[,"Upper"]<-df_urban[,"Mean"]+2*se
df_urban[,"Group"]<-"URBAN"
# data of urban grid cells without urban
df_urbancell<-data.frame(matrix(NA, nrow = 95, ncol = 5))
colnames(df_urbancell)<-c("Mean","Lower","Upper","Year","Group")
df_urbancell[,"Year"]<-2006:2100
df_urbancell[,"Mean"]<-apply(AnnualUrbLandMeanT_CMIP5,1,mean)
se<-apply(AnnualUrbLandMeanT_CMIP5,1,function(x) sd(x)/sqrt(nESM))
df_urbancell[,"Lower"]<-df_urbancell[,"Mean"]-2*se
df_urbancell[,"Upper"]<-df_urbancell[,"Mean"]+2*se
df_urbancell[,"Group"]<-"NO-URBAN"
# data of global land
df_globland<-data.frame(matrix(NA, nrow = 95, ncol = 5))
colnames(df_globland)<-c("Mean","Lower","Upper","Year","Group")
df_globland[,"Year"]<-2006:2100
df_globland[,"Mean"]<-apply(AnnualGlobLandMeanT_CMIP5,1,mean)
se<-apply(AnnualGlobLandMeanT_CMIP5,1,function(x) sd(x)/sqrt(nESM))
df_globland[,"Lower"]<-df_globland[,"Mean"]-2*se
df_globland[,"Upper"]<-df_globland[,"Mean"]+2*se
df_globland[,"Group"]<-"GLOBLAND"
# combine 3 dataframes
df_CMIP5 <- rbind(df_urban,df_urbancell,df_globland)
# plot
p<-ggplot(data=df_CMIP5, aes(x=Year, y=Mean, colour=Group)) + geom_line()
p<-p+geom_ribbon(aes(ymin=df_CMIP5$Lower, ymax=df_CMIP5$Upper),linetype=0, alpha=0.3)
p<-p+scale_colour_discrete(name="",
                           labels=c("Global land", "No-urban", "Urban"))
p<-p+xlab("Year")
p<-p+ylab("Annual mean 2m air temperature (K)")

#df <- data.frame(pred_Tu, year = as.factor(rep(1:95, each = 12) + 2005))
#df2 <- df %>%
#  group_by(year) %>%
#  summarize(avg_T = mean(pred_Tu))

#ggplot(df2, aes(x = year, y = avg_T)) +
#  geom_point() +
#  theme_minimal()
