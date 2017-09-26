#=================#
# Spatial mapping
#=================#
# Calculate the multi-model mean change and robustness

ESMlist <- read.table("CMIP5ESMlist.txt",colClasses = "character")
load("data/is_urban.RData")
pred_GlobTu_CMIP5<-matrix(0,nrow=4439,ncol=1140)
pred_GlobTu_3D<-array(NA, c(4439,1140,length(ESMlist)))
dimnames(pred_GlobTu_3D)[[3]]<-ESMlist
for (ESM in ESMlist) {
  load(paste0("data/Tu_pred_",ESM,".RData"))
  pred_GlobTu_CMIP5<-pred_GlobTu_CMIP5+pred_mat
  pred_GlobTu_3D[,,ESM]<-pred_mat
}
pred_GlobTu_CMIP5<-pred_GlobTu_CMIP5/length(ESMlist)
Jun20812100<-seq(from=906,to=1140,by=12)
Jul20812100<-seq(from=907,to=1140,by=12)
Aug20812100<-seq(from=908,to=1140,by=12)
summer20812100<-sort(c(Jun20812100,Jul20812100,Aug20812100))
Jun20062015<-seq(from=6,to=120,by=12)
Jul20062015<-seq(from=7,to=120,by=12)
Aug20062015<-seq(from=8,to=120,by=12)
summer20062015<-sort(c(Jun20062015,Jul20062015,Aug20062015))
Dec20812100<-seq(from=912,to=1140,by=12)
Jan20812100<-seq(from=901,to=1140,by=12)
Feb20812100<-seq(from=902,to=1140,by=12)
winter20812100<-sort(c(Dec20812100,Jan20812100,Feb20812100))
Dec20062015<-seq(from=12,to=120,by=12)
Jan20062015<-seq(from=1,to=120,by=12)
Feb20062015<-seq(from=2,to=120,by=12)
winter20062015<-sort(c(Dec20062015,Jan20062015,Feb20062015))
GlobTuSummer20812100<-rowMeans(pred_GlobTu_CMIP5[,summer20812100])
GlobTuSummer20062015<-rowMeans(pred_GlobTu_CMIP5[,summer20062015])
GlobTuChangeSummer<-GlobTuSummer20812100 - GlobTuSummer20062015
GlobTuWinter20812100<-rowMeans(pred_GlobTu_CMIP5[,winter20812100])
GlobTuWinter20062015<-rowMeans(pred_GlobTu_CMIP5[,winter20062015])
GlobTuChangeWinter<-GlobTuWinter20812100 - GlobTuWinter20062015
GlobTu20812100<-rowMeans(pred_GlobTu_CMIP5[,901:1140])
GlobTu20062015<-rowMeans(pred_GlobTu_CMIP5[,1:120])
GlobTuChange<-GlobTu20812100 - GlobTu20062015
GlobTuUncert<-apply(pred_GlobTu_3D[,901:1140,], c(1,3), mean)
GlobTuUncert<-apply(GlobTuUncert,1,function(x) sd(x)/sqrt(length(ESMlist)))
load("data/cesm_urban.rda")
library(stringr)
latlon <- dimnames(urban)[[1]] %>%
  str_split(", ") %>%
  unlist()
lat <- as.integer(latlon[seq(1, length(latlon), 2)])
lon <- as.integer(latlon[seq(2, length(latlon), 2)])
lat <- lat[is_urban]
lon <- lon[is_urban]
lon[lon > 180] <- lon[lon > 180] - 360

# Plot RMSE map
library(ggplot2)
library(mapdata)
map.world <- map_data(map = "world")
map.world$long <- map.world$long - 180
over<-ifelse(GlobTuUncert<0.48,1,NA)
ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTuChange), aes(x = lon, y = lat, colour = GlobTuChange), size = 1) +
  scale_color_distiller(name = expression(paste(Delta,"T (K)",sep="")),
                        palette = "Spectral",
                        limits = c(2, 7),
                        labels = c("3", "4", "5", "6"),
                        breaks = c(3, 4, 5, 6)) +
  geom_point(data = as.data.frame(over), aes(x = lon, y = lat), shape = ".", size = over)
ggsave("GlobTuChangeWithUncert.pdf", dpi=600)

ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTu20812100), aes(x = lon, y = lat, colour = GlobTu20812100), size = 1) +
  scale_color_distiller(name = "T (K)",
                        palette = "Spectral",
                        limits = c(260, 310),
                        labels = c("270", "280", "290", "300"),
                        breaks = c(270, 280, 290, 300)) 
ggsave("GlobTu20812100.pdf", dpi=600)

ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTu20062015), aes(x = lon, y = lat, colour = GlobTu20062015), size = 1) +
  scale_color_distiller(name = "T (K)",
                        palette = "Spectral",
                        limits = c(260, 310),
                        labels = c("270", "280", "290", "300"),
                        breaks = c(270, 280, 290, 300))
ggsave("GlobTu20062015.pdf", dpi=600)

ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTuUncert), aes(x = lon, y = lat, colour = GlobTuUncert), size = 1) +
  scale_color_distiller(name = "T (K)",
                        palette = "Spectral",
                        limits = c(0.2, 1),
                        labels = c("0.3", "0.6", "0.9"),
                        breaks = c(0.3, 0.6, 0.9))
ggsave("GlobTuUncert.pdf", dpi=600)

ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTuChangeSummer), aes(x = lon, y = lat, colour = GlobTuChangeSummer), size = 1) +
  scale_color_distiller(name = "T (K)",
                        palette = "Spectral",
                        limits = c(2, 7),
                        labels = c("3", "4", "5", "6"),
                        breaks = c(3, 4, 5, 6)) 
ggsave("GlobTuChangeSummer.pdf", dpi=600)

ggplot(map.world, aes(x = long + 180, y = lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_tile(data = as.data.frame(GlobTuChangeWinter), aes(x = lon, y = lat, colour = GlobTuChangeWinter), size = 1) +
  scale_color_distiller(name = "T (K)",
                        palette = "Spectral",
                        limits = c(2, 7),
                        labels = c("3", "4", "5", "6"),
                        breaks = c(3, 4, 5, 6)) 
ggsave("GlobTuChangeWinter.pdf", dpi=600)


