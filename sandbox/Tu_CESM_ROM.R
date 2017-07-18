library(ncdf4)
library(MASS)

# replicating matlab script

ncdir <-'/Users/Face2sea/Documents/MATLAB/GlobalUrbanProj/'
# urban screen-height temperature
file_Tu <-'b40.rcp8_5.1deg.001.clm2.h0.TSA_U.200501-210012.nc' 
f <- nc_open(paste0(ncdir,file_Tu))
Tu<-ncvar_get(f,'TSA_U'); nc_close(f)
# atmospheric potential tempetature at reference height
file_TH <-'b40.rcp8_5.1deg.001.clm2.h0.THBOT.200501-210012.nc' 
f <- nc_open(paste0(ncdir,file_TH))
TH<-ncvar_get(f,'THBOT'); nc_close(f)
# specific humidity at reference height
file_Q<-'b40.rcp8_5.1deg.001.clm2.h0.QBOT.200501-210012.nc' 
f <- nc_open(paste0(ncdir,file_Q))
Q<-ncvar_get(f,'QBOT'); nc_close(f)
# pressture at reference height
file_P<-'b40.rcp8_5.1deg.001.clm2.h0.PBOT.200501-210012.nc' 
f <- nc_open(paste0(ncdir,file_P))
P<-ncvar_get(f,'PBOT'); nc_close(f)
# atmospheric tempetature at reference height
file_Tref<-'b40.rcp8_5.1deg.001.clm2.h0.TBOT.200501-210012.nc'  
f <- nc_open(paste0(ncdir,file_Tref))
Tref<-ncvar_get(f,'TBOT'); nc_close(f)
# longwave down
file_Ld<-'b40.rcp8_5.1deg.001.clm2.h0.FLDS.200501-210012.nc'
f <- nc_open(paste0(ncdir,file_Ld))
Ld<-ncvar_get(f,'FLDS'); nc_close(f)
# shortwave down
file_Sd<-'b40.rcp8_5.1deg.001.clm2.h0.FSDS.200501-210012.nc'
f <- nc_open(paste0(ncdir,file_Sd))
Sd<-ncvar_get(f,'FSDS'); nc_close(f)
# rain
file_LiqPrecip<-'b40.rcp8_5.1deg.001.clm2.h0.RAIN.200501-210012.nc' 
f <- nc_open(paste0(ncdir,file_LiqPrecip))
LiqPrecip<-ncvar_get(f,'RAIN'); nc_close(f)
# SNOW
file_SolidPrecip<-'b40.rcp8_5.1deg.001.clm2.h0.SNOW.200501-210012.nc'
f <- nc_open(paste0(ncdir,file_SolidPrecip))
SolidPrecip<-ncvar_get(f,'SNOW'); nc_close(f)
# CO2 pressure
file_PCO2<-'b40.rcp8_5.1deg.001.clm2.h0.PCO2.200501-210012.nc'
f <- nc_open(paste0(ncdir,file_PCO2))
PCO2<-ncvar_get(f,'PCO2'); nc_close(f)

urbflag=!is.na(Tu[,,1])
count=0
R2Map=matrix(data=NA,nrow=288,ncol=192)
RMSEMap=matrix(data=NA,nrow=288,ncol=192)
mdls<-array(list(),c(288,192))
for (i in 1:dim(Tu)[1]) {
  for (j in 1:dim(Tu)[2]) {
    if (urbflag[i,j]) {
      count=count+1
      Tu1d=Tu[i,j,]; TH1d=TH[i,j,]; Q1d=Q[i,j,]; P1d=P[i,j,]; Q1d=Q[i,j,] 
      Tref1d=Tref[i,j,];Ld1d=Ld[i,j,];Sd1d=Sd[i,j,];LiqPrecip1d=LiqPrecip[i,j,]
      SolidPrecip1d=SolidPrecip[i,j,]; PCO21d=PCO2[i,j,]
      cesm_data=cbind(Sd1d,Ld1d,TH1d,Tref1d,Q1d,P1d,LiqPrecip1d,SolidPrecip1d,PCO21d,Tu1d)
      mdl = lm('Tu1d~.',data=as.data.frame(cesm_data))
      step <- stepAIC(mdl, direction = 'backward')
      mdls[[i,j]]=mdl
      R2Map[i,j]=summary(mdl)$r.squared
      print(count)
    }
  }
}
