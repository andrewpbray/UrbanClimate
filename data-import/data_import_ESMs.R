# This script is to read .nc files and to structure the forcings variables to be used 
library(ncdf4)

ncdir <-'/Users/Face2sea/Documents/MATLAB/GlobalUrbanProj/'
# atmospheric tempetature at reference height
file_Tref<-'tas_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc'  
f <- nc_open(paste0(ncdir,file_Tref))
Tref<-ncvar_get(f,'tas'); nc_close(f)
# near-surface specific humidity
file_Q<-'huss_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc' 
f <- nc_open(paste0(ncdir,file_Q))
Q<-ncvar_get(f,'huss'); nc_close(f)
# near-surface pressture
file_P<-'ps_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc' 
f <- nc_open(paste0(ncdir,file_P))
P<-ncvar_get(f,'ps'); nc_close(f)
# longwave down
file_Ld<-'rlds_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc'
f <- nc_open(paste0(ncdir,file_Ld))
Ld<-ncvar_get(f,'rlds'); nc_close(f)
# shortwave down
file_Sd<-'rsds_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc'
f <- nc_open(paste0(ncdir,file_Sd))
Sd<-ncvar_get(f,'rsds'); nc_close(f)
# rain
file_LiqPrecip<-'pr_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc' 
f <- nc_open(paste0(ncdir,file_LiqPrecip))
LiqPrecip<-ncvar_get(f,'pr'); nc_close(f)
# CO2 
file_CO2<-'co2_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc'
f <- nc_open(paste0(ncdir,file_CO2))
CO2<-ncvar_get(f,'co2'); nc_close(f)








