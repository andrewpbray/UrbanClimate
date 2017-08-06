library(ncdf4)
library(tidyverse)
library(purrr)
library(devtools)
library(abind)

ncdir <-'/glade/p/work/leizhao/GlobUrbProj/CanESM2/'
#ncdir <-'/Users/Face2sea/Documents/Study/@Princeton/Research/UrbanProjectionCMIP5/UrbanClimate/data-import/'
modelrcp <- '_Amon_CanESM2_rcp85_'
rlztn <- c('r1i1p1','r2i1p1','r3i1p1','r4i1p1','r5i1p1','r6i1p1')
suffix <- '_200601-210012.nc'
for (rl in 1:1) {
  print(rl)
  # atmospheric tempetature at reference height
  varprefix <- 'tas'
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  Tref<-ncvar_get(f,'tas'); lat<-ncvar_get(f,'lat');lon<-ncvar_get(f,'lon')
  lat2d<-matrix(rep(lat,each=length(lon)),ncol = length(lat))
  lon2d<-matrix(rep(lon,times=length(lat)),ncol = length(lat))
  nc_close(f)
  # near-surface specific humidity
  varprefix<-'huss' 
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  Q<-ncvar_get(f,'huss'); nc_close(f)
  # near-surface pressture
  varprefix<-'ps' 
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  P<-ncvar_get(f,'ps'); nc_close(f)
  # longwave down
  varprefix<-'rlds'
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  Ld<-ncvar_get(f,'rlds'); nc_close(f)
  # shortwave down
  varprefix<-'rsds'
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  Sd<-ncvar_get(f,'rsds'); nc_close(f)
  # rain
  varprefix<-'pr' 
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix))
  LiqPrecip<-ncvar_get(f,'pr'); nc_close(f)
  
  # squeeze lat/lon into a single margin and make into a 3d array
  # dims: [gridcell, time, variable]
  varlist<-list(Tref,Q,P,Ld,Sd,LiqPrecip)
  urban <- map(varlist, ~matrix(.x, ncol = dim(.x)[3])) %>%
    abind(along = 3)
  # add lat/lon as names to each gridcell
  latlon<-list(lat2d,lon2d); names(latlon) <- c('lat2d', 'lon2d')
  dimnames(urban)[[1]] <- paste0(latlon[["lat2d"]], ", ", latlon[["lon2d"]])
  save(urban,file=paste0('CanESM2_r',toString(rl),'.RData'))
  print(paste0('done saving realization #',toString(rl)))
}




