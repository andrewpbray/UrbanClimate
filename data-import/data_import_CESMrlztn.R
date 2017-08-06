library(ncdf4)
library(tidyverse)
library(purrr)
library(devtools)
library(abind)

ncdir <-'/glade/p/work/leizhao/GlobUrbProj/CESM/'
#ncdir <-'/Users/Face2sea/Documents/Study/@Princeton/Research/UrbanProjectionCMIP5/UrbanClimate/data-import/'
modelrcp <- '_Amon_CCSM4_rcp85_'
rlztn <- c('r1i1p1','r2i1p1','r3i1p1','r4i1p1','r5i1p1','r6i1p1')
suffix <-c('_200601-210012.nc','_200501-210012.nc')
for (rl in 1:6) {
  print(rl)
  # atmospheric tempetature at reference height
  varprefix <- 'tas'
  if (rl < 6) {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  } else {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[2]))
  }
  Tref<-ncvar_get(f,'tas'); lat<-ncvar_get(f,'lat');lon<-ncvar_get(f,'lon')
  lat2d<-matrix(rep(lat,each=length(lon)),ncol = length(lat))
  lon2d<-matrix(rep(lon,times=length(lat)),ncol = length(lat))
  nc_close(f)
  # near-surface specific humidity
  varprefix<-'huss' 
  if (rl < 6) {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  } else {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[2]))
  }
  Q<-ncvar_get(f,'huss'); nc_close(f)
  # near-surface pressture
  varprefix<-'ps' 
  if (rl < 6) {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  } else {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[2]))
  }
  P<-ncvar_get(f,'ps'); nc_close(f)
  # longwave down
  varprefix<-'rlds'
  if (rl < 6) {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  } else {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[2]))
  }
  Ld<-ncvar_get(f,'rlds'); nc_close(f)
  # shortwave down
  varprefix<-'rsds'
  if (rl < 6) {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  } else {
    f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[2]))
  }
  Sd<-ncvar_get(f,'rsds'); nc_close(f)
  # rain
  varprefix<-'pr' 
  f <- nc_open(paste0(ncdir,varprefix,modelrcp,rlztn[rl],suffix[1]))
  LiqPrecip<-ncvar_get(f,'pr'); nc_close(f)
  
  # squeeze lat/lon into a single margin and make into a 3d array
  # dims: [gridcell, time, variable]
  if (rl<6) {
    varlist<-list(Tref,Q,P,Ld,Sd,LiqPrecip)
  } else {
    varlist<-list(Tref[,,13:1152],Q[,,13:1152],P[,,13:1152],Ld[,,13:1152],Sd[,,13:1152],LiqPrecip)
  }
  urban <- map(varlist, ~matrix(.x, ncol = dim(.x)[3])) %>%
    abind(along = 3)
  # add lat/lon as names to each gridcell
  latlon<-list(lat2d,lon2d); names(latlon) <- c('lat2d', 'lon2d')
  dimnames(urban)[[1]] <- paste0(latlon[["lat2d"]], ", ", latlon[["lon2d"]])
  save(urban,file=paste0('cesm_r',toString(rl),'.RData'))
  print(paste0('done saving realization #',toString(rl)))
}




