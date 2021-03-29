
rm(list=ls())

library(sf)         # version 0.8-1
library(rgdal)      # version 1.4-8
library(rgeos)      # version 0.5-2
library(data.table) # version 1.12.2
library(curl)       # version 3.3
library(raster)     # version 3.0-7

setwd('')

# WorldPop Global covariates are based on WPG SSD mastergrid
# OCHA boundaries used for this work cover larger area than WPG SSD mastergrid 
# so included CAF, DRC, ETH, KEN, SDN, SSD, UGA WorldPop Global covariates
# cropped multi-country area to match SSD OCHA admin level 2 boundaries

# WorldPop Global covariates:
# https://data.worldpop.org/GIS/Covariates/

# SSD OCHA admin level 2 boundaries
# https://data.humdata.org/dataset/south-sudan-administrative-boundaries


# raster mastergrid used for this study based on OCHA boundaries can be downloaded here:
# https://wopr.worldpop.org/?SSD/Population/v2.0
mgrid <- raster('input/bf_SSD_region/SSD_population_v2_0_mastergrid.tif')
countries <- c('CAF', 'COD', 'ETH', 'KEN', 'SDN', 'SSD', 'UGA')


### masking wpg cov layers using mastergrid

# user should point to the folder with WorldPop Global covariates that have extents matching mastergrid
full <- list.files('input/wpg_covs/SSDplus_region_unmasked',full.names = FALSE)

for(i in full){
  cov_temp <- raster(paste('input/wpg_covs/SSDplus_region_unmasked/',i,sep=''))
  cov_temp <- mask(cov_temp, mgrid)
  writeRaster(cov_temp, paste('input/wpg_covs/SSDplus_region_masked/',i,sep=''))
  rm(cov_temp)
}


### masking worldclim cov layers then resampling to obtain same resolution grid as the mastergrid

# user should point to the folder with WorldClim precipitation data
# Downloaded from here: https://www.worldclim.org/data/worldclim21.html
full <- list.files('input/worldclim/wc2.1_30s_prec',full.names = FALSE)
full <- full[grep('.tif$', full)]

for(i in full){
  cov_temp <- raster(paste('input/worldclim/wc2.1_30s_prec/',i,sep=''))
  cov_temp <- crop(cov_temp,extent(mgrid))
  cov_temp <- disaggregate(cov_temp,fact=10)
  cov_temp <- resample(cov_temp, mgrid,method='ngb')
  cov_temp <- mask(cov_temp, mgrid)
  writeRaster(cov_temp, paste('input/worldclim/masked_prec/',i,sep=''))
  rm(cov_temp)
}


### crop & mask hydrosheds layers 

# user should point to the folder with HydroSHEDS flow accumulation data
# Downloaded from here: www.hydrosheds.org
fa <- raster('input/hydrosheds/af_acc_15s_bil/af_acc_15s.bil')
fa <- crop(fa, extent(mgrid))
fa <- disaggregate(fa,fact=5)
fa <- resample(fa, mgrid,method='ngb')
fa <- mask(fa, mgrid)
writeRaster(fa, 'input/hydrosheds/masked_hydrosheds/flow_acc.tif')
rm(fa)



