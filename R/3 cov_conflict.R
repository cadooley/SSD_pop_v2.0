

rm(list=ls())
gc()
memory.limit(100000000000)

library(raster)
library(data.table)
library(sf)
library(fasterize)

setwd('')

# Armed Conflict Location Events Data ACLED downloaded from here (for recorded events up to 30th Sept 2020):
# https://www.acleddata.com/data/
acled <- read.csv("input/acled/2012-01-01-2020-09-30-South_Sudan.csv",header=T)

# raster mastergrid used for this study based on OCHA boundaries can be downloaded here:
# https://wopr.worldpop.org/?SSD/Population/v2.0
mgrid <- raster('input/bf_SSD_region/SSD_population_v2_0_mastergrid.tif')

years <- list(c('2020'),c('2019'),c('2018'),c('2017'),c('2016'),c('2015'),c('2014'))

acled$fat_bin <- 0
acled$fat_bin[acled$fatalities >= 5] <- 1
acled$fat_high <- 0
acled$fat_high[acled$fatalities >= 50] <- 1  # replace 50 with 20 to produce data for events with 20 or more fatalities

cellIDs <- which(!is.na(mgrid[]))
cellxy <- xyFromCell(mgrid, cell=cellIDs)
cellpts <- st_as_sf(as.data.frame(cellxy), coords=c('x','y'),crs='+proj=longlat +datum=WGS84 +no_defs')


# distance from conflict covariates

for(j in 1:length(years)){
  for(a in 1:3){
  
    print(years[[j]])
    print(a)
    
    r1 <- mgrid
    r1[] <- NA
    
    if(a == 1){
      acled_sub <- acled[acled$fat_bin == 1 & acled$year %in% years[[j]],]
    }
    
    if(a == 2){
      acled_sub <- acled[acled$fat_high == 1 & acled$year %in% years[[j]],]
    }
    
    if(a == 3){
      acled_sub <- acled[acled$event_type %in% c('Battles','Violence against civilians') & acled$year %in% years[[j]],]
    }
    
    acledpts <- st_as_sf(as.data.frame(acled_sub[,c('longitude','latitude')]), coords=c('longitude','latitude'),crs='+proj=longlat +datum=WGS84 +no_defs')
    dists <- st_distance(cellpts, acledpts)
    dt1 <- as.data.table(dists)
    dt1 <- round(dt1/1000,2)
    rowmins <- apply(dt1,1,min)
    r1[cellIDs] <- rowmins
    
    if(a ==1){
      writeRaster(r1, paste('input/acled/covs/dist_fatal5_',years[[j]],'.tif',sep=''))
    }
    
    if(a ==2){
      writeRaster(r1, paste('input/acled/covs/dist_fatal50_',years[[j]],'.tif',sep=''))
    }
    
    if(a ==3){
      writeRaster(r1, paste('input/acled/covs/dist_violent_events_',years[[j]],'.tif',sep=''))
    }
    
    rm(r1,rowmins,dt1,dists,acledpts)
  }
}


