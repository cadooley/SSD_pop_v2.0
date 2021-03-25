


rm(list=ls())

library(data.table)
library(raster)

setwd('')

pop <- raster('output/rasters/final_pop_count.tif')
dt <- data.table(pop[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(pop) <- 'SSD_population_v2_0_gridded_population'
#writeRaster(pop,'output/rasters/final/SSD_population_v2_0_gridded/SSD_population_v2_0_gridded_population.tif')

mgrid <- pop
mgrid[!is.na(mgrid)] <- 1
dt <- data.table(mgrid[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(mgrid) <- 'SSD_population_v2_0_mastergrid'
#writeRaster(mgrid,'output/rasters/final/SSD_population_v2_0_mastergrid.tif')

rm(pop)

idp <- raster('output/rasters/master_idp_count.tif')
dt <- data.table(idp[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
idp[is.na(idp) & !is.na(mgrid)] <- 0
dt <- data.table(idp[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(idp) <- 'SSD_population_v2_0_gridded_idps'
#writeRaster(idp,'output/rasters/final/SSD_population_v2_0_gridded/SSD_population_v2_0_gridded_idps.tif')

rm(idp)

df <- raster('output/rasters/displaced_from_per_cell_edited.tif')
dt <- data.table(df[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(df) <- 'SSD_population_v2_0_intermed_displacedfrom'
#writeRaster(df,'output/rasters/final/SSD_population_v2_0_intermed/SSD_population_v2_0_intermed_displacedfrom.tif')


unadj <- raster('output/rasters/unadj_pop_per_cell.tif')
dt <- data.table(unadj[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
unadj[is.na(unadj) & !is.na(mgrid)] <- 0
dt <- data.table(unadj[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(unadj) <- 'SSD_population_v2_0_intermed_censusproj'
#writeRaster(unadj,'output/rasters/final/SSD_population_v2_0_intermed/SSD_population_v2_0_intermed_censusproj.tif')


nonidps <- unadj - df
dt <- data.table(nonidps[]);dt<-na.omit(dt);nrow(dt);sum(dt[,1])
rm(dt)
names(nonidps) <- 'SSD_population_v2_0_gridded_nonidps'
#writeRaster(nonidps,'output/rasters/final/SSD_population_v2_0_gridded/SSD_population_v2_0_gridded_nonidps.tif')

rm(nonidps)

rm(df,unadj)

