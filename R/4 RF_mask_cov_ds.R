
rm(list=ls())

library(raster)

setwd('')

# mask out displacement sites of all covariate layers ready for RF disaggregation

# user will need to edit the following directories to point to where covariates from scripts 2 & 3 have been saved
lf <- list.files('input/wpg_covs/SSDplus_region_masked/',pattern="tif$",full.names=TRUE) 
lf2 <- list.files('input/bf_SSD_region/',pattern=c('buildings','tif$'),full.names=TRUE) 
lf3 <- list.files('input/acled/covs/',pattern='tif$',full.names=TRUE)  
lf4 <- list.files('input/worldclim/masked_prec/',pattern='wc2.1_30s',full.names=TRUE) 
lf5 <- list.files('input/hydrosheds/masked_hydrosheds/',pattern='tif$',full.names=TRUE) 

lf_full <- c(lf,lf2,lf3,lf4,lf5)

ds <- raster('output/rasters/ds_master_idp_count.tif')
dsdt<-data.table(ds[]);dsdt<-na.omit(dsdt);nrow(dsdt);rm(dsdt) # number of cells excluded because they're displacement sites
cov_names <- read.csv('input/wpg_covs/cov_names.csv',header=T)

for(i in 1:length(lf_full)){
  r1 <- mask(raster(lf_full[i]), ds, inverse=TRUE)
  if(names(r1) %in% cov_names$id_name){
    names(r1) <- cov_names$variable[cov_names$id_name == names(r1)]
  }
  if(names(r1) == 'flow_acc'){
    names(r1) <- 'hydrosheds_flow_acc'
  }
  writeRaster(r1,paste('input/RF_covs/',names(r1),'.tif',sep=''))
  rm(r1)
}


