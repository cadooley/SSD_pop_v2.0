
rm(list=ls())

library(sf)         # version 0.8-1
library(rgdal)      # version 1.4-8
library(rgeos)      # version 0.5-2
library(data.table) # version 1.12.2
library(curl)       # version 3.3
library(raster)     # version 3.0-7

setwd('')

# OCHA boundaries cover larger area than WPG SSD mastergird 
# so need to read-in all countries in region and extend/crop to match OCHA

# countries: CAF, DRC, ETH, KEN, SDN, SSD, UGA

# ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/


### unmasked cov layers

mgrid <- raster('input/bf_SSD_region/SSDplus_mastergrid.tif')

tempfile_path <- '/input/wpg_covs/temp'

countries <- c('CAF', 'COD', 'ETH', 'KEN', 'SDN', 'SSD', 'UGA')


for(cov_no in 2:19){
  print(cov_no)
  dir.create(paste(getwd(),'/input/wpg_covs/temp',sep=''))
  for(iso_code in countries){
    cov_list <- c(paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/Accessibility/',tolower(iso_code),'_tt50k_100m_2000.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/BSGM/2020/DTE/',tolower(iso_code),'_dst_bsgme_100m_2020.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/DMSP/',tolower(iso_code),'_dmsp_100m_2011.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst011_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst040_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst130_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst140_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst150_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst160_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst190_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Annual/2015/',tolower(iso_code),'_esaccilc_dst200_100m_2015.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/ESA_CCI_Water/DST/',tolower(iso_code),'_esaccilc_dst_water_100m_2000_2012.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/OSM/DST/',tolower(iso_code),'_osm_dst_road_100m_2016.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/OSM/DST/',tolower(iso_code),'_osm_dst_roadintersec_100m_2016.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/OSM/DST/',tolower(iso_code),'_osm_dst_waterway_100m_2016.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/Slope/',tolower(iso_code),'_srtm_slope_100m.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/Topo/',tolower(iso_code),'_srtm_topo_100m.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/VIIRS/',tolower(iso_code),'_viirs_100m_2016.tif',sep=''),
      paste('ftp://ftp.worldpop.org/GIS/Covariates/Global_2000_2020/',iso_code,'/WDPA/WDPA_1/',tolower(iso_code),'_wdpa_dst_cat1_100m_2017.tif',sep=''))
  
    # save full list of covariate download locations for ref - only need to do once 
    if(cov_no == 2){
      saveRDS(cov_list,paste('input/wpg_covs/cov_files_list_',iso_code,'.rds',sep='')) 
    }
     
    cov <- cov_list[cov_no]
    utils::download.file(url = cov,
                         destfile = paste(getwd(),tempfile_path,"/",iso_code,"temp.tif",sep=''),
                         mode="wb",
                         quiet=FALSE,
                         method="libcurl")

  }
  full <- list.files(paste(getwd(),tempfile_path,sep=''),full.names = TRUE)
  e1 <- extent(mgrid)
  rmaster <- mgrid
  for(k in 1:length(full)){
    r1 <- crop(raster(full[k]),e1)
    rmaster <- merge(r1,rmaster)
  }
  writeRaster(rmaster,paste('input/wpg_covs/SSDplus_region_unmasked/cov_',cov_no,'.tif',sep=''))
  unlink(paste(getwd(),'/input/wpg_covs/temp',sep=''),recursive=TRUE)
  rm(cov,full,rmaster,r1)
}



### masking wpg cov layers using a bf metric layer

mgrid <- raster('input/bf_SSD_region/SSDplus_buildings_count.tif')

full <- list.files('input/wpg_covs/SSDplus_region_unmasked',full.names = FALSE)

for(i in full){
  cov_temp <- raster(paste('input/wpg_covs/SSDplus_region_unmasked/',i,sep=''))
  cov_temp <- mask(cov_temp, mgrid)
  writeRaster(cov_temp, paste('input/wpg_covs/SSDplus_region_masked/',i,sep=''))
  rm(cov_temp)
}


### masking worldclim cov layers using a bf metric layer

mgrid <- raster('input/bf_SSD_region/SSDplus_buildings_count.tif')

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


### crop & mask climafrica & hydrosheds layers 

mgrid <- raster('input/bf_SSD_region/SSDplus_buildings_count.tif')

fa <- raster('input/hydrosheds/af_acc_15s_bil/af_acc_15s.bil')
fa <- crop(fa, extent(mgrid))
fa <- disaggregate(fa,fact=5)
fa <- resample(fa, mgrid,method='ngb')
fa <- mask(fa, mgrid)
writeRaster(fa, 'input/hydrosheds/masked_hydrosheds/flow_acc.tif')
rm(fa)



