

rm(list=ls())
gc()
memory.limit(100000000000)

library(sf)
library(raster)
library(fasterize)
library(stars)
library(data.table)

setwd('')

### script contents ###

# destination mapping
    # displaced sites - shp then raster
    # host community sites - raster than shp
# origin mapping - county level based on data at destination


# read in data
dtm <- read.csv('input/DTM/20213101 IOM SSD DTM MT R9_Baseline Area Dataset_September 2020_bl_sheet.csv',header=T)
dtmlocs <- read.csv('input/DTM/20210122 IOM DTM SSD MT R9 Baseline Locations for initial data release_0_bl_sheet.csv',header=T)
bc <- raster('input/bf_SSD_region/SSDplus_buildings_count.tif')

# clusters of settled cells
settclumps <- clump(bc,directions=8)


### destination mapping 

# county level idps

dtm_dt <- as.data.table(dtm)
county_idps <- dtm_dt[,sum(a..IDP.individuals),X0.county.name]
names(county_idps)[2] <- 'tot_idps_incounty'

#write.csv(county_idps,'output/tables/total_idps_destination_county.csv',row.names = F)


## displaced sites - shp then raster

# read in IDP locations & summarise
nrow(dtmlocs)                    # 2854 GPS located IDP pops
table((dtmlocs$location_type))
table(dtmlocs$IDP.Location..Yes.No.[dtmlocs$location_type == 'Displacement site ']) # 99 displacement sites with IDPs (some have closed but are still recorded)
table(dtmlocs$IDP.Location..Yes.No.[dtmlocs$location_type == 'Host community'])     # 2,097 host comms with IDPs (some report only returnees)

# subset displacement sites
dslocs <- dtmlocs[dtmlocs$location_type == 'Displacement site ' & dtmlocs$IDP.Location..Yes.No. == 'Yes',] 
dslocs$Estimated...of.IDP.households...total. <- as.numeric(as.character(dslocs$Estimated...of.IDP.households...total.))
dslocs$Estimated...of.IDP.individuals...total. <- as.numeric(as.character(dslocs$Estimated...of.IDP.individuals...total.))
dslocs$Estimated...of..IDP.households.not.previously.abroad <- as.numeric(as.character(dslocs$Estimated...of..IDP.households.not.previously.abroad))
dslocs$Estimated...of.IDP.individuals.not.previously.abroad <- as.numeric(as.character(dslocs$Estimated...of.IDP.individuals.not.previously.abroad))

# need to do each Korijo camp separately
korijo_row <- which(dslocs$village_idp_settlement_name == 'Korijo IDP Camp Zone 1,2 and 3')
korijo <- rbind(dslocs[korijo_row,],dslocs[korijo_row,],dslocs[korijo_row,])
korijo$village_idp_settlement_name <- c('Logo','Kerwa','Ajio')
proppopcount2017 <- c(7542,7381,2444)/sum(c(7542,7381,2444))
korijo$Estimated...of.IDP.individuals...total. <- round(proppopcount2017*dslocs[korijo_row,'Estimated...of.IDP.individuals...total.'])
korijo$longitude <- c(31.414,31.310,31.204)
korijo$latitude <- c(3.802,3.792,3.820)
dslocs <- dslocs[-korijo_row,] 
dslocs <- rbind(dslocs, korijo)


# # cross ref with UNOSAT camps data
# # CE20131218SSD_UNOSAT_REACH_2017_shp dataset
# unosat2017 <- list.files('input/UNOSAT_IDP_sett/CE20131218SSD_UNOSAT_REACH_2017_shp',full.names=T,recursive=T)
# unosat2017 <- unosat2017[grep('.shp$',unosat2017)]
# unosat2017 <- unosat2017[grep(c('IDP_Camp_Extent'),unosat2017)] # exclude files with ind. structures
# # CE20131218SSD_Minkaman_13012016.shp dataset
# unosat2016 <- list.files('input/UNOSAT_IDP_sett/CE20131218SSD_Minkaman_13012016.shp',full.names=T,recursive=T)
# unosat2016 <- unosat2016[grep('.shp$',unosat2016)]
# unosat2016 <- unosat2016[grep('IDP_Camp_Extent',unosat2016)]
# full <- c(unosat2016,unosat2017) # 22 files
# # save uncamp polygons in one file
# uncamps <- st_read(full[1])
# uncamps$fileID <- rep(1,nrow(uncamps))
# uncamps <- uncamps[,names(uncamps)%in%c('geometry','fileID')]
# for(i in 2:length(full)){
#   temp <- st_read(full[i])
#   temp$fileID <- rep(i,nrow(temp))
#   uncamps <- rbind(uncamps,temp[,names(temp)%in%c('geometry','fileID')])
# }
# uncamps_dis <- st_buffer(uncamps,0.0005)
# uncamps_dis <- st_union(uncamps_dis)
# uncamps_dis <- st_cast(uncamps_dis, 'POLYGON')
# uncamps_dis <- st_sf(uncamps_dis)
# uncamps_dis$area <- st_area(uncamps_dis)
# uncamps_dis$id <- 1:nrow(uncamps_dis)
# 
# #st_write(uncamps_dis,'output/polys/un_camps.shp')

# # look for overlap of displacement site gps & uncamp polygons
# pts <- st_as_sf(dslocs, coords=c('longitude','latitude'),crs='+proj=longlat +datum=WGS84 +no_defs')
# test <- st_nearest_feature(pts,uncamps_dis)
# disttest <- st_distance(pts,uncamps_dis)
# mindist <- sapply(1:nrow(pts), function(x){
#   min(disttest[x,])
# })
# length(mindist[mindist==0]) # not many overlap  
# cbind(dslocs[,c(2,5,7,8)],mindist) 

# buffer around points to subset settled cells, then convert subset of cells into points
pts <- st_as_sf(dslocs, coords=c('longitude','latitude'),crs='+proj=longlat +datum=WGS84 +no_defs')
pts_t <- st_transform(pts, crs='+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs')
buffpts_t <- st_buffer(pts_t, dist=18000)
buffpts <- st_transform(buffpts_t,crs='+proj=longlat +datum=WGS84 +no_defs')
buffptsr <- fasterize(buffpts, settclumps)
settclumps_sub <- mask(settclumps, buffptsr)
cellIDs <- which(!is.na(settclumps_sub[]))
cellxy <- xyFromCell(settclumps_sub, cell=cellIDs)
cellpts <- st_as_sf(as.data.frame(cellxy), coords=c('x','y'),crs='+proj=longlat +datum=WGS84 +no_defs')
rm(pts_t,buffpts_t,buffpts,buffptsr,settclumps_sub,cellIDs,cellxy)

# identify nearest cell point to each of the GPS locs for displacement sites
dists <- st_distance(cellpts, pts)
nearestcell <- ds_not_found <- NULL
for(i in 1:nrow(pts)){
  distssub <- dists[,i]
  if(as.numeric(min(distssub)) <= 18000){
    nearestcell <- c(nearestcell, which(distssub==min(distssub)))
  }else{
    ds_not_found <- c(ds_not_found, as.character(dslocs$village_idp_settlement_name[i]) )
  }
}
ds_not_found # null if all gps locs have a settled call within the buffer distance

# use nearest cell as the new coord for each displacement site
dscellcoords <- st_coordinates(cellpts[nearestcell,])

# check if same nearest cell is selected for more than one site
repcellcoords <- dscellcoords[duplicated(dscellcoords)]
listid <- which(dscellcoords[,1]==repcellcoords[1] & dscellcoords[,2]==repcellcoords[2])
dslocs[listid,] 
# change one row to represent both 
two_row_sum <- colSums(dslocs[listid,c("Estimated...of.IDP.households...total.","Estimated...of.IDP.individuals...total.","Estimated...of..IDP.households.not.previously.abroad","Estimated...of.IDP.individuals.not.previously.abroad")])
dslocs[listid[1],c("Estimated...of.IDP.households...total.","Estimated...of.IDP.individuals...total.","Estimated...of..IDP.households.not.previously.abroad","Estimated...of.IDP.individuals.not.previously.abroad")] <- two_row_sum
dslocs <- dslocs[-(listid[2]),]
dslocs$village_idp_settlement_name[listid[1]] <- 'Panyiduay Hospital and School'
dscellcoords <- dscellcoords[!duplicated(dscellcoords),]

# nearest cells sf pts 
dscellpts <- st_as_sf(as.data.frame(dscellcoords), coords=c('X','Y'),crs='+proj=longlat +datum=WGS84 +no_defs')

# add ids to ds table
rowids <- 1:nrow(dslocs)
dslocs <- cbind(dslocs,rowids)

# for dslocs do large camps separately - assume high bld density and restrict to contiguous cells
cut_off_pop <- 7500
dslocs_big <- dslocs[dslocs$Estimated...of.IDP.individuals...total.>cut_off_pop,]
buff_dist <- dslocs_big$Estimated...of.IDP.individuals...total./25
max_buff_dist <- 1500 # based on knowledge of biggest camp
buff_dist[buff_dist > max_buff_dist] <- max_buff_dist
pts_t <- st_transform(dscellpts[dslocs_big$rowids,], crs='+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs')
buffpts_t <- st_buffer(pts_t, dist=buff_dist)
buffpts <- st_transform(buffpts_t,crs='+proj=longlat +datum=WGS84 +no_defs')
buffptsr <- fasterize(buffpts, settclumps)
gc()
settclumps_sub <- mask(settclumps, buffptsr)   # the 'source' in settclumps_sub needs to be 'memory' for the next line to work (if source is a temp folder the stars fn doesn't work)
settclumps_sub_polys <- st_as_stars(settclumps_sub) %>% st_as_sf(merge = TRUE)
# an alternative:
# settclumps_sub_polys <- rasterToPolygons(settclumps_sub, dissolve=TRUE)
# settclumps_sub_polys <- st_as_sf(settclumps_sub_polys)
# st_write(settclumps_sub_polys,'input/bf_SSD_region/settlement_clumps.shp')

clustids <- st_intersects(dscellpts[dslocs_big$rowids,], settclumps_sub_polys)
ds_polys <- settclumps_sub_polys[as.numeric(clustids),]
ds_polys <- cbind(ds_polys,dslocs[dslocs_big$rowids,c("state_pcode","state_name",
                                                      "location_ssid","county_pcode",
                                                      "county_name","payam_pcode","payam_name",
                                                      "village_idp_settlement_name","location_type",                                       
                                                      "latitude","longitude",                               
                                                      "Estimated...of.IDP.households...total.",              
                                                      "Estimated...of.IDP.individuals...total.",
                                                      "Source_Round","rowids")])
ds_polys$polyarea <- as.numeric(st_area(ds_polys))
ds_polys <- ds_polys[,2:ncol(ds_polys)]
rm(buff_dist,max_buff_dist,pts_t,buffpts_t,buffpts,buffptsr,settclumps_sub,settclumps_sub_polys,clustids)

# approx IDPs per cell
ds_polys$Estimated...of.IDP.individuals...total./(ds_polys$polyarea/10000) # max 500 per cell seems reasonable

# now the rest of the disp sites - cells don't need to be contig, iterate through increasing buffer size to get suitable number of cells per ds
dslocs_small <- dslocs[dslocs$Estimated...of.IDP.individuals...total.<=cut_off_pop,]
rowids_track <- dslocs_small$rowids
buffer_dists <- seq(5,3505,50)

for(buff_dist in buffer_dists){
  print(buff_dist)
  pts_t <- st_transform(dscellpts[rowids_track,], crs='+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs')
  buffpts_t <- st_buffer(pts_t, dist=buff_dist)
  buffpts <- st_transform(buffpts_t,crs='+proj=longlat +datum=WGS84 +no_defs')
  buffptsr <- fasterize(buffpts, settclumps)
  gc()
  settclumps_sub <- mask(settclumps, buffptsr)
  settclumps_sub_polys <- st_as_stars(settclumps_sub) %>% 
    st_as_sf(merge = TRUE)
  clustids <- st_intersects(buffpts, settclumps_sub_polys)
  ds_polys_small <- NULL
  for(clust in 1:nrow(clustids)){
    poly_temp <- settclumps_sub_polys[unlist(clustids[clust]),]
    rowid_dstab <- rep(rowids_track[clust],nrow(poly_temp))
    ds_polys_small <- rbind(ds_polys_small, cbind(poly_temp, rowid_dstab))
    rm(rowid_dstab)
  }
  ds_polys_small <- ds_polys_small[,c('rowid_dstab', 'geometry')]
  ds_polys_small2 <- aggregate(ds_polys_small,by=list(ds_polys_small$rowid_dstab),FUN=mean)
  ds_polys_small2 <- ds_polys_small2[,2:3] # get rid of superfluous col aggregate puts in
  ds_polys_small2$polyarea <- as.numeric(st_area(ds_polys_small2))
  tot_area_clust <- as.data.table(ds_polys_small2)
  tot_area_clust <- tot_area_clust[,c('polyarea','rowid_dstab')]
  dslocs_small_dt <- as.data.table(dslocs_small[,c("Estimated...of.IDP.individuals...total.","rowids")])
  ppc_dt <- merge(tot_area_clust,dslocs_small_dt,by.x='rowid_dstab',by.y='rowids')
  ppc_dt$ppc <- ppc_dt$Estimated...of.IDP.individuals...total./(ppc_dt$polyarea/10000)
  rm(rowids_track)
  if(buff_dist == buffer_dists[length(buffer_dists)]){
    accept_rowids <- ppc_dt$rowid_dstab
  }else{
    rowids_track <- ppc_dt$rowid_dstab[ppc_dt$ppc > 500]
    accept_rowids <- ppc_dt$rowid_dstab[ppc_dt$ppc <= 500]
  }
  if(length(accept_rowids) > 0){
    dslocs_small_dt2 <- dslocs_small[dslocs_small$rowids %in% accept_rowids,c("state_pcode","state_name",
                                                          "location_ssid","county_pcode",
                                                          "county_name","payam_pcode","payam_name",
                                                          "village_idp_settlement_name","location_type",                                       
                                                          "latitude","longitude",                               
                                                          "Estimated...of.IDP.households...total.",              
                                                          "Estimated...of.IDP.individuals...total.",
                                                          "Source_Round","rowids")]
    ds_polys_x <- merge(ds_polys_small2,dslocs_small_dt2, by.x='rowid_dstab' , by.y='rowids')
    names(ds_polys_x)[names(ds_polys_x)=="rowid_dstab"] <- "rowids"
    ds_polys <- rbind(ds_polys, ds_polys_x)
    rm(ds_polys_x,dslocs_small_dt2)
  }
  rm(pts_t,buffpts_t,buffpts,buffptsr,settclumps_sub,settclumps_sub_polys,clustids,ds_polys_small,ds_polys_small2,dslocs_small_dt,tot_area_clust)
  if(length(rowids_track) == 0) break
}
gc()

# then look at any overlaps 
table(lengths(st_intersects(ds_polys,sparse=T)) > 1)

# save displacement sites
names(ds_polys)[which(names(ds_polys)=="Estimated...of.IDP.households...total.")] <- 'est_idp_hh'
names(ds_polys)[which(names(ds_polys)=="Estimated...of.IDP.individuals...total.")] <- 'est_idp_ind'

#st_write(ds_polys,'output/polys/displacement_sites.shp')

# create ds raster

ds_polys <- st_read('output/polys/displacement_sites.shp')
ds_polys$id <- 1:nrow(ds_polys)
test_polys <- NULL
for(i in 1:nrow(ds_polys)){test <- st_cast(ds_polys[i,],'POLYGON'); test_polys <- rbind(test_polys,test)}
# ignore warning about repeating attributes
ds_cells <- fasterize(test_polys,bc,field='id')
bcdt <- data.table(id=ds_cells[])
bcdt$cellid <- 1:nrow(bcdt)
sumcells <- bcdt[,.N,id]
sumcells <- na.omit(sumcells)
sumcells <- merge(sumcells,test_polys[,c('est_dp_n','id')],'id')[,1:3]
sumcells <- unique(sumcells)
sumcells$ppc <- sumcells$est_dp_n/sumcells$N
bcdt <- merge(bcdt,sumcells[,c('id','ppc')],'id',all=TRUE)
dsr <- ds_cells
dsr[] <- NA
dsr[bcdt$cellid] <- bcdt$ppc

#check
colSums(data.table(dsr[]),na.rm=T)

#writeRaster(dsr, 'output/rasters/ds_master_idp_count.tif')


## host sites 

ds_polys <- st_read('output/polys/displacement_sites.shp')
ds_cells <- fasterize(ds_polys,settclumps)
settclumps <- mask(settclumps, ds_cells,inverse=TRUE)
rm(ds_polys, ds_cells)

# subset displacement sites
hclocs <- dtmlocs[dtmlocs$location_type == 'Host community' & dtmlocs$IDP.Location..Yes.No. == 'Yes',] 
nrow(hclocs)   # 2,097
hclocs$Estimated...of.IDP.households...total. <- as.numeric(as.character(hclocs$Estimated...of.IDP.households...total.))
hclocs$Estimated...of.IDP.individuals...total. <- as.numeric(as.character(hclocs$Estimated...of.IDP.individuals...total.))
hclocs$Estimated...of..IDP.households.not.previously.abroad <- as.numeric(as.character(hclocs$Estimated...of..IDP.households.not.previously.abroad))
hclocs$Estimated...of.IDP.individuals.not.previously.abroad <- as.numeric(as.character(hclocs$Estimated...of.IDP.individuals.not.previously.abroad))

# add ids to ds table
rowids <- 1:nrow(hclocs)
hclocs <- cbind(hclocs,rowids)

buffer_dists <- c(seq(150,5050,100), seq(5550,10050,500), seq(12550,20050,2500), seq(30050,50050,10000))  #seq(12575,100075,2500) #seq(175,10075,100)  
max_buff_dist <- buffer_dists[length(buffer_dists)]

hccellpts <- st_as_sf(hclocs[,c('longitude','latitude')], coords=c('longitude','latitude'),crs='+proj=longlat +datum=WGS84 +no_defs')
rowids_track <- 1:nrow(hclocs)
hclocs_dt <- as.data.table(hclocs[,c("Estimated...of.IDP.individuals...total.","rowids")])

bc_pts <- rasterToPoints(bc)
bcpts <- st_as_sf(as.data.frame(bc_pts), coords=c('x','y'),crs='+proj=longlat +datum=WGS84 +no_defs')

hc_polys_master <- NULL

gc()
for(buff_dist in buffer_dists){
  print(buff_dist)
  pts_t <- st_transform(hccellpts[rowids_track,], crs='+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs')
  buffpts_t <- st_buffer(pts_t, dist=buff_dist)
  buffpts <- st_transform(buffpts_t,crs='+proj=longlat +datum=WGS84 +no_defs')
  buffptsr <- fasterize(buffpts, settclumps)
  rm(pts_t,buffpts_t)
  gc()
  settclumps_sub <- mask(settclumps, buffptsr)
  settclumps_sub_polys <- st_as_stars(settclumps_sub) %>% 
    st_as_sf(merge = TRUE)
  clustids <- st_intersects(buffpts, settclumps_sub_polys)
  if(length(unlist(clustids))==0){
    rm(buffpts,buffptsr,settclumps_sub,settclumps_sub_polys,clustids)
    print('skipped this buffer')
    next
  }
  hc_polys <- NULL
  for(clust in 1:nrow(clustids)){
    if(length(unlist(clustids[clust]))==0) next
    poly_temp <- settclumps_sub_polys[unlist(clustids[clust]),]
    rowid_hctab <- rep(rowids_track[clust],nrow(poly_temp))
    hc_polys <- rbind(hc_polys, cbind(poly_temp, rowid_hctab))
    rm(rowid_hctab,poly_temp)
  }
  hc_polys2 <- hc_polys[,c('rowid_hctab', 'geometry')]
  pip <- st_join(bcpts, hc_polys2, join = st_within)
  pip <- pip[!is.na(pip$rowid_hctab),]
  bcdt <- as.data.table(pip)
  bcdt <- bcdt[,c('SSDplus_buildings_count', 'rowid_hctab')]
  bcsum_dt <- bcdt[,sum(SSDplus_buildings_count),rowid_hctab]
  names(bcsum_dt)[names(bcsum_dt)=='V1'] <- 'bcsum'
  ppc_dt <- merge(bcsum_dt, hclocs_dt, by.x='rowid_hctab', by.y='rowids')
  ppc_dt$ppc <- ppc_dt$Estimated...of.IDP.individuals...total./ppc_dt$bcsum
  if(buff_dist == max_buff_dist){
    accept_rowids <- ppc_dt$rowid_hctab
  }else{
    accept_rowids <- ppc_dt$rowid_hctab[ppc_dt$ppc <= 1]
  }
  rowids_track <- rowids_track[!rowids_track%in%accept_rowids]
  rm(buffpts,buffptsr,settclumps_sub,settclumps_sub_polys,clustids,hc_polys,pip,bcdt,bcsum_dt)
  if(length(accept_rowids) > 0){
    hc_polys2 <- hc_polys2[hc_polys2$rowid_hctab %in% accept_rowids,]
    hc_polys2 <- merge(hc_polys2, ppc_dt[,c('rowid_hctab', 'bcsum','ppc')],by='rowid_hctab')
    hclocs_dt2 <- hclocs[hclocs$rowids %in% accept_rowids,c("state_pcode","state_name",
                                                            "location_ssid","county_pcode",
                                                            "county_name","payam_pcode","payam_name",
                                                            "village_idp_settlement_name","location_type",                                       
                                                            "latitude","longitude",                               
                                                            "Estimated...of.IDP.households...total.",              
                                                            "Estimated...of.IDP.individuals...total.",
                                                            "Source_Round","rowids")]
    hc_polys_x <- merge(hc_polys2,hclocs_dt2, by.x='rowid_hctab' , by.y='rowids')
    names(hc_polys_x)[names(hc_polys_x)=="rowid_hctab"] <- "rowids"  # or do merge other way round
    hc_polys_master <- rbind(hc_polys_master, hc_polys_x)
    #hc_polys_x_r <- fasterize(hc_polys_x,settclumps)
    #settclumps <- mask(settclumps, hc_polys_x_r,inverse=TRUE)
    rm(hc_polys_x,hclocs_dt2) #,hc_polys_x_r
  }
  rm(accept_rowids,ppc_dt,hc_polys2)
  print(length(rowids_track))
  if(length(rowids_track) == 0) break
}
gc()

names(hc_polys_master)[15:16] <- c('est_idp_hh','est_idp_ind')

#st_write(hc_polys_master,'output/polys/hc_polys_master_overlap.shp')

# save a version with a mix of polys and multipolys
hc_polys_master_mix <- aggregate(hc_polys_master,by=list(hc_polys_master$rowids),FUN=mean) 
hc_polys_master_mix <- hc_polys_master_mix[,c('rowids','bcsum','ppc','latitude', 'longitude',
                                              'est_idp_hh', 'est_idp_ind', 'geometry')]

#st_write(hc_polys_master_mix,'output/polys/hc_polys_master_overlap_multipol.shp')

hc_raster_master <- fasterize(hc_polys_master[,'ppc'],settclumps,field='ppc',fun='sum')
hc_raster_master <- hc_raster_master * bc

#writeRaster(hc_raster_master,'output/rasters/hc_master_idp_count.tif')


# raster of all IDPs

dsr <- raster('output/rasters/ds_master_idp_count.tif')
hc_raster_master <- raster('output/rasters/hc_master_idp_count.tif')
idp_locs <- merge(hc_raster_master,dsr)

#check
idp_locs_dt <- data.table(idp_locs[]); idp_locs_dt <- na.omit(idp_locs_dt); sum(idp_locs_dt[,1])
sum(dtmlocs$Estimated...of.IDP.individuals...total.)

#writeRaster(idp_locs, 'output/rasters/master_idp_count.tif')

par(mfrow=c(1,2))
hist(dsr[],main='',xlab='IDPs per cell for displacement sites')
hist(hc_raster_master[],breaks=100,main='',xlab='IDPs per cell for host communities')

# poly of all idps

hc_clumps <- clump(hc_raster_master, direction=8)
hc_merged <- rasterToPolygons(hc_clumps, dissolve=TRUE)
hc_merged_sf <- st_as_sf(hc_merged)

hcdt <- data.table(clumpid = hc_clumps[], idp_count = hc_raster_master[])
hcdt <- na.omit(hcdt)
hc_sums <- hcdt[,sum(idp_count),clumpid]

# merge pop counts per clump
hc_merged_sf <- merge(hc_merged_sf,hc_sums,by.x='clumps',by.y='clumpid')
names(hc_merged_sf)[2] <- 'IDP_count' 
hc_merged_sf$type <- 'Host community'

# rbind ds and hc polys
ds_polys <- st_read('output/polys/displacement_sites.shp')
ds_polys2 <- ds_polys[,c('lctn_ty','est_dp_n')]
names(ds_polys2)[1:2] <- c('type','IDP_count')
master_idp_count_poly <- rbind(ds_polys2,hc_merged_sf[,c('type','IDP_count')])

#st_write(master_idp_count_poly, 'output/polys/master_idp_count_poly.shp')




### origin mapping 


# county level origin based on displaced abroad

table(dtm$p..displaced.outside.ssd.Yes.No) # number of payams with displaced persons abroad
dtmabroad <- dtm[dtm$p..displaced.outside.ssd.Yes.No == 'Yes',]
length(unique(dtmabroad$X0.county.name)) # 72 counties report displaced persons outside SSD
nyr_county <- dtmabroad[,sum(n..displaced.and.not.returned.to.payam.individuals),X0.county.name]
county_state <- unique(dtm[,c('X0.county.name','X0.state.name')])
nyr_county <- merge(nyr_county,county_state,by='X0.county.name') 
names(nyr_county)[2] <- 'nyr'
sum(nyr_county$nyr) # 630456 
sum(dtm$n..displaced.and.not.returned.to.payam.individuals) # 680713 
#in DTM summary tables this is 100,000 more than the 580713 figure
#this table with ind locations data sums nyr to Morobo to 187283
#whereas the summary table has 87283 for Morobo 
#I imagine this was a manual edit by IOM because 187283 is higher than the pop proj of 164502
#here we follow IOM's lead and assume that the ind loc reported nyr is incorrect
nyr_county$nyr[nyr_county$X0.county.name == 'Morobo'] <- 87283

#write.csv(nyr_county,'output/tables/counties_reported_refugees_nyrcounts.csv',row.names = F)


# county level origin based on data for destination payams

dtmsub <- dtm[,grep('^[X0.a..b..]',names(dtm))]
dtmsub <- dtmsub[,-grep('household',names(dtmsub))]
table(dtmsub$a..IDPs.present..Yes.No.) # 453 payams contain IDPs, 56 don't
dtmsub <- dtmsub[dtmsub$a..IDPs.present..Yes.No. == 'Yes',]
unknowns <- sum(dtmsub$b..IDP.arrival.unknown.period.individuals)
dtmsub <- dtmsub[,-grep('unknown',names(dtmsub))]
first_col <- min(grep('arrival',names(dtmsub)))
names(dtmsub)[first_col:ncol(dtmsub)] <- rep(c("count","ori.state","ori.cnty"), 6)
origin.county.data <- NULL
for(i in seq(first_col,ncol(dtmsub),3)){
  origin.county.data <- rbind(origin.county.data, dtmsub[,c(1:(first_col-1),i:(i+2))])
}
origin.county.data <- as.data.table(origin.county.data)
origin.county.data <- origin.county.data[origin.county.data$count > 0,]
origin.county.data[is.na(origin.county.data$ori.cnty),] # in addition to the IDPs taht were displaced in an unknown period, there are others with known period but unknown origin
unknowns <- unknowns + sum(origin.county.data$count[is.na(origin.county.data$ori.cnty)])  # add these to the unknown count
origin.county.data <- origin.county.data[!is.na(origin.county.data$ori.cnty),] # then exclude these unknowns from main table
# spread unknowns across origin-destination combos according to proportion of total counts
origin.county.data$count <- origin.county.data$count + (unknowns * (origin.county.data$count/sum(origin.county.data$count)))
sum(origin.county.data$count)
sum(round(origin.county.data$count)) # checks out (error of 5 people in 1.6 mil)

origin.county.data <- origin.county.data[,-paste('a..IDP.individuals')]
#write.csv(origin.county.data,'output/tables/within_and_inter_county_displacement.csv',row.names = F)

# exclude counts of IDPS that displace within the same county

origin.diff.county.data <- origin.county.data[!origin.county.data$X0.county.name==origin.county.data$ori.cnty,]
#write.csv(origin.diff.county.data,'output/tables/inter_county_displacement.csv',row.names = F)


# look at inter-county displaced results:

# counts for county of origin
origin.diff.county.counts <- origin.diff.county.data[,.(count.sum = sum(count)),ori.cnty]
#origin.diff.county.counts$ori.cnty[origin.diff.county.counts$ori.cnty == "Abyei Administrative Area"] <- "Abyei"
nrow(origin.diff.county.counts) # 70 counties of origin (so 9 counties have no pops displced from them)
# which counties have no pop displaced from them:
unique(dtmsub$X0.county.name)[!unique(dtmsub$X0.county.name)%in%origin.diff.county.counts$ori.cnty]
# number of IDPs displaced to a different county
sum(origin.diff.county.counts[,2]) # 554080.9
# and number of IDPs displaced within counties
sum(origin.county.data$count) - sum(origin.diff.county.counts[,2]) # 1,061,684

# counts for county of destination
origin.diff.county.idps <- origin.diff.county.data[,.(idps.sum = sum(count)),X0.county.name]
nrow(origin.diff.county.idps) # 58 counties have IDPs from a different county

# total displaced from numbers for each county (including displacement within county)
origin.county.counts <- origin.county.data[,.(count.sum = sum(count)),ori.cnty]
#write.csv(origin.county.counts,'output/tables/total_idps_displaced_from_county.csv',row.names = F)


# IDPs by driver

sub <- dtm_dt[,c(63:66,68:71,73:76,78:81,83:86,88:91)]
sum(dtm_dt[,11])
sub <- colSums(sub)
sub <- rbind(sub[1:4],sub[5:8],sub[9:12],sub[13:16],sub[17:20],sub[21:24])
sub <- colSums(sub, na.rm = T)
names(sub) <-  c('conflict','communal.clashes','disaster','unknown.reason')









