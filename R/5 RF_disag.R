
rm(list=ls())

library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(randomForest)
library(quantregForest)
library(sf)
library(fasterize)

setwd('')

# NBS county totals & IOM displaced from by county 
origin_county_counts <- as.data.table(read.csv('output/tables/total_idps_displaced_from_county.csv',header=T))

# refugees by county based on pop comparisons assessment 
comp_tab <- as.data.table(read.csv('output/tables/county_level_pop_results.csv',header=T))

# match up county names 
mismatches <- origin_county_counts$ori.cnty[!origin_county_counts$ori.cnty %in% comp_tab$county]
# check against list
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Abyei Administrative Area'] <- 'Abyei'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Canal Pigi'] <- 'Canal'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Lafon'] <- 'Lafon/Lopa'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Luakpiny (Nasir)'] <- 'Luakpiny/Nasir'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Mayiendit'] <- 'Mayendit'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Pariang (Ruweng)'] <- 'Pariang'
origin_county_counts$ori.cnty[origin_county_counts$ori.cnty=='Raja'] <- 'Raga'
rm(mismatches)

# table for response vars (pop and total displaced from)
mdt <- merge(comp_tab[,c('county','ref_dispfrom_est','pop')],origin_county_counts, by.x='county',by.y='ori.cnty')
mdt$tot_disp_from <- mdt$ref_dispfrom_est + mdt$count.sum
mdt <- mdt[,c('county','pop','tot_disp_from')]

# create county raster
ocha2 <- st_read('input/OCHA_boundaries/ssd_admbnda_adm2_imwg_nbs_20180817.shp')
bta <- raster('input/RF_covs/SSDplus_buildings_total_area.tif')
ocha2$id <- 1:nrow(ocha2)
ochar <- fasterize(ocha2, bta, field = 'id')
ochar <- mask(ochar, bta)

# match up county names between OCHA and NBS/IOM harmonised
mismatches <- ocha2$ADM2_EN[!ocha2$ADM2_EN %in% comp_tab$county]
ocha2$ADM2_EN[ocha2$ADM2_EN=='Kajo-keji'] <- 'Kajo-Keji'
ocha2$ADM2_EN[ocha2$ADM2_EN=='Lafon'] <- 'Lafon/Lopa'
ocha2$ADM2_EN[ocha2$ADM2_EN=='Canal/Pigi'] <- 'Canal'
ocha2$ADM2_EN[ocha2$ADM2_EN=='Panyijiar'] <- 'Panyijar'
ocha2$ADM2_EN[ocha2$ADM2_EN=='Raja'] <- 'Raga'
ocha2$ADM2_EN[ocha2$ADM2_EN=='Abyei Region'] <- 'Abyei'
rm(mismatches)

county_order <- data.table(ocha2[,c('ADM2_EN','id')]); county_order <- county_order[,c('ADM2_EN','id')]


# pixel area
px_area <- area(ochar)
px_area <- px_area * 10 * 10

# total area of settled pixels and buildings per county
county_px_area <- data.table(zonal(px_area, ochar, fun="sum", na.rm=T))
names(county_px_area) <- c("id","px_area")
county_px_area <- merge(county_px_area,county_order,by='id')
mdt <- merge(mdt,county_px_area,by.x='county',by.y='ADM2_EN')
county_b_area <- data.table(zonal(bta, ochar, fun="sum", na.rm=T))
names(county_b_area) <- c("id","b_area")
county_b_area <- merge(county_b_area,county_order,by='id')
mdt <- merge(mdt,county_b_area[,c('b_area','ADM2_EN')],by.x='county',by.y='ADM2_EN')

# pop densities
mdt$pph <- mdt$pop/mdt$px_area
mdt$dpfph <- mdt$tot_disp_from/mdt$px_area
mdt$ppba <- mdt$pop/mdt$b_area
mdt$dpfpba <- mdt$tot_disp_from/mdt$b_area

# histograms of county totals
par(mfrow=c(2,2))
hist(mdt$pph,breaks=15)
hist(mdt$dpfph,breaks=15)
hist(mdt$ppba,breaks=15)
hist(mdt$dpfpba,breaks=15)

# log county totals
mdt$log_pph <- log(mdt$pph)
mdt$log_dpfph <- log(mdt$dpfph)
mdt$log_ppba <- log(mdt$ppba)
mdt$log_dpfpba <- log(mdt$dpfpba)

ochadt <- data.table(ochar[])
names(ochadt) <- 'zone'
zonal_covs <- data.table(zone=1:79)
# zonal covariates
lf <- list.files('input/RF_covs/',pattern="tif$",full.names=TRUE) 
for(i in 1:length(lf)){
  rdt <- cbind(ochadt, data.table(raster(lf[i])[]))
  rdt <- na.omit(rdt)
  sumrdt <- rdt[,mean(V1),zone]
  names(sumrdt)[2] <- names(raster(lf[i]))
  zonal_covs <- merge(zonal_covs,sumrdt,'zone')
}

# add covs to master dt
mdt <- merge(mdt,zonal_covs,by.x='id',by.y='zone')

# exclude accessibility and dmsp nightlights because they are old
# exclude dst_protected because it is essentially distance to the SW 
mdt <- mdt[,!c('dmsp_nl','dst_protected','dst_urban','SSDplus_buildings_urban','dst_bsgm')]

names(mdt)

#write.csv(mdt,'output/tables/master_RF_county.csv',row.names = F)

# map pop densities
cbind(mdt$county,ocha2$ADM2_EN) #same order
ocha2 <- cbind(ocha2,mdt$pph,mdt$dpfph,mdt$log_pph,mdt$log_dpfph,mdt$pop,mdt$tot_disp_from) 
names(ocha2)
par(mfrow=c(2,2))
plot(ocha2[,20],main='unadjusted population counts (census projection)')
plot(ocha2[,21],main='estimated number of people displaced from each county')
plot(ocha2[,18],main='log unadjusted population density (census projection)')
plot(ocha2[,19],main='log estimated density of people displaced from each county')
#plot(ocha2[,c(20:21,18:19)])

### disagg of unadjusted pop ###

mdf <- as.data.frame(mdt)

# par(mfrow=c(4,4))
# for(i in c(15:37)){
#   hist(mdf[,i],breaks = 15,main=names(mdf)[i])
# }

# subset model data
x.data <- mdf[,c(15,44:78)]

y1 <- mdf$log_pph

## RF model
set.seed(123)
init_popfit = tuneRF(x=x.data, 
                     y=y1, 
                     plot=TRUE, mtryStart=length(x.data), 
                     ntreeTry=length(y1)/20, 
                     improve=0.0001, stepFactor=1.2, 
                     trace=TRUE, doBest=TRUE,
                     na.action=na.omit,importance=TRUE, proximity=TRUE,
                     sampsize=length(y1),
                     replace=TRUE) 

# check importance scores
importance_scores <- importance(init_popfit)[order(importance(init_popfit)[,1], decreasing=TRUE),]
pos_importance <- rownames(importance_scores)[importance_scores[,1] > 2]
print(init_popfit)

if (length(pos_importance) == length(importance_scores[,1])) {
  x.data <- x.data[pos_importance]
  popfit = tuneRF(x=x.data, 
                  y=y1, 
                  plot=TRUE, mtryStart=length(x.data), 
                  ntreeTry=length(y1)/20, 
                  improve=0.0001,stepFactor=1.2, 
                  trace=TRUE,doBest=TRUE, 
                  nodesize=length(y1)/1000, 
                  na.action=na.omit, importance=TRUE,proximity=TRUE, 
                  sampsize=length(y1), 
                  replace=TRUE) 
} else{
  while (length(pos_importance) < length(importance_scores[,1])) {
    ##	Subset our x_data to just those columns having positive scores:
    x.data <- x.data[pos_importance]
    
    popfit = tuneRF(x=x.data, 
                    y=y1, 
                    plot=TRUE, mtryStart=length(x.data), 
                    ntreeTry=length(y1)/20, 
                    improve=0.0001, stepFactor=1.2, 
                    trace=TRUE, doBest=TRUE, 
                    nodesize=length(y1)/1000, 
                    na.action=na.omit,importance=TRUE, proximity=TRUE, 
                    sampsize=length(y1), 
                    replace=TRUE) 
    ##	Re-check importance scores:
    importance_scores <- importance(popfit)[order(importance(popfit)[,1], decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 2]
    print(popfit)
  } 
}


varImpPlot(popfit)

set.seed(1212) #
popfit_final <- randomForest(x=x.data, 
                             y=y1, 
                             mtry=popfit$mtry, ntree=popfit$ntree, 
                             nodesize=length(y1)/1000, 
                             importance=TRUE, proximity=TRUE,do.trace=F)
popfit_final
varImpPlot(popfit_final)

par(mfrow=c(1,1),mar=c(4,4,4,4))
plot(x=y1, y=predict(popfit_final), ylim=c(min(y1),max(y1)), xlim=c(min(y1),max(y1)))
abline(a=0, b=1, lty=2)
plot(x=y1, y=(y1 - predict(popfit_final)), xlim=c(min(y1),max(y1)))
abline(a=0, b=0, lty=2)

rm(popfit, init_popfit)
popfit_final$proximity <- NULL

## predictions

# subset covar layers
lf <- list.files('input/RF_covs/',pattern="tif$",full.names=TRUE) 
fin_lyrs <- stack(lf)
fin_lyrs <- fin_lyrs[[names(popfit_final$forest$xlevels)]]

pred_dat <- NULL
for(i in 1:length(names(fin_lyrs))){
  pred_temp <- data.table(fin_lyrs[[i]][])
  if(i == 1){
    cellIDs <- row.names(pred_temp)[!is.na(pred_temp$V1)]
  }
  pred_temp <- na.omit(pred_temp)
  print(nrow(pred_temp))
  pred_dat <- cbind(pred_dat, pred_temp)
  names(pred_dat)[ncol(pred_dat)] <- names(fin_lyrs)[i]
  rm(pred_temp)
}

cellIDs <- as.numeric(cellIDs)

pred_set <- predict(popfit_final, 
                    newdata=pred_dat,
                    predict.all=T,
                    type="response")

# back-transform
pred_mean_logpop <- apply(pred_set$individual, 1, mean)
pred_mean_pop <- exp(pred_mean_logpop)
pred_sd_logpop <- apply(pred_set$individual, 1, sd)
rm(pred_set)
pred_mean_sd <- data.table(pred_mean_logpop,pred_sd_logpop,pred_mean_pop)
rm(pred_mean_logpop,pred_sd_logpop)

#saveRDS(pred_mean_sd,'output/tables/pred_unadj_pop_per_m2.rds')

# predictions raster
pred_rast <- ochar
values(pred_rast) <- NA
pred_rast[cellIDs] <- pred_mean_pop
pred_rast <- pred_rast * px_area
  
#writeRaster(pred_rast, "output/rasters/pred_unadj_pop_per_cell.tif")


# dasymetric redistribution

# create raster from predicted density
z_pred <- zonal(pred_rast, ochar, fun="sum", na.rm=T)
z_pred_r <- reclassify(ochar, z_pred)

# create raster with county pop total in each county settled cell 
county_popr <- fasterize(ocha2, ochar, field = 'mdt.pop')

# calculate pop
pop_calc <- (pred_rast * county_popr) / z_pred_r

# check
#merge(zonal(pop_calc,ochar,fun='sum'),mdt[,c('pop','id')],by.x='zone',by.y='id')

#writeRaster(pop_calc, "output/rasters/unadj_pop_per_cell.tif")

rm(pred_dat,pred_rast,z_pred_r,z_pred,pred_mean_pop)


### disagg of displaced from est ###

# subset model data
mdf <- as.data.frame(mdt)
x.data <- mdf[,c(15:78)]

y2 <- mdf$log_dpfph

## RF model
set.seed(1234)
init_popfit = tuneRF(x=x.data, 
                     y=y2, 
                     plot=TRUE, mtryStart=length(x.data), 
                     ntreeTry=length(y2)/20, 
                     improve=0.0001, stepFactor=1.2, 
                     trace=TRUE, doBest=TRUE,
                     na.action=na.omit,importance=TRUE, proximity=TRUE,
                     sampsize=length(y2),
                     replace=TRUE) 

# check importance scores
importance_scores <- importance(init_popfit)[order(importance(init_popfit)[,1], decreasing=TRUE),]
pos_importance <- rownames(importance_scores)[importance_scores[,1] > 2]
print(init_popfit)

if (length(pos_importance) == length(importance_scores[,1])) {
  x.data <- x.data[pos_importance]
  popfit = tuneRF(x=x.data, 
                  y=y2, 
                  plot=TRUE, mtryStart=length(x.data), 
                  ntreeTry=length(y2)/20, 
                  improve=0.0001,stepFactor=1.2, 
                  trace=TRUE,doBest=TRUE, 
                  nodesize=length(y2)/1000, 
                  na.action=na.omit, importance=TRUE,proximity=TRUE, 
                  sampsize=min(c(length(y2), 1000)), 
                  replace=TRUE) 
} else{
  while (length(pos_importance) < length(importance_scores[,1])) {
    ##	Subset our x_data to just those columns having positive scores:
    x.data <- x.data[pos_importance]
    
    popfit = tuneRF(x=x.data, 
                    y=y2, 
                    plot=TRUE, mtryStart=length(x.data), 
                    ntreeTry=length(y2)/20, 
                    improve=0.0001, stepFactor=1.2, 
                    trace=TRUE, doBest=TRUE, 
                    nodesize=length(y2)/1000, 
                    na.action=na.omit,importance=TRUE, proximity=TRUE, 
                    sampsize=min(c(length(y2), 1000)), 
                    replace=TRUE) 
    ##	Re-check importance scores:
    importance_scores <- importance(popfit)[order(importance(popfit)[,1], decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 2]
    print(popfit)
  } 
}


plot(x=y2, y=predict(popfit), ylim=c(min(y2),max(y2)), xlim=c(min(y2),max(y2)))
abline(a=0, b=1, lty=2)

plot(x=y2, y=(y2 - predict(popfit)), xlim=c(min(y2),max(y2)))
abline(a=0, b=0, lty=2)

varImpPlot(popfit)

set.seed(121212)
popfit_final <- randomForest(x=x.data, 
                             y=y2, 
                             mtry=popfit$mtry, ntree=popfit$ntree, 
                             nodesize=length(y2)/1000, 
                             importance=TRUE, proximity=TRUE,do.trace=F)
popfit_final
varImpPlot(popfit_final)

plot(x=y2, y=predict(popfit), ylim=c(min(y2),max(y2)), xlim=c(min(y2),max(y2)))
abline(a=0, b=1, lty=2)

plot(x=y2, y=(y2 - predict(popfit)), xlim=c(min(y2),max(y2)))
abline(a=0, b=0, lty=2)

rm(popfit, init_popfit)
popfit_final$proximity <- NULL

## predictions

# subset covar layers
lf <- list.files('input/RF_covs/',pattern="tif$",full.names=TRUE) 
fin_lyrs <- stack(lf)
fin_lyrs <- fin_lyrs[[names(popfit_final$forest$xlevels)]]

pred_dat <- NULL
for(i in 1:length(names(fin_lyrs))){
  pred_temp <- data.table(fin_lyrs[[i]][])
  if(i == 1){
    cellIDs <- row.names(pred_temp)[!is.na(pred_temp$V1)]
  }
  pred_temp <- na.omit(pred_temp)
  print(nrow(pred_temp))
  pred_dat <- cbind(pred_dat, pred_temp)
  names(pred_dat)[ncol(pred_dat)] <- names(fin_lyrs)[i]
  rm(pred_temp)
}

cellIDs <- as.numeric(cellIDs)

pred_set <- predict(popfit_final, 
                    newdata=pred_dat,
                    predict.all=T,
                    type="response")

# back-transform
pred_mean_logpop <- apply(pred_set$individual, 1, mean)
pred_mean_pop <- exp(pred_mean_logpop)
pred_sd_logpop <- apply(pred_set$individual, 1, sd)
rm(pred_set)
pred_mean_sd <- data.table(pred_mean_logpop,pred_sd_logpop,pred_mean_pop)
rm(pred_mean_logpop,pred_sd_logpop)

saveRDS(pred_mean_sd,'output/tables/pred_displaced_from_per_m2.rds')

# predictions raster
pred_rast <- ochar
values(pred_rast) <- NA
pred_rast[cellIDs] <- pred_mean_pop
pred_rast <- pred_rast * px_area

#writeRaster(pred_rast, "output/rasters/pred_displaced_from_per_cell.tif")

# dasymetric redistribution

# create raster from predicted density
z_pred <- zonal(pred_rast, ochar, fun="sum", na.rm=T)
z_pred_r <- reclassify(ochar, z_pred)

# create raster with county pop total in each county settled cell 
county_popr <- fasterize(ocha2, ochar, field = 'mdt.tot_disp_from')

# calculate pop
pop_calc <- (pred_rast * county_popr) / z_pred_r

# check
merge(zonal(pop_calc,ochar,fun='sum'),mdt[,c('tot_disp_from','id')],by.x='zone',by.y='id')

#writeRaster(pop_calc, "output/rasters/displaced_from_per_cell.tif")

rm(pred_dat,pred_rast,z_pred_r,z_pred,pred_mean_pop)


