

rm(list=ls())

library(data.table)
library(raster)

setwd('')

# read in  three layers needed to generate final layer
idp <- raster('output/rasters/master_idp_count.tif')       # idp count destination layer
df <- raster('output/rasters/displaced_from_per_cell.tif') # idp count origin layer
unadj <- raster('output/rasters/unadj_pop_per_cell.tif')   # census projections layer

# add 0s to settled cells that don't include a count 
dt <- data.table(idp=idp[],df=df[],unadj=unadj[])
dt[is.na(idp)&!is.na(df),1] <- 0
dt[is.na(idp)&!is.na(unadj),1] <- 0
dt[is.na(df)&!is.na(idp),2] <- 0
dt[is.na(df)&!is.na(unadj),2] <- 0
dt[is.na(unadj)&!is.na(idp),3] <- 0
dt[is.na(unadj)&!is.na(df),3] <- 0
dt_sub <- na.omit(dt)
nrow(dt_sub)
colSums(dt_sub)

# check any displacement that exceeds baseline census projection pop
dt$pop <- dt$unadj - dt$df 
dt_sub <- na.omit(dt)
nrow(dt_sub)
length(dt_sub$pop[dt_sub$pop<0]) # 12,638 grid cells 
length(dt_sub$pop[dt_sub$pop<(-10)])
sum(dt_sub$pop[dt_sub$pop<0]) # 36,692 people 
hist(dt_sub$pop[dt_sub$pop<0],breaks=100)

# it is expected that there will be a small proportion of grid cells with higher predicted displaced from than predicted base pop
# edit displaced from layer so that final pop is not < 0 
dt$df[dt$pop<0 & !is.na(dt$pop)] <- dt$df[dt$pop<0 & !is.na(dt$pop)] + dt$pop[dt$pop<0 & !is.na(dt$pop)]
df[] <- dt$df 
#writeRaster(df,'output/rasters/displaced_from_per_cell_edited.tif')

# re-do pop based on edited df layer
dt$pop <- dt$unadj - dt$df + dt$idp

sum(na.omit(dt$pop)) #11144563 = national total pop count

pop <- idp
pop[] <- NA
pop[] <- dt$pop
#writeRaster(pop,'output/rasters/final_pop_count.tif')


# final state totals
ocha <- st_read('input/OCHA_boundaries/ssd_admbnda_adm2_imwg_nbs_20180817.shp')
ocha$id <- NA
state_codes <- sort(unique(ocha$ADM1_PCODE))
for(i in 1:10){ ocha$id[ocha$ADM1_PCODE == state_codes[i]] <- i}
ochar <- fasterize(ocha, idp, field = 'id')
dt <- cbind( dt, ochar[])
names(dt)[names(dt) == 'V2'] <- 'state'
dt_sub <- na.omit(dt)
dt_sums <- dt_sub[,sum(pop),state]
state_names <- ocha[,c('ADM1_EN','ADM1_PCODE','id')]
state_names <- unique(as.data.table(state_names)[,1:3])
dt_sums <- merge(dt_sums,state_names,by.x='state',by.y='id')
names(dt_sums)[names(dt_sums)=='V1'] <- 'pop'
  
#write.csv(dt_sums,'output/tables/final_state_totals.csv',row.names=F)




