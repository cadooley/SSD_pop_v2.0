# R code for South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0

**Dataset downloadable from:**
https://wopr.worldpop.org/?SSD/Population

**Dataset citation:**
Dooley CA, Jochem WC, Leasure, DR, Sorichetta A, Lazar AN and Tatem AJ. 2021. South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0. WorldPop, University of Southampton. doi: 10.5258/SOTON/WP00709 

**Method report citation:**
Dooley CA, Jochem WC, Sorichetta A, Lazar AN and Tatem AJ. 2021. Description of methods for South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0. WorldPop, University of Southampton. doi: 10.5258/SOTON/WP00710

**Please see SSD_population_v2_0_README.pdf included in the data release and the methods report for details about the approach, while using these R scripts.**

**Please see the methods report for details about input data. This study uses all publicly available data, excpet the building footprint metric layers that were generated from the raw building footprints data for the spatial extent used in this work. Please contact authors if you would like access to these metric layers.**

This repository includes the following R scripts:
1. iom_data_prep.R
2. cov_prep_unadj_pop.R
3. cov_conflict.R
4. RF_mask_cov_ds.R
5. RF_disag.R
6. final_pop_layer.R
7. saving_final_rasters.R

**1 iom_data_prep.R** - this includes the creation of the initial IDP (internally displaced persons) raster layer, and estimatation of the number of displaced people (IDPs & refugees) by county of origin 

**2 cov_prep_unadj_pop.R** - this code prepares the covariate layers for the machine learning approach used to disaggregate the unadjusted county level census population projections

**3 cov_conflict.R** - this code prepares the conflict covariate layers for the machine learning approach used to disaggregate the number of displaced people (IDPs & refugees) by county of origin

**4 RF_mask_cov_ds.R** - this code masks out 'displacement sites' from all covariate layers ('displacement sites' are considered to be purposefully constructed or converted buildings/structures to accommodate IDPs and, therefore, are occupied by IDPs only)

**5 RF_disag.R** - this includes the code for applying the random forest machine learning approach to disaggregate county level estimates to a high spatial resolution, for a) unadjusted census population projections; and b) number of displaced people (IDPs & refugees) by place of origin

**6 final_pop_layer.R** - this script generates the final grid cell level population estimates that account for displacement

**7 saving_final_rasters.R** - this tidies up the final rasters so that they have zeros in any 'settled' cells not containing counts (e.g. a grid cell with buildings but no IDPs will have a 0 in the SSD_population_v2_0_gridded_idps.tif raster). This scripts saves the rasters with the names found in the data release
