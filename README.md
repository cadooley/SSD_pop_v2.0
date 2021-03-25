# R code for South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0

**Dataset downloadable from:**
https://wopr.worldpop.org/?SSD/Population

**Dataset citation:**
Dooley CA, Jochem WC, Leasure, DR, Sorichetta A, Lazar AN and Tatem AJ. 2021. South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0. WorldPop, University of Southampton. doi: 10.5258/SOTON/WP00709 

**Method report citation:**
Dooley CA, Jochem WC, Sorichetta A, Lazar AN and Tatem AJ. 2021. Description of methods for South Sudan 2020 gridded population estimates from census projections adjusted for displacement, version 2.0. WorldPop, University of Southampton. doi: 10.5258/SOTON/WP00710

**Please see SSD_population_v2_0_README.pdf included in the data release and the methods report for details about the approach, while using these R scripts.**

**Please see the methods report for details about input data and how to access it. This study uses all publicly available data, excpet the building footprint metric layers that were generated from the raw building footprints data for the spatial extent used in this work. Please contact authors if you would like access to these metric layers.**

This repository includes the following R scripts:
1. iom_data_prep.R
2. cov_prep_unadj_pop.R
3. cov_conflict.R
4. RF_mask_cov_ds.R
5. RF_disag.R
6. final_pop_layer.R
7. saving_final_rasters.R
