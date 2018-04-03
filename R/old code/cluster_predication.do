* cd "~/Box Sync/Research/Malawi_FewS/"
cd "D:\Malawi_FewS"
set more off

use "clust_2010.dta",clear 


foreach level of varlist ipczone TA clust{
local `level'_weather `level'_L12raincytot  `level'_L12day1rain `level'_L12maxdays `level'_floodmax
local `level'_lagmkt `level'_lag_price `level'_lag_thinn 
local `level'_mkt `level'_price `level'_thinn 
local `level'_asset1 `level'_roof `level'_cells_own
local `level'_land `level'_percent_ag  `level'_elevation  
local `level'_nutri  `level'_nutri_reten_constrained
local `level'_dist `level'_dist_road `level'_dist_admarc 
local `level'_demo `level'_hhsize `level'_hh_age `level'_hh_gender `level'_asset 
}


local weather clust_L12raincytot clust_floodmax clust_L12day1rain clust_L12maxdays 
local lagmkt Llnprice_impu Lmkt_thinn
local mkt mkt_thinness lnprice_impu
local asset1  roof cells_own 
local demo hh_age hh_gender hhsize asset_index2 
local dist dist_road dist_admarc 
local land percent_ag elevation  
local nutri nutri_reten_constrained




use "clust_2010.dta",clear 
tobit RCSI ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist',ll
use "clust_2013.dta",clear 
predict RCSI_predict  
drop if RCSI_predict ==.
keep RCSI RCSI_predict
 export delimited "RCSI_tobit_CLUST.csv",replace



use "clust_2010.dta",clear 
xi: reg clust_RCSI ipc_lag1 ipc_lag12    `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 
predict clust_RCSI_predict  
 keep clust_RCSI clust_RCSI_predict 
 export delimited "RCSI_predict_CLUST.csv",replace
 

use "clust_2010.dta",clear 
xi: reg clust_HDDS ipc_lag1 ipc_lag12    `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 
predict clust_HDDS_predict  
 keep clust_HDDS clust_HDDS_predict 
 export delimited "HDDS_predict_CLUST.csv",replace
 
 
use "clust_2010.dta",clear 
 xi: reg clust_logFCS ipc_lag1 ipc_lag12 `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 
 predict clust_logFCS_predict  
 keep clust_logFCS clust_logFCS_predict 
 export delimited "logFCS_predict_CLUST.csv",replace

*ipc_lag1 ipc_lag12 
*HDDS_monthaverage  HDDS_ipczone_average 

 

* RCSI_monthaverage  RCSI_ipczone_average

* logFCS_monthaverage  logFCS_ipczone_average
use "clust_2010.dta",clear 

xi: reg clust_logFCS ipc_lag1 ipc_lag12 logFCS_monthaverage  logFCS_ipczone_average `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 

predict clust_logFCS_predict  
 keep clust_logFCS clust_logFCS_predict
 
 export delimited "logFCS_predict_CLUST_after.csv",replace
*logFCS_monthaverage  logFCS_ipczone_average

*HDDS_monthaverage  HDDS_ipczone_average





scalar r2_logFCS =  e(r2)

drop if clust_logFCS_predict ==.
keep clust_logFCS clust_logFCS_predict
export delimited "logFCS_predict_CLUST.csv",replace


scalar r2_HDDS =  e(r2)


drop if clust_HDDS_predict ==.
keep clust_HDDS clust_HDDS_predict
export delimited "HDDS_predict_CLUST.csv",replace


use "clust_2010.dta",clear 

reg clust_RCSI ipc_lag1 ipc_lag12 RCSI_monthaverage  RCSI_ipczone_average  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist' 
use "clust_2013.dta",clear 
predict clust_RCSI_predict  
scalar r2_RCSI  =  e(r2)




drop if clust_RCSI_predict ==.
keep clust_RCSI clust_RCSI_predict
export delimited "RCSI_predict_CLUST.csv",replace
