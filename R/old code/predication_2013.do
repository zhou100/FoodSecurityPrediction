
* cd "~/Box Sync/Research/Malawi_FewS/"
cd "D:\Malawi_FewS"
set more off

*use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 
use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 
* a. ipc-level averages




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



* foreach level of varlist clust {
* egen `level'_logFCS = mean(logFCS), by (`level')
* egen `level'_RCSI = mean(RCSI), by (`level' )
* egen `level'_HDDS = mean(HDDS), by (`level' )
* }

use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 

export delimited "IHS2010.csv",replace

use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 

export delimited "IHS2013.csv",replace


xi: reg logFCS ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 
predict logFCS_predict  
drop if logFCS_predict ==.
keep logFCS logFCS_predict
export delimited "logFCS_predict_table_hh.csv",replace


use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 
xi: reg HDDS ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 
predict HDDS_predict  
drop if HDDS_predict ==.
keep HDDS HDDS_predict
export delimited "HDDS_predict_table.csv",replace


use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 
tobit RCSI ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist',ll
use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 
predict RCSI_predict  
drop if RCSI_predict ==.
keep RCSI RCSI_predict
export delimited "RCSI_predict_table.csv",replace


**************************************************************
* predict 2013 at cluster average 
**************************************************************

use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 
drop if FCS ==0 

drop if HDDS ==0 
replace RCSI =42 if RCSI>42
save "Malawi IHS3 files/IHS2010_PNAS_merged.dta",replace

use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 
drop if FCS ==0 

drop if HDDS ==0 
replace RCSI =42 if RCSI>42
save "Malawi IHS3 files/IHS2013_PNAS_merged.dta",replace

use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 

foreach level of varlist clust {
egen `level'_logFCS = mean(logFCS), by (`level')
egen `level'_RCSI = mean(RCSI), by (`level' )
egen `level'_HDDS = mean(HDDS), by (`level' )
}
  
 levelsof fnid,local(ipczone_levels)
foreach measure of varlist RCSI logFCS HDDS{
 gen `measure'_ipczone_average = . 

 foreach level of local ipczone_levels {
 egen `measure'_ipc_`level' = mean(`measure') if fnid == "`level'"
 tabstat `measure'_ipc_`level',save
 mat total = r(StatTotal)

mat li total

scalar ipc = total[1,1]

replace `measure'_ipczone_average = ipc if fnid == "`level'"


  }  
 }

 
foreach measure of varlist RCSI logFCS HDDS{
 gen `measure'_monthaverage = . 

forvalues i = 1/12 {

egen `measure'_month_`i' = mean(`measure') if month == `i'
tabstat `measure'_month_`i',save
 mat total = r(StatTotal)

mat li total

scalar s = total[1,1]

replace `measure'_monthaverage = s if month == `i'

 }
 }
 
duplicates drop clust_logFCS,force
foreach measure of varlist RCSI logFCS HDDS{

drop `measure'_month_* `measure'_ipc_*
}
save clust_2010.dta, replace


use "Malawi IHS3 files/IHS2013_PNAS_merged.dta",clear 

drop if HDDS ==0 
drop if FCS ==0 

replace RCSI =42 if RCSI>42

foreach level of varlist clust {
egen `level'_logFCS = mean(logFCS), by (`level')
egen `level'_RCSI = mean(RCSI), by (`level' )
egen `level'_HDDS = mean(HDDS), by (`level' )
}

 levelsof fnid,local(ipczone_levels)
foreach measure of varlist RCSI logFCS HDDS{
 gen `measure'_ipczone_average = . 

 foreach level of local ipczone_levels {
 egen `measure'_ipc_`level' = mean(`measure') if fnid == "`level'"
 tabstat `measure'_ipc_`level',save
 mat total = r(StatTotal)

mat li total

scalar ipc = total[1,1]

replace `measure'_ipczone_average = ipc if fnid == "`level'"


  }  
 }

 
foreach measure of varlist RCSI logFCS HDDS{
 gen `measure'_monthaverage = . 

forvalues i = 1/12 {

egen `measure'_month_`i' = mean(`measure') if month == `i'
tabstat `measure'_month_`i',save
 mat total = r(StatTotal)

mat li total

scalar s = total[1,1]

replace `measure'_monthaverage = s if month == `i'

 }
 }
 
duplicates drop clust_logFCS,force
foreach measure of varlist RCSI logFCS HDDS{

drop `measure'_month_* `measure'_ipc_*
}

duplicates drop clust_logFCS,force

 
save clust_2013.dta, replace


foreach measure of varlist RCSI logFCS HDDS{
local `measure'_month_effect `measure'_month*
}



use "clust_2010.dta",clear 

xi: reg clust_logFCS ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 

predict clust_logFCS_predict  

scalar r2_logFCS =  e(r2)

drop if clust_logFCS_predict ==.
keep clust_logFCS clust_logFCS_predict
export delimited "logFCS_predict_CLUST.csv",replace



use "clust_2010.dta",clear 
xi: reg clust_HDDS ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist'
use "clust_2013.dta",clear 


predict clust_HDDS_predict  

scalar r2_HDDS =  e(r2)


drop if clust_HDDS_predict ==.
keep clust_HDDS clust_HDDS_predict
export delimited "HDDS_predict_CLUST.csv",replace


use "clust_2010.dta",clear 

reg clust_RCSI ipc_lag1 ipc_lag12 `clust_weather'  `clust_mkt'  `clust_asset1' `clust_demo' `clust_land' `clust_nutri' `clust_dist' 
use "clust_2013.dta",clear 
predict clust_RCSI_predict  
scalar r2_RCSI  =  e(r2)




drop if clust_RCSI_predict ==.
keep clust_RCSI clust_RCSI_predict
export delimited "RCSI_predict_CLUST.csv",replace


use "clust_2013.dta",clear 
export delimited "clust_2013.csv",replace


use "clust_2010.dta",clear 
export delimited "clust_2010.csv",replace


foreach m of varlist clust_logFCS clust_HDDS clust_RCSI{

foreach level of varlist ipczone TA clust {


use "clust_2010.dta",clear 
xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist'
use "clust_2013.dta",clear 
predict `m'_`level'_predict_m1 
drop if `m'_`level'_predict_m1 ==.
keep `m' `m'_`level'_predict_m1
export delimited "`m'_predict_`level'_m1.csv",replace


use "clust_2010.dta",clear 
xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist'
use "clust_2013.dta",clear 
predict `m'_`level'_predict_m2  
drop if `m'_`level'_predict_m2 ==.
keep `m' `m'_`level'_predict_m2
export delimited "`m'_predict_`level'_m2.csv",replace

 

use "clust_2010.dta",clear 
xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist'
use "clust_2013.dta",clear 
predict `m'_`level'_predict_m3  
drop if `m'_`level'_predict_m3 ==.
keep `m' `m'_`level'_predict_m3
export delimited "`m'_predict_`level'_m3.csv",replace

use "clust_2010.dta",clear  
}
}


 
use "clust_2010.dta",clear 

foreach m of varlist clust_logFCS clust_HDDS clust_RCSI{

foreach level of varlist ipczone TA clust {

xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist'
predict `m'_`level'_predict_m3 
keep `m' `m'_`level'_predict_m3
export delimited "`m'_predict_`level'_m3_2010.csv",replace

use "clust_2010.dta",clear 
}
}

use "clust_2010.dta",clear 
 
foreach level of varlist clust{
keep  `level'_logFCS `level'_RCSI `level'_HDDS  ``level'_weather'   ``level'_lagmkt' ``level'_lag_price' ``level'_lag_thinn' ``level'_mkt'  ``level'_asset1'   ``level'_land'    ``level'_nutri'    ``level'_dist'   ``level'_demo'  
}
export delimited "train_data.csv",replace



use "clust_2013.dta",clear 
foreach level of varlist clust{
keep  `level'_logFCS `level'_RCSI `level'_HDDS  ``level'_weather'   ``level'_lagmkt' ``level'_lag_price' ``level'_lag_thinn' ``level'_mkt'  ``level'_asset1'   ``level'_land'    ``level'_nutri'    ``level'_dist'   ``level'_demo'  
}
export delimited "test_data.csv",replace


****************************************
*** prediction using ipc value only 
****************************************

use "clust_2010.dta",clear 
 tab ipc_lag1,gen(ipc_lag1_dummy)
 save "clust_2010.dta",replace


use "clust_2013.dta",clear 
 tab ipc_lag1,gen(ipc_lag1_dummy)
save "clust_2013.dta",replace

foreach m of varlist logFCS    {
use "clust_2010.dta",clear 
xi: reg `m' ipc_lag1_dummy2 ipc_lag1_dummy3
use "clust_2013.dta",clear 
predict  clust_`m'_predict_ipc  
drop if clust_`m'_predict_ipc ==.
keep clust_`m' clust_`m'_predict_ipc
*export delimited "`m'_predict_CLUST_IPC.csv",replace
}

use "clust_2010.dta",clear 

foreach m of varlist logFCS RCSI HDDS   {
use "clust_2010.dta",clear 
xi: reg `m' ipc_lag1_dummy2 ipc_lag1_dummy3
predict  clust_`m'_predict_ipc 
drop if clust_`m'_predict_ipc ==.
keep clust_`m' clust_`m'_predict_ipc
export delimited "`m'_predict_CLUST_IPC_2010.csv",replace
}




use "clust_2010.dta",clear 
xi: tobit RCSI ipc_lag1_dummy2 ipc_lag1_dummy3,ll
use "clust_2013.dta",clear 
predict  clust_RCSI_predict_ipc  
drop if clust_RCSI_predict_ipc ==.
keep clust_RCSI clust_RCSI_predict_ipc
export delimited "RCSI_predict_CLUST_IPC.csv",replace

