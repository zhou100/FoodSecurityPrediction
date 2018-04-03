
*  running our models against area averages in the meanwhile tho? 

 *  (so FS averages at the cluster, TA and IPC level)

cd "~/Box Sync/Research/Malawi_FewS/"
set more off

*use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 
use "Malawi IHS3 files/IHS2010_PNAS_merged.dta",clear 



xtset case_id yearmo



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

* foreach level of varlist ipczone TA clust {
* egen `level'_logFCS = mean(logFCS), by (`level')
* egen `level'_RCSI = mean(RCSI), by (`level' )
* egen `level'_HDDS = mean(HDDS), by (`level' )
* }

* duplicates drop clust_logFCS,force







use "clust_2010.dta",clear 

foreach level of varlist ipczone TA clust{
replace `level'_L12raincytot = `level'_L12raincytot /1000
replace `level'_L12day1rain = `level'_L12day1rain /100
replace `level'_L12maxdays = `level'_L12maxdays /100
replace `level'_floodmax = `level'_floodmax /1000
replace `level'_elevation = `level'_elevation /1000
}



foreach level of varlist clust{

xi: reg clust_logFCS ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist'
}


foreach m of varlist logFCS  HDDS  RCSI{

foreach level of varlist ipczone TA clust{

xi: reg clust_`m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist'
est store `m'_`level'_col1_agg
scalar `m'_`level'_r2_m1_agg =  e(r2)
predict `m'_`level'_hat_1_agg,xb
 


xi: reg clust_`m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col2_agg
scalar `m'_`level'_r2_m2_agg =  e(r2)
predict `m'_`level'_hat_2_agg,xb


xi: reg clust_`m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col3_agg
scalar `m'_`level'_r2_m3_agg =  e(r2)
predict `m'_`level'_hat_3_agg,xb


esttab `m'_`level'_col1_agg `m'_`level'_col2_agg `m'_`level'_col3_agg  using `m'_`level'_aggre.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'  `"R^2"'))
}
}

export delimited "cluster_fs.csv",replace



 foreach level of varlist ipczone TA clust{

tobit clust_RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist' ,ll
predict RCSI_`level'_tobit_1_agg,xb
scalar RCSI_`level'_r2_m1_agg =  e(r2_p)
est store RCSI_`level'_tobit_1_agg

tobit clust_RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist',ll
est store RCSI_`level'_tobit_2_agg
scalar RCSI_`level'_r2_m2_agg =  e(r2_p)
predict RCSI_`level'_tobit_2_agg,xb

tobit clust_RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist',ll
est store RCSI_`level'_tobit_3_agg
scalar RCSI_`level'_r2_m3_agg =  e(r2_p)
predict RCSI_`level'_tobit_3_agg,xb


esttab RCSI_`level'_tobit_1_agg RCSI_`level'_tobit_2_agg RCSI_`level'_tobit_3_agg  using RCSI_`level'_tobit_aggre.tex, replace f ///
label booktabs b(3) se(3)   keep (ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2_p, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'  `"Psedo R^2"'))
}








local weather L12rain_cytot flood_maxprecip L12day1rain L12maxdays 
local lagmkt Llnprice_impu Lmkt_thinn
local mkt mkt_thinness lnprice_impu
local asset1  roof cells_own 
local demo hh_age hh_gender hhsize asset_index2 
local dist dist_road dist_admarc 
local land percent_ag elevation  
local nutri nutri_reten_constrained


foreach m of varlist logFCS RCSI  HDDS  {

xi: reg clust_`m' ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri'
est store clust_`m'_ind_col1
predict clust_`m'_ind_hat_1,xb


xi: reg clust_`m' ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `land' `dist' `nutri'
est store clust_`m'_ind_col2
predict clust_`m'_ind_hat_2,xb
 


xi: reg clust_`m' ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `demo' `land' `dist' `nutri'
est store clust_`m'_ind_col3
predict clust_`m'_ind_hat_3,xb


esttab clust_`m'_ind_col1 clust_`m'_ind_col2 clust_`m'_ind_col3 using clust_`m'_ind_aggre.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri' `asset1' `demo'   ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"R^2"'))
}




******************************************************
** Regressions using only the IPC values 
******************************************************

foreach m of varlist logFCS RCSI  HDDS  {
xi: reg `m' i.ipc_lag1 
predict  `m'_ipc,xb
}

export delimited "malawi_data_ipc.csv",replace

