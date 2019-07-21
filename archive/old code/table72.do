
cd "~/Box Sync/Research/Malawi_FewS/"
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

* RCSI_tail logFCS_tail HDDS_tail

foreach m of varlist logFCS  HDDS   {

 foreach level of varlist ipczone TA clust{

xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist'
est store `m'_`level'_col1
scalar `m'_`level'_r2_m1 =  e(r2_a)
predict `m'_`level'_hat_1,xb
 


xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col2
scalar `m'_`level'_r2_m2 =  e(r2_a)
predict `m'_`level'_hat_2,


xi: reg `m' ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col3
scalar `m'_`level'_r2_m3 =  e(r2_a)
predict `m'_`level'_hat_3,xb


esttab `m'_`level'_col1 `m'_`level'_col2 `m'_`level'_col3  using `m'_`level'_table.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"R^2"'))
}
}

local weather L12rain_cytot flood_maxprecip L12day1rain L12maxdays 
local lagmkt Llnprice_impu Lmkt_thinn
local mkt mkt_thinness lnprice_impu
local asset1  roof cells_own 
local demo hh_age hh_gender hhsize asset_index2 
local dist dist_road dist_admarc 
local land percent_ag elevation  
local nutri nutri_reten_constrained


foreach m of varlist logFCS  HDDS  logFCS_tail HDDS_tail  {

xi: reg `m' ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri'
est store `m'_ind_col1
predict `m'_ind_hat_1,xb
scalar `m'_ind_r2_m1 =  e(r2_a)



xi: reg `m' ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `land' `dist' `nutri'
est store `m'_ind_col2
predict `m'_ind_hat_2,xb
 scalar `m'_ind_r2_m2 =  e(r2_a)



xi: reg `m' ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `demo' `land' `dist' `nutri'
est store `m'_ind_col3
predict `m'_ind_hat_3,xb
scalar `m'_ind_r2_m3 =  e(r2_a)


esttab `m'_ind_col1 `m'_ind_col2 `m'_ind_col3 using `m'_ind_table.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri' `asset1' `demo'   ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'  `"R^2"'))
}



 foreach level of varlist ipczone TA clust{

tobit RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist' ,ll
predict RCSI_`level'_hat_1,xb
scalar RCSI_`level'_r2_m1 =  e(r2_p)
est store RCSI_`level'_tobit1

tobit RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist',ll
predict RCSI_`level'_hat_2,xb
est store RCSI_`level'_tobit2
scalar RCSI_`level'_r2_m2 =  e(r2_p)

tobit RCSI ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist',ll
predict RCSI_`level'_hat_3,xb
est store RCSI_`level'_tobit3
scalar RCSI_`level'_r2_m3 =  e(r2_p)


esttab RCSI_`level'_tobit1 RCSI_`level'_tobit2 RCSI_`level'_tobit3  using RCSI_`level'_tobit.tex, replace f ///
label booktabs b(3) se(3)   keep (ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2_p, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'  `"Psedo R^2"'))
}



tobit RCSI ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri',ll
est store  RCSI_ind_col1
predict RCSI_ind_hat_1,xb
scalar RCSI_ind_r2_m1 =  e(r2_p)



tobit RCSI ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `land' `dist' `nutri',ll
est store RCSI_ind_col2
predict RCSI_ind_hat_2,xb
 scalar RCSI_ind_r2_m2 =  e(r2_p)



tobit RCSI ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `demo' `land' `dist' `nutri',ll
est store RCSI_ind_col3
predict RCSI_ind_hat_3,xb
scalar RCSI_ind_r2_m3 =  e(r2_p)




esttab RCSI_ind_col1 RCSI_ind_col2 RCSI_ind_col3 using RCSI_ind_tobit.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri' `asset1' `demo'   ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2_p, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Psedo R^2"'))




 foreach level of varlist ipczone TA clust{

tobit RCSI_tail ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist' ,ll
predict RCSI_tail_`level'_hat_1,xb
scalar RCSI_tail_`level'_r2_m1 =  e(r2_p)
est store RCSI_tail_`level'_tobit1

tobit RCSI_tail ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist',ll
est store RCSI_tail_`level'_tobit2
scalar RCSI_tail_`level'_r2_m2 =  e(r2_p)
predict RCSI_tail_`level'_hat_2,xb

tobit RCSI_tail ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist',ll
est store RCSI_tail_`level'_tobit3
scalar RCSI_tail_`level'_r2_m3 =  e(r2_p)
predict RCSI_tail_`level'_hat_3,xb


esttab RCSI_tail_`level'_tobit1 RCSI_tail_`level'_tobit2 RCSI_tail_`level'_tobit3  using RCSI_tail_`level'_tobit.tex, replace f ///
label booktabs b(3) se(3)   keep (ipc_lag1 ipc_lag12 ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2_p, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"'  `"Psedo R^2"'))
}



tobit RCSI_tail ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri',ll
est store  RCSI_tail_ind_col1
predict RCSI_tail_ind_hat_1,xb
scalar RCSI_tail_ind_r2_m1 =  e(r2_p)



tobit RCSI_tail ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `land' `dist' `nutri',ll
est store RCSI_tail_ind_col2
predict RCSI_tail_ind_hat_2,xb
 scalar RCSI_tail_ind_r2_m2 =  e(r2_p)



tobit RCSI_tail ipc_lag1 ipc_lag12 `weather'  `mkt'  `asset1' `demo' `land' `dist' `nutri',ll
est store RCSI_tail_ind_col3
predict RCSI_tail_ind_hat_3,xb
scalar RCSI_tail_ind_r2_m3 =  e(r2_p)




esttab RCSI_tail_ind_col1 RCSI_tail_ind_col2 RCSI_tail_ind_col3 using RCSI_tail_ind_tobit.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 ipc_lag12 `weather'  `mkt' `land' `dist' `nutri' `asset1' `demo'   ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N  r2_p, fmt(0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Psedo R^2"'))

export delimited "malawi_data_new.csv",replace



preserve

parmest,format(estimate  %8.2f p %8.1e) list(,)


mat beta=e(b)

svmat double beta, names(matcol)

display _b[_cons]
ereturn list




