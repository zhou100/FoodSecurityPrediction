import delimited "MW_IPC_missing 200904 quarter.csv",clear varnames(1)
rename ïfnid fnid
keep fnid cs20090101  cs20090401
rename cs20090101 cs200901
rename cs20090401 cs200904

gen cs200905 = cs200904
gen cs200906 = cs200904
gen cs200903 = cs200901


reshape long cs, i(fnid) j(yearmo)
rename cs ipc2 
drop if yearmo ==200901
save MW_missing.dta,replace

use "malawi_ipc_only.dta",clear
drop if yearmo<200907
drop if yearmo>201106


encode fnid,gen(fnid_code)
duplicates drop fnid_code yearmo,force


merge 1:1 yearmo fnid using "MW_missing.dta",force 
drop fnid_code

encode fnid,gen(fnid_code)
xtset fnid_code yearmo
gen ipc_lag1 = ipc2 ,by (fnid_code yearmo)

by fnid_code :gen ipc_lag1=ipc2[_n-1]
by fnid_code :gen  =ipc2[_n-12]

drop _merge fnid_code

save malawi_ipc_value.dta,replace




***********************
* with the PNAS data 
************************
cd "~/Box Sync/Research/Malawi_FewS/"
set more off

*use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 
use "Malawi IHS3 files/IHS2010_PNAS.dta",clear 
* use "Malawi IHS3 files/IHS2013_PNAS.dta",clear 
* distance to closest market (or other measure of remoteness), 
* dist_road
* dist_admarc 

* share of land in agriculture  
rename fsrad3_lcmaj percent_ag 
label variable percent_ag "percent of ag 1km"

* soil nutrient availability and soil nutrient retention capacity,
rename sq1 nutri_avail 
rename sq2 nutri_retention
* elevation and terrain roughness,
rename srtm_eaf elevation
rename srtm_eaf_5_15 terrain_rough 
rename hh_f34 cells_own
label variable cells_own "cells_owned"
rename hh_f35 cell_cost
label variable dist_admarc "kms to mkt"
label variable dist_road "kms to road"


* population density （need to merge that )




drop _merge
merge m:1 yearmo fnid using "malawi_ipc_value.dta",force

drop _merge
merge m:1 yearmo case_id using "hh_thin_new.dta",force
* only those month that have hh data are merged 
keep if _merge==3
drop _merge
rename mkt_thinn mkt_thinness

merge m:1 yearmo case_id using "hh_price_new.dta",force
* only those month that have hh data are merged 
keep if _merge==3
drop _merge

xtset  case_id yearmo
rename hh_a02b TA
rename ea_id clust


pca Refrigerator  Television Radio Bicycle Motorcycle Car 
rotate
predict asset_index2

label variable FCS "FCS"
label variable RCSI "RCSI"
label variable HDDS "HDDS"

label variable hhsize "HHsize"
label variable asset_index2 "Asset index"


gen logFCS = log(FCS) 

gen roof = 1- roof_natural


* inverse of roof

tab nutri_retention,gen(nutri_reten)
tab nutri_avail,gen(nutri_avail)

****** collapse by annual ***************
preserve 

ds
local a = r(varlist)
dis "`a'"

local b = year 
local aaa = case_id 
local bbb:list  a - aaa
dis "`bbb'"
dis "`aaa'"

local xyz  year case_id 
local zyx:list  a-xyz 

dis "`zyx'"

destring `bbb' ,force replace
 

collapse (mean) `zyx' ,by(case_id year) 
****** collapse by annual ***************

foreach level of varlist ipczone TA clust {
egen `level'_L12raincytot = mean(L12rain_cytot), by (`level')
egen `level'_L12day1rain = mean(L12day1rain), by (`level' )
egen `level'_L12maxdays = mean(L12maxdays), by (`level' )
egen `level'_floodmax = mean(flood_max), by (`level' )
egen `level'_lag_price = mean(Llnprice_impu), by (`level' )
egen `level'_lag_thinn = mean(Lmkt_thinn), by (`level' )
egen `level'_price = mean(lnprice_impu), by (`level' )
egen `level'_thinn = mean(mkt_thinn), by (`level')
egen `level'_floor= mean(floor_dirt_sand_dung), by (`level' )
egen `level'_roof = mean(roof), by (`level' )
egen `level'_cell = mean(cell_phone), by (`level' )
egen `level'_hhsize = mean(hhsize), by (`level' )
egen `level'_hh_age = mean(hh_age), by (`level' )
egen `level'_hh_gender= mean(hh_gender), by (`level' )
egen `level'_asset= mean(asset_index2), by (`level' )
egen `level'_dist_road= mean(dist_road), by (`level' )
egen `level'_dist_admarc= mean(dist_admarc), by (`level' )
egen `level'_percent_ag= mean(percent_ag), by (`level' )
forvalues i= 1(1)5{
egen `level'_nutri_reten_`i' = mean(nutri_reten`i'),by (`level' )
egen `level'_nutri_avail_`i' = mean(nutri_avail`i'),by (`level' )
}
egen `level'_elevation= mean(elevation), by (`level' )
egen `level'_terrain_rough= mean(terrain_rough), by (`level' )
egen `level'_cells_own= mean(cells_own), by (`level' )
}



 


foreach level of varlist ipczone TA clust{
 local `level'_weather `level'_L12raincytot  `level'_L12day1rain `level'_L12maxdays `level'_floodmax 
local `level'_lagmkt `level'_lag_price `level'_lag_thinn 
local `level'_mkt `level'_price `level'_thinn 
local `level'_asset1 `level'_roof `level'_cells_own
local `level'_land `level'_percent_ag  `level'_elevation `level'_terrain_rough
local `level'_nutri `level'_nutri_reten_1 `level'_nutri_reten_2  `level'_nutri_avail_1 `level'_nutri_avail_2 `level'_nutri_avail_3
local `level'_nutri_demo 
local `level'_dist `level'_dist_road `level'_dist_admarc 
local `level'_demo `level'_hhsize `level'_hh_age `level'_hh_gender `level'_asset 
}




gen RCSI_tail = RCSI if RCSI>3
gen logFCS_tail =logFCS if FCS<36
gen HDDS_tail = HDD if HDD < 7



* a. ipc-level averages

foreach m of varlist logFCS RCSI  HDDS RCSI_tail logFCS_tail HDDS_tail  {

 foreach level of varlist ipczone TA clust{

xi: reg `m' ipc_lag1   ``level'_weather'  ``level'_mkt'   ``level'_land' ``level'_nutri'  ``level'_dist'
est store `m'_`level'_col1
predict `m'_`level'_hat_1,xb
 


xi: reg `m' ipc_lag1   ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col2
predict `m'_`level'_hat_2,


xi: reg `m' ipc_lag1   ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri' ``level'_dist'
est store `m'_`level'_col3
predict `m'_`level'_hat_3,xb


esttab `m'_`level'_col1 `m'_`level'_col2 `m'_`level'_col3  using `m'_`level'_table_annual.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1   ``level'_weather'  ``level'_mkt'  ``level'_asset1' ``level'_demo' ``level'_land' ``level'_nutri'  ``level'_dist') ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))
}
}

local weather L12rain_cytot flood_maxprecip L12day1rain L12maxdays 
local lagmkt Llnprice_impu Lmkt_thinn
local mkt mkt_thinness lnprice_impu
local asset1  roof cells_own 
local demo hh_age hh_gender hhsize asset_index2 
local dist dist_road dist_admarc 
local land percent_ag elevation terrain_rough 
local nutri nutri_reten1 nutri_reten2   nutri_avail1 nutri_avail2 nutri_avail3  



foreach m of varlist logFCS RCSI  HDDS RCSI_tail logFCS_tail HDDS_tail  {

xi: reg `m' ipc_lag1   `weather'  `mkt' `land' `dist' `nutri'
est store `m'_ind_col1
predict `m'_ind_hat_1,xb


xi: reg `m' ipc_lag1   `weather'  `mkt'  `asset1' `land' `dist' `nutri'
est store `m'_ind_col2
predict `m'_ind_hat_2,xb
 


xi: reg `m' ipc_lag1   `weather'  `mkt'  `asset1' `demo' `land' `dist' `nutri'
est store `m'_ind_col3
predict `m'_ind_hat_3,xb


esttab `m'_ind_col1 `m'_ind_col2 `m'_ind_col3 using `m'_ind_table_annual.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1   `weather'  `mkt' `land' `dist' `nutri' `asset1' `demo'   ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))
}





export delimited "malawi_data_new.csv",replace

 
 
count if FCS <=21 & FCS!=.
scalar FCS_LOW = r(N)
count if FCS <=35 & FCS >21 & FCS!=.
scalar FCS_mid = r(N)
count if FCS >35 & FCS!=.
scalar FCS_high = r(N) 
 

forvalues x = 1(1)6{
count if FCShat_`x' <=21 & FCShat_`x'!=.
count if FCShat_`x' <=35 & FCShat_`x' >21 & FCShat_`x'!=.
count if FCShat_`x' >35 & FCShat_`x'!=.
}
 


count if RCSI <=3 & RCSI!=.
count if RCSI <=8 & RCSI >3 & RCSI!=.
count if RCSI <=19 & RCSI >8 & RCSI!=.
count if RCSI >19 & RCSI!=.


 forvalues x = 1(1)6{
count if RCSIhat_`x' <=3 & RCSIhat_`x'!=.
count if RCSIhat_`x' <=8 & RCSIhat_`x' >3 & RCSIhat_`x'!=.
count if RCSIhat_`x' <=19 & RCSIhat_`x' >8 & RCSIhat_`x'!=.
count if RCSIhat_`x' >19 & RCSIhat_`x'!=.
}


count if HDDS <=4.5 & HDDS!=.
count if HDDS <=6 & HDDS >4.5 & HDDS!=.
count if HDDS >6 & HDDS!=.

forvalues x = 1(1)6{
count if HDDShat_`x' <=4.5 & HDDShat_`x'!=.
count if HDDShat_`x' <=6 & HDDShat_`x' >4.5 & HDDShat_`x'!=.
count if HDDShat_`x' >6 & HDDShat_`x'!=.
}



* create measures 


