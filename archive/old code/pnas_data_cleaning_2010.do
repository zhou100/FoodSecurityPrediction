**************************************************
** 2010 data clean
***************************************************


cd "~/Box Sync/Research/Malawi_FewS/"


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


encode fnid,gen(fnid_code)
duplicates drop fnid_code yearmo,force


merge 1:1 yearmo fnid using "MW_missing.dta",force 
drop _merge 
drop fnid_code

encode fnid,gen(fnid_code)
xtset fnid_code yearmo
* gen ipc_lag1 = ipc2 ,by (fnid_code yearmo)

by fnid_code :gen ipc_lag1=ipc2[_n-1]
by fnid_code :gen ipc_lag12=ipc2[_n-12]

drop  fnid_code

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
* rename fsrad3_lcmaj percent_ag 
gen percent_ag = 0.6 if fsrad3_lcmaj== 20| fsrad3_lcmaj== 30
replace  percent_ag = 1 if fsrad3_lcmaj== 14
replace  percent_ag = 0 if percent_ag==.
label variable percent_ag "percent of ag within 1km"

* soil nutrient availability and soil nutrient retention capacity,
rename sq1 nutri_avail 
rename sq2 nutri_retention
* elevation and terrain roughness,
rename srtm_eaf elevation
* rename srtm_eaf_5_15 terrain_rough 

*gen plateau_or_mountain =1 if srtm_eaf_5_15 ==7 | srtm_eaf_5_15 ==6 |  srtm_eaf_5_15 ==8 |srtm_eaf_5_15 ==11 | srtm_eaf_5_15 ==12 |  srtm_eaf_5_15 ==13

*replace plateau_or_mountain =0  if plateau_or_mountain==.


*gen mountains =1 if srtm_eaf_5_15 ==11 | srtm_eaf_5_15 ==12 |  srtm_eaf_5_15 ==13
*replace   =0  if  ==.

/*

gen terrain_rough=1 if srtm_eaf_5_15 ==7
replace terrain_rough=1 if srtm_eaf_5_15 ==8 | srtm_eaf_5_15 ==11 | srtm_eaf_5_15 ==12 | srtm_eaf_5_15 ==13
replace terrain_rough=0 if terrain_rough==.
*/


rename hh_f34 cells_own
label variable cells_own "cells_owned"
rename hh_f35 cell_cost
label variable dist_admarc "kms to mkt"
label variable dist_road "kms to road"


* population density （need to merge that )

drop _merge 
merge m:1 yearmo fnid using "ipc_mkt_thin_zone.dta",force
drop id 
keep if _merge==3



drop _merge
merge m:1 yearmo fnid using "ipc_price_zone.dta",force
drop id 
keep if _merge==3

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

gen nutri_reten_constrained =1 if nutri_reten2==1 | nutri_reten3==1 | nutri_reten4==1 
replace nutri_reten_constrained =0 if nutri_reten_constrained==.


gen nutri_avail_constrained =1 if nutri_avail2==1 | nutri_avail3==1 | nutri_avail4==1 
replace nutri_avail_constrained =0 if nutri_avail_constrained==.

label variable nutri_reten_constrained "Constrained by Nutrient retention capacity"




merge m:1 yearmo clust using "rainstat_cluster_weather.dta",force 
keep if _merge==3
drop _merge 

foreach level of varlist ipczone TA clust {
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
egen `level'_nutri_reten_constrained = mean(nutri_reten_constrained),by (`level' )
egen `level'_nutri_avail_constrained = mean(nutri_avail_constrained),by (`level' )
egen `level'_elevation= mean(elevation), by (`level' )
*egen `level'_plateau_or_mountain= mean(plateau_or_mountain), by (`level' )
*egen `level'_mountains= mean(mountains), by (`level' )
egen `level'_cells_own= mean(cells_own), by (`level' )
}

* replace the ipczone level price and thinness by the ipczone ones 
replace ipczone_thinn= ipc_mkt_thin
replace ipczone_price = ipc_price
replace ipczone_lag_price = Lipc_price
replace ipczone_lag_thinn = Lipc_mkt_thin


rename  L12rain_cytot ipczone_L12raincytot
rename  L12day1rain ipczone_L12day1rain
rename  L12maxdays ipczone_L12maxdays




egen TA_L12raincytot = mean(clust_L12raincytot), by (TA)
egen TA_L12day1rain = mean(clust_L12day1rain), by (TA )
egen TA_L12maxdays = mean(clust_L12maxdays), by (TA )

*`level'_nutri_reten_constrained

gen RCSI_tail = RCSI if RCSI>4
gen logFCS_tail =logFCS if FCS<42
gen HDDS_tail = HDD if HDD < 6


save "Malawi IHS3 files/IHS2010_PNAS_merged.dta",replace


