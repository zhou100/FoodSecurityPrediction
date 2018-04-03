
***********************
* with the PNAS data 
************************


*********************************************************************
* The purpose of this code is to clean the X variables in 2013 
**********************************************************************

cd "~/Box Sync/Research/Malawi_FewS/"
set more off

*use "Malawi IHS3 files/IHS2013_PNAS.dta",clear 
*keep ea_id LAT_DD_MOD LON_DD_MOD
*export delimited "hhloc_2013.csv",replace

* import delimited "hh_loc_zone_2013.csv",clear varnames(1)
* keep ea_id fnid
* duplicates list ea_id
* save ipczone_2013_loc.dta,replace





use "Malawi IHS3 files/IHS2013_PNAS.dta",clear 

* merge in the correct TA ID 



merge 1:1 y2_hhid using "Malawi IHS3 files/IHS2013_TA.dta",force 
drop _merge 




* merge in the FNID 
destring ea_id,force replace
merge m:m ea_id using "ipczone_2013_loc.dta",force 
drop _merge 


* generate the year and month 
rename hh_a23a_2 month
rename hh_a23a_3 year
replace year=2013 if year==.

replace year = hh_a23c_3 if hh_a23c_3!=.
replace year = hh_a23b_3 if hh_a23b_3!=.

replace month = hh_a23c_2 if hh_a23c_2!=.
replace month = hh_a23b_2 if hh_a23c_2==. & hh_a23b_2 !=. 
gen yearmo=year*100+mo

drop hh_a23b_2 hh_a23b_3 hh_a23c_2 hh_a23c_3



* Merge in the level weather 

* use MalawiIPC.dta,clear
* keep L12rain_cytot flood_maxprecip L12day1rain L12maxdays fnid yearmo 
* save ipczone_weather.dta,replace


merge m:m fnid yearmo using "ipczone_weather.dta",force 
keep if _merge ==3
drop _merge 




* merge in the prices 




* population density （need to merge that )

merge m:1 yearmo fnid using "ipc_mkt_thin_zone.dta",force
drop id 
keep if _merge==3



drop _merge
merge m:1 yearmo fnid using "ipc_price_zone.dta",force
drop id 
keep if _merge==3

drop _merge
merge m:1 yearmo fnid using "malawi_ipc_value.dta",force
keep if _merge==3


drop _merge
rename y2_hhid case_id

merge m:1 yearmo ea_id using "clust_thin_new_2013.dta",force


* only those month that have hh data are merged 
keep if _merge==3
drop _merge
rename mkt_thinn mkt_thinness



merge m:1 yearmo ea_id using "clust_price_2013_new.dta",force
* only those month that have hh data are merged 
keep if _merge==3
drop _merge

encode case_id, generate(case_id_num)
xtset  case_id_num yearmo
rename hh_a10b  TA
rename ea_id clust




* * use "Malawi IHS3 files/IHS2013_PNAS.dta",clear 
* distance to closest market (or other measure of remoteness), 
* dist_road
* dist_admarc 

* share of land in agriculture  
* rename fsrad3_agpct percent_ag 
gen percent_ag = 0.6 if fsrad3_agpct== 20| fsrad3_agpct== 30
replace  percent_ag = 1 if fsrad3_agpct== 14
replace  percent_ag = 0 if percent_ag==.
label variable percent_ag "percent of ag within 1km"

* soil nutrient availability and soil nutrient retention capacity,
rename sq1 nutri_avail 
rename sq2 nutri_retention
* elevation and terrain roughness,
rename srtm_1k  elevation
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

encode fnid,gen(ipczone)


foreach level of varlist ipczone TA clust {
egen `level'_floodmax = mean(flood_max), by (`level' )
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

rename   ipc_mkt_thin ipczone_thinn
rename   ipc_price ipczone_price
rename   Lipc_price ipczone_lag_price
rename   Lipc_mkt_thin ipczone_lag_thinn


foreach level of varlist TA clust {
egen `level'_lag_price = mean(Llnprice_impu), by (`level' )
egen `level'_lag_thinn = mean(Lmkt_thinn), by (`level' )
egen `level'_price = mean(lnprice_impu), by (`level' )
egen `level'_thinn = mean(mkt_thinn), by (`level')
}

foreach level of varlist ipczone {
egen `level'_L12raincytot = mean(L12rain_cytot), by (`level')
egen `level'_L12day1rain = mean(L12day1rain), by (`level' )
egen `level'_L12maxdays = mean(L12maxdays), by (`level' )
}

*`level'_nutri_reten_constrained

tostring clust,generate (clust_id)
rename clust clust_num
rename clust_id clust 


merge m:1 yearmo clust using "rainstat_cluster_weather.dta",force 
keep if _merge==3
drop _merge 


egen TA_L12raincytot = mean(clust_L12raincytot), by (TA)
egen TA_L12day1rain = mean(clust_L12day1rain), by (TA )
egen TA_L12maxdays = mean(clust_L12maxdays), by (TA )



save "Malawi IHS3 files/IHS2013_PNAS_merged.dta",replace


