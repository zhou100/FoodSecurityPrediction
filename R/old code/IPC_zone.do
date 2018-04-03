cd "~/Box Sync/Research/Malawi_FewS/"

import delimited "ipc_price_long.csv",clear varnames(1)
drop v1 
sort fnid yearmo 

save ipc_price.dta,replace 




import delimited "ipc_market_thinness_long.csv",clear varnames(1)
drop v1 
sort fnid yearmo 

save ipc_mkt_thinn.dta,replace 

import delimited "ipc_market_thinness_long.csv",clear varnames(1)
drop v1 
sort fnid yearmo 

save ipc_mkt_thinn.dta,replace 

import delimited "ipc_mkt_thin_nopop_long.csv",clear varnames(1)
drop v1 
sort fnid yearmo 
save ipc_mkt_thinn2.dta,replace 



import delimited "hh_thinness.csv",clear varnames(1)
drop v1 
sort case_id yearmo 
destring yearmo, force replace 
duplicates drop case_id yearmo,force
save hh_thin.dta,replace 


collapse  mkt_thinn,by (yearmo near_mkt)
 encode near_mkt, generate(mkt_code) 
 xtset  mkt_code yearmo
by mkt_code :gen Lmkt_thinn=mkt_thinn[_n-1]
save hh_thin_lag.dta,replace

use hh_thin.dta,clear
merge  m:1 near_mkt yearmo using "hh_thin_lag.dta"
drop _merge mkt_code
save hh_thin_new.dta,replace 


import delimited "hh_price.csv",clear varnames(1)
drop v1 
sort case_id yearmo 
destring yearmo price lnprice_impu, force replace 
duplicates drop case_id yearmo,force
save hh_price.dta,replace 


collapse price lnprice_impu,by (yearmo near_mkt)
encode near_mkt, generate(mkt_code) 
xtset  mkt_code yearmo
by mkt_code :gen Lprice=price[_n-1]
by mkt_code :gen Llnprice_impu=lnprice_impu[_n-1]
save hh_price_lag.dta,replace


use hh_price.dta,clear
merge  m:1 near_mkt yearmo using "hh_price_lag.dta"
drop _merge mkt_code
save hh_price_new.dta,replace 

****************************************
* IPC level regressions 
****************************************
use "MalawiIPC.dta", clear
sort fnid dateno

set more off

*use "market_thinness/thinness_measure_by_ipczone.dta",clear

drop _merge

merge m:1 fnid yearmo using "ipc_price.dta"

* unmatched in user :  most recent data 
* tab yearmo if _merge==2
* unmatched in master: ipczones that don't have prices 
* tab fnid if _merge==1

drop if _merge==2
drop _merge

*merge fnid dateno using "market_thinness/thinness_measure_by_ipczone.dta"

merge m:1 fnid yearmo using "ipc_mkt_thinn.dta"

* unmatched in user :  most recent data 
* tab yearmo if _merge==2
* unmatched in master: ipczones that don't have prices 
* tab fnid if _merge==1
drop if _merge==2
drop _merge



sort ipczone yearmo
bysort ipczone: replace dateno=_n


**************************************************************
*Original Regressions
**************************************************************
*fiddle
xtset ipczone dateno

xi:reg mkt_thinness  L12rain_cytot  flood_max L12day1rain L12maxdays   i.mo  
est store tabel1col1
logit mkt_thinness  L12rain_cytot  flood_max L12day1rain L12maxdays    i.mo  
est store tabel1col2

probit mkt_thinness  L12rain_cytot  flood_max L12day1rain L12maxdays   i.mo  
est store tabel1col3

esttab tabel1col1 tabel1col2 tabel1col3 using table1.tex, replace f ///
label booktabs b(3) se(3) r2  ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))



****************************
* Table 3 results 
*************************

*with new rainfall data
* col 1 
xi: reg q_ipc L12.rain_cytot flood_max L12.day1rain L12.maxdays i.year i.mo
* col 2
xi: reg q_ipc lnprice L12.rain_cytot L12.day1rain L12.maxdays flood_max i.year i.mo
xi: reg q_ipc lnprice_impu L12.rain_cytot L12.day1rain L12.maxdays flood_max i.year i.mo



****************************
*with mkt_thinness
*************************
* col 1 coefficients bigger, with mkt thinness 
xi: reg q_ipc mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays  i.year i.mo
est store t3col1

* col 2 same 
xi: reg q_ipc mkt_thinness lnprice_impu L12rain_cytot L12day1rain L12maxdays flood_max i.year i.mo
est store t3col2



esttab t3col1 t3col2 using mktthin_t3.tex, replace f ///
label booktabs b(5) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays  ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))


****************************
* with lagged ipc and no fixed effect 
*************************
* col 1 
xi: reg q_ipc L3.q_ipc mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays  
est store t3col1_lag

* col 2 same 
xi: reg q_ipc L3.q_ipc mkt_thinness lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays  
est store t3col2_lag

esttab t3col1_lag t3col2_lag using mktthin_t3_lag.tex, replace f ///
label booktabs b(5) se(3) r2 keep (mkt_thinness L3.q_ipc lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays  ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))


****************************
* Table 4 results : mkt thiness not significant 
*************************
* col 1  
xi: reg lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo
est store t4col1

* col 2 
xi: reg lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
xi: reg lnprice_impu mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
est store t4col2

* col 3 
*including deviations
xi: reg lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
est store t4col3


* col 4 and 5
*including interactions with dryipczone
xi: reg lnprice_impu L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12dryipc_rain L12dryipc_max L12.dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo
est store t4col4


xi: reg lnprice_impu L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo i.ipczone
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
est store t4col5


esttab t4col1 t4col2 t4col3 t4col4 t4col5 using mktthin_t4.tex, replace f ///
label booktabs b(6) se(3) r2 keep (mkt_thinness L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))


**************************
* measures without the pop weights
**************************



*use "market_thinness/thinness_measure_by_ipczone.dta",clear

drop _merge

merge m:1 fnid yearmo using "ipc_mkt_thinn2.dta"
drop if _merge==2
drop _merge

sort ipczone yearmo
bysort ipczone: replace dateno=_n


**************************************************************
*Original Regressions
**************************************************************
*fiddle
xtset ipczone dateno



****************************
* Table 3 results 
*************************

*with new rainfall data
* col 1 
xi: reg q_ipc L12.rain_cytot flood_max L12.day1rain L12.maxdays i.year i.mo
* col 2
xi: reg q_ipc lnprice_impu L12.rain_cytot L12.day1rain L12.maxdays flood_max i.year i.mo

****************************
*with mkt_thinness
*************************
* col 1 coefficients bigger, with mkt thinness 
xi: reg q_ipc mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays  i.year i.mo
est store t3col1

* col 2 same 
xi: reg q_ipc mkt_thinness lnprice_impu L12rain_cytot L12day1rain L12maxdays flood_max i.year i.mo
est store t3col2



esttab t3col1 t3col2 using mktthin_t3_nopop.tex, replace f ///
label booktabs b(5) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays  ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))




****************************
* Table 4 results : mkt thiness not significant 
*************************
* col 1  
xi: reg lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo
est store t4col1

* col 2 
xi: reg lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
xi: reg lnprice_impu mkt_thinness L12rain_cytot flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
est store t4col2

* col 3 
*including deviations
xi: reg lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
est store t4col3


* col 4 and 5
*including interactions with dryipczone
xi: reg lnprice_impu L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12dryipc_rain L12dryipc_max L12.dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo
est store t4col4


xi: reg lnprice_impu L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo i.ipczone
xi: reg lnprice_impu mkt_thinness L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
est store t4col5


esttab t4col1 t4col2 t4col3 t4col4 t4col5 using mktthin_t4_nopop.tex, replace f ///
label booktabs b(6) se(3) r2 keep (mkt_thinness L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))

*sort fnid dateno
*drop _merge
*save "LSMS_IPC.dta",replace

* merge LSMS_IPC 


****************************
* Table 5 results : replicating 
*************************

use  "LSMS_IPC.dta",clear
xtset  case_id yearmo

xi: reg RCSI i.ipc2 i.mo
xi: reg RCSI i.ipc2 i.mo i.ipczone

****************************
* Table 6 results : replicating 
*************************
* col 1    
xi: reg RCSI lnprice L12rain_cytot  flood_max L12day1rain L12maxdays  i.year i.mo
* col 2
xi: reg RCSI lnprice L12rain_cytot  flood_max L12day1rain L12maxdays i.year i.mo  i.ipczone


* col 3 
*including deviations
xi: reg RCSI lnprice L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
* col 4 and 5
*including interactions with dryipczone
xi: reg RCSI lnprice L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo
xi: reg RCSI lnprice L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo i.ipczone



****************************
*merge in with mkt_thinness
*************************
use  "LSMS_IPC.dta",clear



merge m:1 yearmo case_id using "hh_thin.dta",force
* only those month that have hh data are merged 
keep if _merge==3
drop _merge
rename mkt_thinn mkt_thinness

merge m:1 yearmo case_id using "hh_price.dta",force
* only those month that have hh data are merged 
keep if _merge==3
drop _merge


gen holiday = 1 if month ==12 |  month ==1
replace holiday =0 if holiday == .
gen day1rain_dummy =1 if month ==12 |  month ==11 |  month ==10
replace day1rain_dummy =0 if day1rain_dummy == .




*collapse (mean) RCSI, by (fnid dateno)
*sort fnid dateno
*drop _merge
*save "LSMS_IPC_zone.dta",replace


****************************
* Table 6 results : with mkt_thinness 
*************************
duplicates drop case_id yearmo,force
xtset  case_id yearmo



* col 1    
xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays  i.year i.mo
est store t6col1

* col 2
xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays i.year i.mo  i.ipczone
est store t6col2


* col 3 
*including deviations
xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays i.year i.mo
est store t6col3

* col 4 and 5
*including interactions with dryipczone
xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo
est store t6col4

xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1 flood_max L12day1rain L12maxdays i.year i.mo i.ipczone
est store t6col5


esttab t6col1 t6col2 t6col3 t6col4 t6col5 using mktthin_t6.tex, replace f ///
label booktabs b(3) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))



****************************
* Table 7 with mkt_thinness  household level 
*************************
* Here is a list of possible models to get at how the IPC approach does
* as a first cut of food security measures relative to the richer, 
* but less frequently available DHS measures, 
* which allow for a variety of targeting approaches:
 
*MODELS:
* Outcomes: RCSI, HDDS, FCS (maybe not HDDS, since we haven’t done much with it yet?)

* Targeting approaches:



use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 

collapse  ipc2,by (yearmo ipczone)
 xtset  ipczone yearmo
by ipczone :gen ipc_lag1=ipc2[_n-1]
by ipczone :gen ipc_lag3=ipc2[_n-3]

save ipc_value_lag.dta,replace


use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 


drop _merge
merge m:1 yearmo ipczone using "ipc_value_lag.dta",force



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

* col 1    

* Community based targeting: 
* community fixed effects (e.g., hh_a01 (district) or hh_a02b (District + TA);


* Readily observables at hh level: 

* Floor_dirt_sand_dung = 1 and/or roof_natural = 1
* Readily observable / measurable hh demographic characteristics: 
* hh_age, hh_gender, hhsize
* (maybe making a dependency ratio, depending on what data are available at hh level)

* Proxy means testing: principle components analysis to create an asset index:
* Refrigerator, Television, Radio, Bicycle, Motorcycle, Car,
* maybe cellphone (and possibly flooring and roofing;
* I’m not sure what the current state of the art is for DHS asset indices);
* cellphone, flooring and roofing if not in the PCA asset index; 

pca Refrigerator  Television Radio Bicycle Motorcycle Car 
rotate
predict asset_index2


pca Refrigerator  Television Radio Bicycle Motorcycle Car floor_dirt_sand_dung roof_natural cell_phone 
rotate
predict asset_index3

xi: reg FCS mean_thinn mean_price mean_weather mean_maxdays mean_day1rain  hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 



*demographics
* Filmer and Pritchett is my go-to, but I feel like DHS must have made 
* some improvements in the past 15 years? 
* Is there info on / use of livestock or land?
* I know a critique of the asset index is that it is urban biased,
*  since it tends to not measure major rural assets 
*(such as tractors, livestock, lands…).

xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index3 i.year i.mo i.hh_a01 
est store t7col1

xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays  hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo   i.ipczone i.hh_a01 
est store t7col2


* col 3 
*including deviations 

xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t7col3


* col 4 and 5
*including interactions with dryipczone
xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t7col4

xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.ipczone i.mo i.hh_a01 
est store t7col5



esttab t7col1 t7col2 t7col3 t7col4 t7col5 using mktthin_t7.tex, replace f ///
label booktabs b(3) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))


* FCS 
*xi: reg RCSI mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays  i.year i.mo i.hh_a02b

xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t8col1

xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays  hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo   i.ipczone i.hh_a01 
est store t8col2


* col 3 
*including deviations 

xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t8col3


* col 4 and 5
*including interactions with dryipczone
xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t8col4

xi: reg FCS mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.ipczone i.mo i.hh_a01 
est store t8col5



esttab t8col1 t8col2 t8col3 t8col4 t8col5 using mktthin_t8.tex, replace f ///
label booktabs b(3) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))




** HDDS 
xi: reg HDDS mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t9col1

xi: reg HDDS mkt_thinness lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays  hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo   i.ipczone i.hh_a01 
est store t9col2


* col 3 
*including deviations 

xi: reg HDDS mkt_thinness lnprice_impu L12rain_cytot L12pos_rain L12neg_rain flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t9col3


* col 4 and 5
*including interactions with dryipczone
xi: reg HDDS mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.mo i.hh_a01 
est store t9col4

xi: reg HDDS mkt_thinness lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone i.year i.ipczone i.mo i.hh_a01 
est store t9col5



esttab t9col1 t9col2 t9col3 t9col4 t9col5 using mktthin_t9.tex, replace f ///
label booktabs b(3) se(3) r2 keep (mkt_thinness lnprice_impu L12rain_cytot flood_maxprecip L12day1rain L12maxdays L12dryipc_max L12dryipc_rain L12dryipc_day1 L12pos_rain L12neg_rain hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))




**********************************************************
* regressions with IPC averages  with monthly data 
**********************************************************

* (1)    Can you do histograms of the 3 hh measures of FS?
* (2)    regress market thinness as a function of month dummies, concurrent
 * and annually lagged weather,
 * a dummy for holiday week denoting the last week in Dec and 1st week 
 * in Jan and a dummy for the first two weeks when the rains first begin
 *  that year 
 *  (i.e. = 1 the third and fourth week of November in 2012 or whatever). 
use "Malawi IHS3 files/201710_rcsi_hhinfo.dta",clear 


drop _merge
merge m:1 yearmo ipczone using "ipc_value_lag.dta",force



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

gen holiday = 1 if yearmo ==201012 | yearmo ==201101
replace holiday =0 if holiday == .
gen day1rain_dummy =1 if yearmo ==201012 | yearmo ==201011
replace day1rain_dummy =0 if day1rain_dummy == .

egen mean_L12raincytot = mean(L12rain_cytot), by (ipczone)
egen mean_L12day1rain = mean(L12day1rain), by (ipczone )
egen mean_L12maxdays = mean(L12maxdays), by (ipczone )
egen mean_lag_price = mean(Llnprice_impu), by (ipczone )
egen mean_lag_thinn = mean(Lmkt_thinn), by (ipczone )


egen mean_price = mean(lnprice_impu), by (ipczone )
egen mean_thinn = mean(mkt_thinn), by (ipczone )
  
egen mean_floor= mean(floor_dirt_sand_dung), by (ipczone )
egen mean_roof = mean(roof_natural), by (ipczone )
egen mean_cell_phone = mean(cell_phone), by (ipczone )

 gen logFCS = log(FCS) 

pca Refrigerator  Television Radio Bicycle Motorcycle Car 
rotate
predict asset_index2

label variable FCS "FCS"
label variable RCSI "RCSI"
label variable HDDS "HDDS"

label variable hhsize "HHsize"
label variable asset_index2 "Asset index"


egen clmean_L12raincytot = mean(L12rain_cytot), by (hh_a02b )
egen clmean_L12day1rain = mean(L12day1rain), by (hh_a02b  )
egen clmean_L12maxdays = mean(L12maxdays), by (hh_a02b  )
egen clmean_lag_price = mean(Llnprice_impu), by (hh_a02b  )
egen clmean_lag_thinn = mean(Lmkt_thinn), by (hh_a02b  )
  
egen clmean_floor= mean(floor_dirt_sand_dung), by (hh_a02b  )
egen clmean_roof = mean(roof_natural), by (hh_a02b  )
egen clmean_cell_phone = mean(cell_phone), by (hh_a02b  )


egen clmean_hhsize= mean(floor_dirt_sand_dung), by (hh_a02b  )
egen clmean_hhage = mean(roof_natural), by (hh_a02b  )
egen clmean_hhgender = mean(cell_phone), by (hh_a02b  )
* xi:reg mkt_thinness ipc2 L12rain_cytot  flood_max L12day1rain L12maxdays  holiday day1rain_dummy


foreach m of varlist logFCS FCS RCSI HDDS  {
	*(a)    Run the regression with ipc-level averages of the weather and price data
xi:reg `m'  ipc_lag1 mean_floor mean_roof mean_cell_phone mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn  
est store t`m'col1
predict `m'hat_1,xb

* (b) cluster model 
xi:reg `m' ipc_lag1 clmean_L12raincytot  clmean_L12day1rain clmean_L12maxdays  clmean_floor clmean_roof clmean_cell_phone clmean_lag_price clmean_lag_thinn  
est store t`m'col2_cl
predict cl`m'hat_2,xb

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays  
est store t`m'col3
predict `m'hat_3,xb
 


*(d)    Run the regression with household-level measures of all of the variables you currently include, including weather etc except assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize  floor_dirt_sand_dung roof_natural cell_phone 
est store t`m'col4
predict `m'hat_4,xb
 

*(e)    Run the regression with household-level measures of all of the variables you currently include, along with assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone  
est store t`m'col5
predict `m'hat_5,xb

 
esttab t`m'col1 t`m'col2_cl t`m'col3 t`m'col4 t`m'col5  using mktthin_t`m'_new.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 mean_floor mean_roof mean_cell_phone mkt_thinness clmean_L12raincytot  clmean_L12day1rain clmean_L12maxdays  clmean_floor clmean_roof clmean_cell_phone clmean_lag_price clmean_lag_thinn  lnprice_impu  mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn L12rain_cytot flood_maxprecip L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))
}


*(b)    Run the regression with ipc-level averages of weather, 
* price and conceptually-observable variables (such as roofing material)

foreach m of varlist logFCS FCS RCSI HDDS  {
xi:reg `m' ipc_lag1 mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_floor mean_roof mean_cell_phone mean_lag_price mean_lag_thinn i.hh_a01 
est store t`m'col2
predict `m'hat_2,xb

}

esttab tlogFCScol2  tFCScol2 tRCSIcol2 tHDDScol2 using mktthin_tcol2_new.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 mean_floor mean_roof mean_cell_phone  mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))





foreach m of varlist logFCS FCS RCSI HDDS  {
xi:reg `m' ipc_lag1 clmean_L12raincytot  clmean_L12day1rain clmean_L12maxdays  clmean_floor clmean_roof clmean_cell_phone clmean_lag_price clmean_lag_thinn  
est store t`m'col2_cl
predict cl`m'hat_2,xb
}

esttab tlogFCScol2_cl  tFCScol2_cl tRCSIcol2_cl tHDDScol2_cl using mktthin_tcol2_cl.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 clmean_L12raincytot  clmean_L12day1rain clmean_L12maxdays  clmean_floor clmean_roof clmean_cell_phone clmean_lag_price clmean_lag_thinn) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))

 
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


foreach m of varlist FCS_cat RCSI_cat HDDS_cat {
	*(a)    Run the regression with ipc-level averages of the weather and price data
xi:reg `m'  ipc_lag1  mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn  
est store t`m'col1
predict `m'hat_1,xb

*(b)    Run the regression with ipc-level averages of weather, 
* price and conceptually-observable variables (such as roofing material)
xi:reg `m' ipc_lag1 mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn 
est store t`m'col2
predict `m'hat_2,xb
 


*(c)     Run the regression with cluster-level averages of the above
xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays  
est store t`m'col3
predict `m'hat_3,xb
 


*(d)    Run the regression with household-level measures of all of the variables you currently include, including weather etc except assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu  L12rain_cytot flood_max L12day1rain L12maxdays hh_age hh_gender hhsize  floor_dirt_sand_dung roof_natural cell_phone 
est store t`m'col4
predict `m'hat_4,xb
 

*(e)    Run the regression with household-level measures of all of the variables you currently include, along with assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone 

est store t`m'col5
predict `m'hat_5,xb


*(f)    Run the regression with household-level measures of all of the variables you currently include, along with assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone 

est store t`m'col6
predict `m'hat_6,xb
 
esttab t`m'col1 t`m'col2 t`m'col3 t`m'col4 t`m'col5 t`m'col6  using mktthin_t`m'.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn Lmkt_thinn Llnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_maxprecip L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))
}


* create measures 
gen RCSI_tail = RCSI if RCSI>3
gen FCS_tail =FCS if FCS<36
gen HDDS_tail = HDD if HDD <7



foreach m of varlist FCS RCSI  HDDS RCSI_tail FCS_tail HDDS_tail {
	*(a)    Run the regression with ipc-level averages of the weather and price data
xi:reg `m'  ipc_lag1  mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn  
est store t`m'col1
predict `m'hat_1,xb

*(b)    Run the regression with ipc-level averages of weather, 
* price and conceptually-observable variables (such as roofing material)
xi:reg `m' ipc_lag1 mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn 
est store t`m'col2
predict `m'hat_2,xb

*(c)     Run the regression with cluster-level averages of the above
xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays  
est store t`m'col3
predict `m'hat_3,xb
 
*(d)    Run the regression with household-level measures of all of the variables you currently include, including weather etc except assets
xi: reg `m' ipc_lag1 mkt_thin lnprice_impu  L12rain_cytot flood_max L12day1rain L12maxdays hh_age hh_gender hhsize  floor_dirt_sand_dung roof_natural cell_phone 
est store t`m'col4
predict `m'hat_4,xb
 
*(e)    Run the regression with household-level measures of all of the variables you currently include, along with assets
xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone 
est store t`m'col5
predict `m'hat_5,xb

*(f)    Run the regression with household-level measures of all of the variables you currently include, along with assets

xi: reg `m' ipc_lag1 mkt_thin lnprice_impu L12rain_cytot flood_max L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone 

est store t`m'col6
predict `m'hat_6,xb
 
esttab t`m'col1 t`m'col2 t`m'col3 t`m'col4 t`m'col5 t`m'col6  using mktthin_t`m'.tex, replace f ///
label booktabs b(3) se(3) r2 keep (ipc_lag1 mean_L12raincytot  mean_L12day1rain mean_L12maxdays  mean_lag_price mean_lag_thinn Lmkt_thinn Llnprice_impu L12rain_cytot L12dryipc_rain L12dryipc_max L12dryipc_day1  flood_maxprecip L12day1rain L12maxdays hh_age hh_gender hhsize asset_index2 floor_dirt_sand_dung roof_natural cell_phone ) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(N N_clust r2, fmt(0 0 3) layout( "\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Clusters"' `"R^2"'))
}
*I don’t know what has happened though since people started expressing
* frustration with it. Maybe nothing, since it is so standardized now?

*Additional covariates:
*I’m not sure whether we would want to include our community fixed effects 
*for models #2-4. Probably it is worth doing both, since usually targeting is 
*first by community or district then within district / community
*Probably we also want to include month of survey, 
*since seasonality certainly matters 
*(possibly the month will be the biggest driver of need, which is interesting)
