
***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - March 15, 2018
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** 2016 obs. 2508 ***

*cd "/Users/yujunzhou/Box Sync/lsms/malawi_2016/MWI_2013_IHPS_v01_M_STATA/Household"
cd "D:\lsms\Malawi_2016\household"

*_______________________________________________________________________________

         *Q 1: FOOD CONSUMPTION SCORES (FCS)
*_______________________________________________________________________________

********************************************************************************
*NOTES ON THE WEIGHTS OF THE DIFFERENT FOOD CATEGORIES
* A Cereals, Grains and Cereal Products: Weight = 2
* B Root, Tubers and Plantains: Weight = 2
* C Nuts and Pulses: Weight = 3
* D Vegetables: Weight = 1
* E Meat, Fish and Animal Products: Weight = 4
* F Fruits => weight = 1
* G Milk/Milk Products: Weight = 4
* H Fats/Oil => Weight = 0.5
* I Sugar/Sugar Products/Honey: Weight = 0.5
* J Spices/Condiments: Weight = 0
********************************************************************************
/*gen weighted score FOR CEREALS AND ROOTS*/
*Listing of the various food categories
set more off 

use hh_mod_g2_16.dta,clear

foreach var of varlist hh_g08a - hh_g08j{
tab `var'
}
 
gen hh_staple = max(hh_g08a,hh_g08b)
 
***Computing Weighted Food Categories
* combine category a and b
gen FCS = hh_staple *2 + hh_g08c*3 + hh_g08d*1 + hh_g08e*4 +/*
*/ hh_g08f*1 + hh_g08g*4 + hh_g08h *0.5 + hh_g08i*0.5 + hh_g08j *0 

label var FCS "Food Consumption Score"


*_______________________________________________________________________________
    
	*Q2: HOUSEHOLD DIETARY DIVERSITY SCORE (HDDS)
*_______________________________________________________________________________

********************************************************************************
*A diet diversity score is a household-measure of food security that captures ///
*something about the quality of a diet. It is calculated by counting the number///
*of foods or food groups from which a household acquired food over the survey ///
*reference period (24 hours). 

*NOTES ON FOOD CATEGORIES ITEMS TO BE USED FOR THE COMPUTATION (7 CATEGORIES)
* A Cereals, Grains and Cereal Products: Weight = 2
* B Root, Tubers and Plantains: Weight = 2
* C Nuts and Pulses: Weight = 3
* D Vegetables: Weight = 1
* E Meat, Fish and Animal Products: Weight = 4
* F Fruits => weight = 1
* G Milk/Milk Products: Weight = 4
* H Fats/Oil => Weight = 0.5
********************************************************************************
* combine the cereals 
* this lines says if the household has eaten at least one day of root 
* or one day of cereasl, then it's 1. 
egen cereals =   anymatch(hh_g08a hh_g08b),value(1 2 3 4 5 6 7)
recode cereals (.=0)

* similarly, this lines says if the household has eaten at least one day of root 
foreach category in c d e f g h {
egen hdds_`category'= anymatch(hh_g08`category'),value(1 2 3 4 5 6 7)
}

*Exclude SUGAR and SPICES

gen HDDS = cereals + hdds_c + hdds_d + hdds_e + hdds_f + hdds_g 
tab HDDS 

keep y3_hhid HDDS FCS 
sort y3_hhid
save malawi_2016.dta, replace
 

*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

set more off 
use "hh_mod_h_16",clear

//Questions relating to COPING STRATEGIES (Full labels from Survey REPORT)
*In the past 7 days, did you worry that your household would not have enough food
*hh_h02a: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Rely on less preferred and/or less expensive foods?"(WGT1)
*hh_h02b: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Limit portion size at mealtimes?" (WGT1)
*hh_h02c: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Reduce number of meals eaten in a day?" (WGT2)
* hh_h02d "In the past 7 days, how many days have you or someone in your ///
	*household had to: Restrict consumption by adults in order for small ///
	*children to eat?" (WGT2)
*hh_h02e "In the past 7 days, how many days have you or someone in your ///
	*household had to: Borrow food, or rely on help from a friend or ///
	*relative?" (WGT2)
tab hh_h01


**Constructing rCSI
tab hh_h02a
gen rCSI=1*hh_h02a + 1*hh_h02b + 2*hh_h02c + 2*hh_h02d +2*hh_h02e

label var rCSI "HH Reduced Coping Strategies Index"

/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort rCSI
sum rCSI
graph box rCSI

gen id=_n
label var id "id"
line rCSI id
plot rCSI id 
*/

***Merging the data
keep y3_hhid  rCSI 
merge m:m y3_hhid using malawi_2016
drop _merge
save malawi_2016, replace

*________

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "hh_mod_a_filt_16",clear

 
keep y3_hhid reside  ea_id  ta_code district  region consumption_date  hhsize 
 
 split consumption_date ,parse("-") gen(time) destring
rename time1 FS_year
rename time2 FS_month

destring FS_year FS_month ,force replace 
label variable FS_month "Month FS module was administered"
label variable FS_year "Year FS module was administered"

tostring ta_code ,gen(TA_names)
 rename district hh_a01 
 
drop ta_code 
drop consumption_date time3  

gen survey_round ="Malawi IHPS 2016"
 

*Merge the Datafile
merge m:m y3_hhid using malawi_2016
drop _merge
save malawi_2016, replace


* Merge in geolocation 
use "HouseholdGeovariablesIHPSY3.dta",clear

* use "/Users/yujunzhou/Box Sync/lsms/malawi_2016/MWI_2013_IHPS_v01_M_STATA/Geovariables/HouseholdGeovariables_IHPS",clear
keep y3_hhid lat_modified  lon_modified dist_road dist_popcenter dist_admarc /*
*/fsrad3_agpct srtm_1k srtm_mwi_5_15 sq1 sq2    
 
rename srtm_1k elevation
rename srtm_mwi_5_15 terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename fsrad3_agpct ag_percent


merge m:m y3_hhid using malawi_2016
drop _merge
save malawi_2016, replace

 

* Merge in cell phone  
use "hh_mod_f_16",clear
keep y3_hhid  hh_f34 hh_f08  hh_f09 
tab hh_f08,gen(roof)
tab hh_f09,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 | floor2 ==2
recode floor_dirt_sand_dung (. =0)
rename floor3 floor_cement
rename floor5 floor_tile
  

rename roof1 roof_natural   
rename roof2 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)


tab hh_f34
gen cell_phone = 1 if hh_f34!=0
replace cell_phone = 0 if hh_f34==0
rename hh_f34  number_celphones

keep y3_hhid number_celphones cell_phone roof_other roof_natural roof_iron floor_dirt_sand_dung floor_cement  floor_tile
merge m:m y3_hhid using malawi_2016
drop if _merge ==2
drop _merge
save malawi_2016, replace



** merge in the assets
use "hh_mod_l_16",clear

tab hh_l02 
tab hh_l02  ,nolabel
keep if hh_l02  ==518 | hh_l02  ==516 | hh_l02  ==517 |hh_l02  ==514 |hh_l02  ==507 |hh_l02  ==509


egen Refrigerator = sum(hh_l03 ) if hh_l02  ==514,by(y3_hhid)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_l03 ) if hh_l02  ==507,by(y3_hhid)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_l03 ) if hh_l02  ==509,by(y3_hhid)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_l03 ) if hh_l02  ==516,by(y3_hhid)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_l03 ) if hh_l02  ==517,by(y3_hhid)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_l03 ) if hh_l02  ==518,by(y3_hhid)
replace Car =1 if Car !=0 & Car !=.

egen r1 = sum(Refrigerator),by(y3_hhid)
egen r2 = sum(Radio),by(y3_hhid)
egen T = sum(Television),by(y3_hhid)
egen B = sum(Bicycle),by(y3_hhid)
egen M = sum(Motorcycle),by(y3_hhid)
egen C = sum(Car),by(y3_hhid)


duplicates drop y3_hhid,force

keep y3_hhid r1 r2 T B M C

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car

merge m:m y3_hhid using malawi_2016
drop if _merge ==2
drop _merge

drop if FCS ==0 
drop if HDDS ==0

rename y3_hhid case_id 
save "D:\lsms\cleaned_dataset\FCS_2016_Malawi.dta", replace



*save "/Users/yujunzhou/Box Sync/lsms/FCS_2016_Malawi.dta",replace

*_______________________________________________________________________

           *Q4. MONTHES OF ADEQUATE HOUSEHOLD FOOD PROVISIONING (MAHFP)
*_______________________________________________________________________________

********************************************************************************
*NOTES:
//Use the rest of the questions in Module H to calculate MAHFP. Calculate the //
//MAHFP as twelve months minus the total number of months out of the previous //
//twelve months that the household was unable to meet their food needs. If the///
//household responded no to hh_h04, then all of the hh_h05* variables should ///
//be coded as 0s. Your MAHFP score should be between 12 and 0. 
********************************************************************************
/*
clear
set more off 
use "hh_mod_h_16"

*Past 12 Months Experienced Food Shortage
tab  hh_h04
*Describing the data
foreach var of varlist hh_h05a-hh_h05s{
	tab `var'
}


*Recoding values of the response variables
foreach var of varlist hh_h05a-hh_h05s {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hh_h04==1
	tab `var'_num
}



*Summing the number of response across y3_hhids
egen H_Count=rowtotal (hh_h05a_num-hh_h05s_num)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

*Drop irrelevant variables
keep y3_hhid qx_type interview_status MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data
merge m:m y3_hhid using malawi_2016
drop _merge
save malawi_2016, replace

*/
