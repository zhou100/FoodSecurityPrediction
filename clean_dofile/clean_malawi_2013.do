***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2018
** update March 13,2018
* recheck the measures and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** 2013 Baseline subsample obs. 3246 ***

*cd "/Users/yujunzhou/Box Sync/lsms/Malawi_2013/MWI_2013_IHPS_v01_M_STATA/Household"
cd "C:\Users\Administrator\Desktop\lsms\Malawi_2013\MWI_2013_IHPS_v01_M_STATA/Household/"




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

use HH_MOD_G2.dta,clear

*list hh_g08a  hh_g08c
*tab hh_g08a,nolabel
tab hh_g08a


*Recoding of outliers (change 8 days to 7 days)
replace hh_g08c=7 if hh_g08c==8
*Combining Cereals and roots (Category A and Category B)
replace hh_g08a=1 if hh_g08a==1 | hh_g08a==2
*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if hh_g08a=="AB"
collapse (max)hh_g08c, by(y2_hhid      hh_g08a )
label var hh_g08c "# Days specific food is eaten"

***Specifying Weights Different Food Categories
gen FWeight = 0
replace FWeight=2 if hh_g08a==1
replace FWeight=3 if hh_g08a==3
replace FWeight=1 if hh_g08a==4 | hh_g08a==6
replace FWeight=4 if hh_g08a==5 | hh_g08a==4
replace FWeight=0.5 if hh_g08a==8 | hh_g08a==9
label var FWeight "Food weight"

 ***Computing Weighted Food Categories
gen FCS = hh_g08c*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(y2_hhid)
label var FCS "HH food consumption score"


sort y2_hhid
save malawi_2013.dta, replace

/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort FCS
sum FCS
graph bar (mean) FCS, over(FCS_Thresh) 
graph box FCS, over(FCS_Thresh)  */ 

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
set more off 
use "HH_MOD_G2",clear

*Recoding of outliers (change 8 days to 7 days)
replace hh_g08c=7 if hh_g08c==8
*Combining Cereals and roots (Category A and Category B)
replace hh_g08a=1 if hh_g08a==1 | hh_g08a==2
*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if hh_g08a=="AB"
collapse (max)hh_g08c, by(y2_hhid   hh_g08a )
label var hh_g08c "# Days specific food is eaten"

*Exclude SUGAR and SPICES
drop if hh_g08a==9 | hh_g08a==10

*Generating Food consumed by hhs over the 24 hours
gen HDDS=0
replace HDDS=1 if hh_g08c>=1 & hh_g08c!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(y2_hhid)
label var HDDS "Household Dietary Diversity Score"

 

/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort HDDS
sum HDDS
graph box HDDS, over(HDDS_Thresh) */

***Merging FCS and HDDS
merge m:m y2_hhid using malawi_2013
drop _merge
sort y2_hhid 
save malawi_2013, replace

*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

set more off 
use "HH_MOD_H",clear

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
keep y2_hhid rCSI 
merge m:m y2_hhid using malawi_2013
drop _merge
save malawi_2013, replace

*_______________________________________________________________________________

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

clear
set more off 
use "HH_MOD_H"

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



*Summing the number of response across y2_hhids
egen H_Count=rowtotal (hh_h05a_num-hh_h05s_num)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

*Drop irrelevant variables
keep y2_hhid  MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data
merge m:m y2_hhid using malawi_2013
drop _merge
save malawi_2013, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "HH_MOD_A_FILT",clear

 
keep y2_hhid reside ea_id  hh_a10b district   region 
 
rename hh_a10b TA_names 
rename district hh_a01 
 
 
*Merge the Datafile
merge m:m y2_hhid using malawi_2013
drop _merge
save malawi_2013, replace


* Merge in geolocation 
 use "C:\Users\Administrator\Desktop\lsms/Malawi_2013/MWI_2013_IHPS_v01_M_STATA/Geovariables/HouseholdGeovariables_IHPS",clear

* use "/Users/yujunzhou/Box Sync/lsms/Malawi_2013/MWI_2013_IHPS_v01_M_STATA/Geovariables/HouseholdGeovariables_IHPS",clear
keep y2_hhid LAT_DD_MOD  LON_DD_MOD dist_road dist_popcenter dist_admarc /*
*/fsrad3_agpct srtm_1k srtm_mwi_5_15 sq1 sq2    

rename  LAT_DD_MOD lat_modified
rename LON_DD_MOD lon_modified 
rename srtm_1k elevation
rename srtm_mwi_5_15 terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename fsrad3_agpct ag_percent


merge m:m y2_hhid using malawi_2013
drop _merge
save malawi_2013, replace

* Merge in year and round 
use  HH_MOD_A_FILT.dta,clear
keep y2_hhid hh_a23a_2 hh_a23a_3 hhsize

rename hh_a23a_3 FS_year
rename hh_a23a_2 FS_month
label variable FS_month "Month FS module was administered"
label variable FS_year "Year FS module was administered"
gen survey_round ="Malawi IHPS 2013"


merge m:m y2_hhid using malawi_2013
drop if _merge ==2
drop _merge
save malawi_2013, replace



* Merge in cell phone  
use "HH_MOD_F",clear
keep y2_hhid  hh_f34 hh_f08  hh_f09 
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

keep y2_hhid number_celphones cell_phone roof_other roof_natural roof_iron floor_dirt_sand_dung floor_cement  floor_tile

merge m:m y2_hhid using malawi_2013
drop if _merge ==2
drop _merge
save malawi_2013, replace


** merge in the assets
use "HH_MOD_L.dta",clear
tab hh_l02 
tab hh_l02  ,nolabel
keep if hh_l02  ==518 | hh_l02  ==516 | hh_l02  ==517 |hh_l02  ==514 |hh_l02  ==507 |hh_l02  ==509


egen Refrigerator = sum(hh_l03 ) if hh_l02  ==514,by(y2_hhid)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_l03 ) if hh_l02  ==507,by(y2_hhid)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_l03 ) if hh_l02  ==509,by(y2_hhid)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_l03 ) if hh_l02  ==516,by(y2_hhid)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_l03 ) if hh_l02  ==517,by(y2_hhid)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_l03 ) if hh_l02  ==518,by(y2_hhid)
replace Car =1 if Car !=0 & Car !=.

egen r1 = sum(Refrigerator),by(y2_hhid)
egen r2 = sum(Radio),by(y2_hhid)
egen T = sum(Television),by(y2_hhid)
egen B = sum(Bicycle),by(y2_hhid)
egen M = sum(Motorcycle),by(y2_hhid)
egen C = sum(Car),by(y2_hhid)


duplicates drop y2_hhid,force

keep y2_hhid r1 r2 T B M C

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car

merge m:m y2_hhid using malawi_2013
drop if _merge ==2
drop _merge

drop if FCS ==0 
drop if HDDS ==0

rename y2_hhid case_id 
save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_2013_Malawi.dta", replace



*save "/Users/yujunzhou/Box Sync/lsms/FCS_2013_Malawi.dta",replace



