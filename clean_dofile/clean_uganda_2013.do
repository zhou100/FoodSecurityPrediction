***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - March 20, 2018
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** 2013/14   obs. 3119 ***

* cd "/Users/yujunzhou/Box Sync/lsms/Uganda_2013/UGA_2013_UNPS_v01_M_STATA8"

cd "C:\Users\Administrator\Desktop\lsms/Uganda_2013/UGA_2013_UNPS_v01_M_STATA8"


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

use GSEC15B.dta,clear
des 
tab itmcd if h15bq2c == 12

drop if h15bq2c == 11 | h15bq2c == 12

*drop spices 
drop if itmcd ==150

* combine starches and cereals
replace h15bq2c =1 if h15bq2c ==2
* combine nuts and pulses
replace h15bq2c =5 if h15bq2c ==4

* collapse by food group 
collapse (max)h15bq3b , by(h15bq2c HHID )

numlabel h15bq2c, add
tab  h15bq2c  
 
tab h15bq3b
label var h15bq3b "# Days specific food is eaten"

***Specifying Weights Different Food Categories

*J Spices/Condiments: Weight = 0
gen FWeight = 0
* A Cereals, Grains and Cereal Products: Weight = 2
replace FWeight=2 if h15bq2c==1
* C Nuts and Pulses: Weight = 3
replace FWeight=3 if h15bq2c==5
* D Vegetables: Weight = 1
* F Fruits => weight = 1
replace FWeight=1 if h15bq2c==6 | h15bq2c == 7
* E Meat, Fish and Animal Products: Weight = 4 
* G Milk/Milk Products: Weight = 4
replace FWeight=4 if h15bq2c==8| h15bq2c == 9
* H Fats/Oil => Weight = 0.5
* I Sugar/Sugar Products/Honey: Weight = 0.5
replace FWeight=0.5 if h15bq2c==10 |h15bq2c == 3
label var FWeight "Food weight"

***Computing Weighted Food Categories
gen FCS = h15bq3b*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(HHID)
label var FCS "HH food consumption score"
 

sort HHID
save uganda_2013.dta, replace

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

use GSEC15B.dta,clear
des 
tab itmcd if h15bq2c == 12


drop if h15bq2c == 11 | h15bq2c == 12

*drop spices 
drop if itmcd ==150

* combine starches and cereals
replace h15bq2c =1 if h15bq2c ==2
* combine nuts and pulses
replace h15bq2c =5 if h15bq2c ==4

* collapse by food group 
collapse (max)h15bq3b , by(h15bq2c HHID )

numlabel h15bq2c, add
tab  h15bq2c  
 
tab h15bq3b
label var h15bq3b "# Days specific food is eaten"

*Exclude SUGAR and SPICES
drop if h15bq2c==3 

*Generating Food consumed by hhs over the 24 hours (7 days in this dataset)
gen HDDS=0
replace HDDS=1 if h15bq3b>=1 & h15bq3b!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(HHID)
label var HDDS "Household Dietary Diversity Score"
 
 
tab HDDS 
/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort HDDS
sum HDDS
graph box HDDS, over(HDDS_Thresh) */

***Merging FCS and HDDS
merge m:m HHID using uganda_2013
drop _merge
sort HHID 
save uganda_2013, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "GSEC1",clear

 
keep HHID urban ea   year month  h1aq1a h1aq3a   region 
 
rename h1aq3a hh_a02 
rename h1aq1a hh_a01 
  
rename year FS_year 
rename month FS_month




label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"

 gen survey_round ="Uganda NPS 2013/2014"
 
*Merge the Datafile
merge m:m HHID using uganda_2013
drop _merge
save uganda_2013, replace


 /*
 * Merge in other Geovariables
use TZY2.HH.Geovariables.dta,clear
keep y4_hhid  hh_envi15   hh_geo06 hh_soil_con01 srtm_eaf_5_15  hh_soil_con02 afmnslp_pct 

rename hh_geo06  elevation
rename srtm_eaf_5_15 terrain_rough
rename hh_soil_con01 nutri_avail
rename hh_soil_con02 nutri_rentention
rename afmnslp_pct slope
rename hh_envi15 ag_percent
rename hh_geo01  dist_road
rename hh_geo02  dist_popcenter
rename hh_geo05  dist_headquater

merge m:m y4_hhid using tanzania_2014
drop _merge
save tanzania_2014, replace
*/ 
 
 
rename HHID case_id 
 rename urban reside 

 drop if HDDS ==0 
  drop if FCS ==0 


save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_2013_Uganda.dta", replace
 
 

* No rCSI related questions for uganda 


/*


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
keep HHID qx_type interview_status rCSI 
merge m:m HHID using uganda_2013
drop _merge
save uganda_2013, replace

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



*Summing the number of response across case_ids
egen H_Count=rowtotal (hh_h05a_num-hh_h05s_num)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

*Drop irrelevant variables
keep HHID qx_type interview_status MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data
merge m:m HHID using uganda_2013
drop _merge
save uganda_2013, replace

*/
