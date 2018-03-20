***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2018
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** Tan 2008 Baseline subsample obs. 3246 ***

*cd "/Users/yujunzhou/Box Sync/lsms/Tanzania_2008/TZNPS1HHDTA_E"
cd "C:\Users\Administrator\Desktop\lsms\Tanzania_2008/TZNPS1HHDTA_E"

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


*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

set more off 
use SEC_K1.dta,clear
*use SEC_H1_J_K2_O2_P1_Q1_S1.dta,clear

//Questions relating to COPING STRATEGIES (Full labels from Survey REPORT)

	*hh_s7q02_a: "In the past 7 days, how many days have you or someone in your ///
		*household had to: Rely on less preferred and/or less expensive foods?"(WGT1)


	*hh_s7q02_b: "In the past 7 days, how many days have you or someone in your ///
		*household had to: limit the VARIETY of foods eaten ?" (WGT1)


	*hh_s7q02_c: "In the past 7 days, how many days have you or someone in your ///
		*household had to: Limit portion size at mealtimes?" (WGT1)

	*hh_s7q02_d: "In the past 7 days, how many days have you or someone in your ///
		*household had to: Reduce number of meals eaten in a day?" (WGT2)

	* hh_s7q02_e "In the past 7 days, how many days have you or someone in your ///
		*household had to: Restrict consumption by adults in order for small ///
		*children to eat?" (WGT2)

	*hh_s7q02_f "In the past 7 days, how many days have you or someone in your ///
		*household had to: Borrow food, or rely on help from a friend or ///
		*relative?" (WGT2)


	*hh_s7q02_g "In the past 7 days, how many days have you or someone in your ///
		*household had to: have no food of any kind in your household?" (WGT4)

	*hh_s7q02_h "In the past 7 days, how many days have you or someone in your ///
		*household had to: go a whole day and night without eating anything?" (WGT4)


**Constructing rCSI
tab hh_i01 
gen rCSI=1*hh_i02_1 + 1*hh_i02_2 + 1*hh_i02_3 + 2*hh_i02_4 +2*hh_i02_5 + 2*hh_i02_6 +4*hh_i02_7 +4*hh_i02_8
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
keep hhid rCSI 
merge m:m hhid using tanzania_2008
drop _merge
save tanzania_2008, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "HH_SEC_A",clear

 
keep hhid y2_rural region y2_weight district  hh_a18_month  hh_a18_year
 

rename hh_a18_month FS_month
rename hh_a18_year FS_year

gen survey_round ="Tanzania NPS 2010/2011"


tab FS_month if FS_year == 2010
tab FS_month if FS_year == 2011



label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"


rename district hh_a01 
label variable hh_a01 "Woreda/District Code"
rename y2_weight hh_wgt  
rename y2_rural reside 

merge m:m hhid using tanzania_2008
drop if _merge ==2
drop _merge
save tanzania_2008, replace

 

* Merge in geolocation 
 use TZY2.EA.Offsets.dta,clear

 keep clusterid lat_modified lon_modified

/*
merge m:m y3_hhid using tanzania_2008
drop _merge
save tanzania_2008, replace


* Merge in cell phone  
use "HH_SEC_M",clear
keep hhid itemcode hh_m01 
keep if itemcode == 403
gen cell_phone = 1 if hh_m01!=0
replace cell_phone = 0 if hh_m01==0
rename hh_m01  number_celphones


merge m:m y3_hhid using tanzania_2008
drop if _merge ==2
drop _merge
*/

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
use HH_SEC_I1.dta,clear

*Past 12 Months Experienced Food Shortage
tab  hh_i08


use HH_SEC_I2.dta,clear

*Describing the data
foreach var of varlist hh_i09_1_01-hh_i09_3_12{
	tab `var'
}


*Recoding values of the response variables
foreach var of varlist hh_i09_1_01-hh_i09_3_12 {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" 
	tab `var'_num
}

keep hh_i09_1_01_num-hh_i09_3_12_num hhid 


merge m:m hhid using tanzania_2008
drop _merge
save tanzania_2008, replace


* for 2010 month, it's 10 - 12
* for 2011 month, it's 1 to 11 

*Summing the number of response across case_ids
egen H_Count_1 =rowtotal (hh_i09_1_10_num-hh_i09_2_09_num) if FS_year ==2010
egen H_Count_2 =rowtotal (hh_i09_2_01_num-hh_i09_3_08_num) if FS_year ==2011

egen H_Count = rowtotal (H_Count_1 H_Count_2)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

keep hhid  MAHFP

merge m:m hhid using tanzania_2008
drop _merge
save tanzania_2008, replace

use tanzania_2008,clear
drop hh_i09_1_01_num-hh_i09_3_12_num

save tanzania_2008, replace


***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data


save "/Users/yujunzhou/Box Sync/lsms/FCS_2010_Tanzania.dta",replace



