***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2017
** update March 18,2018
* recheck the measures and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** Tanzania 2012/13 Baseline subsample obs. 5010 ***

* cd "/Users/yujunzhou/Box Sync/lsms/Tanzania_2012/TZA_2012_LSMS_v01_M_STATA_English_labels"
cd "D:\lsms\Tanzania_2012\TZA_2012_LSMS_v01_M_STATA_English_labels"

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

use HH_SEC_J3.dta,clear

tab hh_j09_3 


*Recoding of outliers (if any)


numlabel  HH_J09_1 ,add
tab itemcode
 
 
*Combining Cereals and roots (Category A and Category G)


replace itemcode=1 if itemcode==2
 

*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if itemcode=="AB"
collapse (max) hh_j09_3, by(y3_hhid itemcode)
label var hh_j09_3 "# Days specific food is eaten"

***Specifying Weights Different Food Categories
 
*J Spices/Condiments: Weight = 0
gen FWeight = 0
* A Cereals, Grains and Cereal Products: Weight = 2
replace FWeight=2 if itemcode==1
* C Nuts and Pulses: Weight = 3
replace FWeight=3 if itemcode==3
* D Vegetables: Weight = 1
* F Fruits => weight = 1
replace FWeight=1 if itemcode==4 | itemcode == 6
* E Meat, Fish and Animal Products: Weight = 4 
* G Milk/Milk Products: Weight = 4
replace FWeight=4 if itemcode==5| itemcode == 7
* H Fats/Oil => Weight = 0.5
* I Sugar/Sugar Products/Honey: Weight = 0.5
replace FWeight=0.5 if itemcode==8 |itemcode == 9




 ***Computing Weighted Food Categories
gen FCS = hh_j09_3*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(y3_hhid  )
label var FCS "HH food consumption score"


sort y3_hhid  
save tanzania_2012.dta, replace


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

use HH_SEC_J3.dta,clear

 

*Recoding of outliers (change 9 days to 7 days)
 
*Combining Cereals and roots (Category A and Category G)
replace itemcode=1 if itemcode==2
 

numlabel  HH_J09_1 ,add
tab itemcode

*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if itemcode=="AB"
collapse (max)hh_j09_3 , by(y3_hhid   itemcode    )
label var hh_j09_3 "# Days specific food is eaten"

 

*Exclude SUGAR and SPICES
drop if itemcode==9 | itemcode==10

*Generating Food consumed by hhs over the 24 hours
gen HDDS=0
replace HDDS=1 if hh_j09_3>=1 & hh_j09_3!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(y3_hhid  )
label var HDDS "Household Dietary Diversity Score"


merge m:m y3_hhid   using tanzania_2012
drop if _merge ==1
drop _merge
save tanzania_2012, replace


*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

set more off 
use HH_SEC_H.dta,clear

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
tab hh_h02_1 
gen rCSI=1*hh_h02_1 + 1*hh_h02_2 + 1*hh_h02_3 + 2*hh_h02_4 +2*hh_h02_5 + 2*hh_h02_6 +4*hh_h02_7 +4*hh_h02_8
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
merge m:m y3_hhid using tanzania_2012
drop _merge
save tanzania_2012, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "HH_SEC_A",clear
des
 
keep y3_hhid y3_rural clusterid  hh_a02_2 hh_a03_2  hh_a01_2  hh_a18_2  hh_a18_3 
 

rename hh_a18_3 FS_year
rename hh_a18_2 FS_month
rename clusterid ea_id
rename hh_a03_2 ward
gen survey_round ="Tanzania NPS 2012/2013"

label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"


rename hh_a02_2 hh_a01 
label variable hh_a01 "Woreda/District Code"
 
rename hh_a01_2 region 
rename y3_rural reside 

merge m:m y3_hhid using tanzania_2012
drop if _merge ==2
drop _merge
save tanzania_2012, replace

 

* Merge in geolocation 
set more off 
use HouseholdGeovars_Y3.dta,clear
des 
 
 keep y3_hhid lat_dd_mod  lon_dd_mod dist01-dist04 land02 soil01 soil02 /*
*/ soil04-soil06 

rename  lat_dd_mod lat_modified
rename lon_dd_mod lon_modified 

rename soil01  elevation
rename soil04 terrain_rough
rename soil05 nutri_avail
rename soil06 nutri_rentention
rename soil02 slope
rename land02 ag_percent
rename dist01  dist_road
rename dist02  dist_popcenter
rename dist03  dist_agmkt
rename dist04  dist_headquater


merge m:m y3_hhid using tanzania_2012
drop _merge
save tanzania_2012, replace


* Merge in cell phone 
use "HH_SEC_M",clear
keep y3_hhid itemcode hh_m01 
 
numlabel HH_M00,add

tab itemcode
keep if itemcode  ==401 | itemcode  ==403 | itemcode  ==404 | /*
*/itemcode  ==406|itemcode  ==426 |itemcode  ==427 |itemcode  ==425 


egen cellphone = sum(hh_m01 ) if itemcode  ==403,by(y3_hhid)
replace cellphone =1 if cellphone !=0 & cellphone !=.


egen number_celphones = sum(hh_m01 ) if itemcode  ==403,by(y3_hhid)


egen Refrigerator = sum(hh_m01 ) if itemcode  ==404,by(y3_hhid)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_m01 ) if itemcode  ==401,by(y3_hhid)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_m01 ) if itemcode  ==406,by(y3_hhid)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_m01 ) if itemcode  ==427,by(y3_hhid)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_m01 ) if itemcode  ==426,by(y3_hhid)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_m01 ) if itemcode  ==425,by(y3_hhid)
replace Car =1 if Car !=0 & Car !=.



egen r1 = sum(Refrigerator),by(y3_hhid)
egen r2 = sum(Radio),by(y3_hhid)
egen T = sum(Television),by(y3_hhid)
egen B = sum(Bicycle),by(y3_hhid)
egen M = sum(Motorcycle),by(y3_hhid)
egen C = sum(Car),by(y3_hhid)
egen cell = sum(cellphone),by(y3_hhid)
egen cell_num = sum(number_celphones),by(y3_hhid)



duplicates drop y3_hhid,force

keep y3_hhid r1 r2 T B M C cell cell_num

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcyclet
rename C Car
rename cell cellphone
rename cell_num number_celphones

merge m:m y3_hhid using tanzania_2012
drop if _merge ==2
drop _merge




 * Merge in housing

 use "HH_SEC_I",clear

** floor ***

tab hh_i10,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 
recode floor_dirt_sand_dung (. =0)
rename floor2 floor_cement
  
** roof ***
tab hh_i09 ,gen(roof)
gen roof_natural =1 if roof1==1  | roof2==1 
recode roof_natural (. =0)

rename roof4 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)

 
keep y3_hhid roof_natural roof_iron roof_other floor_cement floor_dirt


merge m:m y3_hhid using tanzania_2012
drop _merge
save tanzania_2012, replace


rename y3_hhid case_id 

 
 drop if HDDS ==0
 drop if FCS ==0
 
 *save "/Users/yujunzhou/Box Sync/lsms/FCS_2010_Tanzania.dta",replace

save "D:\lsms\cleaned_dataset\FCS_2012_Tanzania.dta", replace

 
/*

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
use HH_SEC_H.dta,clear

*Past 12 Months Experienced Food Shortage
tab  hh_h08
*Describing the data
foreach var of varlist hh_h09_1-hh_h09_36{
	tab `var'
}


*Recoding values of the response variables
foreach var of varlist hh_h09_10-hh_h09_36 {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hh_h08==1
	tab `var'_num
}

keep hh_h09_10_num-hh_h09_36_num y3_hhid 


merge m:m y3_hhid using tanzania_2012
drop _merge
save tanzania_2012, replace



*Summing the number of response across case_ids
egen H_Count_1 =rowtotal (hh_h09_10_num-hh_h09_22_num) if FS_year ==2012
egen H_Count_2 =rowtotal (hh_h09_13_num-hh_h09_36_num) if FS_year ==2013

egen H_Count = rowtotal (H_Count_1 H_Count_2)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

keep y3_hhid  MAHFP

merge m:m y3_hhid using tanzania_2012
drop _merge
save tanzania_2012, replace

use tanzania_2012,clear
drop hh_h09_10_num-hh_h09_36_num
drop hh_h09_1_num-hh_h09_9_num

save tanzania_2012, replace


***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data


*/
