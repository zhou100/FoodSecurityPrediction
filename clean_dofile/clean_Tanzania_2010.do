***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2018
** update March 18,2018
* recheck the measures and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** Tanzania 2010/11 obs. 3924***

*cd "/Users/yujunzhou/Box Sync/lsms/Tanzania_2010/TZA_2010_NPS2_v01_M_STATA"
cd "C:\Users\Administrator\Desktop\lsms\Tanzania_2010\TZA_2010_NPS2_v01_M_STATA"

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


use HH_SEC_K2.dta,clear

tab hh_k08_3


*Recoding of outliers (change 9 days to 7 days)
replace hh_k08_3=7 if hh_k08_3==9
tab hh_k08_2  
tab itemcode
 



*Combining Cereals and roots (Category A and Category G)
replace itemcode="A" if itemcode=="G" 
* generate a numeric version of the item_code to use the collapse functtion
encode  itemcode, gen(item_code)

*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if itemcode=="AB"
collapse (max)item_code, by(y2_hhid hh_k08_3 hh_k08_2 itemcode  )
label var hh_k08_3 "# Days specific food is eaten"

***Specifying Weights Different Food Categories
gen FWeight = 0
replace FWeight=2 if itemcode=="A"
replace FWeight=3 if itemcode=="E"
replace FWeight=1 if itemcode=="J" | itemcode=="C"
replace FWeight=4 if itemcode=="D" | itemcode=="E"
replace FWeight=0.5 if itemcode=="I" | itemcode=="B"
label var FWeight "Food weight"

 ***Computing Weighted Food Categories
gen FCS = hh_k08_3*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(y2_hhid)
label var FCS "HH food consumption score"


sort y2_hhid
save tanzania_2010.dta, replace


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

use HH_SEC_K2.dta,clear

 

*Recoding of outliers (change 9 days to 7 days)
replace hh_k08_3=7 if hh_k08_3==9
tab hh_k08_2  
tab itemcode
 

*Combining Cereals and roots (Category A and Category G)
replace itemcode="A" if itemcode=="G" 
* generate a numeric version of the item_code to use the collapse functtion
encode  itemcode, gen(item_code)

*replace hh_g08b="Main Staples; cereals and grains, roots and tubers" if itemcode=="AB"
collapse (max)item_code, by(y2_hhid hh_k08_3 hh_k08_2 itemcode  )


label var hh_k08_3 "# Days specific food is eaten"
 

*Exclude SUGAR and SPICES
drop if itemcode=="H" | itemcode=="I"

*Generating Food consumed by hhs over the 24 hours
gen HDDS=0
replace HDDS=1 if hh_k08_3>=1 & hh_k08_3!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(y2_hhid)
label var HDDS "Household Dietary Diversity Score"


merge m:m y2_hhid using tanzania_2010
drop if _merge ==1
drop _merge
save tanzania_2010, replace

 

*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

set more off 
use HH_SEC_I1.dta,clear

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
keep y2_hhid rCSI 
merge m:m y2_hhid using tanzania_2010
drop _merge
save tanzania_2010, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "HH_SEC_A",clear

 
keep y2_hhid clusterid  y2_rural region district ea ward  hh_a18_month  hh_a18_year
 

rename hh_a18_month FS_month
rename hh_a18_year FS_year

gen survey_round ="Tanzania NPS 2010/2011"


tab FS_month if FS_year == 2010
tab FS_month if FS_year == 2011



label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"


rename district hh_a01 
label variable hh_a01 "Woreda/District Code"
rename y2_rural reside 

merge m:m y2_hhid using tanzania_2010
drop if _merge ==2
drop _merge
save tanzania_2010, replace

 

* Merge in geolocation 
use TZY2.EA.Offsets.dta,clear

keep clusterid lat_modified lon_modified
 
merge m:m clusterid using tanzania_2010
drop if _merge ==2
drop _merge
save tanzania_2010, replace
 
 * Merge in other Geovariables
use TZY2.HH.Geovariables.dta,clear
keep y2_hhid  hh_envi15  hh_geo01  hh_geo02  hh_geo05      hh_geo06 hh_soil_con01 srtm_eaf_5_15  hh_soil_con02 afmnslp_pct 

rename hh_geo06  elevation
rename srtm_eaf_5_15 terrain_rough
rename hh_soil_con01 nutri_avail
rename hh_soil_con02 nutri_rentention
rename afmnslp_pct slope
rename hh_envi15 ag_percent
rename hh_geo01  dist_road
rename hh_geo02  dist_popcenter
rename hh_geo05  dist_headquater


merge m:m y2_hhid using tanzania_2010
drop _merge
save tanzania_2010, replace


* Merge in cell phone  and other assets 
set more off
use "HH_SEC_N",clear
keep y2_hhid itemcode hh_n01_2 
numlabel HH_N00,add

tab itemcode
keep if itemcode  ==401 | itemcode  ==403 | itemcode  ==404 | /*
*/itemcode  ==406|itemcode  ==426 |itemcode  ==427 |itemcode  ==425 


egen cellphone = sum(hh_n01_2 ) if itemcode  ==403,by(y2_hhid)
replace cellphone =1 if cellphone !=0 & cellphone !=.


egen number_celphones = sum(hh_n01_2 ) if itemcode  ==403,by(y2_hhid)


egen Refrigerator = sum(hh_n01_2 ) if itemcode  ==404,by(y2_hhid)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_n01_2 ) if itemcode  ==401,by(y2_hhid)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_n01_2 ) if itemcode  ==406,by(y2_hhid)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_n01_2 ) if itemcode  ==427,by(y2_hhid)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_n01_2 ) if itemcode  ==426,by(y2_hhid)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_n01_2 ) if itemcode  ==425,by(y2_hhid)
replace Car =1 if Car !=0 & Car !=.



egen r1 = sum(Refrigerator),by(y2_hhid)
egen r2 = sum(Radio),by(y2_hhid)
egen T = sum(Television),by(y2_hhid)
egen B = sum(Bicycle),by(y2_hhid)
egen M = sum(Motorcycle),by(y2_hhid)
egen C = sum(Car),by(y2_hhid)
egen cell = sum(cellphone),by(y2_hhid)
egen cell_num = sum(number_celphones),by(y2_hhid)



duplicates drop y2_hhid,force

keep y2_hhid r1 r2 T B M C cell cell_num

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car
rename cell cellphone
rename cell_num number_celphones

merge m:m y2_hhid using tanzania_2010
drop if _merge ==2
drop _merge
 
 * Merge in housing

 use "HH_SEC_J1",clear

** floor ***

tab hh_j07,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 
recode floor_dirt_sand_dung (. =0)
rename floor2 floor_cement
  
** roof ***
tab hh_j06,gen(roof)
gen roof_natural =1 if roof1==1  | roof2==1 
rename roof4 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)

 
keep y2_hhid roof_natural roof_iron roof_other floor_cement floor_dirt


merge m:m y2_hhid using tanzania_2010
drop _merge
save tanzania_2010, replace


rename y2_hhid case_id 
rename clusterid ea_id
rename ea town 
 
 
 *save "/Users/yujunzhou/Box Sync/lsms/FCS_2010_Tanzania.dta",replace

save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_2010_Tanzania.dta", replace



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

keep hh_i09_1_01_num-hh_i09_3_12_num y2_hhid 


merge m:m y2_hhid using tanzania_2010
drop _merge
save tanzania_2010, replace


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

keep y2_hhid  MAHFP

merge m:m y2_hhid using tanzania_2010
drop _merge
save tanzania_2010, replace

use tanzania_2010,clear
drop hh_i09_1_01_num-hh_i09_3_12_num

save tanzania_2010, replace


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




