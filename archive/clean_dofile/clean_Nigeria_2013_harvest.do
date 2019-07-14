***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2018
* * Edit 03/16/18, recheck the measures (calculated FCS and HDDS) and added household level information

* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

* cd "/Users/yujunzhou/Box Sync/lsms/Nigeria_2012/DATA/Post Harvest Wave 2/Household"
cd "C:\Users\Administrator\Desktop\lsms\Nigeria_2012/DATA/Post Harvest Wave 2/Household" 

* sect7_hh_w1.dta
* Hh_s7q02

*_______________________________________________________________________________

         *Q 1: FOOD CONSUMPTION SCORES (FCS)
         *sect10c_harvestw3
*_______________________________________________________________________________

********************************************************************************
*NOTES ON THE WEIGHTS OF THE DIFFERENT FOOD CATEGORIES
* 1 Cereals, Grains and Cereal Products: Weight = 2
* 2 Root, Tubers and Plantains: Weight = 2
* 3 Nuts and Pulses: Weight = 3
* 4 Vegetables: Weight = 1
* 5 Meat, Fish and Animal Products: Weight = 4
* 6 Meat, Fish and Animal Products used as Condiments: Weight = 0
* 7 Fruits : Weight = 1
* 8 Milk/Milk Products: Weight = 4
* 9 Fats/Oil : Weight = 0.5
* 10 Sugar/Sugar Products/Honey: Weight = 0.5
* 11 Spices/Condiments: Weight = 0
********************************************************************************

/*gen weighted score FOR CEREALS AND ROOTS*/
*Listing of the various food categories
*list item_cd item_desc s10cq8

*drop if s10cq8>9

set more off
use "sect10c_harvestw2",clear

**Recoding of outliers (change more than 7 days to 7 days)**
tab s10cq8
replace s10cq8=7 if s10cq8>7 

* combine Cereals and roots (Category A and Category B)

replace item_cd =1 if item_cd==2
******Note: this collpasing step is to get rid of the category b in the data ******  
collapse (mean)s10cq8, by(hhid ea item_cd state sector lga)
label var s10cq8 "# Days specific food is eaten"
 
***Specifying Weights Different Food Categories
gen FWeight = 0
replace FWeight=2 if item_cd==1 
replace FWeight=3 if item_cd==3
replace FWeight=1 if item_cd==4 | item_cd==7
replace FWeight=4 if item_cd==5 | item_cd==8
replace FWeight=0.5 if item_cd==9 | item_cd==10
label var FWeight "Food weight"
 
 
***Computing Weighted Food Categories
gen FCS = s10cq8*FWeight
label var FCS "Food Consumption Score"
 
**Aggregating FCS by households
collapse (sum)FCS, by(hhid state sector lga)
label var FCS "HH food consumption score"
 

sort hhid

save Nigeria_2013_harvest.dta, replace



*_______________________________________________________________________________
    
	*Q2: HOUSEHOLD DIETARY DIVERSITY SCORE (HDDS)
*_______________________________________________________________________________

********************************************************************************
*A diet diversity score is a household-measure of food security that captures ///
*something about the quality of a diet. It is calculated by counting the number///
*of foods or food groups from which a household acquired food over the survey ///
*reference period (24 hours). 

********************************************************************************

set more off
use "sect10c_harvestw2",clear

**Recoding of outliers (change more than 7 days to 7 days)**
tab s10cq8
replace s10cq8=7 if s10cq8>7 


* combine Cereals and roots (Category A and Category B)
replace item_cd =1 if item_cd==2
******Note: this collpasing step is to get rid of the category b in the data ******  
collapse (mean)s10cq8, by(hhid ea item_cd state sector lga)
label var s10cq8 "# Days specific food is eaten"
 
*Exclude SUGAR and SPICES
drop if item_cd==11 | item_cd==10

*Generating Food consumed by hhs over the past 7 days 
gen HDDS=0
replace HDDS=1 if s10cq8>=1 & s10cq8!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(hhid)
label var HDDS "Household Dietary Diversity Score"




sort hhid
merge m:m hhid using Nigeria_2013_harvest
drop _merge
save Nigeria_2013_harvest.dta, replace


*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

	set more off 
	use "sect12_harvestw2.dta",clear

	//Questions relating to COPING STRATEGIES (Full labels from Survey REPORT)

	*In the past 7 days, did you worry that your household would not have enough food

	*s9q1a : "In the past 7 days, how many days have you or someone in your ///
		*household had to: Rely on less preferred and/or less expensive foods?"(WGT1)


	*s9q1b: "In the past 7 days, how many days have you or someone in your ///
		*household had to: limit the VARIETY of foods eaten ?" (WGT1)


	*s9q1c: "In the past 7 days, how many days have you or someone in your ///
		*household had to: Limit portion size at mealtimes?" (WGT1)

	*s9q1d: "In the past 7 days, how many days have you or someone in your ///
		*household had to: Reduce number of meals eaten in a day?" (WGT2)

	* s9q1e  "In the past 7 days, how many days have you or someone in your ///
		*household had to: Restrict consumption by adults in order for small ///
		*children to eat?" (WGT2)

	*s9q1f "In the past 7 days, how many days have you or someone in your ///
		*household had to: Borrow food, or rely on help from a friend or ///
		*relative?" (WGT2)


	*s9q1g "In the past 7 days, how many days have you or someone in your ///
		*household had to: have no food of any kind in your household?" (WGT4)

	*s9q1h "In the past 7 days, how many days have you or someone in your ///
		*household had to: go to sleep at night hungry because there is not enough food" (WGT4)

	*s9q1i "In the past 7 days, how many days have you or someone in your ///
		*household had to: go a whole day and night without eating anything?" (WGT4)




	**Constructing rCSI
 
	foreach var of varlist s12q1a - s12q1i {
	*	 tab `var'
	} 
 


	gen rCSI=1*s12q1a + 1*s12q1b + 1*s12q1c + 2*s12q1d +2*s12q1e +2*s12q1f + 4*s12q1g + 4*s12q1h + 4*s12q1i

	label var rCSI "HH Reduced Coping Strategies Index"

	***Merging the data
	keep hhid rCSI 
	merge m:m hhid using Nigeria_2013_harvest
	drop _merge
	save Nigeria_2013_harvest, replace



*******************************************
* Merge in cell phone 
*******************************************
use "sect5_harvestw2",clear


keep hhid s5q1 s5q4 s5q11 s5q10

* change  2 to 0 to make them dummies 
recode s5q1 (2 = 0)
recode s5q4 (2 = 0)
recode s5q11 (2 = 0)

rename s5q1 Radio
rename s5q4 Television
rename s5q11 Computer


gen cell_phone = 1 if s5q10!=0
label variable cell_phone "Owns a cellphone or not"
replace cell_phone = 0 if s5q10==0
rename s5q10  number_celphones

collapse (max)R T n C cell , by (hhid)
 
*drop s5q10 

merge m:m hhid using Nigeria_2013_harvest
drop if _merge ==2
drop _merge
save Nigeria_2013_harvest, replace
*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "secta_harvestw2",clear
* Merge in year and round 


keep hhid  ea zone sector ric  state   saq13m  saq13y saq17m saq17y saq21m saq21y 
  
 
gen FS_year = saq13y  
replace FS_year = saq17y if FS_year ==.
replace FS_year = saq21y if FS_year ==.

gen FS_month = saq13m 
replace FS_month = saq17m if FS_month ==.
replace FS_month = saq21m if FS_month ==.


label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"

keep hhid FS_month FS_year ea zone sector  state     ric

  
 

merge m:m hhid using Nigeria_2013_harvest
drop if _merge ==1
drop _merge

gen survey_round ="2012/13 Nigeria post-harv hh survey"

save Nigeria_2013_harvest, replace


* Merge in geolocation 
 *use "/Users/yujunzhou/Box Sync/lsms/Nigeria_2012/DATA/Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta",clear
use "C:\Users\Administrator\Desktop/lsms/Nigeria_2012/DATA/Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta",clear

 
keep hhid LAT_DD_MOD  LON_DD_MOD  dist_road dist_popcenter dist_market /*
*/ afmnslp_pct  srtm_nga   srtm_nga_5_15 fsrad3_lcmaj  sq1 sq2 

rename  LAT_DD_MOD lat_modified
rename LON_DD_MOD lon_modified 

rename fsrad3_lcmaj  ag_percent
rename srtm_nga elevation
rename srtm_nga_5_15  terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename afmnslp_pct slope 
 
merge m:m hhid using Nigeria_2013_harvest
drop if _merge ==1

drop _merge
save Nigeria_2013_harvest, replace

 
*_______________________________________________________________________________

* Merge in housing related info  
*_______________________________________________________________________________
use "sect8_harvestw2",clear
 
keep hhid s8q7 s8q8 

tab s8q8,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 | floor2 ==2
recode floor_dirt_sand_dung (. =0)

rename floor3 floor_cement
rename floor5 floor_tile
rename floor6 floor_other 
replace floor_other=1 if floor4==1
 
** roof ***
tab s8q7,gen(roof)
rename roof1 roof_natural   
rename roof2 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)

keep hhid  roof_natural roof_iron roof_other floor_other floor_cement floor_tile floor_dirt


collapse (max) roof_natural roof_iron roof_other floor_other floor_cement floor_tile floor_dirt, by (hhid)


merge m:m hhid using Nigeria_2013_harvest
drop if _merge ==1
drop _merge
save Nigeria_2013_harvest, replace


*_______________________________________________________________________________

* Merge in household asset info  
*____________________________________________________________________
use "sect7_harvestw2",clear
numlabel  S7CODE,add
tab item_cd  


keep if item_cd == 317 | item_cd == 318 | item_cd == 319 | item_cd ==312
* if owned in post planting 
tab s7


 
gen Refrigerator = 1 if item_cd  ==312 & s7>0  
recode Refrigerator (. =0)

gen Bicycle = 1 if item_cd  ==317 & s7>0 
recode Bicycle (. =0)


gen Motorcycle= 1 if item_cd  ==318 & s7>0 
recode Motorcycle (. =0)


gen Car = 1 if item_cd  ==319 & s7>0 
recode Car (. =0)
 

collapse (max) Refrigerator Bicycle Motorcycle Car, by (hhid)
 

label variable Refrigerator "Do you have a Refrigerator"
label variable Bicycle "Do you have a Bicycle"
label variable Motorcycle "Do you have a Motorcycle"
label variable Car "Do you have a Car"


merge m:m hhid using Nigeria_2013_harvest
drop if _merge ==1
drop _merge
save Nigeria_2013_harvest, replace

 
******************************************************
* change variables to help with join 
*******************************************************

use Nigeria_2013_harvest,clear

rename hhid case_id 
rename ric hh_a01 
rename ea ea_id 

label variable hh_a01 "Woreda/District Code"

 rename zone region 
rename sector reside


* a HDDS of 0 is probably unreliable data 
drop if HDDS==0 
 drop if FCS==0 
 
 *save "/Users/yujunzhou/Box Sync/lsms/FCS_Nigeria_2013_harvest.dta",replace

save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_Nigeria_2013_harvest.dta", replace


