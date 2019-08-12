***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2017
* * Edit 03/14/18, recheck the measures (calculated FCS and HDDS) and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

* cd "/Users/yujunzhou/Box Sync/lsms/Nigeria_2010/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household"
cd "C:\Users\Administrator\Desktop\lsms\Nigeria_2010/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household"

**** year 2011, observation 4815 ***
 
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
use "sect10c_harvestw1",clear

**Recoding of outliers (change more than 7 days to 7 days)**
tab s10cq7
replace s10cq7=7 if s10cq7>7 

* combine Cereals and roots (Category A and Category B)

replace item_cd =1 if item_cd==2
******Note: this collpasing step is to get rid of the category b in the data ******  
collapse (mean)s10cq7, by(hhid ea item_cd state sector lga)
label var s10cq7 "# Days specific food is eaten"
 
***Specifying Weights Different Food Categories
gen FWeight = 0
replace FWeight=2 if item_cd==1 
replace FWeight=3 if item_cd==3
replace FWeight=1 if item_cd==4 | item_cd==7
replace FWeight=4 if item_cd==5 | item_cd==8
replace FWeight=0.5 if item_cd==9 | item_cd==10
label var FWeight "Food weight"
 
 
***Computing Weighted Food Categories
gen FCS = s10cq7*FWeight
label var FCS "Food Consumption Score"
 
**Aggregating FCS by households
collapse (sum)FCS, by(hhid state sector lga)
label var FCS "HH food consumption score"
 

sort hhid

save Nigeria_2010_harvest.dta, replace



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
use "sect10c_harvestw1",clear

**Recoding of outliers (change more than 7 days to 7 days)**
tab s10cq7
replace s10cq7=7 if s10cq7>7 


* combine Cereals and roots (Category A and Category B)
replace item_cd =1 if item_cd==2
******Note: this collpasing step is to get rid of the category b in the data ******  
collapse (mean)s10cq7, by(hhid ea item_cd state sector lga)
label var s10cq7 "# Days specific food is eaten"
 
*Exclude SUGAR and SPICES
drop if item_cd==9 | item_cd==10

*Generating Food consumed by hhs over the past 7 days 
gen HDDS=0
replace HDDS=1 if s10cq7>=1 & s10cq7!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(hhid)
label var HDDS "Household Dietary Diversity Score"




sort hhid
merge m:m hhid using Nigeria_2010_harvest
drop _merge
save Nigeria_2010_harvest.dta, replace





*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

	set more off 
	use "sect12_harvestw1.dta",clear

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
		 tab `var'
	} 
 


	gen rCSI=1*s12q1a + 1*s12q1b + 1*s12q1c + 2*s12q1d +2*s12q1e +2*s12q1f + 4*s12q1g + 4*s12q1h + 4*s12q1i

	label var rCSI "HH Reduced Coping Strategies Index"

	***Merging the data
	keep hhid rCSI 
	merge m:m hhid using Nigeria_2010_harvest
	drop if _merge ==1
	drop _merge 
	save Nigeria_2010_harvest, replace



*******************************************
* Merge in cell phone, radio, tv and computer 
*******************************************
set more off
use "sect5_harvestw1",clear


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

merge m:m hhid using Nigeria_2010_harvest
drop if _merge ==1
drop _merge
save Nigeria_2010_harvest, replace
*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "secta_harvestw1",clear
* Merge in year and round 

keep hhid  ea zone sector ric  state  wt_wave1 saq13m  saq13y saq17m saq17y saq21m saq21y 
  

*replace hh_saq13_c= 2006 if hh_saq13_c ==6

gen FS_year = saq13y  
replace FS_year = saq17y if FS_year ==.
replace FS_year = saq21y if FS_year ==.

gen FS_month = saq13m 
replace FS_month = saq17m if FS_month ==.
replace FS_month = saq21m if FS_month ==.


label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"

keep hhid FS_month FS_year ea zone sector  state  wt_wave1  ric

merge m:m hhid using Nigeria_2010_harvest
drop if _merge ==1
drop _merge

gen survey_round ="2010/11 Nigeria post-harv hh survey"

save Nigeria_2010_harvest, replace

*_______________________________________________________________________________

* Merge in geolocation and geographic related info
*_______________________________________________________________________________

*use "/Users/yujunzhou/Box Sync/lsms/Nigeria_2010/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Geodata/NGA_HouseholdGeovariables_Y1.dta",clear
use "C:\Users\Administrator\Desktop/lsms/Nigeria_2010/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Geodata/NGA_HouseholdGeovariables_Y1.dta",clear

keep hhid lat_dd_mod  lon_dd_mod dist_road dist_popcenter dist_market /*
*/ afmnslp_pct  srtm_nga   srtm_nga_5_15 fsrad3_lcmaj  sq1 sq2 

rename  lat_dd_mod lat_modified
rename lon_dd_mod lon_modified 

rename fsrad3_lcmaj  ag_percent
rename srtm_nga elevation
rename srtm_nga_5_15  terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename afmnslp_pct slope 
 

merge m:m hhid using Nigeria_2010_harvest
drop if _merge ==1

drop _merge
save Nigeria_2010_harvest, replace



*_______________________________________________________________________________

* Merge in housing related info  
*_______________________________________________________________________________
use "sect8_harvestw1",clear

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


merge m:m hhid using Nigeria_2010_harvest
drop if _merge ==1
drop _merge
save Nigeria_2010_harvest, replace


*_______________________________________________________________________________

* Merge in household asset info  
*____________________________________________________________________
use "sect7_harvestw1",clear
numlabel item_cd,add



keep if item_cd == 317 | item_cd == 318 | item_cd == 319 | item_cd ==312
* if owned in post planting 
tab s7x,gen(own)
recode own1 (. = 0)
* 
tab item_cd

egen Refrigerator = sum(own1 ) if item_cd  ==312,by(hhid )
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Bicycle = sum(own1 ) if item_cd  ==317,by(hhid )
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(own1 ) if item_cd  ==318,by(hhid )
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(own1 ) if item_cd  ==319,by(hhid )
replace Car =1 if Car !=0 & Car !=.

egen r1 = sum(Refrigerator),by(hhid)
egen B = sum(Bicycle),by(hhid)
egen M = sum(Motorcycle),by(hhid)
egen C = sum(Car),by(hhid)

collapse (max) r1 B M C, by (hhid)

rename r1 Refrigerator 
rename B Bicycle 
rename M Motorcycle
rename C Car

label variable Refrigerator "Do you have a Refrigerator"
label variable Bicycle "Do you have a Bicycle"
label variable Motorcycle "Do you have a Motorcycle"
label variable Car "Do you have a Car"

merge m:m hhid using Nigeria_2010_harvest
drop if _merge ==1
drop _merge
save Nigeria_2010_harvest, replace
 
******************************************************
* change variables to help with join 
*******************************************************

use Nigeria_2010_harvest,clear

rename hhid case_id 
rename ric hh_a01 
rename ea ea_id 

label variable hh_a01 "Woreda/District Code"

rename wt_wave1 hh_wgt  
rename zone region 
rename sector reside


* a HDDS of 0 is probably unreliable data 
drop if HDDS==0 
 drop if FCS==0 


* save "/Users/yujunzhou/Box Sync/lsms/FCS_Nigeria_2010_harvest.dta",replace
save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_Nigeria_2010_harvest.dta", replace



*** probably some problem with calculating MAHFP measure ***
*** will visit if need to later ***

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
use "sect12_harvestw1.dta",clear

*Past 12 Months Experienced Food Shortage
tab  s12q5

*Describing the data


foreach var of varlist s12q6a-s12q6f{
numlabel `var',add
}

foreach var of varlist s12q6a-s12q6f{
	tab `var'
}


keep s12q6a-s12q6f hhid 


 *Recoding values of the response variables
foreach var of varlist hh_h05a-hh_h05s {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hh_h04==1
	tab `var'_num
}





*Drop irrelevant variables
keep y2_hhid qx_type interview_status MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)

sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP

***Merging the data

**** since FS year is 2011 and month are from jan to dec.
* therefore, for 2010 months, need month to be month_variable to be less than 
* the FS_month, and that for the 2011 months, should be greater.




foreach var of varlist s12q6a-s12q6f {
tab `var'
}

tab FS_month
tab FS_year



foreach var of varlist s9q6a-s9q6c {
replace  `var'_count = `var'_count + 1 if FS_month == 8 & `var' > 7 & `var'!=. 
replace  `var'_count = `var'_count + 1 if FS_month == 9 & `var' > 8 & `var'!=. 
replace  `var'_count = `var'_count + 1 if FS_month == 10 & `var' > 9 & `var'!=. 
replace  `var'_count = `var'_count + 1 if FS_month == 11 & `var' > 10 & `var'!=. 

}

* therefore, for 2010 months, need month to be month_variable to be bigger than  
* the FS_month


foreach var of varlist s12q6a-s12q6f {
gen `var'_count = 0
}

foreach var of varlist s12q6a-s12q6c {
	forvalues i = 1(1)12 {
		replace `var'_count = `var'_count + 1 if FS_month == `i' & `var' > (`i'-1) & `var'!=. 

	}
}




foreach var of varlist s12q6a-s12q6c {
 tab `var'_count
}

egen  month_10= rowtotal (s12q6a_count - s12q6c_count  ) 
tab month_10 

 * for the 2011 months, FS_month should be greater than the starve month .

foreach var of varlist s12q6d-s12q6f {
	forvalues i = 1(1)12 {
		replace  `var'_count = `var'_count + 1 if FS_month == `i' & `var' < (`i') & `var'!=. 

	}
}



foreach var of varlist s12q6d-s12q6f {
 tab `var'_count
}


egen  month_11= rowtotal (s12q6e_count - s12q6f_count  ) 
tab month_11

gen H_Count = month_10 + month_11 
label var H_Count "Number of months hh was food insecure" 

*Summing the number of response across case_ids
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

keep hhid MAHFP 
merge m:m hhid using Nigeria_2010_harvest
drop _merge
save Nigeria_2010_harvest, replace

*/
