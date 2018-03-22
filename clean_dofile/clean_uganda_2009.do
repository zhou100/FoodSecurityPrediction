***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - March 20, 2018
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** 2009/10 obs. 2975 ***

* cd "/Users/yujunzhou/Box Sync/lsms/uganda_2009/UGA_2013_UNPS_v01_M_STATA8"

cd "C:\Users\Administrator\Desktop\lsms/Uganda_2009/UGA_2009_UNPS_v01_M_STATA"


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



tab  itmcd    
* doesn't have a food category variable 
* use  mapping in the 2010 survey 
use "C:\Users\Administrator\Desktop\lsms/Uganda_2013/UGA_2013_UNPS_v01_M_STATA8/GSEC15B.dta",clear
numlabel itmcd ,add
tab itmcd
tab h15bq2c  if itmcd ==140
 

* combine starches and cereals
replace h15bq2c =1 if h15bq2c ==2
* combine nuts and pulses
replace h15bq2c =5 if h15bq2c ==4
 collapse (max) h15bq2c, by ( itmcd)
rename h15bq2c category

save item_mapping.dta,replace 

use GSEC15B.dta,clear
des 
format hh %20.00f 
tostring hh,gen(HHID) format("%20.0f")
drop hh 

 merge m:m itmcd using item_mapping.dta
drop if _merge==2



* still have missings 
* fix the missing category 
numlabel H15BQ2,add
tab itmcd if _merge ==1
tab itmcd if _merge ==3
* try a few of those to see if the merge is correct 
tab category  if itmcd ==132

 

 
tab category 

* 1. A Cereals, Grains and Cereal Products: Weight = 2
* 5.  C  Nuts and Pulses: Weight = 3
* 6. D Vegetables: Weight = 1
* 8. E Meat, Fish and Animal Products: Weight = 4
* 7. F Fruits => weight = 1
* 9. G Milk/Milk Products: Weight = 4
* 10. H Fats/Oil => Weight = 0.5
* 3. I  Sugar/Sugar Products/Honey: Weight = 0.5
* 10. J Spices/Condiments: Weight = 0


replace category =1 if itmcd ==100
replace category =1 if itmcd ==101
replace category =1 if itmcd ==102
replace category =1 if itmcd ==103
replace category =1 if itmcd ==104
replace category =1 if itmcd ==105
replace category =1 if itmcd ==106
replace category =1 if itmcd ==107
replace category =1 if itmcd ==108
replace category =1 if itmcd ==109
replace category =1 if itmcd ==114
replace category =1 if itmcd ==116
replace category =8 if itmcd ==119
replace category =8 if itmcd ==120
replace category =8 if itmcd ==121
replace category =8 if itmcd ==122
replace category =8 if itmcd ==123
replace category =9 if itmcd ==125
replace category =9 if itmcd ==126
replace category =10 if itmcd ==129
replace category =7 if itmcd ==130
replace category =7 if itmcd ==131

replace category =6 if itmcd ==139
replace category =5 if itmcd ==140
replace category =5 if itmcd ==143
replace category =5 if itmcd ==144
replace category =5 if itmcd ==145
replace category =5 if itmcd ==146
replace category =11 if itmcd ==153
replace category =1 if itmcd ==170


* drop beverage and food away from home 
drop if category == 11 | category == 12

*drop spices (salt) 
tab itmcd if category==10
tab itmcd if itmcd==150

drop if itmcd ==150
 

* collapse by food group 
collapse (max)h15bq3b , by(category HHID)

 
tab h15bq3b
replace h15bq3b =7 if h15bq3b==8

label var h15bq3b "# Days specific food is eaten"


save food_consumption.dta, replace 

*** Generate FCS 
use food_consumption,clear 


* 1. A Cereals, Grains and Cereal Products: Weight = 2
* 5.  C  Nuts and Pulses: Weight = 3
* 6. D Vegetables: Weight = 1
* 8. E Meat, Fish and Animal Products: Weight = 4
* 7. F Fruits => weight = 1
* 9. G Milk/Milk Products: Weight = 4
* 10. H Fats/Oil => Weight = 0.5
* 3. I  Sugar/Sugar Products/Honey: Weight = 0.5
* 10. J Spices/Condiments: Weight = 0

***Specifying Weights Different Food Categories

*J Spices/Condiments: Weight = 0
gen FWeight = 0
* A Cereals, Grains and Cereal Products: Weight = 2
replace FWeight=2 if category==1
* C Nuts and Pulses: Weight = 3
replace FWeight=3 if category==5
* D Vegetables: Weight = 1
* F Fruits => weight = 1
replace FWeight=1 if category==6 | category == 7
* E Meat, Fish and Animal Products: Weight = 4 
* G Milk/Milk Products: Weight = 4
replace FWeight=4 if category==8| category == 9
* H Fats/Oil => Weight = 0.5
* I Sugar/Sugar Products/Honey: Weight = 0.5
replace FWeight=0.5 if category==10 |category == 3
label var FWeight "Food weight"

***Computing Weighted Food Categories
gen FCS = h15bq3b*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(HHID)
label var FCS "HHID food consumption score"
 

sort HHID
save uganda_2009.dta, replace

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
use food_consumption,clear 
des 
 
 
* 1. A Cereals, Grains and Cereal Products: Weight = 2
* 5.  C  Nuts and Pulses: Weight = 3
* 6. D Vegetables: Weight = 1
* 8. E Meat, Fish and Animal Products: Weight = 4
* 7. F Fruits => weight = 1
* 9. G Milk/Milk Products: Weight = 4
* 10. H Fats/Oil => Weight = 0.5
* 3. I  Sugar/Sugar Products/Honey: Weight = 0.5
* 10. J Spices/Condiments: Weight = 0

  
 
tab category
*Exclude SUGAR and SPICES
drop if category==3

*Generating Food consumed by HHIDs over the 24 hours (7 days in this dataset)
gen HDDS=0
replace HDDS=1 if h15bq3b>=1 & h15bq3b!=.
// Food categories consumed by HHIDs - COUNTS
collapse (sum) HDDS, by(HHID)
label var HDDS "Household Dietary Diversity Score"
 
 
tab HDDS 
/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort HDDS
sum HDDS
graph box HDDS, over(HDDS_Thresh) */

***Merging FCS and HDDS
merge m:m HHID using uganda_2009
drop _merge
sort HHID 
save uganda_2009, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "GSEC1",clear
des
 
keep HHID urban comm   h1bq2c   h1bq2b   h1aq1 h1aq3b   region 
  
rename h1aq3b  hh_a02 
tostring h1aq1,gen(hh_a01)
drop h1aq1
rename comm ea_id
  
rename h1bq2c FS_year 
rename h1bq2b FS_month


tab FS_year

tab FS_month

label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"

 gen survey_round ="Uganda NPS 2009/2010"
 
*Merge the Datafile
merge m:m HHID using uganda_2009
drop if _merge == 1

drop _merge
save uganda_2009, replace

 
 * Merge in other Geovariables
use UNPS_Geovars_0910.dta,clear
keep lat_mod lon_mod HHID      dist_road  dist_popcenter dist_market/* 
*/ dist_admctr afmnslp_pct srtm_uga srtm_uga_5_15 sq1 sq2 fsrad3_agpct 
 
  

rename lat_mod lat_modified
rename lon_mod lon_modified


rename srtm_uga   elevation
rename srtm_uga_5_15 terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename afmnslp_pct slope
rename fsrad3_agpct  ag_percent
 

merge m:m HHID using uganda_2009
drop if _merge == 1
drop _merge
save uganda_2009, replace 

* Merge in cell phone  and other assets 
set more off
use "GSEC14.dta",clear
des 


keep HHID h14q2  h14q3   h14q4  
 
 
numlabel H14Q2 ,add

tab h14q2

keep if h14q2  ==6 | h14q2  ==7 | h14q2  ==10 | /*
*/h14q2  ==11|h14q2  ==12 |h14q2  ==16  

egen cellphone = sum(h14q4 ) if h14q2  ==16,by(HHID)
replace cellphone =1 if cellphone !=0 & cellphone !=.


egen number_celphones = sum(h14q4 ) if h14q2  ==16,by(HHID)

 

egen Radio = sum(h14q4 ) if h14q2  ==7,by(HHID)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(h14q4 ) if h14q2  ==6,by(HHID)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(h14q4 ) if h14q2  ==10,by(HHID)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(h14q4 ) if h14q2  ==11,by(HHID)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(h14q4 ) if h14q2  ==12,by(HHID)
replace Car =1 if Car !=0 & Car !=.



 egen r2 = sum(Radio),by(HHID)
egen T = sum(Television),by(HHID)
egen B = sum(Bicycle),by(HHID)
egen M = sum(Motorcycle),by(HHID)
egen C = sum(Car),by(HHID)
egen cell = sum(cellphone),by(HHID)
egen cell_num = sum(number_celphones),by(HHID)



duplicates drop HHID,force

keep HHID  r2 T B M C cell cell_num

 rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car
rename cell cellphone
rename cell_num number_celphones
 
merge m:m HHID using uganda_2009
drop if _merge ==1
drop _merge
 
save uganda_2009, replace 

 
 * Merge in housing

 use "GSEC9.dta",clear
des
** floor ***

tab h9q06  ,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 |floor2==1
recode floor_dirt_sand_dung (. =0)
rename floor3 floor_cement
  
** roof ***
tab h9q04,gen(roof)
gen roof_natural =1 if roof1==1  | roof2==1  | roof3==1  
rename roof4 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)
 

keep HHID roof_natural roof_iron roof_other floor_cement floor_dirt


merge m:m HHID using uganda_2009
drop if _merge ==1
drop _merge
save uganda_2009, replace


rename HHID case_id 
 rename urban reside 

 drop if HDDS ==0 
  drop if FCS ==0 

 
save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_2009_Uganda.dta", replace

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
use "hh_MOD_H",clear

//Questions relating to COPING STRATEGIES (Full labels from Survey REPORT)
*In the past 7 days, did you worry that your household would not have enough food
*hhh02a: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Rely on less preferred and/or less expensive foods?"(WGT1)
*hhh02b: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Limit portion size at mealtimes?" (WGT1)
*hhh02c: "In the past 7 days, how many days have you or someone in your ///
	*household had to: Reduce number of meals eaten in a day?" (WGT2)
* hhh02d "In the past 7 days, how many days have you or someone in your ///
	*household had to: Restrict consumption by adults in order for small ///
	*children to eat?" (WGT2)
*hhh02e "In the past 7 days, how many days have you or someone in your ///
	*household had to: Borrow food, or rely on help from a friend or ///
	*relative?" (WGT2)


**Constructing rCSI
tab hhh02a
gen rCSI=1*hhh02a + 1*hhh02b + 2*hhh02c + 2*hhh02d +2*hhh02e

label var rCSI "HHID Reduced Coping Strategies Index"

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
merge m:m HHID using uganda_2009
drop _merge
save uganda_2009, replace

*_______________________________________________________________________________

           *Q4. MONTHES OF ADEQUATE HOUSEHOLD FOOD PROVISIONING (MAHFP)
*_______________________________________________________________________________

********************************************************************************
*NOTES:
//Use the rest of the questions in Module H to calculate MAHFP. Calculate the //
//MAHFP as twelve months minus the total number of months out of the previous //
//twelve months that the household was unable to meet their food needs. If the///
//household responded no to hhh04, then all of the hhh05* variables should ///
//be coded as 0s. Your MAHFP score should be between 12 and 0. 
********************************************************************************

clear
set more off 
use "hhMOD_H"

*Past 12 Months Experienced Food Shortage
tab  hhh04
*Describing the data
foreach var of varlist hhh05a-hhh05s{
	tab `var'
}


*Recoding values of the response variables
foreach var of varlist hhh05a-hhh05s {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hhh04==1
	tab `var'_num
}



*Summing the number of response across case_ids
egen H_Count=rowtotal (hhh05a_num-hhh05s_num)
label var H_Count "Number of months HHID was food insecure" 
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
merge m:m HHID using uganda_2009
drop _merge
save uganda_2009, replace

*/
