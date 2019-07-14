***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2017
** update March 13,2018 
* recheck the measures and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 
*cd "/Users/yujunzhou/Box Sync/lsms/Malawi_2010/Household_Batch3of5_DTA/"
cd "D:\lsms\Malawi_2010/Household_Batch3of5_DTA/"



use HH_MOD_G2.dta,clear

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
*list hh_g08a hh_g08b hh_g08c


*Recoding of outliers (change 8 days to 7 days)
replace hh_g08c=7 if hh_g08c ==8
*Combining Cereals and roots (Category A and Category B)
replace hh_g08a="AB" if hh_g08a=="A" | hh_g08a=="B"
replace hh_g08b="Main Staples; cereals and grains, roots and tubers" ///
if hh_g08a=="AB"
collapse (mean)hh_g08c, by(case_id visit ea_id hh_g08a hh_g08b)
label var hh_g08c "# Days specific food is eaten"

***Specifying Weights Different Food Categories
gen FWeight = 0
replace FWeight=2 if hh_g08a=="AB"
replace FWeight=3 if hh_g08a=="C"
replace FWeight=1 if hh_g08a=="D" | hh_g08a=="F"
replace FWeight=4 if hh_g08a=="E" | hh_g08a=="G"
replace FWeight=0.5 if hh_g08a=="H" | hh_g08a=="I"
label var FWeight "Food weight"

***Computing Weighted Food Categories
gen FCS = hh_g08c*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(case_id)
label var FCS "HH food consumption score"
 

sort case_id
save HW3_FSMs.dta, replace

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
replace hh_g08a="AB" if hh_g08a=="A" | hh_g08a=="B"
replace hh_g08b="Main Staples; cereals and grains, roots and tubers" ///
if hh_g08a=="AB"
collapse (max)hh_g08c, by(case_id visit ea_id hh_g08a hh_g08b)
label var hh_g08c "# Days specific food is eaten"

*Exclude SUGAR and SPICES
drop if hh_g08a=="I" | hh_g08a=="J"

*Generating Food consumed by hhs over the 24 hours
gen HDDS=0
replace HDDS=1 if hh_g08c>=1 & hh_g08c!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(case_id)
label var HDDS "Household Dietary Diversity Score"
 

/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort HDDS
sum HDDS
graph box HDDS, over(HDDS_Thresh) */

***Merging FCS and HDDS
merge m:m case_id using HW3_FSMs
drop _merge
sort case_id 
save HW3_FSMs, replace

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
tab hh_h01
gen rCSI=1*hh_h02a + 1*hh_h02b + 2*hh_h02c + 2*hh_h02d +2*hh_h02e

*Replace HHs which do not experience FOOD SHORTAGE
* replace rCSI=0 if hh_h01==2
label var rCSI "HH Reduced Coping Strategies Index"


** rCSI cut-offs:  0-4 safe, 5-10 moderate, >11 insecure 



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
drop visit-hh_h06c_os
merge m:m case_id using HW3_FSMs
drop _merge
save HW3_FSMs, replace

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
foreach var of varlist hh_h05a_01-hh_h05b_15 {
	tab `var'
}

*Recoding values of the response variables
foreach var of varlist hh_h05a_01-hh_h05b_15 {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hh_h04==1
	*replace `var'_num=1 if `var'=="X" 
	replace `var'_num = 0 if hh_h04==2
	tab `var'_num
}



*Summing the number of response across case_ids
egen H_Count=rowtotal (hh_h05a_01_num-hh_h05b_15_num)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

*Drop irrelevant variables
keep case_id ea_id MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data
merge m:m case_id using HW3_FSMs
drop _merge
save HW3_FSMs, replace

*_______________________________________________________________________________

           *Q5. TOP THREE (RANKED) REASONS FOR INSUFFICIENT FOOD
*_______________________________________________________________________________

********************************************************************************
*NOTES:
//Create a variable or variables using household responses to H06 – the top ///
//three (ranked) reasons that households reported experiencing a month in ///
//which there was insufficient food to feed the family.
********************************************************************************
clear
set more off 
use "HH_MOD_H"

*List of variables with labels (string)
label dir

********************************************************************************
*FIRST Most frequent REASON is #4 (REASON1)
********************************************************************************
tab hh_h06a
//Inadequate household food stocks due to lack of farm inputs (Reason #4)
gen id=_n
*Enumerating the FIRST cause of Food Insecurity
//Most FREQUAENT Reason
egen K=sum(hh_h06a) if hh_h06a==4, by(id)
egen M=max(K) if hh_h06a!=. 
egen Reason1= max(M) 
label var Reason1 "1st reason why HH's were food insecure"

//Displays labels for this variable
label list HH_H06A 

label define Reason1 1 "Inadequate household stocks due to drought/poor rains" ///
2 "Inadequate household food stocks due to crop pest damage" ///
3 "Inadequate household food stocks due to small land size" ///
4 "Inadequate household food stocks due to lack of farm inputs" ///
5 "Food in the market was very expensive" ///
6 "Unable to reach the market due to high transportation costs" ///
7 "No food in the market" ///
8 "Floods/water logging" ///
9 "Other (Specify)"
label values Reason1 Reason1

********************************************************************************
*SCEOND Most frequent reason is #4 (REASON2)
********************************************************************************
tab hh_h06b
//Inadequate household food stocks due to lack of farm inputs (#Reason 4)
egen N=sum(hh_h06a) if hh_h06b==4, by(id)
egen O=max(N) if hh_h06b!=. 
egen Reason2= max(O)  
label var Reason2 "2nd reason why HH's were food insecure"

//Displays labels for this variable
label list HH_H06A 

label define Reason2 1 "Inadequate household stocks due to drought/poor rains" ///
2 "Inadequate household food stocks due to crop pest damage" ///
3 "Inadequate household food stocks due to small land size" ///
4 "Inadequate household food stocks due to lack of farm inputs" ///
5 "Food in the market was very expensive" ///
6 "Unable to reach the market due to high transportation costs" ///
7 "No food in the market" ///
8 "Floods/water logging" ///
9 "Other (Specify)"
label values Reason2 Reason2

********************************************************************************
*THIRD Most frequent reason is #1 (REASON2)
********************************************************************************
tab hh_h06c
//No food in the market (Reason #7)
egen P=sum(hh_h06a) if hh_h06c==7, by(id)
egen Q=max(P) if hh_h06c!=. 
egen Reason3= max(Q) 
label var Reason3 "3rd reason why HH's were food insecure"

//Displays labels for this variable
label list HH_H06A 

label define Reason3 1 "Inadequate household stocks due to drought/poor rains" ///
2 "Inadequate household food stocks due to crop pest damage" ///
3 "Inadequate household food stocks due to small land size" ///
4 "Inadequate household food stocks due to lack of farm inputs" ///
5 "Food in the market was very expensive" ///
6 "Unable to reach the market due to high transportation costs" ///
7 "No food in the market" ///
8 "Floods/water logging" ///
9 "Other (Specify)"
label values Reason3 Reason3

*Maintain Relevant Data
keep case_id Reason1 Reason2 Reason3 

**Summary statistics
sum Reason1 Reason2 Reason3 

***Merging the data
merge m:m case_id using HW3_FSMs
drop _merge
save HW3_FSMs, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 
*use "/Users/yujunzhou/Box Sync/lsms/Malawi_2010/Household_Batch1of5_DTA/HH_MOD_A_FILT",clear
use "D:\lsms\Malawi_2010/Household_Batch1of5_DTA/HH_MOD_A_FILT",clear

 
keep case_id reside hh_wgt hh_a01   hh_a02b 

*Creating region variable
tab hh_a01
decode hh_a02b, gen (TA_names)
drop hh_a02b
*tab hh_a02b

gen region = 0
replace region = 1 if hh_a01 == 101|hh_a01 == 102|hh_a01 == 103|hh_a01 == 104| ///
hh_a01 == 105|hh_a01 == 107 
replace region = 2 if hh_a01 == 201|hh_a01 == 202|hh_a01 == 203|hh_a01 == 204| ///
hh_a01 == 205|hh_a01 == 206|hh_a01 == 207|hh_a01 == 208|hh_a01 == 209|hh_a01 == 210 
replace region = 3 if hh_a01 == 301|hh_a01 == 302|hh_a01 == 303|hh_a01 == 304| ///
hh_a01 == 305|hh_a01 == 306|hh_a01 == 307|hh_a01 == 308|hh_a01 == 309|hh_a01 == ///
310|hh_a01 == 311|hh_a01 == 312|hh_a01 == 313|hh_a01 == 314|hh_a01 == 315
replace region = . if region == 0 
tab region

label var region "District of Household head"
label define region 1 "Northern" 2 "Central" 3 "Southern"
label values region region
label list region

*Merge the Datafile
merge m:m case_id using HW3_FSMs
drop _merge
save HW3_FSMs, replace


* Merge in geolocation 
*use "/Users/yujunzhou/Box Sync/lsms/Malawi_2010/HouseholdGeovariables.dta", clear
use "D:\lsms\Malawi_2010/HouseholdGeovariables.dta",clear

keep case_id ea_id lat_modi lon_modi srtm_eaf srtm_eaf_5_15 sq1 sq2 dist_road dist_admarc dist_popcenter afmnslp_pct    fsrad3_agpct 
rename srtm_eaf elevation
rename srtm_eaf_5_15 terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention
rename fsrad3_agpct ag_percent
rename afmnslp_pct slope

merge m:m case_id using HW3_FSMs
drop _merge
save HW3_FSMs, replace

* Merge in the basic information
* use "/Users/yujunzhou/Box Sync/lsms/Malawi_2010/ihs3_summary.dta"
use "D:\lsms\Malawi_2010/ihs3_summary.dta",clear


keep case_id ea_id intmonth intyear head_age head_gender head_edlevel  hhsize
rename intmonth FS_month
rename intyear FS_year

merge m:m case_id using HW3_FSMs
drop if _merge ==2
drop _merge

gen survey_round ="Malawi IHS 2010"
save HW3_FSMs, replace



*_______________________________________________________________________________

                *MERGE in cellphone, roof/floor and other household assets
*_______________________________________________________________________________

* Merge in cell phone , roof/floor
*use "/Users/yujunzhou/Box Sync/lsms/Malawi_2010/Household_Batch1of5_DTA/HH_MOD_F",clear
use "D:\lsms\Malawi_2010/Household_Batch1of5_DTA/HH_MOD_F.dta",clear

keep case_id ea_id hh_f34 hh_f35 hh_f08 hh_f09

** floor ***
tab hh_f09,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 | floor2 ==2
recode floor_dirt_sand_dung (. =0)
rename floor3 floor_cement
rename floor5 floor_tile
 
** roof ***
tab hh_f08,gen(roof)
rename roof1 roof_natural   
rename roof2 roof_iron
gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)


** cellphone ***
gen cell_phone = 1 if hh_f34!=0
replace cell_phone = 0 if hh_f34==0
rename hh_f34  number_celphones

rename hh_f35 cellphone_cost

keep case_id ea_id  cell_phone number_celphones cellphone_cost roof_natural roof_iron roof_other floor_cement floor_tile floor_dirt

merge m:m case_id using HW3_FSMs
drop if _merge ==2
drop _merge
save HW3_FSMs, replace


** merge in the assets
use "D:\lsms\Malawi_2010\Household_Batch4of5_DTA\HH_MOD_L.dta",clear

tab hh_l02  ,nolabel
keep if hh_l02  ==518 | hh_l02  ==516 | hh_l02  ==517 |hh_l02  ==514 |hh_l02  ==507 |hh_l02  ==509


egen Refrigerator = sum(hh_l03 ) if hh_l02  ==514,by(case_id)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_l03 ) if hh_l02  ==507,by(case_id)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_l03 ) if hh_l02  ==509,by(case_id)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_l03 ) if hh_l02  ==516,by(case_id)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_l03 ) if hh_l02  ==517,by(case_id)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_l03 ) if hh_l02  ==518,by(case_id)
replace Car =1 if Car !=0 & Car !=.

egen r1 = sum(Refrigerator),by(case_id)
egen r2 = sum(Radio),by(case_id)
egen T = sum(Television),by(case_id)
egen B = sum(Bicycle),by(case_id)
egen M = sum(Motorcycle),by(case_id)
egen C = sum(Car),by(case_id)


duplicates drop case_id,force

keep case_id r1 r2 T B M C

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car

merge m:m case_id using HW3_FSMs
drop if _merge ==2
drop _merge

drop if FCS ==0 
drop if HDDS ==0
*save "/Users/yujunzhou/Box Sync/lsms/FCS_2010_Malawi.dta", replace
save "D:\lsms\cleaned_dataset\FCS_2010_Malawi.dta", replace



