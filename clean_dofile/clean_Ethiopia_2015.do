***********************************
*Code for cleaning the LSMS data 
* Yujun Zhou - June 12, 2018
* Edit 03/14/18, recheck the measures and added household level information
* with help from Dr.Hope Michelson and Edward Martey
***********************************

capture log close 
clear
set more off 

**** 2008 obs.  4954 ***

*cd "/Users/yujunzhou/Box Sync/lsms/Ethiopia_2015/ETH_2015_ESS_v01_M_STATA8/Household"

cd "C:\Users\Administrator\Desktop\lsms\Ethiopia_2015/ETH_2015_ESS_v01_M_STATA8/Household"

* sect7_hh_w1.dta
* Hh_s7q02

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

use sect5b_hh_w3.dta,clear

*list hh_s5bq00  hh_s5bq02
*tab hh_s5bq00
tab hh_s5bq02

*Recoding of outliers of days (change 8 days to 7 days)
*replace hh_s5bq02=7 if hh_s5bq02==8

*adjust label
numlabel HH_S5BQ00,add

tab hh_s5bq00
*Combining Cereals (enjera, rice,.., potato, pasta)
replace hh_s5bq00=1 if hh_s5bq00==1 | hh_s5bq00== 2 | hh_s5bq00==3 | hh_s5bq00==4 | hh_s5bq00==16
*Combine meat, fish and eggs
replace hh_s5bq00=9 if hh_s5bq00==10 | hh_s5bq00== 11 |  hh_s5bq00== 12

collapse (max)hh_s5bq02, by(household_id2  hh_s5bq00)
label var hh_s5bq02 "# of Days specific food is eaten"

***Specifying Weights Different Food Categories
*J Spices/Condiments: Weight = 0
gen FWeight = 0
* A Cereals, Grains and Cereal Products: Weight = 2
replace FWeight=2 if hh_s5bq00==1
* C Nuts and Pulses: Weight = 3
replace FWeight=3 if hh_s5bq00==6
* D Vegetables: Weight = 1
* F Fruits => weight = 1
replace FWeight=1 if hh_s5bq00==7 | hh_s5bq00 == 8
* E Meat, Fish and Animal Products: Weight = 4 
* G Milk/Milk Products: Weight = 4
replace FWeight=4 if hh_s5bq00==9 | hh_s5bq00 == 14
* H Fats/Oil => Weight = 0.5
* I Sugar/Sugar Products/Honey: Weight = 0.5
replace FWeight=0.5 if hh_s5bq00==5 |hh_s5bq00 == 13


label var FWeight "Food weight"

***Computing Weighted Food Categories
gen FCS = hh_s5bq02*FWeight
label var FCS "Food Consumption Score"

**Aggregating FCS by households
collapse (sum)FCS, by(household_id2)
label var FCS "HH food consumption score"
 

sort household_id2
save ethiopia_2015.dta, replace

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
use "sect5b_hh_w3",clear

*adjust label
numlabel HH_S5BQ00,add

*Combining Cereals (enjera, rice,.., potato, pasta)
replace hh_s5bq00=1 if hh_s5bq00==1 | hh_s5bq00== 2 | hh_s5bq00==3 | hh_s5bq00==4 | hh_s5bq00==16
*Combine meat, fish and eggs
replace hh_s5bq00=9 if hh_s5bq00==10 | hh_s5bq00== 11 |  hh_s5bq00== 12

collapse (max)hh_s5bq02, by(household_id2  ea_id2 hh_s5bq00)
label var hh_s5bq02 "# of Days specific food is eaten"


*Exclude SUGAR and SPICES
drop if hh_s5bq00==5 | hh_s5bq00==15

*Generating Food consumed by hhs over the 24 hours
gen HDDS=0
replace HDDS=1 if hh_s5bq02>=1 & hh_s5bq02!=.
// Food categories consumed by hhs - COUNTS
collapse (sum) HDDS, by(household_id2)
label var HDDS "Household Dietary Diversity Score"
 

/*
***Summary Characteristics and Graphs (Bar Charts and Box Plot)
sort HDDS
sum HDDS
graph box HDDS, over(HDDS_Thresh) */

***Merging FCS and HDDS
merge m:m household_id2 using ethiopia_2015
drop _merge
sort household_id2 
save ethiopia_2015, replace

*_______________________________________________________________________________

         *Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
*_______________________________________________________________________________

*NOTES:
*The IHS3 includes questions to compute the rCSI (Module H). As with the FCS, ///
*the rCSI involves assigning weights to household responses. The rCSI (like ///
*the CSI) is designed to capture quantity or sufficiency of consumption. 

	set more off 
	use "sect7_hh_w3",clear

	//Questions relating to COPING STRATEGIES (Full labels from Survey REPORT)

	*In the past 7 days, did you worry that your household would not have enough food

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
	tab hh_s7q01

	foreach var of varlist hh_s7q02_a - hh_s7q02_h {
		* tab `var'
	} 

	tab hh_s7q02_a

	* fix a typo 
	replace hh_s7q01=2 if hh_s7q01 == 0 
	replace hh_s7q01=. if hh_s7q01 == 5 


	gen rCSI=1*hh_s7q02_a + 1*hh_s7q02_b + 1*hh_s7q02_c + 2*hh_s7q02_d +2*hh_s7q02_e +2*hh_s7q02_f + 4*hh_s7q02_g + 4*hh_s7q02_h

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
	keep household_id2 rCSI 
	merge m:m household_id2 using ethiopia_2015
	drop _merge
	save ethiopia_2015, replace

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
use "sect7_hh_w3",clear

*Past 12 Months Experienced Food Shortage
tab  hh_s7q06
*Describing the data
foreach var of varlist hh_s7q07_a-hh_s7q07_l{
	tab `var'
}


*Recoding values of the response variables
foreach var of varlist  hh_s7q07_a-hh_s7q07_l {
	gen `var'_num=0
	replace `var'_num=1 if `var'=="X" & hh_s7q06==1
	tab `var'_num
}



*Summing the number of response across case_ids
egen H_Count=rowtotal (hh_s7q07_a_num-hh_s7q07_l_num)
label var H_Count "Number of months hh was food insecure" 
*The values must be between 0 and 12
sum H_Count

*Generating Months of Adequate Household Food Provisioning (MAHFP)
gen MAHFP = 12 - H_Count
label var MAHFP "Months of Adequate Household Food Provisioning"

sum MAHFP
*Drop irrelevant variables
keep household_id2  MAHFP

***Summary Characteristics and Graphs (Bar Charts and Box Plot)
/*
sort MAHFP
sum MAHFP
tab MAHFP
hist MAHFP
graph box MAHFP
*/
***Merging the data
merge m:m household_id2 using ethiopia_2015
drop _merge
save ethiopia_2015, replace

*_______________________________________________________________________________

                *MERGE in DIST, geolocation and time 
*_______________________________________________________________________________

set more off 

use "sect_cover_hh_w3",clear

keep household_id2 pw_w3 household_id2 ea_id2 rural  saq03 saq04 saq01 hh_saq09
rename hh_saq09 hhsize

 
*Merge the Datafile
merge m:m household_id2 using ethiopia_2015
drop _merge
save ethiopia_2015, replace


* Merge in geolocation 


 use "C:\Users\Administrator\Desktop\lsms\Ethiopia_2015/ETH_2015_ESS_v01_M_STATA8/Geovariables/ETH_HouseholdGeovars_y3.dta",clear
 keep household_id2 lat_dd_mod  lon_dd_mod srtm srtm_5_15 sq1 sq2 dist_road dist_admctr  dist_popcenter fsrad3_agpct
rename  lat_dd_mod lat_modified
rename lon_dd_mod lon_modified 

rename fsrad3_agpct ag_percent
rename srtm elevation
rename srtm_5_15 terrain_rough
rename sq1 nutri_avail
rename sq2 nutri_rentention

merge m:m household_id2 using ethiopia_2015
drop _merge
save ethiopia_2015, replace

* Merge in year and round 
use  sect_cover_hh_w3.dta,clear
keep household_id2 hh_saq13_c hh_saq13_b hh_saq17_b hh_saq17_c 

tab hh_saq13_c
tab hh_saq13_b,nolabel

*replace hh_saq13_c= 2006 if hh_saq13_c ==6

gen FS_year = hh_saq13_c  
replace FS_year = hh_saq17_c if FS_year ==.

gen FS_month = hh_saq13_b 
replace FS_month = hh_saq17_b if FS_month ==.

label variable FS_month "Month FS module was administered"
 label variable FS_year "Year FS module was administered"

 keep household_id2 FS_month FS_year 

merge m:m household_id2 using ethiopia_2015
drop if _merge ==2
drop _merge

gen survey_round ="2015/16 Ethiopia  socioeconomic survey wave three"
gen country = "Ethiopia"
save ethiopia_2015, replace



*_______________________________________________________________________________

                *MERGE in cellphone, roof/floor and other household assets
*_______________________________________________________________________________

* Merge in cell phone  
use "sect10_hh_w3",clear
numlabel HH_S10Q00,add
tab hh_s10q00
keep if hh_s10q00 == 8|hh_s10q00 == 9 |hh_s10q00 == 10 |hh_s10q00 ==14 |hh_s10q00 ==15 |hh_s10q00 == 23 |hh_s10q00 == 22

*tab hh_s10q01
keep household_id2  hh_s10q00 hh_s10q01



gen cell_phone = 1 if hh_s10q00 == 8 & hh_s10q01!=0 
label variable cell_phone "Owns a cellphone or not"
replace cell_phone = 0 if hh_s10q00 == 8 & hh_s10q01==0
 
egen number_celphones =sum(hh_s10q01 ) if hh_s10q00  ==8,by(household_id2)

egen Refrigerator = sum(hh_s10q01 ) if hh_s10q00  ==22,by(household_id2)
replace Refrigerator =1 if Refrigerator !=0 & Refrigerator !=.


egen Radio = sum(hh_s10q01 ) if hh_s10q00  ==9,by(household_id2)
replace Radio =1 if Radio !=0 & Radio !=.


egen Television = sum(hh_s10q01 ) if hh_s10q00  ==10,by(household_id2)
replace Television =1 if Television !=0 & Television !=.


egen Bicycle = sum(hh_s10q01 ) if hh_s10q00  ==14,by(household_id2)
replace Bicycle =1 if Bicycle !=0 & Bicycle !=.


egen Motorcycle = sum(hh_s10q01 ) if hh_s10q00  ==15,by(household_id2)
replace Motorcycle =1 if Motorcycle !=0 & Motorcycle !=.


egen Car = sum(hh_s10q01 ) if hh_s10q00  ==23,by(household_id2)
replace Car =1 if Car !=0 & Car !=.

egen r1 = sum(Refrigerator),by(household_id2)
egen r2 = sum(Radio),by(household_id2)
egen T = sum(Television),by(household_id2)
egen B = sum(Bicycle),by(household_id2)
egen M = sum(Motorcycle),by(household_id2)
egen C = sum(Car),by(household_id2)


duplicates drop household_id2,force

keep household_id2 r1 r2 T B M C cell_phone number_celphones

rename r1 Refrigerator 
rename r2 Radio 
rename T Television  
rename B Bicycle 
rename M Motorcycle
rename C Car


 

* Merge in the housing related information 

use "sect9_hh_w3",clear

keep household_id2 hh_s9q06  hh_s9q07  
tab hh_s9q07,gen(floor)
gen floor_dirt_sand_dung = 1 if floor1==1 | floor2 ==1 
recode floor_dirt_sand_dung (. =0)
rename floor4 floor_cement
gen floor_tile =1 if  floor5==1 | floor6 ==1 |  floor7==1 | floor8 ==1
recode floor_tile (. =0)
 

tab hh_s9q06,gen(roof)

rename roof3 roof_natural   
rename roof1 roof_iron
replace roof_natural =1 if roof4==1
replace roof_natural =1 if roof5==1

gen roof_other = 1 if roof_natural ==0 & roof_iron==0
recode roof_other (. =0)

keep household_id2   roof_natural roof_iron roof_other floor_cement floor_tile floor_dirt



merge m:m household_id2 using ethiopia_2015
drop if _merge ==2
drop _merge


rename household_id2 case_id 
rename saq04 town 
rename saq03 hh_a01 
label variable hh_a01 "Woreda/District Code"

rename pw_w3 hh_wgt  
rename saq01 region 
rename rural reside

drop if FCS ==0 
drop if HDDS ==0

*save "/Users/yujunzhou/Box Sync/lsms/FCS_ethiopia_2015.dta",replace
save "C:\Users\Administrator\Desktop\lsms\cleaned_dataset\FCS_2015_Ethipopia.dta", replace



