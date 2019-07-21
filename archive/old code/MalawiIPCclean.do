
*************************************************************
**do-file to clean Malawi chirps + IPC data
*************************************************************
clear
*cd "C:\Users\baylis\Dropbox\Current\Development\Hope\"

cd "~/Box Sync/Research/Malawi_FewS/"

import delimited malawi2.csv
save "MalawiIPC_clean.dta", replace

use MalawiIPC_clean.dta,clear
set more off 
**************************************************************
*General cleaning
**************************************************************
rename obs n
replace price_mean="" if price_mean=="NA"
replace price_w="" if price_w=="NA" 
sort n

*generate yearmo
gen year="20"+substr(date,-2,2)
replace year="" if date=="NA"
gen mo=1 if substr(date,1,3)=="Jan"
replace mo=2 if substr(date,1,3)=="Feb"
replace mo=3 if substr(date,1,3)=="Mar"
replace mo=4 if substr(date,1,3)=="Apr"
replace mo=5 if substr(date,1,3)=="May"
replace mo=6 if substr(date,1,3)=="Jun"
replace mo=7 if substr(date,1,3)=="Jul"
replace mo=8 if substr(date,1,3)=="Aug"
replace mo=9 if substr(date,1,3)=="Sep"
replace mo=10 if substr(date,1,3)=="Oct"
replace mo=11 if substr(date,1,3)=="Nov"
replace mo=12 if substr(date,1,3)=="Dec"
replace mo=. if date=="NA"
destring year, replace
gen yearmo=year*100+mo

drop if date=="NA"

*stretch IPC data to cover all months in quarter
gen q=1 if mo<4
replace q=2 if mo>3&mo<7
replace q=3 if mo>6&mo<10
replace q=4 if mo>9

rename v ipczone
*destring stuff
replace chirps_mean="" if chirps_mean=="NA"
replace chirps_mean_w="" if chirps_mean_w=="NA"
destring chirps_mean, replace
destring chirps_mean_w, replace
destring price_mean, replace
destring price_w, replace
replace chirps_max="" if chirps_max=="NA"
destring chirps_max, replace

*destring ipczone
gen ipczoneno=substr(ipczone,2,.)
replace ipczoneno="" if ipczone=="NA"
destring ipczoneno, replace
drop ipczone
rename ipczoneno ipczone

*stretch ipc codes across the quarter
replace ipc="" if ipc=="NA"
replace ipc="" if ipc=="99"
replace ipc="" if ipc=="88"
destring ipc, replace
bysort ipczone year q: egen ipc2=mode(ipc)
replace ipc2=ipc if ipc~=.
replace ipc=ipc2
drop ipc


sort ipczone yearmo
bysort ipczone: gen dateno=_n
xtset ipczone dateno

************************************************************************
**generate relevant rainfall data - relative to crop year
************************************************************************

gen rain_m10=chirps_mean if mo==10
gen rain_m11=chirps_mean if mo==11
gen rain_m12=chirps_mean if mo==12
gen rain_m01=chirps_mean if mo==1
gen rain_m02=chirps_mean if mo==2
gen rain_m03=chirps_mean if mo==3
gen rain_m04=chirps_mean if mo==4
recode rain_* (.=0)

gen cropyr=1 if yearmo<200805
replace cropyr=2 if yearmo>200804&yearmo<200905
replace cropyr=3 if yearmo>200904&yearmo<201005
replace cropyr=4 if yearmo>201004&yearmo<201105
replace cropyr=5 if yearmo>201104&yearmo<201205
replace cropyr=6 if yearmo>201204&yearmo<201305
replace cropyr=7 if yearmo>201304&yearmo<201405
replace cropyr=8 if yearmo>201404&yearmo<201505
replace cropyr=9 if yearmo>201504&yearmo<201605
label var cropyr "May to Apr"

bysort ipczone cropyr: egen rain_Oct=sum(rain_m10)
bysort ipczone cropyr: egen rain_Nov=sum(rain_m11)
bysort ipczone cropyr: egen rain_Dec=sum(rain_m12)
bysort ipczone cropyr: egen rain_Jan=sum(rain_m01)
bysort ipczone cropyr: egen rain_Feb=sum(rain_m02)
bysort ipczone cropyr: egen rain_Mar=sum(rain_m03)
bysort ipczone cropyr: egen rain_Apr=sum(rain_m04)
gen rain_cytot=rain_Oct+rain_Nov+rain_Dec+rain_Jan+rain_Feb+rain_Mar+rain_Apr
drop rain_m*
label var rain_cytot "total rainfall from Oct to Apr by ipczone and cropyear"
gen rain_cy2=rain_cy^2

**generate dummy for flood susceptible areas
gen area_flood=1 if ipczone==4| ipczone==35|ipczone==40
gen area_fish=1 if ipczone==40

recode area_* (.=0)

gen flood_precip=area_flood*chirps_mean 
gen flood_maxprecip=area_flood*chirps_max
gen flood_raincy=area_flood*rain_cytot
gen flood_Oct=area_flood*rain_Oct
gen flood_Nov=area_flood*rain_Nov
gen flood_Dec=area_flood*rain_Dec
gen flood_Jan=area_flood*rain_Jan
bysort ipczone cropyr (dateno): gen sumrain=sum(chirps_mean)
gen flood_sum=area_flood*sumrain

**generate precipitation norms and deviations
bysort ipczone: egen raincy_norm=mean(rain_cytot)
gen rain_dev=(rain_cytot-raincy_norm)^2
gen pos_rain_dev=rain_dev if rain_cytot-raincy_norm>0
gen neg_rain_dev=rain_dev if rain_cytot-raincy_norm<0
recode pos_rain neg_rain (.=0)
gen pos_rain1=pos_rain^1/2
gen neg_rain1=neg_rain^1/2

**generate ipc for quarters
gen q_ipc=ipc2 if mo==1|mo==4|mo==7|mo==10

**************************************************************
*Price
**************************************************************
gen lnprice=ln(price_m)

*****price deviation stuff here*******


**************************************************************
*Merge in Rainstat
*First run rainstat.do
**************************************************************
sort ipczone yearmo
merge ipczone yearmo using "rainstat.dta"
tab _merge
*note bad merges are NAs in master data or early 2007 or later 2016 in merging data
drop if _merge~=3

**************************************************************
*ID dry vs wet(ter) zones
**************************************************************

*split into upper vs lower rainfall areas by median total precip 
*during rainy season (raincy_norm)

gen dryipczone=1 if raincy_norm<911
recode dryipc (.=0)

gen dryipc_raincy=dryipcz*rain_cytot
gen dryipc_maxdays=dryipcz*maxdaysno
gen dryipc_day1=dryipcz*day1rain

**************************************************************
*gen lagged variables for merge
**************************************************************

xtset ipczone dateno
gen L12rain_cytot=L12.rain_cytot
gen L12day1rain=L12.day1rain
gen L12maxdays=L12.maxdays
gen L12pos_rain=L12.pos_rain1
gen L12neg_rain=L12.neg_rain1
gen L12dryipc_rain=L12.dryipc_rain
gen L12dryipc_max=L12.dryipc_max
gen L12dryipc_day1=L12.dryipc_day1

**************************************************************
*Regressions
**************************************************************
*fiddle
xtset ipczone dateno

*with new rainfall data
xi: reg lnprice L12.rain_cytot flood_max L12.day1rain L12.maxdays i.year i.mo
xi: reg lnprice L12.rain_cytot flood_max L12.day1rain L12.maxdays i.year i.mo i.ipczone
xi: reg q_ipc L12.rain_cytot flood_max L12.day1rain L12.maxdays i.year i.mo
xi: reg q_ipc lnprice L12.rain_cytot L12.day1rain L12.maxdays flood_max i.year i.mo

*including deviations
xi: reg lnprice L12.rain_cytot L12.pos_rain_ L12.neg_rain_ flood_max L12.day1rain L12.maxdays i.year i.mo

*including interactions with dryipczone
xi: reg lnprice L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo
xi: reg lnprice L12.rain_cytot L12.dryipc_rain L12.dryipc_max L12.dryipc_day1 flood_max L12.day1rain L12.maxdays i.year i.mo i.ipczone


******Take Aways*******
*rainfall matters for price: total rainfall, dry spells, late rains all raise prices
*contemporaneous high rainfall in floodprone areas also raise prices
*the effects are much stronger w/o the regional FE
*effects of low rainfall are worse for areas that are generally dry
*once one controls for these interactions, the ipczone FE produce consistent results
*Price strongly predicts IPC, but rainfall data are still informative (at least w/o regional FE)

**************************************************************
*Set for merge with LSMS data
************************************************************** 
sort ipczone yearmo
save "MalawiIPC.dta", replace
