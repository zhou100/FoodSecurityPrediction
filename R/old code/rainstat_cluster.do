***generate rainfall variables
clear


cd "~/Box Sync/Research/Malawi_FewS/"
set more off

import delimited "CHIRPS_malawi_cluster_long.csv",clear
save "dailyrain_cluster.dta", replace
 
use "dailyrain_cluster.dta",clear
rename v1 obs 
rename date DATE
* reshape long x, i(DATE) j(clust)
sort clust DATE
by clust: gen day=_n
gen d_rain=1 if rain>0
recode d_rain (.=0)
gen norain=1 if d_rain==0
recode norain (.=0)
gen year=substr(DATE,1,4)
destring year, replace
gen mo=substr(DATE,6,2)
destring mo, replace
gen yearmo=year*100+mo

gen cropyr=1 if yearmo<200805
replace cropyr=2 if yearmo>200804&yearmo<200905
replace cropyr=3 if yearmo>200904&yearmo<201005
replace cropyr=4 if yearmo>201004&yearmo<201105
replace cropyr=5 if yearmo>201104&yearmo<201205
replace cropyr=6 if yearmo>201204&yearmo<201305
replace cropyr=7 if yearmo>201304&yearmo<201405
replace cropyr=8 if yearmo>201404&yearmo<201505
replace cropyr=9 if yearmo>201504&yearmo<201605
replace cropyr=10 if yearmo>201604

gen rainseason=1 if mo>9|mo<5
recode rainseason (.=0)
xtset clust day
gen Lnorain=L.norain
sort clust cropyr day
by clust cropyr: gen newrain=1 if Lnorain==1&norain==0
replace newrain=1 if mo>3&mo<10
bysort clust cropyr: gen sumnewrain=sum(newrain)


bysort clust cropyr sumnewrain: egen daysnorain=sum(norain)
bysort clust cropyr: egen maxdaysnorain=max(daysnorain)
label var maxdaysnorain "longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"

**generate first day of rain
sort clust day
gen fivedayrain=L4.rain+L3.rain+L2.rain+L1.rain+rain
sort clust year day
bysort clust year mo: gen daymo=_n
gen dayrseason=daymo if mo==10
replace dayrseason=daymo+31 if mo==11
replace dayrseason=daymo+61 if mo==12
replace dayrseason=daymo+92 if mo==1
replace dayrseason=daymo+92+28 if mo==2
recode dayrseason (.=0)


sort clust cropyr day
by clust cropyr: gen sumannrain=sum(rain)
by clust cropyr: egen annrain=max(sumannrain)
xtset clust day
gen day10rain=dayrseas if fivedayrain>10&(norain+L.norain+L2.norain+L3.norain+L4.norain<3)
recode day10rain (.=999)
recode day10rain (0=999)
bysort clust cropyr: egen day1rain=min(day10rain)

**check that things largely make sense**
* list clust year mo daymo rain norain fivedayrain day10rain day1rain if mo==10|mo==11

**Collapse to merge into IPC Malawi data

collapse (mean) annrain maxdaysnorain day1rain mo (sum) rain, by(clust yearmo)

label var annrain "total rainfall by cropyear"
label var day1rain "no days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
label var maxdaysnorain "longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
rename rain totrainmo
label var totrainmo "total rain that fell during the month"

sort clust yearmo

save "rainstat_cluster.dta", replace



use rainstat_cluster.dta,clear


gen rain_m10=totrainmo if mo==10
gen rain_m11=totrainmo if mo==11
gen rain_m12=totrainmo if mo==12
gen rain_m01=totrainmo if mo==1
gen rain_m02=totrainmo if mo==2
gen rain_m03=totrainmo if mo==3
gen rain_m04=totrainmo if mo==4
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

bysort clust cropyr: egen rain_Oct=sum(rain_m10)
bysort clust cropyr: egen rain_Nov=sum(rain_m11)
bysort clust cropyr: egen rain_Dec=sum(rain_m12)
bysort clust cropyr: egen rain_Jan=sum(rain_m01)
bysort clust cropyr: egen rain_Feb=sum(rain_m02)
bysort clust cropyr: egen rain_Mar=sum(rain_m03)
bysort clust cropyr: egen rain_Apr=sum(rain_m04)
gen rain_cytot=rain_Oct+rain_Nov+rain_Dec+rain_Jan+rain_Feb+rain_Mar+rain_Apr
drop rain_m*
label var rain_cytot "total rainfall from Oct to Apr by clust and cropyear"
gen rain_cy2=rain_cy^2



sort clust yearmo
bysort clust: gen dateno=_n
xtset clust dateno

gen L12rain_cytot=L12.rain_cytot
gen L12day1rain=L12.day1rain
gen L12maxdays=L12.maxdays


rename L12rain_cytot clust_L12raincytot
rename L12day1rain clust_L12day1rain
rename  L12maxdays clust_L12maxdays

keep clust yearmo clust_L12raincytot clust_L12day1rain clust_L12maxdays


sort clust yearmo
format clust %10.0g

 tostring clust, gen(clust_id4)
 rename clust clust_num
 rename clust_id4 clust


 save "rainstat_cluster_weather.dta", replace


