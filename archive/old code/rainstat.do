***generate rainfall variables
clear
import delimited C:\Users\baylis\Dropbox\Current\Development\Hope\dailyrain2.csv
save "C:\Users\baylis\Dropbox\Current\Development\Hope\dailyrain.dta", replace
 
rename v1 obs 
rename date DATE
reshape long v, i(DATE) j(ipczone)
replace ipczone=ipczone-1
sort ipczone DATE
by ipczone: gen day=_n
gen d_rain=1 if v>0
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
xtset ipczone day
gen Lnorain=L.norain
sort ipczone cropyr day
by ipczone cropyr: gen newrain=1 if Lnorain==1&norain==0
replace newrain=1 if mo>3&mo<10
bysort ipczone cropyr: gen sumnewrain=sum(newrain)


bysort ipczone cropyr sumnewrain: egen daysnorain=sum(norain)
bysort ipczone cropyr: egen maxdaysnorain=max(daysnorain)
label var maxdaysnorain "longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"

**generate first day of rain
sort ipczone day
gen fivedayrain=L4.v+L3.v+L2.v+L1.v+v
sort ipczone year day
bysort ipczone year mo: gen daymo=_n
gen dayrseason=daymo if mo==10
replace dayrseason=daymo+31 if mo==11
replace dayrseason=daymo+61 if mo==12
replace dayrseason=daymo+92 if mo==1
replace dayrseason=daymo+92+28 if mo==2
recode dayrseason (.=0)


sort ipczone cropyr day
by ipczone cropyr: gen sumannrain=sum(v)
by ipczone cropyr: egen annrain=max(sumannrain)
xtset ipczone day
gen day10rain=dayrseas if fivedayrain>10&(norain+L.norain+L2.norain+L3.norain+L4.norain<3)
recode day10rain (.=999)
recode day10rain (0=999)
bysort ipczone cropyr: egen day1rain=min(day10rain)

**check that things largely make sense**
list ipczone year mo daymo v norain threedayrain day10rain day1rain if mo==10|mo==11

**Collapse to merge into IPC Malawi data

collapse (mean) annrain maxdaysnorain day1rain (sum) v, by(ipczone yearmo)

label var annrain "total rainfall by cropyear"
label var day1rain "no days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
label var maxdaysnorain "longest dry spell during the rainy season (Oct-Mar) per crop year (May-Apr)"
rename v totrainmo
label var totrainmo "total rain that fell during the month"

sort ipczone yearmo

save "rainstat.dta", replace


