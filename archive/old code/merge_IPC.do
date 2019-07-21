cd "/Users/yujunzhou/Box Sync/Research/Malawi_FewS"

/*
*(1) assign each household observation (case_id is the unique identifier) to an IPC livelihood zone - using the variables: lat_modified lon_modified
import delimited hhid.csv, encoding(ISO-8859-1) clear

save IPCzone.dta,replace

use FCS.dta,clear
destring case_id,replace
merge 1:1 case_id using IPCzone.dta

drop _merge 
save FCS_IPC.dta,replace
*keep case_id fnid 
*save link_FCS.dta,replace */

* (2) make the month/year maps using the variables FS_month FS_year. 
* You should have 13 maps for each bullet point below as there is both a March 2010 and a March 2011. 
set more off
use FCS_IPC.dta,clear
gen yearmon = FS_year *100 +FS_month
*tab yearmon


levelsof yearmon, local(levels) 
 foreach l of local levels {
 preserve
keep if yearmon == `l'
collapse (mean) RCSI,by(fnid)
rename RCSI RCSI_`l'
export delimited using "Collapsed/RCSI/`l'_RCSI.csv", replace
*save "Collapsed/`l'_whole.dta",replace
restore
  }



levelsof yearmon, local(levels) 
 foreach l of local levels {
 preserve
keep if yearmon == `l'
collapse (mean) FCS,by(fnid)
rename FCS FCS_`l'
export delimited using "Collapsed/FCS/`l'_FCS.csv", replace
*save "Collapsed/`l'_whole.dta",replace
restore
  }




use FCS_IPC.dta,clear
gen yearmon = FS_year *100 +FS_month
keep if reside==2
save FCS_IPC_rural.dta,replace

use FCS_IPC_rural.dta,clear
levelsof yearmon, local(levels) 
foreach l of local levels {
 preserve
keep if yearmon == `l'
collapse (mean) RCSI,by(fnid)
rename RCSI RCSI_`l'
export delimited using "Collapsed/RCSI_rural/`l'_RCSI_rural.csv", replace
restore
  }

use FCS_IPC_rural.dta,clear
levelsof yearmon, local(levels) 
 foreach l of local levels {
 preserve
keep if yearmon == `l'
collapse (mean) FCS,by(fnid)
rename FCS FCS_`l'
export delimited using "Collapsed/FCS_rural/`l'_FCS_rural.csv", replace
restore
  }

* Clean the IPC data and generate by yearmon
/*
use ipc_2010.dta,clear
drop if ipc_qtr_leads == 88
drop if ipc_qtr_leads == 99


levelsof yearmo, local(levels) 

foreach l of local levels { 	
preserve
keep if yearmo == `l'
rename ipc_qtr_leads ipc_value_`l'
keep fnid ipc_value_`l' 
export delimited using "Collapsed/ipcvalue/`l'_ipc_value.csv", replace
restore
}



import delimited using "Collapsed/IPC_value",clear
duplicates drop *,force

export delimited using "Collapsed/IPC_value_adjusted.csv", replace

// For the maps: 
*Plot the mean RCSI by IPC Zone for all households whose EA lat/long coordinates fall in the zone, 
* color coding the edge of the zone to indicate the IPC classification for that quarter (13 maps)
* As above, but use FCS instead of RCSI (13 maps)
* Redo the RCSI, but only for rural households (reside==2) (13 maps)

*/


