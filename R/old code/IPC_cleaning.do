 use "/Users/mbprouser/Data/IPC/data/MalawiIPC_clean_oldstata.dta"
 
 
replace chirps_max_wsum = " " if chirps_max_wsum == "NA"
replace chirps_min_wsum = " " if chirps_min_wsum == "NA"
replace chirps_min = " " if chirps_min == "NA"
replace chirps_max = " " if chirps_max == "NA"

foreach var in chirps_min chirps_max  chirps_max_wsum chirps_min_wsum price_mean price_w {
destring `var', replace 
}


	egen month = ends(date), punct(-) trim last
				replace month = " " if month == "NA"
				destring month, replace
*generate quarterly values

	
	gen quarter = .
	replace quarter = 1 if month ==1 | month == 2 | month == 3
	replace quarter = 2 if month == 4 | month ==5 | month == 6
	replace quarter = 3 if month == 7 | month ==8 | month ==9
	replace quarter = 4 if month == 10 | month ==11 | month ==12
	
	*gen yearmo = (year*100)+ month
	
	gen ipczone_qtr = " "
	gen v = ipczone
				replace ipczone_qtr=substr(v, -2, 2)
				
				replace ipczone_qtr = " " if v == "NA"
				replace ipczone_qtr = substr(v, -1, 2) if v == "V1" | v=="V2" | v == "V3" | v== "V4" | v=="V5" /*
				*/ |v == "V6" | v=="V7" | v=="V8" | v=="V9"
				
				destring ipczone_qtr, replace
	sort ipczone_qtr yearmo
	bysort ipczone_qtr: gen date_qtr=_n
	
	xtset ipczone_qtr date_qtr
	gen ipc_qtr = ipc
	bysort ipczone_qtr year quarter: gen lag1 = ipc[_n-1]
	replace ipc_qtr = lag1 if ipc_qtr == .
	
	bysort ipczone_qtr year quarter: gen lag2 = ipc[_n-2]
	replace ipc_qtr = lag2 if ipc == .
	
	**fill backwards by a month for quarters when assessments occured in FEB**
	gen ipc_qtr_leads = ipc_qtr
	bysort ipczone_qtr year quarter: gen lead1 = ipc_qtr[_n+1]
	replace ipc_qtr = lead1 if ipc_qtr == . 
	drop lag1 lag2 lead1 ipc_qtr v date_qtr
	rename ipc_qtr_leads ipc_qtr
	rename ipczone_qtr ipczone_numeric
	
	save "/Users/mbprouser/Data/IPC/data/MalawiIPC_clean_v2.dta", replace
	
	
	
	*****
*merge in market thinness values.	
*use FNID to link to market


use "/Users/mbprouser/Data/IPC/data/MalawiIPC_clean_v2.dta"



use "/Users/mbprouser/Data/Malawi rCSI/Prices/201705MonthlyPrices_thinness.dta"
merge m:m  mkt_name using "/Users/mbprouser/Data/IPC/FNID_full.dta"

save "/Users/mbprouser/Data/Malawi rCSI/Prices/thinness_measure.dta", replace
