cd "/Users/yujunzhou/Box Sync/Research/data_wrangle_malawi"

************************************************************************
***** The purpose of this code is to generate lagged price measures for each cluster
*** in 2013 ******
************************************************************

import delimited "clust_thinness_2013.csv",clear varnames(1)
drop v1 
sort ea_id yearmo 
destring yearmo, force replace 
duplicates drop ea_id yearmo,force
save "~/Box Sync/Research/Malawi_FewS/clust_thin_2013.dta",replace



collapse  mkt_thinn,by (yearmo near_mkt)
 encode near_mkt, generate(mkt_code) 
 xtset  mkt_code yearmo
by mkt_code :gen Lmkt_thinn=mkt_thinn[_n-1]
save "~/Box Sync/Research/Malawi_FewS/clust_thin_lag_2013.dta",replace


use "~/Box Sync/Research/Malawi_FewS/clust_thin_2013.dta",clear
merge  m:1 near_mkt yearmo using "~/Box Sync/Research/Malawi_FewS/clust_thin_lag_2013.dta"
drop _merge mkt_code
save "~/Box Sync/Research/Malawi_FewS/clust_thin_new_2013.dta",replace 


import delimited "clust_price_2013.csv",clear varnames(1)
drop v1 
sort ea_id yearmo 
destring yearmo price lnprice_impu, force replace 
duplicates drop ea_id yearmo,force
save "~/Box Sync/Research/Malawi_FewS/clust_price_2013.dta",replace 


collapse price lnprice_impu,by (yearmo near_mkt)
encode near_mkt, generate(mkt_code) 
xtset  mkt_code yearmo
by mkt_code :gen Lprice=price[_n-1]
by mkt_code :gen Llnprice_impu=lnprice_impu[_n-1]
save "~/Box Sync/Research/Malawi_FewS/clust_price_lag_2013.dta",replace


use "~/Box Sync/Research/Malawi_FewS/clust_price_2013.dta",clear
merge  m:1 near_mkt yearmo using "~/Box Sync/Research/Malawi_FewS/clust_price_lag_2013.dta"
drop _merge mkt_code
save "~/Box Sync/Research/Malawi_FewS/clust_price_2013_new.dta",replace 