******************************************************************
* Goal : aggreagate different years of data 
* input : dta files 
* output: dta and csv files  
* 
* Yujun Zhou -  03/20/18
*******************************************************************

cd "D:\lsms\cleaned_dataset "

******* Malawi *************
use FCS_2010_Malawi.dta,clear
append using FCS_2013_Malawi

save Malawi_aggregate.dta,replace  
append using FCS_2016_Malawi

gen country = "Malawi"
save Malawi_aggregate.dta,replace  
export delimited Malawi_aggregate.csv,replace 


******* Tanzania *************
cd "D:\lsms\cleaned_dataset "

use FCS_2010_Tanzania.dta,clear
append using FCS_2012_Tanzania
save Tanzania_aggregate.dta,replace  
append using FCS_2014_Tanzania
save Tanzania_aggregate.dta,replace  

gen country = "Tanzania"
save Tanzania_aggregate.dta,replace  
export delimited Tanzania_aggregate.csv ,replace 




******* Uganda *************
cd "D:\lsms\cleaned_dataset "

use FCS_2011_Uganda.dta,clear
append using FCS_2010_Uganda
save Uganda_aggregate.dta,replace  
append using FCS_2009_Uganda
gen country = "Uganda"

save Uganda_aggregate.dta,replace  
export delimited Uganda_aggregate.csv ,replace 
