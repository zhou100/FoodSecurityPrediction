####################################################################################################################################
# Goal : This script aims to clean up household lsms data, generate geocoordinates,  generate variables such as the asset index etc.
# purpose: 
# 1. generate variables at the household level to be used in the regression 
# 2. generate cluster geovariables used to extract weather and match with prices 

# Input : 
# 1. csv file of cleaned lsms household survey   dataset

# Output: 
# 0. cluster geovariables 
# 
# Yujun Zhou -  03/20/18
###################################################################


##################################################################
# Goal : transform numeric month to month names
# this somehow helps with the generating the date variable 
###################################################################
csv_file_list <-list.files(path = "data/clean/cleaned_dataset",pattern =  ".csv",full.names = TRUE)
# csv_file_list

pattern = "data/clean/cleaned_dataset/|.csv"

# read in the csv files and set each of them as variables 
list2env(
  lapply(setNames(csv_file_list, make.names(gsub(pattern, "", csv_file_list))), 
         read.csv), envir = .GlobalEnv)

Malawi_aggregate$Month<- month.name[Malawi_aggregate$FS_month] 
Tanzania_aggregate$Month<- month.name[Tanzania_aggregate$FS_month] 
Uganda_aggregate$Month<- month.name[Uganda_aggregate$FS_month] 


path = "data/clean/cleaned_dataset" 
write.csv(Malawi_aggregate,paste(path,"Malawi_aggregate.csv",sep = "/"))
write.csv(Tanzania_aggregate,paste(path,"Tanzania_aggregate.csv",sep = "/"))
write.csv(Uganda_aggregate,paste(path,"Uganda_aggregate.csv",sep = "/"))


##################################################################
# Goal : retrieve geoordinates information for each 
# input : csv: cleaned aggreagate data for each country  
# output:  a list of cluster geoordinates for the given countries for each given yaer 

# Yujun Zhou -  03/22/18
###################################################################

library(dplyr)
Tanzania<-read.csv("data/clean/cleaned_dataset/Tanzania_aggregate.csv")
Uganda<-read.csv("data/clean/cleaned_dataset/Uganda_aggregate.csv")
Malawi<-read.csv("data/clean/cleaned_dataset/Malawi_aggregate.csv")

Malawi = Malawi %>% 
  dplyr::filter(FS_year >2014)

# subset columns
country_coord_list<-list(Tanzania,Uganda,Malawi)

# cluter id and geocoordiantes 
country_coord_list <- lapply(country_coord_list, function(x){
  x%>% dplyr::select(ea_id,lat_modified, lon_modified)
})

# remove the duplicates in clusters  
country_coord_list <- lapply(country_coord_list, function(x){
  x%>% dplyr::distinct()
})

country_name_list<- c("Tanzania_coord.csv","Uganda_coord.csv","Malawi_coord.csv")
path = "data/clean"

for (i in 1:3){
  write.csv(country_coord_list[[i]],file=paste(path,country_name_list[i],sep="/"))
}


