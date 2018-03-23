##################################################################
# Goal : retrieve geoordinates information for each 
# input : cleaned aggreagate data for each country  
# output:  a list of cluster geoordinates for the given countries for each given yaer 
# 
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

# head(Tanzania)

country_coord_list <- lapply(country_coord_list, function(x){
  x%>% dplyr::select(ea_id,lat_modified, lon_modified)
})


country_coord_list <- lapply(country_coord_list, function(x){
  x%>% dplyr::distinct()
})

country_name_list<- c("Tanzania_coord.csv","Uganda_coord.csv","Malawi_coord.csv")
path = "data/clean"

for (i in 1:3){
  write.csv(country_coord_list[[i]],file=paste(path,country_name_list[i],sep="/"))
}

Uganda<-read.csv("data/clean/cleaned_dataset/Uganda_aggregate.csv")

Uganda<- Uganda%>% 
  dplyr::select(case_id,lat_modified, lon_modified)

Uganda<-Uganda%>% 
  dplyr::distinct()

Uganda %>% 
  write.csv(file=paste(path,"Uganda_coord_hh.csv",sep="/"))
