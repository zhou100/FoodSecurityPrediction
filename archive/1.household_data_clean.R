####################################################################################################################################
# Goal : This script aims to clean up household lsms data, generate geocoordinates,  generate variables such as the asset index etc.
# purpose: 
# 1. generate variables at the household level to be used in the regression 
# 2. generate cluster geovariables used to extract weather and match with prices 

# Input : 
# 1. csv file of cleaned lsms household survey   dataset

# Output: 
# 0. cluster geovariables 
# 1. csv file of lsms merged with lhz FNID and small variables changes 
# 
# Yujun Zhou -  03/20/18
###################################################################
library(zoo)
library(dplyr)


#############################################
# collect the lat and lon from the household data 
#############################################
##################################################################
# Goal : retrieve geoordinates information for each 
# input : csv: cleaned aggreagate data for each country  
# output:  a list of cluster geoordinates for the given countries for each given yaer 

# Yujun Zhou -  03/22/18
###################################################################

library(dplyr)
library(readr)
Malawi_aggregate <- read_csv("data/clean/cleaned_dataset/Malawi_aggregate.csv")

#colnames(Malawi_aggregate)
mw.concord = Malawi_aggregate %>% dplyr::select(ea_id,lat_modified,lon_modified) %>% distinct() %>% na.omit()

write.csv(mw.concord,file="data/clean/concordance/Malawi_coord.csv",row.names = FALSE)



Tanzania_aggregate <- read_csv("data/clean/cleaned_dataset/Tanzania_aggregate.csv")

# colnames(Tanzania_aggregate)
tz.concord = Tanzania_aggregate %>% dplyr::select(ea_id,lat_modified,lon_modified) %>% distinct() %>% na.omit()

write.csv(tz.concord,file="data/clean/concordance/Tanzania_coord.csv")


Uganda_aggregate <- read_csv("data/clean/cleaned_dataset/Uganda_aggregate.csv")

# colnames(Tanzania_aggregate)
ug.concord = Uganda_aggregate %>% dplyr::select(ea_id,lat_modified,lon_modified) %>% distinct() %>% na.omit()

write.csv(ug.concord,file="data/clean/concordance/Uganda_coord.csv")


#############################################
# Spatially join the LHZ and get a concordance table using GIS
#############################################
library(dplyr)

mw.cluster.lhz = read.csv("data/clean/concordance/Malawi_ea_lhz_concordance.csv",stringsAsFactors = FALSE)
mw.cluster.lhz = mw.cluster.lhz %>% select(ea_id,FNID)
write.csv(mw.cluster.lhz,file="data/clean/concordance/mw_cluster_lhz.csv",row.names = FALSE)

tz.cluster.lhz = read.csv("data/clean/concordance/Tanzania_ea_lhz_concordance.csv",stringsAsFactors = FALSE)
tz.cluster.lhz = tz.cluster.lhz %>% select(ea_id,FNID)
write.csv(tz.cluster.lhz,file="data/clean/concordance/tz_cluster_lhz.csv",row.names = FALSE)

ug.cluster.lhz = read.csv("data/clean/concordance/Uganda_ea_lhz_concordance.csv",stringsAsFactors = FALSE)
ug.cluster.lhz = ug.cluster.lhz %>% select(ea_id,FNID)
write.csv(ug.cluster.lhz,file="data/clean/concordance/ug_cluster_lhz.csv",row.names = FALSE)




##################################################################
# Goal : 
# 1. transform numeric month to month names
# 2. deal with cateogorical variables 
###################################################################


##################################################################################################
#####  Clean Tanzania Data 
##################################################################################################
rm(list=ls())

Tanzania_aggregate = read.csv("data/clean/cleaned_dataset/Tanzania_aggregate.csv",stringsAsFactors = FALSE)


Tanzania_aggregate$Month<- month.name[Tanzania_aggregate$FS_month] 

## deal with nutrition variables 

Tanzania_aggregate$nutri_avail[Tanzania_aggregate$nutri_avail != "Severe Constraint" & Tanzania_aggregate$nutri_avail != "Moderate Constraint" ]=""
Tanzania_aggregate$nutri_rentention[Tanzania_aggregate$nutri_rentention != "Severe Constraint" & Tanzania_aggregate$nutri_rentention != "Moderate Constraint" ]=""

Tanzania_aggregate$terrain_rough =  as.character(Tanzania_aggregate$terrain_rough)
Tanzania_aggregate["dummy_terrain_rough"] <- ifelse(Tanzania_aggregate$terrain_rough=="Mid altitude mountains" & Tanzania_aggregate$terrain_rough=="Rugged lowlands" & Tanzania_aggregate$terrain_rough== "High-altitude plains", 1,0)


library(stats)
dummy_nutri_avail = model.matrix( ~ nutri_avail - 1, data=Tanzania_aggregate )
Tanzania_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,4]
Tanzania_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

dummy_nutri_rentention = model.matrix( ~ nutri_rentention - 1, data=Tanzania_aggregate )
Tanzania_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,5]
Tanzania_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

yearmon= paste(Tanzania_aggregate["FS_year"][,1],Tanzania_aggregate["FS_month"][,1], sep = "-")         
Tanzania_aggregate["yearmon"]= as.yearmon(yearmon,"%Y-%m")





# colnames(Tanzania_aggregate)
tz.asset = Tanzania_aggregate[3:15]
tz.asset[is.na(tz.asset)]=0
tz.asset.pca <- prcomp(na.omit(tz.asset),
                 center = TRUE,
                 scale. = TRUE) 

# summary(tz.asset.pca)

tz.asset.pca.df= as.data.frame(tz.asset.pca$x)

Tanzania_aggregate["asset_index"] = tz.asset.pca.df$PC1
  
Tanzania_aggregate = Tanzania_aggregate %>% select(-X, -lat_modified,-lon_modified,-region,-ward,-survey_round,-terrain_rough, -country,-nutri_avail,-nutri_rentention)  

Tanzania_aggregate = Tanzania_aggregate %>% distinct()

## merge with lhz FNID


tz_concordance <-  read.csv("data/clean/concordance/tz_cluster_lhz.csv")
tz_concordance =  tz_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

Tanzania_aggregate = Tanzania_aggregate  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
Tanzania_aggregate = dplyr::left_join(Tanzania_aggregate,tz_concordance)
 
 
write.csv(Tanzania_aggregate,"data/clean/tz_lsms.csv",row.names = FALSE)


##################################################################################################
##### Malawi hh data cleaning 
##################################################################################################
rm(list=ls())

Malawi_aggregate = read.csv("data/clean/cleaned_dataset/Malawi_aggregate.csv",stringsAsFactors = FALSE)
Malawi_aggregate$Month<- month.name[Malawi_aggregate$FS_month] 


##################################################################################################
##### deal with nutrition variables 
##################################################################################################

Malawi_aggregate$nutri_avail[Malawi_aggregate$nutri_avail != "Severe Constraint" & Malawi_aggregate$nutri_avail != "Moderate Constraint" ]=""
Malawi_aggregate$nutri_rentention[Malawi_aggregate$nutri_rentention != "Severe Constraint" & Malawi_aggregate$nutri_rentention != "Moderate Constraint" ]=""

Malawi_aggregate$terrain_rough =  as.character(Malawi_aggregate$terrain_rough)
Malawi_aggregate["dummy_terrain_rough"] <- ifelse(Malawi_aggregate$terrain_rough=="Mid altitude mountains" & Malawi_aggregate$terrain_rough=="Rugged lowlands" & Malawi_aggregate$terrain_rough== "High-altitude plains", 1,0)


library(stats)
dummy_nutri_avail = model.matrix( ~ nutri_avail - 1, data=Malawi_aggregate )
Malawi_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,4]
Malawi_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

dummy_nutri_rentention = model.matrix( ~ nutri_rentention - 1, data=Malawi_aggregate )
Malawi_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,5]
Malawi_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

yearmon= paste(Malawi_aggregate["FS_year"][,1],Malawi_aggregate["FS_month"][,1], sep = "-")         
Malawi_aggregate["yearmon"]= as.yearmon(yearmon,"%Y-%m")


colnames(Malawi_aggregate)
mw.asset = Malawi_aggregate[3:18]
mw.asset[is.na(mw.asset)]=0
mw.asset.pca <- prcomp(na.omit(mw.asset),
                       center = TRUE,
                       scale. = TRUE) 

#summary(mw.asset.pca)

mw.asset.pca.df= as.data.frame(mw.asset.pca$x)

Malawi_aggregate["asset_index"] = mw.asset.pca.df$PC1

Malawi_aggregate = Malawi_aggregate %>% select(-X, -lat_modified,-hh_wgt,-lon_modified,-region,-survey_round,-terrain_rough, -country,-nutri_avail,-nutri_rentention)  

Malawi_aggregate = Malawi_aggregate %>% distinct()

 
mw_concordance <-  read.csv("data/clean/concordance/mw_cluster_lhz.csv")
mw_concordance =  mw_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

Malawi_aggregate = Malawi_aggregate  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
Malawi_aggregate = dplyr::left_join(Malawi_aggregate,mw_concordance)

write.csv(Malawi_aggregate,"data/clean/mw_lsms.csv",row.names = FALSE)





##################################################################################################
##### Uganda hh data cleaning 
##################################################################################################

rm(list=ls())

Uganda_aggregate = read.csv("data/clean/cleaned_dataset/Uganda_aggregate.csv",stringsAsFactors = FALSE)
Uganda_aggregate$Month<- month.name[Uganda_aggregate$FS_month] 


##################################################################################################
##### deal with nutrition variables 
##################################################################################################


Uganda_aggregate$nutri_avail[Uganda_aggregate$nutri_avail != "Severe Constraint" & Uganda_aggregate$nutri_avail != "Moderate Constraint" ]=""
Uganda_aggregate$nutri_avail[is.na(Uganda_aggregate$nutri_avail)]=""

Uganda_aggregate$nutri_rentention[Uganda_aggregate$nutri_rentention != "Severe Constraint" & Uganda_aggregate$nutri_rentention != "Moderate Constraint" ]=""
Uganda_aggregate$nutri_rentention[is.na(Uganda_aggregate$nutri_rentention)]=""


Uganda_aggregate$terrain_rough =  as.character(Uganda_aggregate$terrain_rough)
Uganda_aggregate["dummy_terrain_rough"] <- ifelse(Uganda_aggregate$terrain_rough=="Mid altitude mountains" & Uganda_aggregate$terrain_rough=="Rugged lowlands" & Uganda_aggregate$terrain_rough== "High-altitude plains", 1,0)


library(stats)
dummy_nutri_avail = model.matrix( ~ nutri_avail - 1, data=Uganda_aggregate )
Uganda_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,3]
Uganda_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

dummy_nutri_rentention = model.matrix( ~ nutri_rentention - 1, data=Uganda_aggregate )
Uganda_aggregate["nutri_severe_constraint"] = dummy_nutri_avail[,3]
Uganda_aggregate["nutri_moderate_constraint"] = dummy_nutri_avail[,2]

yearmon= paste(Uganda_aggregate["FS_year"][,1],Uganda_aggregate["FS_month"][,1], sep = "-")         
Uganda_aggregate["yearmon"]= as.yearmon(yearmon,"%Y-%m")


colnames(Uganda_aggregate)
ug.asset = Uganda_aggregate[4:14]
ug.asset[is.na(ug.asset)]=0
ug.asset.pca <- prcomp(na.omit(ug.asset),
                       center = TRUE,
                       scale. = TRUE) 

#summary(ug.asset.pca)

ug.asset.pca.df= as.data.frame(ug.asset.pca$x)

Uganda_aggregate["asset_index"] = ug.asset.pca.df$PC1

Uganda_aggregate = Uganda_aggregate %>% dplyr::select(-X,-X.1, -lat_modified,-lon_modified,-region,-survey_round,-terrain_rough,-nutri_avail,-nutri_rentention)  

Uganda_aggregate = Uganda_aggregate %>% distinct()


########################################################

#######################################################

ug_concordance <-  read.csv("data/clean/concordance/ug_cluster_lhz.csv")
ug_concordance =  ug_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

Uganda_aggregate = Uganda_aggregate  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
Uganda_aggregate = dplyr::left_join(Uganda_aggregate,ug_concordance)

write.csv(Uganda_aggregate,"data/clean/ug_lsms.csv",row.names = FALSE)




