##########
# Check for missings in the data, merge multiple years 
rm(list=ls())

library(tidyverse)

# Malawi 
mw10_lsms = read_csv("data/clean/household/mw10_hh.csv")
mw10_lsms = mw10_lsms %>% dplyr::select(-case_id) %>% mutate(HHID = as.character(HHID))


colSums(is.na(mw10_lsms))

mw13_lsms = read_csv("data/clean/household/mw13_hh.csv")

# mw13_lsms = mw13_lsms %>% dplyr::select(ea_id,FS_year) %>% unique()

mw13_lsms = mw13_lsms %>% 
  filter(!is.na(female_head)) %>%
  filter(!is.na(rCSI)) 

colSums(is.na(mw13_lsms))


mw16_lsms = read_csv("data/clean/household/mw16_hh.csv")
mw16_lsms = mw16_lsms %>% filter(!is.na(lon_modified)) %>% filter(!is.na(ea_id))
colSums(is.na(mw16_lsms))



mw_lsms  = bind_rows( mw10_lsms,mw13_lsms ) %>% mutate(HHID = as.character(HHID))


mw_lsms  = bind_rows( mw_lsms,mw16_lsms ) %>% dplyr::select(-case_id)

write.csv(mw_lsms,"data/clean/household/mw_hh_aggregate.csv",row.names = FALSE)


# export coordinates  

# note that there are more lat lons than ea_ids due to random errors, but the outcomes should be averaged at the ea level



# duplicated_lat_lons = mw_coordinates %>% group_by(ea_id,FS_year) %>% summarize(n=n()) %>% arrange(desc(n)) %>% dplyr::filter(n>1) %>% dplyr::select(ea_id)
# duplicated_lat_lons
# 
# duplicated_ea_lat_lons= mw_coordinates %>% filter(ea_id %in% duplicated_lat_lons$ea_id) %>% arrange(ea_id,FS_year)

mw_coordinates = mw_lsms %>% dplyr::select(ea_id,FS_year,lat_modified,lon_modified) %>% unique()
write.csv(mw_coordinates,"data/clean/concordance/mw_coordiantes.csv",row.names = FALSE)

 
# Tanzania  

tz10_lsms = read_csv("data/clean/household/tz10_hh.csv")
tz10_lsms = tz10_lsms %>% na.omit()

tz12_lsms = read_csv("data/clean/household/tz12_hh.csv")
tz12_lsms = tz12_lsms %>% na.omit()

tz14_lsms = read_csv("data/clean/household/tz14_hh.csv")
tz14_lsms  = tz14_lsms %>% na.omit()

tz_lsms  = bind_rows( tz10_lsms,tz12_lsms )
tz_lsms  = bind_rows( tz_lsms,tz14_lsms )

colSums(is.na(tz_lsms))

table(tz_lsms[is.na(tz_lsms$dist_popcenter),]$FS_year)

tz_lsms = tz_lsms %>% dplyr::select(-region22,-region23,-region24,-region25) %>% mutate(ea_id = clusterid) %>% dplyr::select(-clusterid)

write.csv(tz_lsms,"data/clean/household/tz_hh_aggregate.csv",row.names = FALSE)


tz_coordinates = tz_lsms %>% dplyr::select(ea_id,FS_year,lat_modified,lon_modified) %>% unique()

dim(tz_coordinates)


write.csv(tz_coordinates,"data/clean/concordance/tz_coordiantes.csv",row.names = FALSE)

# Uganda  

ug09_lsms = read.csv("data/clean/household/ug09_hh.csv")
ug09_lsms = ug09_lsms %>% na.omit()

# colSums(is.na(ug10_lsms))



ug10_lsms = read.csv("data/clean/household/ug10_hh.csv")
ug10_lsms = ug10_lsms %>% na.omit()

ug11_lsms = read_csv("data/clean/household/ug11_hh.csv" )
ug11_lsms = ug11_lsms %>% na.omit()

# colSums(is.na(ug_lsms))

ug_lsms  = bind_rows( ug10_lsms,ug09_lsms )
ug_lsms  = bind_rows( ug_lsms,ug11_lsms )

 
write.csv(ug_lsms,"data/clean/household/ug_hh_aggregate.csv",row.names = FALSE)


ug_coordinates = ug_lsms %>% dplyr::select(ea_id,FS_year,lat_modified,lon_modified) %>% unique()
dim(ug_coordinates)

dim(ug_lsms %>% dplyr::select(ea_id,FS_year)%>% unique())

write.csv(ug_coordinates,"data/clean/concordance/ug_coordiantes.csv",row.names = FALSE)

# 
# 
# ug_coordinates$country = "ug"
# tz_coordinates$country = "tz"
# mw_coordinates$country = "mw"
# 
# union = mw_coordinates %>% dplyr::union(tz_coordinates) %>% dplyr::union(ug_coordinates)
# 
# write.csv(union,"data/clean/concordance/union_cluster.csv",row.names = FALSE)






