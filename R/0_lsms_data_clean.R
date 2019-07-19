rm(list=ls())
library(tidyverse)
library(haven)

# Malawi 
########################################################
# Malawi 2010
########################################################
path = "data/raw/lsms/Malawi_2010/"

mw10.food <- read_dta(file = paste(path,"Household_Batch3of5_DTA/HH_MOD_G2.dta",sep = "" ) )
head(mw10.food)
  
mw10.food.category.days = mw10.food %>% 
  # Recoding of outliers (change 8 days to 7 days)
  mutate( hh_g08c  = if_else(hh_g08c>7,7,hh_g08c) ) %>%
  #Combining Cereals and roots (Category A and Category B)
  mutate(hh_g08a = if_else(hh_g08a=="A" | hh_g08a=="B","AB",hh_g08a) ) %>%
  # combine A and B 
  group_by(case_id,ea_id,hh_g08a) %>%
  summarise( hh_g08c =max (hh_g08c))
  
# create a mapping for weights
################################################################################
#NOTES ON THE WEIGHTS OF THE DIFFERENT FOOD CATEGORIES
# A Cereals, Grains and Cereal Products: Weight = 2
# B Root, Tubers and Plantains: Weight = 2
# C Nuts and Pulses: Weight = 3
# D Vegetables: Weight = 1
# E Meat, Fish and Animal Products: Weight = 4
# F Fruits => weight = 1
# G Milk/Milk Products: Weight = 4
# H Fats/Oil => Weight = 0.5
# I Sugar/Sugar Products/Honey: Weight = 0.5
# J Spices/Condiments: Weight = 0
################################################################################
FWeight.mapping= read.table(text = "
category FWeight 
                            AB 2
                            C 3
                            D 1
                            F 1
                            E 4
                            G 4
                            H 0.5
                            I 0.5
                            J 0", header = TRUE)

# calculate FCS 
FCS = mw10.food.category.days %>%
  mutate(
    # find the weight from 
    FWeight = with(FWeight.mapping,FWeight[hh_g08a %in% category])
  ) %>%
  mutate( FCS = hh_g08c * FWeight ) %>% 
  group_by(case_id,ea_id) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

# unique(FCS$FCS)

FCS = FCS %>% filter(FCS!=0)
# sum(is.na(FCS$FCS)  )


mw10.merged = FCS
################################################################################
  #A diet diversity score is a household-measure of food security that captures ///
  #something about the quality of a diet. It is calculated by counting the number///
  #of foods or food groups from which a household acquired food over the survey ///
  #reference period (24 hours). 
################################################################################

HDDS = mw10.food %>% 
  #Exclude SUGAR and SPICES
  filter(hh_g08a!="I" & hh_g08a!="J") %>%
  filter(  !is.na(hh_g08c) ) %>%
  mutate( HDDS = if_else( hh_g08c>=1 ,1,0 )  ) %>%
  group_by(case_id,ea_id) %>%
  summarise(HDDS = sum(HDDS,na.rm = TRUE))

mw10.merged = left_join(mw10.merged,HDDS, by=c("ea_id","case_id"))
#________________________________________________________________________

# Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
#_______________________________________________________________________________


#In the past 7 days, did you worry that your household would not have enough food
#hh_h02a: "In the past 7 days, how many days have you or someone in your ///
#household had to: Rely on less preferred and/or less expensive foods?"(WGT1)
#hh_h02b: "In the past 7 days, how many days have you or someone in your ///
#household had to: Limit portion size at mealtimes?" (WGT1)
#hh_h02c: "In the past 7 days, how many days have you or someone in your ///
#household had to: Reduce number of meals eaten in a day?" (WGT2)
# hh_h02d "In the past 7 days, how many days have you or someone in your ///
#household had to: Restrict consumption by adults in order for small ///
#children to eat?" (WGT2)
#hh_h02e "In the past 7 days, how many days have you or someone in your ///
#household had to: Borrow food, or rely on help from a friend or ///
#relative?" (WGT2)

mw10.rcsi <- read_dta(file = paste(path,"Household_Batch3of5_DTA/HH_MOD_H.dta",sep = "" ) )

##Constructing rCSI
rCSI = mw10.rcsi %>%
  mutate(rCSI = 1*hh_h02a + 1*hh_h02b + 2*hh_h02c + 2*hh_h02d +2*hh_h02e ) %>%
  mutate(rCSI = if_else(hh_h01==2 , 0 ,rCSI) ) %>%
  select(case_id,ea_id,rCSI)

mw10.merged = left_join(mw10.merged,rCSI, by=c("ea_id","case_id"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
mw10.region<- read_dta(file = paste(path,"Household_Batch1of5_DTA/HH_MOD_A_FILT.dta",sep = "" ) )

# region 1 "Northern" 2 "Central" 3 "Southern"
mw10.region.rural = mw10.region %>% 
  select(case_id,ea_id,reside,hh_wgt,hh_a01,hh_a02b) %>%
  mutate (region = case_when(
    hh_a01 %% 100 == 1 ~ "Northern",
    hh_a01 %% 100 == 2 ~ "Central",
    hh_a01 %% 100 == 3 ~ "Southern"
  )) %>% 
  mutate( region_north = if_else(region=="Northern",1,0)) %>%
  mutate( region_central = if_else(region=="Central",1,0)) %>%
  #  1 urban 2 rural
  mutate( rural = case_when(
    reside == 1 ~ 0,
    reside == 2 ~ 1)
    ) %>%
  select(case_id,ea_id,region_north,region_central,rural)
  
# mw10.region$reside["Labels"]
# sum(is.na(mw10.basic.region$region_num))
 
mw10.merged = left_join(mw10.merged,mw10.region.rural, by=c("ea_id","case_id"))


# Merge in geolocation 

mw10.geo <- read_dta(file = paste(path,"HouseholdGeovariables.dta",sep = "" ) )

mw10.geo.clean = mw10.geo %>%
  mutate(elevation =  srtm_eaf) %>%
  mutate(percent_ag =  case_when(
    fsrad3_lcmaj == 20 ~ 0.6,
    fsrad3_lcmaj == 30 ~ 0.6,
    fsrad3_lcmaj == 14 ~ 1,
    is.na(fsrad3_lcmaj) ~ 0)
  ) %>% 
  mutate(percent_ag =  case_when(
    fsrad3_lcmaj == 20 ~ 0.6,
    fsrad3_lcmaj == 30 ~ 0.6,
    fsrad3_lcmaj == 14 ~ 1,
    is.na(fsrad3_lcmaj) ~ 0)
  ) %>% 
  #  3       Severe Constraint 
  # 2     Moderate Constraint
  mutate(sq1= as.numeric(sq1))  %>%
  mutate(sq2= as.numeric(sq2))  %>%
  mutate(nutri_avail = if_else(sq1 != 3 & sq1 != 2 ,0,sq1) )  %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail==3,1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail==2,1,0) ) %>% 
  mutate(nutri_rentention = if_else(sq2 != 3 & sq2 != 2,0,sq2))  %>%
  mutate(nutri_reten_severe_constraint=if_else(nutri_rentention==3,1,0) ) %>%
  mutate(nutri_reten_moderate_constraint=if_else(nutri_rentention==2,1,0) ) %>% 
  
  mutate(dummy_terrain_rough = if_else(srtm_eaf_5_15=="Mid altitude mountains" 
                                       & srtm_eaf_5_15=="Rugged lowlands" & 
                                         srtm_eaf_5_15== "High-altitude plains", 1,0 ))   %>%
  mutate(slope = afmnslp_pct)  %>% 
  
  select(case_id,ea_id,lat_modified,lon_modified, dist_road,dist_admarc,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough,slope
         )


# mw10.geo$sq1

mw10.merged = left_join(mw10.merged,mw10.geo.clean, by=c("ea_id","case_id"))


# Merge in the basic information

mw10.basic <- read_dta(file = paste(path,"ihs3_summary.dta",sep = "" ) )

mw.hhhead = mw10.basic %>%
  mutate(FS_month = as.numeric(intmonth) ) %>%
  mutate(head_gender = as.numeric(head_gender)) %>% 
  mutate(female_head = if_else(head_gender==2, 1, 0) )  %>%
  mutate(head_edu  = as.numeric(head_edlevel)) %>% 
  mutate(head_educated = if_else(head_edu!=1, 1, 0) ) %>% 
  mutate(FS_year = as.numeric(intyear)) %>%
  select(case_id,ea_id,FS_month,FS_year,head_age, female_head, head_educated, hhsize)
 
# mw10.basic$head_edlevel
#1   Male 2 Female

# mw10.basic$head_edlevel

# table(mw10.basic$head_edlevel)
# 1      None
# 2   Primary
# 3 Secondary
# 4  Tertiary

mw10.merged = left_join(mw10.merged,mw.hhhead, by=c("ea_id","case_id"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone , roof/floor


# mw10.asset$hh_f09
# value           label
# 1            Sand
# 2    Smoothed mud
# 3   Smooth cement
# 4            Wood
# 5            Tile
# 6 Other (Specify)

# mw10.asset$hh_f08
# 1            Grass
# 2      Iron sheets
# 3       Clay tiles
# 4         Concrete
# 5 Plastic sheeting
# 6  Other (specify)

mw10.asset <- read_dta(file = paste(path,"Household_Batch1of5_DTA/HH_MOD_F.dta",sep = "" ) )

mw10.asset.cell.floor = mw10.asset %>%
  mutate (hh_f09 = as.numeric(hh_f09)) %>%
  ## floor ###
  mutate( floor_dirt_sand_dung = case_when(
    hh_f09 == 1 ~ 1,
    hh_f09 == 2 ~ 1,
    hh_f09 == 3 ~ 0, 
    hh_f09 == 4 ~ 0,
    hh_f09 == 5 ~ 0, 
    hh_f09 == 6 ~ 0)) %>%
  mutate (hh_f08 = as.numeric(hh_f08)) %>% 
  ## roof ###
  mutate( roof_not_natural = if_else( hh_f08 != 1 ,1,0)) %>%
  mutate( roof_iron = if_else( hh_f08 == 2 ,1,0)) %>%
  ## cellphone ###
  mutate (number_celphones = as.numeric(hh_f34)) %>% 
  mutate (cell_phone = if_else(number_celphones>0,1,0)) %>% 
  select(case_id,ea_id,floor_dirt_sand_dung,cell_phone,
         number_celphones,roof_not_natural,roof_iron)

mw10.merged = left_join(mw10.merged,mw10.asset.cell.floor, by=c("ea_id","case_id"))

 #   mw10.asset$hh_f34
  
## merge in the other assets

# mw10.other.asset$hh_l02
# 507                Radio ('wireless')
# 509                        Television
# 514                      Refrigerator
# 516                           Bicycle
# 517                Motorcycle/scooter
# 518                               Car

mw10.other.asset <- read_dta(file = paste(path,"Household_Batch4of5_DTA/HH_MOD_L.dta",sep = "" ) )

frige = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_frige = sum (hh_l03[hh_l02  ==514],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(case_id,Refrigerator)

radio = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_radio = sum (hh_l03[hh_l02  ==507],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(case_id,Radio)

tv = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_tv = sum (hh_l03[hh_l02  ==509],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(case_id,Television)

 
bike = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_bike = sum (hh_l03[hh_l02  ==516],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(case_id,Bicycle) 

moto = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_moto= sum (hh_l03[hh_l02  ==517],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(case_id,Motorcycle) 
 
car = mw10.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(case_id) %>%
  summarise( num_car= sum (hh_l03[hh_l02  ==518],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car >0,1,0) ) %>%
  select(case_id,Car) 
 
mw10.asset.clean = left_join(frige,radio, by="case_id")
mw10.asset.clean = left_join(mw10.asset.clean,tv, by="case_id")
mw10.asset.clean = left_join(mw10.asset.clean,bike, by="case_id")
mw10.asset.clean = left_join(mw10.asset.clean,moto, by="case_id")
mw10.asset.clean = left_join(mw10.asset.clean,car, by="case_id")


case_id = mw10.asset.clean %>% select(case_id)
mw10.asset.pca =   mw10.asset.clean %>% select(-case_id)
#  generate asset index from the dummies 


asset.pca <- prcomp(mw10.asset.pca,
                 center = TRUE,
                 scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

mw10.asset.index = bind_cols(case_id,asset_index)
#summary(asset.pca)



mw10.merged = left_join(mw10.merged,mw10.asset.index, by=c("case_id"))

min(mw10.merged$HDDS)

write.csv(mw10.merged,"data/clean/household/mw10_hh.csv",row.names = FALSE)


 
########################################################
# Malawi 2013
########################################################
path = "data/raw/lsms/Malawi_2013/MWI_2013_IHPS_v01_M_STATA/"

mw13.food <- read_dta(file = paste(path,"Household/HH_MOD_G2.dta",sep = "" ) )
head(mw13.food)

# mw13.food$hh_g08a

mw13.food.category.days = mw13.food %>% 
  # Recoding of outliers (change 8 days to 7 days)
  mutate(hh_g08a = as.numeric(hh_g08a)) %>% 
  mutate( hh_g08c  = if_else(hh_g08c>7,7,hh_g08c) ) %>%
  #Combining Cereals and roots (Category A and Category B)
  mutate(hh_g08a = if_else(hh_g08a==1 | hh_g08a==2,1,hh_g08a) ) %>%
  # combine A and B 
  group_by(y2_hhid,hh_g08a) %>%
  summarise( hh_g08c =max (hh_g08c,na.rm = TRUE))

# create a mapping for weights
################################################################################
#NOTES ON THE WEIGHTS OF THE DIFFERENT FOOD CATEGORIES
# A Cereals, Grains and Cereal Products: Weight = 2
# B Root, Tubers and Plantains: Weight = 2
# C Nuts and Pulses: Weight = 3
# D Vegetables: Weight = 1
# E Meat, Fish and Animal Products: Weight = 4
# F Fruits => weight = 1
# G Milk/Milk Products: Weight = 4
# H Fats/Oil => Weight = 0.5
# I Sugar/Sugar Products/Honey: Weight = 0.5
# J Spices/Condiments: Weight = 0
################################################################################
FWeight.mapping= read.table(text = "
                            category FWeight 
                            1 2
                            3 3
                            4 1
                            6 1
                            5 4
                            7 4
                            8 0.5
                            9 0.5
                            10 0", header = TRUE)

# calculate FCS 
FCS = mw13.food.category.days %>%
  mutate(
    # find the weight from 
    FWeight = with(FWeight.mapping,FWeight[hh_g08a %in% category])
  ) %>%
  mutate( FCS = hh_g08c * FWeight ) %>% 
  group_by(y2_hhid) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

# unique(FCS$FCS)

FCS = FCS %>% filter(FCS!=0)
# sum(is.na(FCS$FCS)  )


mw13.merged = FCS
################################################################################
#A diet diversity score is a household-measure of food security that captures ///
#something about the quality of a diet. It is calculated by counting the number///
#of foods or food groups from which a household acquired food over the survey ///
#reference period (24 hours). 
################################################################################

HDDS = mw13.food %>% 
  #Exclude SUGAR and SPICES
  filter(hh_g08a!=9 & hh_g08a!=10) %>%
  filter(  !is.na(hh_g08c) ) %>%
  mutate( HDDS = if_else( hh_g08c>=1 ,1,0 )  ) %>%
  group_by(y2_hhid) %>%
  summarise(HDDS = sum(HDDS,na.rm = TRUE))

mw13.merged = left_join(mw13.merged,HDDS, by=c("y2_hhid"))
#________________________________________________________________________

# Q3. REDUCED COPING STRATEGIES INDEX (rCSI)
#_______________________________________________________________________________


#In the past 7 days, did you worry that your household would not have enough food
#hh_h02a: "In the past 7 days, how many days have you or someone in your ///
#household had to: Rely on less preferred and/or less expensive foods?"(WGT1)
#hh_h02b: "In the past 7 days, how many days have you or someone in your ///
#household had to: Limit portion size at mealtimes?" (WGT1)
#hh_h02c: "In the past 7 days, how many days have you or someone in your ///
#household had to: Reduce number of meals eaten in a day?" (WGT2)
# hh_h02d "In the past 7 days, how many days have you or someone in your ///
#household had to: Restrict consumption by adults in order for small ///
#children to eat?" (WGT2)
#hh_h02e "In the past 7 days, how many days have you or someone in your ///
#household had to: Borrow food, or rely on help from a friend or ///
#relative?" (WGT2)

mw13.rcsi <- read_dta(file = paste(path,"Household/HH_MOD_H.dta",sep = "" ) )

##Constructing rCSI
rCSI = mw13.rcsi %>%
  mutate(rCSI = 1*hh_h02a + 1*hh_h02b + 2*hh_h02c + 2*hh_h02d +2*hh_h02e ) %>%
  mutate(rCSI = if_else(hh_h01==2 , 0 ,rCSI) ) %>%
  select(y2_hhid,rCSI)

mw13.merged = left_join(mw13.merged,rCSI, by=c("y2_hhid"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
mw13.region<- read_dta(file = paste(path,"Household/HH_MOD_A_FILT.dta",sep = "" ) )

# Merge in year and round 

# mw13.region$region
# region 1 "Northern" 2 "Central" 3 "Southern"
mw13.region.rural = mw13.region %>% 
  select(y2_hhid,ea_id,reside,region) %>%
  mutate( region = as.numeric(region)) %>%
  mutate(FS_year  = as.numeric(hh_a23a_3) )%>%
  mutate(FS_month  = as.numeric(hh_a23a_2)) %>%
  mutate( region_north = if_else(region==1,1,0)) %>%
  mutate( region_central = if_else(region==2,1,0)) %>%
  #  1 urban 2 rural
  mutate( rural = case_when(
    reside == 1 ~ 0,
    reside == 2 ~ 1)
  ) %>%
  select(y2_hhid,ea_id,region_north,region_central,rural,hhsize,FS_month,FS_year)

# mw13.region$reside["Labels"]
# 
# sum(is.na(mw13.basic.region$region_num))

mw13.merged = left_join(mw13.merged,mw13.region.rural, by=c("y2_hhid"))


# Merge in geolocation 

mw13.geo <- read_dta(file = paste(path,"Geovariables/HouseholdGeovariables_IHPS.dta",sep = "" ) )
# mw13.geo$fsrad3_agpct

mw13.geo.clean = mw13.geo %>%
  mutate( lat_modified= LAT_DD_MOD) %>%
  mutate( lon_modified= LON_DD_MOD) %>%
  mutate(elevation =  srtm_1k) %>%
  mutate(percent_ag =  case_when(
    fsrad3_lcmaj == 20 ~ 0.6,
    fsrad3_lcmaj == 30 ~ 0.6,
    fsrad3_lcmaj == 14 ~ 1,
    is.na(fsrad3_lcmaj) ~ 0)
  ) %>% 
  mutate(percent_ag = as.numeric(fsrad3_agpct)
  ) %>% 
  #  3       Severe Constraint 
  # 2     Moderate Constraint
  mutate(sq1= as.numeric(sq1))  %>%
  mutate(sq2= as.numeric(sq2))  %>%
  mutate(nutri_avail = if_else(sq1 != 3 & sq1 != 2 ,0,sq1) )  %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail==3,1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail==2,1,0) ) %>% 
  mutate(nutri_rentention = if_else(sq2 != 3 & sq2 != 2,0,sq2))  %>%
  mutate(nutri_reten_severe_constraint=if_else(nutri_rentention==3,1,0) ) %>%
  mutate(nutri_reten_moderate_constraint=if_else(nutri_rentention==2,1,0) ) %>% 
  
  mutate(dummy_terrain_rough = if_else(srtm_mwi_5_15=="Mid altitude mountains" 
                                       & srtm_mwi_5_15=="Rugged lowlands" & 
                                         srtm_mwi_5_15== "High-altitude plains", 1,0 ))   %>%
  # mutate(slope = afmnslp_pct)  %>% 
  
  select(y2_hhid,lat_modified,lon_modified, dist_road,dist_admarc,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough)


# mw13.geo$sq1

mw13.merged = left_join(mw13.merged,mw13.geo.clean, by=c("y2_hhid"))


# Merge in household gender, education level and age 
mw13.basic <- read_dta(file = paste(path,"Household/HH_MOD_B.dta",sep = "" ) )

mw.hhhead = mw13.basic %>%
  #  keep only household head
  filter(hh_b04 ==1) %>%
  mutate(head_gender = as.numeric(hh_b03)) %>% 
  mutate(head_age = as.numeric(hh_b05a)) %>% 
  mutate(female_head = if_else(hh_b03==2, 1, 0) )  %>%
  mutate(head_edu  = as.numeric(hh_b22_3)) %>% 
  mutate(head_educated = if_else(head_edu!=1, 1, 0) ) %>% 
  select(y2_hhid,head_age, female_head, head_educated)

# mw13.basic$head_edlevel
#1   Male 2 Female

# mw13.basic$head_edlevel

# table(mw13.basic$head_edlevel)
# 1      None
# 2   Primary
# 3 Secondary
# 4  Tertiary

mw13.merged = left_join(mw13.merged,mw.hhhead, by=c("y2_hhid"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone , roof/floor


# mw13.asset$hh_f09
# value           label
# 1            Sand
# 2    Smoothed mud
# 3   Smooth cement
# 4            Wood
# 5            Tile
# 6 Other (Specify)

# mw13.asset$hh_f08
# 1            Grass
# 2      Iron sheets
# 3       Clay tiles
# 4         Concrete
# 5 Plastic sheeting
# 6  Other (specify)

mw13.asset <- read_dta(file = paste(path,"Household/HH_MOD_F.dta",sep = "" ) )

mw13.asset.cell.floor = mw13.asset %>%
  mutate (hh_f09 = as.numeric(hh_f09)) %>%
  ## floor ###
  mutate( floor_dirt_sand_dung = case_when(
    hh_f09 == 1 ~ 1,
    hh_f09 == 2 ~ 1,
    hh_f09 == 3 ~ 0, 
    hh_f09 == 4 ~ 0,
    hh_f09 == 5 ~ 0, 
    hh_f09 == 6 ~ 0)) %>%
  mutate (hh_f08 = as.numeric(hh_f08)) %>% 
  ## roof ###
  mutate( roof_not_natural = if_else( hh_f08 != 1 ,1,0)) %>%
  mutate( roof_iron = if_else( hh_f08 == 2 ,1,0)) %>%
  ## cellphone ###
  mutate (number_celphones = as.numeric(hh_f34)) %>% 
  mutate (cell_phone = if_else(number_celphones>0,1,0)) %>% 
  select(y2_hhid,floor_dirt_sand_dung,cell_phone,
         number_celphones,roof_not_natural,roof_iron)

mw13.merged = left_join(mw13.merged,mw13.asset.cell.floor, by=c("y2_hhid"))

#   mw13.asset$hh_f34

## merge in the other assets

# mw13.other.asset$hh_l02
# 507                Radio ('wireless')
# 509                        Television
# 514                      Refrigerator
# 516                           Bicycle
# 517                Motorcycle/scooter
# 518                               Car

mw13.other.asset <- read_dta(file = paste(path,"Household_Batch4of5_DTA/HH_MOD_L.dta",sep = "" ) )

frige = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_frige = sum (hh_l03[hh_l02  ==514],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(y2_hhid,Refrigerator)

radio = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_radio = sum (hh_l03[hh_l02  ==507],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(y2_hhid,Radio)

tv = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_tv = sum (hh_l03[hh_l02  ==509],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(y2_hhid,Television)


bike = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_bike = sum (hh_l03[hh_l02  ==516],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(y2_hhid,Bicycle) 

moto = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_moto= sum (hh_l03[hh_l02  ==517],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(y2_hhid,Motorcycle) 

car = mw13.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y2_hhid) %>%
  summarise( num_car= sum (hh_l03[hh_l02  ==518],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car >0,1,0) ) %>%
  select(y2_hhid,Car) 

mw13.asset.clean = left_join(frige,radio, by="y2_hhid")
mw13.asset.clean = left_join(mw13.asset.clean,tv, by="y2_hhid")
mw13.asset.clean = left_join(mw13.asset.clean,bike, by="y2_hhid")
mw13.asset.clean = left_join(mw13.asset.clean,moto, by="y2_hhid")
mw13.asset.clean = left_join(mw13.asset.clean,car, by="y2_hhid")


y2_hhid = mw13.asset.clean %>% select(y2_hhid)
mw13.asset.pca =   mw13.asset.clean %>% select(-y2_hhid)
#  generate asset index from the dummies 


asset.pca <- prcomp(mw13.asset.pca,
                    center = TRUE,
                    scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

mw13.asset.index = bind_cols(y2_hhid,asset_index)
#summary(asset.pca)



mw13.merged = left_join(mw13.merged,mw13.asset.index, by=c("y2_hhid"))

min(mw13.merged$HDDS)

write.csv(mw13.merged,"data/clean/household/mw13_hh.csv",row.names = FALSE)

