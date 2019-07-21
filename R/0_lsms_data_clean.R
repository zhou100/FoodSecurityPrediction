rm(list=ls())
library(tidyverse)
library(haven)

# Malawi 
########################################################
# Malawi 2010
########################################################
path = "data/raw/lsms/Malawi_2010/"

mw10.food <- read_dta(file = paste(path,"Household_Batch3of5_DTA/HH_MOD_G2.dta",sep = "" ) )
# head(mw10.food)
  
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

HDDS = mw10.food.category.days %>% 
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
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
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
  mutate(percent_ag = as.numeric(fsrad3_agpct)   ) %>% 
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

HDDS = mw13.food.category.days %>% 
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
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
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

mw13.other.asset <- read_dta(file = paste(path,"Household/HH_MOD_L.dta",sep = "" ) )

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



########################################################
# Malawi 2016
########################################################
path = "data/raw/lsms/Malawi_2016/MWI_2016_IHPS_v01_M_STATA/"

mw16.food <- read_dta(file = paste(path,"Household/hh_mod_g2_16.dta",sep = "" ) )
# head(mw16.food)

# mw16.food$hh_g08a




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

# calculate FCS 
FCS = mw16.food %>%
  mutate( hh_staple = pmax (hh_g08a,hh_g08b,na.rm = TRUE) ) %>%
  mutate( FCS =  hh_staple*2 + hh_g08c*3 + hh_g08d*1 + hh_g08e*4 +hh_g08f*1 + hh_g08g*4 + hh_g08h *0.5 + hh_g08i*0.5 + hh_g08j *0 ) %>% 
  group_by(y3_hhid) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

# unique(FCS$FCS)

FCS = FCS %>% filter(FCS!=0)
# sum(is.na(FCS$FCS)  )


mw16.merged = FCS
################################################################################
#A diet diversity score is a household-measure of food security that captures ///
#something about the quality of a diet. It is calculated by counting the number///
#of foods or food groups from which a household acquired food over the survey ///
#reference period (24 hours). 
################################################################################

HDDS = mw16.food %>% 
  mutate( hh_staple = pmax (hh_g08a,hh_g08b,na.rm = TRUE) ) %>%
  mutate( hdds_cereals = if_else(hh_staple>0,1,0) ) %>%
  mutate( hdds_c = if_else(hh_g08c>0,1,0) ) %>%
  mutate( hdds_d = if_else(hh_g08d>0,1,0) ) %>%
  mutate( hdds_e = if_else(hh_g08e>0,1,0) ) %>%
  mutate( hdds_f = if_else(hh_g08f>0,1,0) ) %>%
  mutate( hdds_g = if_else(hh_g08g>0,1,0) ) %>%
  mutate( HDDS = hdds_cereals + hdds_c + hdds_d + hdds_e + hdds_f + hdds_g  ) %>%
  select(y3_hhid,HDDS )
#Exclude SUGAR and SPICES

mw16.merged = left_join(mw16.merged,HDDS, by=c("y3_hhid"))
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

mw16.rcsi <- read_dta(file = paste(path,"Household/hh_mod_h_16.dta",sep = "" ) )
# mw16.rcsi$hh_h01

##Constructing rCSI
rCSI = mw16.rcsi %>%
  mutate(rCSI = 1*hh_h02a + 1*hh_h02b + 2*hh_h02c + 2*hh_h02d +2*hh_h02e ) %>%
  mutate(hh_h01 = as.numeric(hh_h01)) %>% 
  mutate(rCSI = if_else(hh_h01==2 , 0 ,rCSI) ) %>%
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
  select(y3_hhid,rCSI)

mw16.merged = left_join(mw16.merged,rCSI, by=c("y3_hhid"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
mw16.region<- read_dta(file = paste(path,"household/hh_mod_a_filt_16.dta",sep = "" ) )

# Merge in year and round 
# mw16.region$region
# region 1 "Northern" 2 "Central" 3 "Southern"
mw16.region.rural = mw16.region %>% 
  separate(consumption_date, into = c("FS_year","FS_month"), extra = 'drop', remove = TRUE) %>% 
  mutate(FS_year  = as.numeric(FS_year) )%>%
  mutate(FS_month  = as.numeric(FS_month)) %>%
  mutate( region = as.numeric(region)) %>%
  mutate( region_north = if_else(region==1,1,0)) %>%
  mutate( region_central = if_else(region==2,1,0)) %>%
  mutate(hhsize = as.numeric(hhsize)) %>%
  #  1 urban 2 rural
  mutate( rural = case_when(
    reside == 1 ~ 0,
    reside == 2 ~ 1)
  ) %>%
  select(y3_hhid,ea_id,region_north,region_central,rural,hhsize,FS_month,FS_year) %>%
  drop_na()

# mw16.region$reside["Labels"]
# 
# sum(is.na(mw16.basic.region$region_num))

mw16.merged = left_join(mw16.merged,mw16.region.rural, by=c("y3_hhid"))


# Merge in geolocation 

mw16.geo <- read_dta(file = paste(path,"household/HouseholdGeovariablesIHPSY3.dta",sep = "" ) )
# mw16.geo$fsrad3_agpct

mw16.geo.clean = mw16.geo %>%
  mutate(elevation =  srtm_1k) %>%
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
  
  select(y3_hhid,lat_modified,lon_modified, dist_road,dist_admarc,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough)


# mw16.geo$sq1

mw16.merged = left_join(mw16.merged,mw16.geo.clean, by=c("y3_hhid"))


# Merge in household gender, education level and age 
mw16.basic <- read_dta(file = paste(path,"household/hh_mod_b_16.dta",sep = "" ) )

# mw16.basic$hh_b21

mw16.hh.head = mw16.basic %>%
  #  keep only household head
  mutate(hh_b04 = as.numeric(hh_b04)) %>% 
  filter(hh_b04 ==1) %>%
  mutate(head_age = as.numeric(hh_b05a)) %>% 
  mutate(female_head = if_else(hh_b03==2, 1, 0) )  %>%
  mutate(head_edu  = as.numeric(hh_b21)) %>% 
  mutate(head_educated = if_else(head_edu!=1, 1, 0) ) %>% 
  select(y3_hhid,head_age, female_head, head_educated)

# mw16.basic$head_edlevel
#1   Male 2 Female

# mw16.basic$head_edlevel

# table(mw16.basic$head_edlevel)
# 1      None
# 2   Primary
# 3 Secondary
# 4  Tertiary

mw16.merged = left_join(mw16.merged,mw16.hh.head, by=c("y3_hhid"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone , roof/floor


# mw16.asset$hh_f09
# value           label
# 1            Sand
# 2    Smoothed mud
# 3   Smooth cement
# 4            Wood
# 5            Tile
# 6 Other (Specify)

# mw16.asset$hh_f08
# 1            Grass
# 2      Iron sheets
# 3       Clay tiles
# 4         Concrete
# 5 Plastic sheeting
# 6  Other (specify)

mw16.asset <- read_dta(file = paste(path,"household/hh_mod_f_16.dta",sep = "" ) )

mw16.asset.cell.floor = mw16.asset %>%
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
  select(y3_hhid,floor_dirt_sand_dung,cell_phone,
         number_celphones,roof_not_natural,roof_iron)

mw16.merged = left_join(mw16.merged,mw16.asset.cell.floor, by=c("y3_hhid"))

#   mw16.asset$hh_f34

## merge in the other assets

# mw16.other.asset$hh_l02
# 507                Radio ('wireless')
# 509                        Television
# 514                      Refrigerator
# 516                           Bicycle
# 517                Motorcycle/scooter
# 518                               Car

mw16.other.asset <- read_dta(file = paste(path,"household/hh_mod_l_16.dta",sep = "" ) )

frige = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_frige = sum (hh_l03[hh_l02  ==514],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(y3_hhid,Refrigerator)

radio = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_radio = sum (hh_l03[hh_l02  ==507],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(y3_hhid,Radio)

tv = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_tv = sum (hh_l03[hh_l02  ==509],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(y3_hhid,Television)


bike = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_bike = sum (hh_l03[hh_l02  ==516],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(y3_hhid,Bicycle) 

moto = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_moto= sum (hh_l03[hh_l02  ==517],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(y3_hhid,Motorcycle) 

car = mw16.other.asset %>%
  mutate(hh_l02 = as.numeric(hh_l02)) %>%
  group_by(y3_hhid) %>%
  summarise( num_car= sum (hh_l03[hh_l02  ==518],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car >0,1,0) ) %>%
  select(y3_hhid,Car) 

mw16.asset.clean = left_join(frige,radio, by="y3_hhid")
mw16.asset.clean = left_join(mw16.asset.clean,tv, by="y3_hhid")
mw16.asset.clean = left_join(mw16.asset.clean,bike, by="y3_hhid")
mw16.asset.clean = left_join(mw16.asset.clean,moto, by="y3_hhid")
mw16.asset.clean = left_join(mw16.asset.clean,car, by="y3_hhid")


y3_hhid = mw16.asset.clean %>% select(y3_hhid)
mw16.asset.pca =   mw16.asset.clean %>% select(-y3_hhid)
#  generate asset index from the dummies 


asset.pca <- prcomp(mw16.asset.pca,
                    center = TRUE,
                    scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

mw16.asset.index = bind_cols(y3_hhid,asset_index)
#summary(asset.pca)



mw16.merged = left_join(mw16.merged,mw16.asset.index, by=c("y3_hhid"))

min(mw16.merged$HDDS)

write.csv(mw16.merged,"data/clean/household/mw16_hh.csv",row.names = FALSE)

# Tanzania  
########################################################
# Tanzania 2010
########################################################
path = "data/raw/lsms/Tanzania_2010/TZA_2010_NPS2_v01_M_STATA/"

tz10.food <- read_dta(file = paste(path,"HH_SEC_K2.dta",sep = "" ) )
 
# tz10.food$hh_k08_2


tz10.food.category.days = tz10.food %>%
  mutate( hh_k08_3 = if_else(hh_k08_3>7,7,hh_k08_3)) %>%
  # Combining Cereals and roots (Category A and Category B)
  mutate(itemcode =  if_else(itemcode=="B","A",itemcode) ) %>%
  group_by(y2_hhid,itemcode) %>%
  summarise(days = max(hh_k08_3))
  

 #  generate a numeric version of the item_code to use the collapse functtion
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

## Specifying Weights Different Food Categories
FWeight.mapping= read.table(text = "
category FWeight 
                            A 2
                            C 3
                            D 1
                            F 1
                            E 4
                            G 4
                            H 0.5
                            I 0.5
                            J 0", header = TRUE)

# calculate FCS 
FCS = tz10.food.category.days %>%
  mutate(
    # find the weight from 
    FWeight = with(FWeight.mapping,FWeight[itemcode %in% category])
  ) %>%
  mutate( FCS = days * FWeight ) %>% 
  group_by(y2_hhid) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

FCS = FCS %>% filter(FCS!=0)

tz10.merged = FCS
################################################################################
#A diet diversity score is a household-measure of food security that captures ///
#something about the quality of a diet. It is calculated by counting the number///
#of foods or food groups from which a household acquired food over the survey ///
#reference period (24 hours). 
################################################################################
HDDS = tz10.food.category.days %>% 
  #Exclude SUGAR and SPICES
  filter(itemcode!="I" & itemcode!="J") %>%
  mutate( HDDS = if_else( days>=1 ,1,0 )  ) %>%
  group_by(y2_hhid) %>%
  summarise(HDDS = sum(HDDS,na.rm = TRUE))

tz10.merged = left_join(tz10.merged,HDDS, by=c("y2_hhid"))
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

tz10.rcsi <- read_dta(file = paste(path,"HH_SEC_I1.dta",sep = "" ) )

# tz10.rcsi$hh_i01

##Constructing rCSI
rCSI = tz10.rcsi %>%
  mutate(rCSI = 1*hh_i02_1 + 1*hh_i02_2 + 1*hh_i02_3 + 2*hh_i02_4 +2*hh_i02_5 + 2*hh_i02_6 +4*hh_i02_7 +4*hh_i02_8 ) %>%
  mutate(rCSI = if_else(hh_i01==2 , 0 ,rCSI) ) %>%
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
  select(y2_hhid,rCSI)


tz10.merged = left_join(tz10.merged,rCSI, by=c("y2_hhid"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
tz10.region<- read_dta(file = paste(path,"HH_SEC_A.dta",sep = "" ) )

# Merge in year and round 

unique(tz10.region$region)

# one-hot encoding 
region = factor(tz10.region$region) 
region_dummy  = as.data.frame(model.matrix(~region)[,-1])


  
# region 1 "Northern" 2 "Central" 3 "Southern"
tz10.region.rural = tz10.region %>% 
  mutate(FS_year  = as.numeric(hh_a18_3) )%>%
  mutate(FS_month  = as.numeric(hh_a18_2)) %>%
  #  1 urban 2 rural
  mutate( rural = as.numeric(y2_rural)
  ) %>%
  select(y2_hhid,rural,FS_month,FS_year,clusterid)

tz10.region.rural = bind_cols(tz10.region.rural,region_dummy)
# mw13.region$reside["Labels"]
# 
# sum(is.na(mw13.basic.region$region_num))

tz10.merged = left_join(tz10.merged,tz10.region.rural, by=c("y2_hhid"))


# Merge in geolocation 

tz10.geo <- read_dta(file = paste(path,"TZY2.EA.Offsets.dta",sep = "" ) )
tz10.geo = tz10.geo %>% select(-rum)

tz10.merged = left_join(tz10.merged,tz10.geo,by="clusterid")

# Merge in other Geovariables
tz10.geo.other <- read_dta(file = paste(path,"TZY2.HH.Geovariables.dta",sep = "" ) )

tz10.geo.other.clean = tz10.geo.other %>%
  mutate(elevation = as.numeric(hh_geo06) ) %>% 
  mutate(dummy_terrain_rough = if_else(srtm_eaf_5_15==13 
                                       & srtm_eaf_5_15==5 & 
                                         srtm_eaf_5_15== 3, 1,0 ))   %>%
  mutate(hh_soil_con01 = as.numeric(hh_soil_con01)) %>%
  mutate(hh_soil_con02 = as.numeric(hh_soil_con02)) %>%
  mutate(nutri_avail = if_else(hh_soil_con01 != 3 & hh_soil_con01 != 2 & hh_soil_con01!=4 ,0,hh_soil_con01) )  %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail==3|nutri_avail==4,1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail==2,1,0) ) %>% 
  mutate(nutri_rentention = if_else(hh_soil_con02 != 3 & hh_soil_con02 != 2 & hh_soil_con02!=4 ,0,hh_soil_con02)) %>%
  mutate(nutri_reten_severe_constraint=if_else(nutri_rentention==3|nutri_rentention==4,1,0) ) %>%
  mutate(nutri_reten_moderate_constraint=if_else(nutri_rentention==2,1,0) ) %>% 
  mutate(slope = afmnslp_pct) %>%
  mutate(dist_road = as.numeric(hh_geo01) ) %>%
  mutate(dist_popcenter =  as.numeric(hh_geo02)) %>%
  mutate(dist_headquater =  as.numeric(hh_geo05) ) %>%
  mutate(percent_ag =  as.numeric(hh_envi15) ) %>%
  select(y2_hhid, dist_road,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough,slope)

tz10.merged = left_join(tz10.merged,tz10.geo.other.clean, by=c("y2_hhid"))


# Merge in household gender, education level and age 
tz10.basic <- read_dta(file = paste(path,"HH_SEC_B.dta",sep = "" ) )

tz10.hhhead = tz10.basic %>%
  #  keep only household head
  filter(hh_b05 ==1) %>%
  mutate(hh_b02 = as.numeric(hh_b02)) %>% 
  mutate(head_age = as.numeric(hh_b04)) %>% 
  mutate(female_head = if_else(hh_b02==2, 1, 0) )  %>%
  select(y2_hhid,head_age, female_head)

tz10.merged = left_join(tz10.merged,tz10.hhhead, by=c("y2_hhid"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone 
tz10.asset <- read_dta(file = paste(path,"HH_SEC_N.dta",sep = "" ) )

tz10.asset.cell.floor = tz10.asset %>%
  filter( itemcode  ==401 | itemcode  ==403 | itemcode  ==404 | 
          itemcode  ==406|itemcode  ==426 |itemcode  ==427 |itemcode  ==425)

tz10.cellphone  = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_cell = sum (hh_n01_2[itemcode  ==403],na.rm = TRUE) ) %>%
  mutate( Cellphone = if_else(num_cell>0,1,0) ) %>%
  select(y2_hhid,Cellphone,num_cell)

fridge = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_frige = sum (hh_n01_2[itemcode  ==404],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(y2_hhid,Refrigerator)

radio = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_radio = sum (hh_n01_2[itemcode  ==401],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(y2_hhid,Radio)

tv = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_tv = sum (hh_n01_2[itemcode  ==406],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(y2_hhid,Television) 

bike = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_bike = sum (hh_n01_2[itemcode  ==427],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(y2_hhid,Bicycle) 

moto = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_moto = sum (hh_n01_2[itemcode  ==426],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(y2_hhid,Motorcycle) 
 
car = tz10.asset.cell.floor %>%
  mutate(hh_n01_2 = as.numeric(hh_n01_2)) %>%
  group_by(y2_hhid) %>%
  summarise( num_car = sum (hh_n01_2[itemcode  ==425],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car>0,1,0) ) %>%
  select(y2_hhid,Car)  
 
tz10.asset.clean = left_join(fridge,radio, by="y2_hhid")
tz10.asset.clean = left_join(tz10.asset.clean,tv, by="y2_hhid")
tz10.asset.clean = left_join(tz10.asset.clean,bike, by="y2_hhid")
tz10.asset.clean = left_join(tz10.asset.clean,moto, by="y2_hhid")
tz10.asset.clean = left_join(tz10.asset.clean,car, by="y2_hhid")


y2_hhid = tz10.asset.clean %>% select(y2_hhid)
tz10.asset.pca =   tz10.asset.clean %>% select(-y2_hhid)
#  generate asset index from the dummies 


asset.pca <- prcomp(tz10.asset.pca,
                    center = TRUE,
                    scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

tz10.asset.index = bind_cols(y2_hhid,asset_index)
#summary(asset.pca)


tz10.merged = left_join(tz10.merged,tz10.asset.index, by=c("y2_hhid"))
tz10.merged = left_join(tz10.merged,tz10.cellphone, by=c("y2_hhid"))


# merge in housing
tz10.housing <- read_dta(file = paste(path,"HH_SEC_J1.dta",sep = "" ) )

# tz10.housing$hh_j06
tz10.housing.clean =   tz10.housing %>%
  mutate (hh_j07 = as.numeric(hh_j07)) %>%
  ## floor ###
  mutate( floor_dirt_sand_dung = case_when(
    hh_j07 == 1 ~ 1,
    hh_j07 == 2 ~ 0,
    hh_j07 == 3 ~ 1)) %>%
  mutate (hh_j06 = as.numeric(hh_j06)) %>% 
  ## roof ###
  mutate( roof_not_natural = if_else( hh_j06 != 1 & hh_j06!=2  ,1,0)) %>%
  mutate( roof_iron = if_else( hh_j06 == 4 ,1,0)) %>%
  select(y2_hhid,floor_dirt_sand_dung,
         roof_not_natural,roof_iron)

tz10.merged = left_join(tz10.merged,tz10.housing.clean, by=c("y2_hhid"))


write.csv(tz10.merged,"data/clean/household/tz10_hh.csv",row.names = FALSE)


########################################################
# Tanzania 2012
########################################################
path = "data/raw/lsms/Tanzania_2012/TZA_2012_LSMS_v01_M_STATA_English_labels/"

tz12.food <- read_dta(file = paste(path,"HH_SEC_J3.dta",sep = "" ) )

#   value                                  label
# 1 A. CEREALS, GRAINS AND CEREAL PRODUCTS
# 2        B. ROOTS, TUBERS, AND PLANTAINS
# 3                     C. NUTS AND PULSES
# 4                          D. VEGETABLES
# 5      E. MEAT, FISH AND ANIMAL PRODUCTS
# 6                              F. FRUITS
# 7                  G. MILK/MILK PRODUCTS
# 8                            H. FATS/OIL
# 9          I. SUGAR/SUGAR PRODUCTS/HONEY
# 10                   J. SPICES/CONDIMENTS


tz12.food.category.days = tz12.food %>%
  mutate( itemcode = as.numeric(itemcode)) %>% 
  mutate( hh_j09_3 = if_else(hh_j09_3>7,7,hh_j09_3)) %>%
  # Combining Cereals and roots (Category A and Category B)
  mutate(itemcode =  if_else(itemcode==2,1,itemcode) ) %>%
  group_by(y3_hhid,itemcode) %>%
  summarise(days = max(hh_j09_3))


#  generate a numeric version of the item_code to use the collapse functtion
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

## Specifying Weights Different Food Categories
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
FCS = tz12.food.category.days %>%
  mutate(
    # find the weight from 
    FWeight = with(FWeight.mapping,FWeight[itemcode %in% category])
  ) %>%
  mutate( FCS = days * FWeight ) %>% 
  group_by(y3_hhid) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

FCS = FCS %>% filter(FCS!=0)

tz12.merged = FCS
################################################################################
#A diet diversity score is a household-measure of food security that captures ///
#something about the quality of a diet. It is calculated by counting the number///
#of foods or food groups from which a household acquired food over the survey ///
#reference period (24 hours). 
################################################################################
HDDS = tz12.food.category.days %>% 
  #Exclude SUGAR and SPICES
  filter(itemcode!=9 & itemcode!=10) %>%
  mutate( HDDS = if_else( days>=1 ,1,0 )  ) %>%
  group_by(y3_hhid) %>%
  summarise(HDDS = sum(HDDS,na.rm = TRUE))

tz12.merged = left_join(tz12.merged,HDDS, by=c("y3_hhid"))
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

tz12.rcsi <- read_dta(file = paste(path,"HH_SEC_H.dta",sep = "" ) )

# tz12.rcsi$hh_i01

##Constructing rCSI
rCSI = tz12.rcsi %>%
  mutate(rCSI = 1*hh_h02_1 + 1*hh_h02_2 + 1*hh_h02_3 + 2*hh_h02_4 +2*hh_h02_5 + 2*hh_h02_6 +4*hh_h02_7 +4*hh_h02_8 ) %>%
  mutate(rCSI = if_else(hh_h02_1==2 , 0 ,rCSI) ) %>%
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
  select(y3_hhid,rCSI)


tz12.merged = left_join(tz12.merged,rCSI, by=c("y3_hhid"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
tz12.region<- read_dta(file = paste(path,"HH_SEC_A.dta",sep = "" ) )

# Merge in year and round 

unique(tz12.region$hh_a01_1)

# one-hot encoding 
region = factor(tz12.region$hh_a01_1) 
region_dummy  = as.data.frame(model.matrix(~region)[,-1])

tz_region_mapping = tz12.region %>% 
  group_by(hh_a01_1,hh_a01_2) %>%
  count()  

write.csv(tz_region_mapping,"data/clean/household/tz_region_map.csv",row.names = FALSE)

# region 1 "Northern" 2 "Central" 3 "Southern"
tz12.region.rural = tz12.region %>% 
  mutate(FS_year  = as.numeric(hh_a18_3) )%>%
  mutate(FS_month  = as.numeric(hh_a18_2)) %>%
  #  1 urban 2 rural
  mutate( rural = as.numeric(y3_rural)
  ) %>%
  select(y3_hhid,rural,FS_month,FS_year,clusterid)

tz12.region.rural = bind_cols(tz12.region.rural,region_dummy)
# mw13.region$reside["Labels"]
# 
# sum(is.na(mw13.basic.region$region_num))

tz12.merged = left_join(tz12.merged,tz12.region.rural, by=c("y3_hhid"))


# Merge in geolocation 

tz12.geo <- read_dta(file = paste(path,"HouseholdGeovars_Y3.dta",sep = "" ) )


tz12.geo.clean = tz12.geo %>%
  mutate(lat_modified = lat_dd_mod) %>% 
  mutate(lon_modified = lon_dd_mod) %>% 
  mutate(elevation = as.numeric(soil01) ) %>% 
  
  mutate(dummy_terrain_rough = if_else(soil04==13 &
                                         soil04==14 & 
                                         soil04==5 & 
                                         soil04== 3, 1,0 ))   %>%
  mutate(soil05 = as.numeric(soil05)) %>%
  mutate(soil06 = as.numeric(soil06)) %>%
  mutate(nutri_avail = if_else(soil05 != 3 & soil05 != 2 & soil05!=4 ,0,soil05) )  %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail==3|nutri_avail==4,1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail==2,1,0) ) %>% 
  mutate(nutri_rentention = if_else(soil06 != 3 & soil06 != 2 & soil06!=4 ,0,soil06)) %>%
  mutate(nutri_reten_severe_constraint=if_else(nutri_rentention==3|nutri_rentention==4,1,0) ) %>%
  mutate(nutri_reten_moderate_constraint=if_else(nutri_rentention==2,1,0) ) %>% 
  mutate(slope = soil02) %>%
  mutate(dist_road = as.numeric(dist01) ) %>%
  mutate(dist_popcenter =  as.numeric(dist02)) %>%
  mutate(dist_headquater =  as.numeric(dist04) ) %>%
  mutate(percent_ag =  as.numeric(land02) ) %>%
  select(y3_hhid, dist_road,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough,slope)

tz12.merged = left_join(tz12.merged,tz12.geo.clean, by=c("y3_hhid"))


# Merge in household gender, education level and age 
tz12.basic <- read_dta(file = paste(path,"HH_SEC_B.dta",sep = "" ) )

tz12.hhhead = tz12.basic %>%
  #  keep only household head
  filter(hh_b05 ==1) %>%
  mutate(hh_b02 = as.numeric(hh_b02)) %>% 
  mutate(head_age = as.numeric(hh_b04)) %>% 
  mutate(female_head = if_else(hh_b02==2, 1, 0) )  %>%
  select(y3_hhid,head_age, female_head)

tz12.merged = left_join(tz12.merged,tz12.hhhead, by=c("y3_hhid"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone 
tz12.asset <- read_dta(file = paste(path,"HH_SEC_M.dta",sep = "" ) )

tz12.asset.cell.floor = tz12.asset %>%
  filter( itemcode  ==401 | itemcode  ==403 | itemcode  ==404 | 
            itemcode  ==406|itemcode  ==426 |itemcode  ==427 |itemcode  ==425)

tz12.cellphone  = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_cell = sum (hh_m01[itemcode  ==403],na.rm = TRUE) ) %>%
  mutate( Cellphone = if_else(num_cell>0,1,0) ) %>%
  select(y3_hhid,Cellphone,num_cell)

fridge = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_frige = sum (hh_m01[itemcode  ==404],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(y3_hhid,Refrigerator)

radio = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_radio = sum (hh_m01[itemcode  ==401],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(y3_hhid,Radio)

tv = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_tv = sum (hh_m01[itemcode  ==406],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(y3_hhid,Television) 

bike = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_bike = sum (hh_m01[itemcode  ==427],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(y3_hhid,Bicycle) 

moto = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_moto = sum (hh_m01[itemcode  ==426],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(y3_hhid,Motorcycle) 

car = tz12.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_car = sum (hh_m01[itemcode  ==425],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car>0,1,0) ) %>%
  select(y3_hhid,Car)  

tz12.asset.clean = left_join(fridge,radio, by="y3_hhid")
tz12.asset.clean = left_join(tz12.asset.clean,tv, by="y3_hhid")
tz12.asset.clean = left_join(tz12.asset.clean,bike, by="y3_hhid")
tz12.asset.clean = left_join(tz12.asset.clean,moto, by="y3_hhid")
tz12.asset.clean = left_join(tz12.asset.clean,car, by="y3_hhid")


y3_hhid = tz12.asset.clean %>% select(y3_hhid)
tz12.asset.pca =   tz12.asset.clean %>% select(-y3_hhid)
#  generate asset index from the dummies 


asset.pca <- prcomp(tz12.asset.pca,
                    center = TRUE,
                    scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

tz12.asset.index = bind_cols(y3_hhid,asset_index)
#summary(asset.pca)


tz12.merged = left_join(tz12.merged,tz12.asset.index, by=c("y3_hhid"))
tz12.merged = left_join(tz12.merged,tz12.cellphone, by=c("y3_hhid"))


# merge in housing
tz12.housing <- read_dta(file = paste(path,"HH_SEC_I.dta",sep = "" ) )

# tz12.housing$hh_j06
tz12.housing.clean =   tz12.housing %>%
  mutate (hh_i10 = as.numeric(hh_i10)) %>%
  ## floor ###
  mutate( floor_dirt_sand_dung = case_when(
    hh_i10 == 1 ~ 1,
    hh_i10 == 2 ~ 0,
    hh_i10 == 3 ~ 1)) %>%
  mutate (hh_i09 = as.numeric(hh_i09)) %>% 
  ## roof ###
  mutate( roof_not_natural = if_else( hh_i09 != 1 & hh_i09!=2  ,1,0)) %>%
  mutate( roof_iron = if_else( hh_i09 == 4 ,1,0)) %>%
  select(y3_hhid,floor_dirt_sand_dung,
         roof_not_natural,roof_iron)

tz12.merged = left_join(tz12.merged,tz12.housing.clean, by=c("y3_hhid"))


write.csv(tz12.merged,"data/clean/household/tz12_hh.csv",row.names = FALSE)

########################################################
# Tanzania 2014
########################################################
path = "data/raw/lsms/Tanzania_2014/TZA_2014_NPS-R4-v01_M_STATA8/"

tz14.food <- read_dta(file = paste(path,"HH_SEC_J3.dta",sep = "" ) )

#   value                                  label
# 1 A. CEREALS, GRAINS AND CEREAL PRODUCTS
# 2        B. ROOTS, TUBERS, AND PLANTAINS
# 3                     C. NUTS AND PULSES
# 4                          D. VEGETABLES
# 5      E. MEAT, FISH AND ANIMAL PRODUCTS
# 6                              F. FRUITS
# 7                  G. MILK/MILK PRODUCTS
# 8                            H. FATS/OIL
# 9          I. SUGAR/SUGAR PRODUCTS/HONEY
# 10                   J. SPICES/CONDIMENTS


tz14.food.category.days = tz14.food %>%
  mutate( itemcode = as.numeric(itemcode)) %>% 
  mutate( hh_j09_3 = if_else(hh_j09_3>7,7,hh_j09_3)) %>%
  # Combining Cereals and roots (Category A and Category B)
  mutate(itemcode =  if_else(itemcode==2,1,itemcode) ) %>%
  group_by(y3_hhid,itemcode) %>%
  summarise(days = max(hh_j09_3))


#  generate a numeric version of the item_code to use the collapse functtion
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

## Specifying Weights Different Food Categories
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
FCS = tz14.food.category.days %>%
  mutate(
    # find the weight from 
    FWeight = with(FWeight.mapping,FWeight[itemcode %in% category])
  ) %>%
  mutate( FCS = days * FWeight ) %>% 
  group_by(y3_hhid) %>%
  summarise( FCS  = sum (FCS,na.rm = TRUE) )

FCS = FCS %>% filter(FCS!=0)

tz14.merged = FCS
################################################################################
#A diet diversity score is a household-measure of food security that captures ///
#something about the quality of a diet. It is calculated by counting the number///
#of foods or food groups from which a household acquired food over the survey ///
#reference period (24 hours). 
################################################################################
HDDS = tz14.food.category.days %>% 
  #Exclude SUGAR and SPICES
  filter(itemcode!=9 & itemcode!=10) %>%
  mutate( HDDS = if_else( days>=1 ,1,0 )  ) %>%
  group_by(y3_hhid) %>%
  summarise(HDDS = sum(HDDS,na.rm = TRUE))

tz14.merged = left_join(tz14.merged,HDDS, by=c("y3_hhid"))
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

tz14.rcsi <- read_dta(file = paste(path,"HH_SEC_H.dta",sep = "" ) )

# tz14.rcsi$hh_i01

##Constructing rCSI
rCSI = tz14.rcsi %>%
  mutate(rCSI = 1*hh_h02_1 + 1*hh_h02_2 + 1*hh_h02_3 + 2*hh_h02_4 +2*hh_h02_5 + 2*hh_h02_6 +4*hh_h02_7 +4*hh_h02_8 ) %>%
  mutate(rCSI = if_else(hh_h02_1==2 , 0 ,rCSI) ) %>%
  mutate(rCSI = if_else(rCSI>42 , 42 ,rCSI) ) %>% 
  select(y3_hhid,rCSI)


tz14.merged = left_join(tz14.merged,rCSI, by=c("y3_hhid"))

#_______________________________________________________________________________
#MERGE in DIST, geolocation and time 
#_______________________________________________________________________________
tz14.region<- read_dta(file = paste(path,"HH_SEC_A.dta",sep = "" ) )

# Merge in year and round 

unique(tz14.region$hh_a01_1)

# one-hot encoding 
region = factor(tz14.region$hh_a01_1) 
region_dummy  = as.data.frame(model.matrix(~region)[,-1])

tz_region_mapping = tz14.region %>% 
  group_by(hh_a01_1,hh_a01_2) %>%
  count()  

write.csv(tz_region_mapping,"data/clean/household/tz_region_map.csv",row.names = FALSE)

# region 1 "Northern" 2 "Central" 3 "Southern"
tz14.region.rural = tz14.region %>% 
  mutate(FS_year  = as.numeric(hh_a18_3) )%>%
  mutate(FS_month  = as.numeric(hh_a18_2)) %>%
  #  1 urban 2 rural
  mutate( rural = as.numeric(y3_rural)
  ) %>%
  select(y3_hhid,rural,FS_month,FS_year,clusterid)

tz14.region.rural = bind_cols(tz14.region.rural,region_dummy)
# mw13.region$reside["Labels"]
# 
# sum(is.na(mw13.basic.region$region_num))

tz14.merged = left_join(tz14.merged,tz14.region.rural, by=c("y3_hhid"))


# Merge in geolocation 

tz14.geo <- read_dta(file = paste(path,"HouseholdGeovars_Y3.dta",sep = "" ) )


tz14.geo.clean = tz14.geo %>%
  mutate(lat_modified = lat_dd_mod) %>% 
  mutate(lon_modified = lon_dd_mod) %>% 
  mutate(elevation = as.numeric(soil01) ) %>% 
  
  mutate(dummy_terrain_rough = if_else(soil04==13 &
                                         soil04==14 & 
                                         soil04==5 & 
                                         soil04== 3, 1,0 ))   %>%
  mutate(soil05 = as.numeric(soil05)) %>%
  mutate(soil06 = as.numeric(soil06)) %>%
  mutate(nutri_avail = if_else(soil05 != 3 & soil05 != 2 & soil05!=4 ,0,soil05) )  %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail==3|nutri_avail==4,1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail==2,1,0) ) %>% 
  mutate(nutri_rentention = if_else(soil06 != 3 & soil06 != 2 & soil06!=4 ,0,soil06)) %>%
  mutate(nutri_reten_severe_constraint=if_else(nutri_rentention==3|nutri_rentention==4,1,0) ) %>%
  mutate(nutri_reten_moderate_constraint=if_else(nutri_rentention==2,1,0) ) %>% 
  mutate(slope = soil02) %>%
  mutate(dist_road = as.numeric(dist01) ) %>%
  mutate(dist_popcenter =  as.numeric(dist02)) %>%
  mutate(dist_headquater =  as.numeric(dist04) ) %>%
  mutate(percent_ag =  as.numeric(land02) ) %>%
  select(y3_hhid, dist_road,dist_popcenter,percent_ag,
         nutri_severe_constraint,nutri_moderate_constraint,nutri_reten_severe_constraint,
         dummy_terrain_rough,slope)

tz14.merged = left_join(tz14.merged,tz14.geo.clean, by=c("y3_hhid"))


# Merge in household gender, education level and age 
tz14.basic <- read_dta(file = paste(path,"HH_SEC_B.dta",sep = "" ) )

tz14.hhhead = tz14.basic %>%
  #  keep only household head
  filter(hh_b05 ==1) %>%
  mutate(hh_b02 = as.numeric(hh_b02)) %>% 
  mutate(head_age = as.numeric(hh_b04)) %>% 
  mutate(female_head = if_else(hh_b02==2, 1, 0) )  %>%
  select(y3_hhid,head_age, female_head)

tz14.merged = left_join(tz14.merged,tz14.hhhead, by=c("y3_hhid"))

# _______________________________________________________________________________

#MERGE in cellphone, roof/floor and other household assets
#_______________________________________________________________________________

# Merge in cell phone 
tz14.asset <- read_dta(file = paste(path,"HH_SEC_M.dta",sep = "" ) )

tz14.asset.cell.floor = tz14.asset %>%
  filter( itemcode  ==401 | itemcode  ==403 | itemcode  ==404 | 
            itemcode  ==406|itemcode  ==426 |itemcode  ==427 |itemcode  ==425)

tz14.cellphone  = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_cell = sum (hh_m01[itemcode  ==403],na.rm = TRUE) ) %>%
  mutate( Cellphone = if_else(num_cell>0,1,0) ) %>%
  select(y3_hhid,Cellphone,num_cell)

fridge = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_frige = sum (hh_m01[itemcode  ==404],na.rm = TRUE) ) %>%
  mutate( Refrigerator = if_else(num_frige>0,1,0) ) %>%
  select(y3_hhid,Refrigerator)

radio = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_radio = sum (hh_m01[itemcode  ==401],na.rm = TRUE) ) %>%
  mutate( Radio = if_else(num_radio>0,1,0) ) %>%
  select(y3_hhid,Radio)

tv = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_tv = sum (hh_m01[itemcode  ==406],na.rm = TRUE) ) %>%
  mutate( Television = if_else(num_tv>0,1,0) ) %>%
  select(y3_hhid,Television) 

bike = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_bike = sum (hh_m01[itemcode  ==427],na.rm = TRUE) ) %>%
  mutate( Bicycle = if_else(num_bike>0,1,0) ) %>%
  select(y3_hhid,Bicycle) 

moto = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_moto = sum (hh_m01[itemcode  ==426],na.rm = TRUE) ) %>%
  mutate( Motorcycle = if_else(num_moto>0,1,0) ) %>%
  select(y3_hhid,Motorcycle) 

car = tz14.asset.cell.floor %>%
  mutate(hh_m01 = as.numeric(hh_m01)) %>%
  group_by(y3_hhid) %>%
  summarise( num_car = sum (hh_m01[itemcode  ==425],na.rm = TRUE) ) %>%
  mutate( Car = if_else(num_car>0,1,0) ) %>%
  select(y3_hhid,Car)  

tz14.asset.clean = left_join(fridge,radio, by="y3_hhid")
tz14.asset.clean = left_join(tz14.asset.clean,tv, by="y3_hhid")
tz14.asset.clean = left_join(tz14.asset.clean,bike, by="y3_hhid")
tz14.asset.clean = left_join(tz14.asset.clean,moto, by="y3_hhid")
tz14.asset.clean = left_join(tz14.asset.clean,car, by="y3_hhid")


y3_hhid = tz14.asset.clean %>% select(y3_hhid)
tz14.asset.pca =   tz14.asset.clean %>% select(-y3_hhid)
#  generate asset index from the dummies 


asset.pca <- prcomp(tz14.asset.pca,
                    center = TRUE,
                    scale. = TRUE) 
asset_index = as.data.frame(asset.pca$x)["PC1"]
colnames(asset_index) = "asset_index"

tz14.asset.index = bind_cols(y3_hhid,asset_index)
#summary(asset.pca)


tz14.merged = left_join(tz14.merged,tz14.asset.index, by=c("y3_hhid"))
tz14.merged = left_join(tz14.merged,tz14.cellphone, by=c("y3_hhid"))


# merge in housing
tz14.housing <- read_dta(file = paste(path,"HH_SEC_I.dta",sep = "" ) )

# tz14.housing$hh_j06
tz14.housing.clean =   tz14.housing %>%
  mutate (hh_i10 = as.numeric(hh_i10)) %>%
  ## floor ###
  mutate( floor_dirt_sand_dung = case_when(
    hh_i10 == 1 ~ 1,
    hh_i10 == 2 ~ 0,
    hh_i10 == 3 ~ 1)) %>%
  mutate (hh_i09 = as.numeric(hh_i09)) %>% 
  ## roof ###
  mutate( roof_not_natural = if_else( hh_i09 != 1 & hh_i09!=2  ,1,0)) %>%
  mutate( roof_iron = if_else( hh_i09 == 4 ,1,0)) %>%
  select(y3_hhid,floor_dirt_sand_dung,
         roof_not_natural,roof_iron)

tz14.merged = left_join(tz14.merged,tz14.housing.clean, by=c("y3_hhid"))


write.csv(tz14.merged,"data/clean/household/tz14_hh.csv",row.names = FALSE)



