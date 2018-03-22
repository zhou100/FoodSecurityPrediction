##################################################################
# Goal : aggreagate different years of data 
# input : csv files 
# output: csv files  
# 
# Yujun Zhou -  03/20/18
###################################################################

rm(list = ls())

data_path = "data/clean/cleaned_dataset/"
csv_file_list <-list.files(path = data_path,pattern =".csv",full.names = TRUE)
csv_file_list

 pattern = "data/clean/cleaned_dataset/|.csv"


list2env(
  lapply(setNames(csv_file_list, make.names(gsub(pattern, "", csv_file_list))), 
         read.csv), envir = .GlobalEnv)

# 1 "Northern" 2 "Central" 3 "Southern"

FCS_2013_Malawi$case_id<-as.character(FCS_2013_Malawi$case_id)
FCS_2010_Malawi$case_id<-as.character(FCS_2010_Malawi$case_id)
FCS_2010_Malawi$region<-as.factor(FCS_2010_Malawi$region)

 
malawi<-dplyr::bind_rows(FCS_2010_Malawi,FCS_2013_Malawi)
 
