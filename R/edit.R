

csv_file_list <-list.files(path = "data/clean/cleaned_dataset",pattern =  ".csv",full.names = TRUE)
csv_file_list

pattern = "data/clean/cleaned_dataset/|.csv"

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