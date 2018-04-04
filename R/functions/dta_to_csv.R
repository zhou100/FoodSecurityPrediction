####################################################################
# Goal : find all the Stata generated data files in the  folder and tranform them to R recognizable format
# input : dta 
# output: csv files  
# 
# # Yujun Zhou -  03/20/18
###################################################################


# install.packages("readstata13")

library(readstata13)

# list all the stata files in the folder 
stata_file_list <-list.files(path = "data/clean/cleaned_dataset",patter = ".dta",full.names = TRUE)
stata_file_list

# list 
csv_file_list <-list.files(path = "data/clean/cleaned_dataset",patter = ".dta",full.names = TRUE)
 csv_file_list<-unlist(lapply(csv_file_list, function(x){gsub(pattern = ".dta",replacement = ".csv",x)}))
csv_file_list



for (i in 1:length(stata_file_list) ){
  
  dat <- read.dta13(stata_file_list[i])
  write.csv(dat,file = csv_file_list[i])
  
}

