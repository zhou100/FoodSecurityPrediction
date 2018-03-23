##################################################################
# Author: Yujun Zhou, University of Illinois , Sep 25, 2017
# R code for automatic cleaning, processing and merging excel tables
#################################################################

#setwd("C:\\Users\\Administrator\\Box Sync\\Research\\Price_data_auto\\Market data")  # wd windows version
setwd("/Users/yujunzhou/Box Sync/Research/Price_data_auto")
rm(list=ls())
library("readxl")
library("tidyr")
library("dplyr")
library("openxlsx")
library("zoo")

# list the price data files
file_list <- list.files(path=paste(getwd(),"new price data",sep="/"), 
                        pattern = "xls$",
                        full.names=TRUE)
#file_list

# list the old price data (not necessary when updating)
file_list2 <- list.files("/Users/yujunzhou/Box Sync/Research/Price_data_auto/Market data", 
                        pattern = "xls$",
                        full.names=TRUE)
file_list_all<-append(file_list,file_list2)

# replace file_list_all with file_list if you are only updating the new data 
dfnames<-file_list_all

# remove irregulars in the file names, 
pattern<-c(getwd(),"/new price data/","/Market data/","MONTTHLY ","MONTHLY ","   \\(Autosaved\\)","MONTHY ","MONTH.","*.xls","prices","//")
for (i in 1:length(pattern)) {
  dfnames <- gsub(pattern[i],"", dfnames)
}
dfnames<-gsub("  ", "_", dfnames)
dfnames<-gsub(" ", "_", dfnames)
dfnames

#length(dfnames)

list2env(
  lapply(setNames(file_list_all, make.names(dfnames)), 
         function(i){read_excel(i,sheet = "Data",na = "empty")}), envir = .GlobalEnv)

# stack the tables in a list 
#slist_df = lapply(dfnames, get)
# start the loop for each data frame 
for (i in 1:length(dfnames)){
  # exclude unneeded cols  
  temp <- get(dfnames[i])
  temp[temp=="NA"]<-NA # replace "NA" with NA just in case..
  #Filter(function(x)!all(is.na(x)), temp)
  temp <- temp[,colSums(is.na(temp))<nrow(temp)]
  # find the row where the first column == ADD
  row_head<-which(temp[,1]=="ADD")
  col_names<-as.numeric(temp[(row_head-1),])
  col_names[which.min(col_names)]<-NA
  col_names<-convertToDate(col_names)
  #col_names[1:3]<-c("Province","DISTRICT","Market")
  colnames(temp)<-col_names
  colnames(temp)[1:3]<-c("Province","DISTRICT","Market")
  col_end<-which(temp[(row_head-1),]=="Start Date:")-2
  
  #colnames(temp)[3]<-"Market"
  temp<-temp[(row_head+1):nrow(temp),]
  #print(head(temp))
  temp<-temp[,1:col_end[1]]
  blanks<-which(rowSums(is.na(temp))==ncol(temp))  # row numbers of blanks 
  #print(length(blanks))
  commodity<-which(rowSums(is.na(temp))==ncol(temp)-1 & !is.na(temp$Market) & temp$Market!="AVERAGE PRICE" & temp$Market!="the end") #row numbers of 
  #print(length(commodity))
  avg_price<-which(!is.na(temp$Market) & temp$Market=="AVERAGE PRICE")  #row numbers of avg_price
  #print(length(avg_price))
  
  #save chunks into separate tables
  commodity_names<-temp$Market[commodity]
  commodity_names<-gsub(" ", "_", commodity_names, fixed = TRUE)
  commodity_names<-gsub("/", "_", commodity_names, fixed = TRUE)
  #commodity_names
  

  
  # the data will be between the row number of commodities and the first break that appears 
  for (j in 1:length(commodity)){
    assign(paste0(commodity_names[j],i), temp[(commodity[j]+1):(avg_price[j]-1),])
  }
}  


# create a master list (list of lists containing all the data) 
mylist=vector(mode="list", length = length(commodity_names)) # one for each commodity

# for each commodity, store all the tables of the same commodity in one list
for (i in 1:length(commodity_names)){
  mylist[[i]]<-assign(paste(commodity_names[i],"list",sep="_"),vector(mode="list", length = length(dfnames)))
}

# get the data from workspace and store them in the master list 
for (j in 1:length(commodity_names)) {
for (i in 1:length(dfnames)) {
  mylist[[j]][[i]]<-get(paste(commodity_names[1],i,sep=""))
}
}


result=vector(mode="list", length = length(commodity_names))

# use reduce merge to merge all the tables of the same commodity
# generate a folder called merged
dir.create("merged")
# mylist[[i]][[1]],...,mylist[[i]][[j]] will need to manually put in regarding the numbers of data tables that you read in.
for (i in 1:length(commodity_names)){
  result[[i]]<-Reduce(function(...) merge(..., all=TRUE), list(mylist[[i]][[1]],mylist[[i]][[2]], mylist[[i]][[3]], mylist[[i]][[4]],mylist[[i]][[5]],mylist[[i]][[6]],mylist[[i]][[7]],mylist[[i]][[8]],mylist[[i]][[9]],mylist[[i]][[10]],mylist[[i]][[11]],mylist[[i]][[12]],mylist[[i]][[13]],mylist[[i]][[14]],mylist[[i]][[15]],mylist[[i]][[16]],mylist[[i]][[17]],mylist[[i]][[18]]))
  table<-as.data.frame(result[[i]])
  table<-table[,colSums(is.na(table))<nrow(table)] #remove empty columns.
  col_Name<-table$Market # save mkt names for later.
  col_Name[duplicated(col_Name)]
  table<-table[,3:ncol(table)] #remove province and district cols.
  date<-colnames(table)[2:ncol(table)] # save the dates for later
  table<-as.data.frame(t(table[,-1])) #  doing the transpose
  colnames(table) <- col_Name # rename colnames to be mkts
  table$date<-date # 
  rownames(table)<-table$date
  table$year<-table$date
  table$week<-strftime(date,format = "%V") #save the weeks
  table$year<-strftime(date,format = "%Y") #save the years
  refcols <- c("year", "week")
  table <- table[, c(refcols, setdiff(names(table), refcols))]
  table<-table[order(date),] 
  table<-subset(table, select=-date)
  write.csv(table,file = paste("merged",paste(commodity_names[i],".csv",sep=""),sep="/")) # write out in csv format 
}




