rm(list=ls())

library("geosphere")
library("reshape2")
library("ggplot2")
library(knitr)
library(kableExtra)
library(psych)


library("matrixStats")
library("readxl")
library(dplyr)
library(lubridate)
library("readxl")
library("tidyr")
library("dplyr")
library("openxlsx")
library("zoo")
setwd("~/Box Sync/Research/Malawi_FewS/")
malawi <- read.csv("malawi_data_new.csv")
#malawi <- read.csv("malawi_data_ipc.csv")



# malawi$IPC_Value<-as.factor(malawi$ipc2)

# ipc2.f <- factor(ipc2, levels= c(1,2,3),
#                 labels = c("IPC=1", "IPC=2", "IPC=3")) 
# 
# library(sm)
# # plot densities 
# sm.density.compare(malawi$RCSI, ipc2, xlab="levels of RCSI",lty=c(5,2,4))
# title(main="RCSI by IPC value")
# colfill<-c(2:(2+length(levels(ipc2.f)))) 
# legend("right",levels(ipc2.f),fill = colfill)
# abline(v= 3,untf=TRUE)
# abline(v= 8)
# abline(v= 19)
# 
# 
# ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind))
# 
# ggplot(malawi, aes(x=RCSI)) + geom_density(aes(group=IPC_Value, colour=IPC_Value, fill=IPC_Value), alpha=0.3,na.rm = TRUE)
# ggsave("density2.png", width = 10, height = 10)
# 
# 
# ggplot(malawi, aes(x=RCSI)) + geom_density(aes(group=IPC_Value, colour=IPC_Value),na.rm = TRUE)
# ggsave("density3.png", width = 10, height = 10)


library(dplyr)

namelist<-c("logFCS","logFCS_tail","RCSI","RCSI_tail","HDDS","HDDS_tail")
levels<-c("ipczone","TA","clust","ind")

name_mat<-matrix(NA,4*3,6)
names_mat2<-matrix(NA,4*3,6)
for (j in 1:length(namelist)){
  for (i in 1:length(levels)){
    name_mat[i,j]  <-paste(paste(paste(namelist[j],levels[i],sep="_"),"_hat_",sep=""))
    for (s in 1:3){
      names_mat2[(i-1)*3+s,j]<-paste(name_mat[i,j],s,sep="")
    }
  }
}

for (j in 1:6){
  for (i in 1:12){
    names_mat2[i,j]  <- paste(names_mat2[i,j], "_agg",sep = "")
  }
  }
names_mat2


name_mat<-matrix(NA,length(levels),length(namelist))
names_mat3<-matrix(NA,4*3,2)
for (j in 3:4){
  
  for (i in 1:length(levels)){
    name_mat[i,j]  <-paste(paste(paste(namelist[j],levels[i],sep="_"),"_tobit_",sep=""))
    for (s in 1:3){
      names_mat3[(i-1)*3+s,j-2]<-paste(name_mat[i,j],s,sep="")
    }
  }
}
names_mat3

name_mat<-matrix(NA,length(levels),length(namelist))
names_mat4<-matrix(NA,4*3,2)
for (j in 3:4){
  
  for (i in 1:length(levels)){
    name_mat[i,j]  <-paste(paste(paste(namelist[j],levels[i],sep="_"),"_hat_",sep=""))
    for (s in 1:3){
      names_mat4[(i-1)*3+s,j-2]<-paste(name_mat[i,j],s,sep="")
    }
  }
}
names_mat4


names_mat3 < names_mat2


mylist=vector(mode="list", length = 12) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat2[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,4,4))
}


### RCSI new cut off
mylist=vector(mode="list", length = 12) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat3[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,3,3))
}

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat3[j,1])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<=4  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,]) 
  
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>10 &  malawi$RCSI<=4 &  is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
}


for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,3],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}


### RCSI tail new cutoff 
mylist=vector(mode="list", length = 9) # one for each commodity
for (i in 1:9){
  mat_name<-names_mat2[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,3,3))
}

for (j in 1:9){
  number<-which(colnames(malawi)==names_mat2[j,1])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<=4  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>10 &  malawi$RCSI<=4 &  is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE,])
}


for (j in 1:9){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}

# logFCS 
# log(28 42 )
mylist=vector(mode="list", length = 12) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat2[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,3,3))
}

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,1])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE & is.na(malawi[,number])==FALSE, ])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]<log(28)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>log(42) &  malawi$FCS<=28 &  is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>42 & is.na(malawi$FCS)==FALSE & is.na(malawi[,number])==FALSE,])
}

for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num

  rownames(DT)<-c("Poor_predict","Borderline_predict","Acceptable_predict")
  colnames(DT)<-c("Poor_actual","Borderline_actual","Acceptable_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}



# logFCS_tail
# log(21 35 )
for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,2])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<log(28)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE,])
  
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE,])
  
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>log(42) &  malawi$FCS<=28 &  is.na(malawi$FCS)==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>42 & is.na(malawi$FCS)==FALSE,])
}


for (j in 1:12){
  DT<-as.data.frame(t(mylist[[j]]))
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  
  rownames(DT)<-c("Poor_predict","Borderline_predict","Acceptable_predict")
  colnames(DT)<-c("Poor_actual","Borderline_actual","Acceptable_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,2],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
}


# HDDS 3 6
for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,5])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]<3  & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6  & malawi$HDDS>6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
}

for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
 
  rownames(DT)<-c("Low_predict","Medium_predict","Good_predict")
  colnames(DT)<-c("Low_actual","Medium_actual","Good_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,5],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}

# Hdds tail

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,6])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<3  & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE,])
  
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS>6 & is.na(malawi$HDDS)==FALSE,])
  
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE,])
}

for (j in 1:12){
  DT<-as.data.frame(t(mylist[[j]]))
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  
  rownames(DT)<-c("Low_predict","Medium_predict","Good_predict")
  colnames(DT)<-c("Low_actual","Medium_actual","Good_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,6],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
}


hit<-matrix(NA,3,4)
for (i in 1:3){
  hit[i,1]<-nrow(filter(malawi, ipc2 == i & RCSI <=3 & is.na(RCSI)==FALSE))
  hit[i,2]<-nrow(filter(malawi, ipc2 == i & RCSI >3 &  RCSI<9 &is.na(RCSI)==FALSE))
  hit[i,3]<-nrow(filter(malawi, ipc2 == i & RCSI >9 &  RCSI<19 &is.na(RCSI)==FALSE))
  hit[i,4]<-nrow(filter(malawi, ipc2 == i & RCSI >19& is.na(RCSI)==FALSE ))
}


############################
# IPC hit and miss tables 
###########################

namelist2<-c("logFCS","HDDS","RCSI")
ipc_names_mat2<-matrix(NA,3,1)
for (j in 1:length(namelist2)){
  ipc_names_mat2[j,1]  <- paste(namelist2[j],"ipc",sep = "_")
}


# RCSI 
mylist=vector(mode="list", length = 1) # one for each commodity
for (i in 3:3){
  mat_name<-ipc_names_mat2[i,1]
  mylist[[i]]<-assign(paste(ipc_names_mat2[i,1],"hit",sep="_"),matrix(NA,3,3))
}

for (j in 3:3){
  number<-which(colnames(malawi)==ipc_names_mat2[j,1])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]<=4 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<=4  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI<=4 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>4 & malawi[,number]<10  & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>10 &  malawi$RCSI<=4 &  is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>4 & malawi$RCSI<=10 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]>10 & malawi$RCSI>10 & is.na(malawi$RCSI)==FALSE  & is.na(malawi[,number])==FALSE ,])
}

for (j in 3:3){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",ipc_names_mat2[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
}

# logFCS
mylist=vector(mode="list", length = 1) # one for each commodity
for (i in 1:1){
  mat_name<-ipc_names_mat2[i,1]
  mylist[[i]]<-assign(paste(ipc_names_mat2[i,1],"hit",sep="_"),matrix(NA,3,3))
}


for (j in 1:1){
  number<-which(colnames(malawi)==ipc_names_mat2[j,1])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]<log(28) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]<log(28)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS<=28 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>log(28) & malawi[,number]<log(42)  & malawi$FCS>42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>log(42) &  malawi$FCS<=28 &  is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>28 & malawi$FCS<=42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>log(42) & malawi$FCS>42 & is.na(malawi$FCS)==FALSE  & is.na(malawi[,number])==FALSE,])
}

for (j in 1:1){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",ipc_names_mat2[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
}


# HDDS
mylist=vector(mode="list", length = 1) # one for each commodity
for (i in 2:2){
  mat_name<-ipc_names_mat2[i,1]
  mylist[[i]]<-assign(paste(ipc_names_mat2[i,1],"hit",sep="_"),matrix(NA,3,3))
}

for (j in 2:2){
  number<-which(colnames(malawi)==ipc_names_mat2[j,1])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]<3  & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE  & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6  & malawi$HDDS>6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>=6 & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE & is.na(malawi[,number])==FALSE,])
}
 

for (j in 2:2){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",ipc_names_mat2[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
}
