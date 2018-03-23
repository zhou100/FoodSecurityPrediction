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
library(caret)





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

name_mat<-matrix(NA,length(levels),length(namelist))
names_mat2<-matrix(NA,4*3,6)
for (j in 1:length(namelist)){
  for (i in 1:length(levels)){
    name_mat[i,j]  <-paste(paste(paste(namelist[j],levels[i],sep="_"),"_hat_",sep=""))
    for (s in 1:3){
      names_mat2[(i-1)*3+s,j]<-paste(name_mat[i,j],s,sep="")
    }
  }
}

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





mylist=vector(mode="list", length = 24) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat3[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,4,4))
}

# RCSI 

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,3])
  mylist[[j]][4,4]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,3]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,2]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,1]<-nrow(malawi[malawi[,number]<3  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][3,4]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9   & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9  &malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][2,4]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][1,4]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  

}


for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  aggregate_diag_percent = ((mat_table[1,1] + mat_table[1,2] + mat_table[2,1] + mat_table[2,2])+mat_table[3,3]+mat_table[4,3]+mat_table[3,4]+mat_table[4,4])/total_num
  aggregate_lower_percent = (mat_table[3,1] + mat_table[3,2] + mat_table[4,1] + mat_table[4,2])/total_num
  aggregate_upper_percent = (mat_table[1,3] + mat_table[2,3] + mat_table[1,4] + mat_table[2,4])/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","mild_pred","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","mild_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,3],sep=" " ))  %>%
    kable_styling(bootstrap_options = "striped", full_width = F,font=9))
 cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  
 cat("The Percent of the aggregated diagonal is ", aggregate_diag_percent, "\\\\ \n\n")
  
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
 cat("The Percent of the aggregated Lower Triangle is ", aggregate_lower_percent, "\\\\ \n\n")
  cat("The Percent of the aggregated Upper Triangle is ", aggregate_upper_percent, "\\\\ \n\n")
  
}
  

 
# RCSI_tail 
for (j in 1:12){
  number<-which(colnames(malawi)==names_mat4[j,2])
  mylist[[j]][4,4]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,3]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,2]<-nrow(malawi[malawi[,number]<3 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][4,1]<-nrow(malawi[malawi[,number]<3  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][3,4]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9   & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,3]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9  &malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<9  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][2,4]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,3]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]>9 & malawi[,number]<19  & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
  mylist[[j]][1,4]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI<=3 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,3]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>3 & malawi$RCSI<=9 & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>9 & malawi$RCSI<=19  & is.na(malawi$RCSI)==FALSE,])
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]>19 & malawi$RCSI>19 & is.na(malawi$RCSI)==FALSE,])
  
}


for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  aggregate_diag_percent = ((mat_table[1,1] + mat_table[1,2] + mat_table[2,1] + mat_table[2,2])+mat_table[3,3]+mat_table[4,3]+mat_table[3,4]+mat_table[4,4])/total_num
  aggregate_lower_percent = (mat_table[3,1] + mat_table[3,2] + mat_table[4,1] + mat_table[4,2])/total_num
  aggregate_upper_percent = (mat_table[1,3] + mat_table[2,3] + mat_table[1,4] + mat_table[2,4])/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","mild_pred","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","mild_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat2[j,3],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  
  cat("The Percent of the aggregated diagonal is ", aggregate_diag_percent, "\\\\ \n\n")
  
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
  cat("The Percent of the aggregated Lower Triangle is ", aggregate_lower_percent, "\\\\ \n\n")
  cat("The Percent of the aggregated Upper Triangle is ", aggregate_upper_percent, "\\\\ \n\n")
  
}


### RCSI new cut off
mylist=vector(mode="list", length = 12) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat3[i,1]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,3,3))
}

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat3[j,1])
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


for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat3[j,1],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}


### RCSI tail new cutoff 
mylist=vector(mode="list", length = 12) # one for each commodity
for (i in 1:12){
  mat_name<-names_mat4[i,2]
  mylist[[i]]<-assign(paste(mat_name,"hit",sep="_"),matrix(NA,3,3))
}

for (j in 1:12){
  number<-which(colnames(malawi)==names_mat4[j,2])
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


for (j in 1:12){
  DT<-as.data.frame(mylist[[j]])
  mat_table<-as.matrix(mylist[[j]])
  total_num<-sum(rowSums(mat_table))
  diag_percent= tr(mat_table)/ total_num
  lower_percent = sum(mat_table[lower.tri(mat_table, diag = FALSE) ] )/total_num   
  upper_percent = sum(mat_table[upper.tri(mat_table, diag = FALSE) ] )/total_num
  rownames(DT)<-c("severe_pred.", "moderate_pred.","food_secure_predict")
  colnames(DT)<-c("severe_actual", "moderate_actual","food_secure_actual")
  print(kable(DT, "latex",caption = paste("Hit and Miss Table with Measure:",names_mat4[j,2],sep=" " ))  %>%
          kable_styling(bootstrap_options = "striped", full_width = F,font=9))
  cat("The Percent of the Diagnol is ", diag_percent, "\\\\ \n\n")
  cat("The Percent of Lower Triangle is ", lower_percent, "\\\\ \n\n")
  cat("The Percent of Upper Triangle is ", upper_percent, "\\\\ \n\n")
  
}

# logFCS 
# log(28 42 )
for (j in 1:12){
  number<-which(colnames(malawi)==names_mat2[j,1])
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
  mylist[[j]][1,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][2,1]<-nrow(malawi[malawi[,number]<3 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][3,1]<-nrow(malawi[malawi[,number]<3  & malawi$HDDS>=6 & is.na(malawi$HDDS)==FALSE,])
  
  mylist[[j]][1,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS<=3 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][2,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6 & malawi$HDDS>3 & malawi$HDDS<6 & is.na(malawi$HDDS)==FALSE,])
  mylist[[j]][3,2]<-nrow(malawi[malawi[,number]>3 & malawi[,number]<6  & malawi$HDDS>6 & is.na(malawi$HDDS)==FALSE,])
  
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


##### 

#4. Scatterplot of actual vs predicated y ( model 1,4,6)  for different levels of hdds 

y1<-malawi[[names_mat2[1,6]]]
y2<-malawi[[names_mat2[2,6]]]
y3<-malawi[[names_mat2[3,6]]]
y<-malawi[["HDDS"]]
plot.df<-data.frame(y1=y1,y2=y2,y3=y3,y=y)
ggplot(plot.df, aes(y1,y,color=as.factor(y)))  + geom_point(colour = 'red') +   geom_smooth(method = "lm",se=TRUE,color =  'red') + 
  geom_point(aes(y2,y),color='blue') + geom_point(aes(y3,y),color='green') + 
  theme_grey()+ labs(title ="Plot of Predict vs Actual HDDS by model at IPC zone level", x = "Predicted HDDS", y = "Actual HDDS") +
  geom_abline(intercept = 0, slope = 1) + 
  theme(plot.margin = unit(c(1, 3, 1, 1), "lines"))
grid.text("Model 1 in red ", x = unit(0.89, "npc"), y = unit(0.80, "npc"))
grid.text("Model 2 in blue", x = unit(0.89, "npc"), y = unit(0.56, "npc"))
grid.text("Model 3 in green", x = unit(0.89, "npc"), y = unit(0.31, "npc"))
grid.text("45 degree line in black", x = unit(0.89, "npc"), y = unit(0.11, "npc"))




y1<-malawi[[names_mat2[4,6]]]
y2<-malawi[[names_mat2[5,6]]]
y3<-malawi[[names_mat2[6,6]]]
y<-malawi[["HDDS"]]
plot.df<-data.frame(y1=y1,y2=y2,y3=y3,y=y)
ggplot(plot.df, aes(y1,y,color=as.factor(y)))  + geom_point(colour = 'red') +   geom_smooth(method = "lm",se=TRUE,color =  'red') + 
  geom_point(aes(y2,y),color='blue') + geom_point(aes(y3,y),color='green') + 
  theme_grey()+ labs(title ="Plot of Predict vs Actual HDDS by model at TA level", x = "Predicted HDDS", y = "Actual HDDS") +
  geom_abline(intercept = 0, slope = 1) + 
  theme(plot.margin = unit(c(1, 3, 1, 1), "lines"))

grid.text("Model 1 in red ", x = unit(0.89, "npc"), y = unit(0.80, "npc"))
grid.text("Model 2 in blue", x = unit(0.89, "npc"), y = unit(0.56, "npc"))
grid.text("Model 3 in green", x = unit(0.89, "npc"), y = unit(0.31, "npc"))
grid.text("45 degree line in black", x = unit(0.89, "npc"), y = unit(0.11, "npc"))


y1<-malawi[[names_mat2[10,1]]]
y2<-malawi[[names_mat2[11,1]]]
y3<-malawi[[names_mat2[12,1]]]
y<-malawi[["logFCS"]]
plot.df<-data.frame(y1=y1,y2=y2,y3=y3,y=y)
ggplot(plot.df, aes(y1,y,color=as.factor(y)))  + geom_point(colour = 'red') +   geom_smooth(method = "lm",se=TRUE,color =  'red') + 
  geom_point(aes(y2,y),color='blue') + geom_point(aes(y3,y),color='green') + 
  theme_grey()+ labs(title ="Plot of Predict vs Actual logFCS by model at cluster level", x = "Predicted logFCS", y = "Actual logFCS") +
  geom_abline(intercept = 0, slope = 1) + 
  theme(plot.margin = unit(c(1, 3, 1, 1), "lines"))

grid.text("Model 1 in red ", x = unit(0.89, "npc"), y = unit(0.80, "npc"))
grid.text("Model 2 in blue", x = unit(0.89, "npc"), y = unit(0.56, "npc"))
grid.text("Model 3 in green", x = unit(0.89, "npc"), y = unit(0.31, "npc"))
grid.text("45 degree line in black", x = unit(0.89, "npc"), y = unit(0.11, "npc"))





y1<-malawi[[names_mat2[10,6]]]
y2<-malawi[[names_mat2[11,6]]]
y3<-malawi[[names_mat2[12,6]]]
y<-malawi[["HDDS"]]
plot.df<-data.frame(y1=y1,y2=y2,y3=y3,y=y)
require(grid) 


# density estimation using hist, locfit and density

FCS_IPC<-na.omit(logFCS_ipczone_hat_3)
FCS_TA<-na.omit(logFCS_TA_hat_3)
FCS_clust<-na.omit(logFCS_clust_hat_3)
FCS_actual<-na.omit(logFCS)

library(sm)
# # plot densities 

plot(density(FCS_IPC), lwd = 3, col = 8, yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(FCS_TA), lwd = 3, col = 2, yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(FCS_clust), lwd = 3, col = 3, yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(FCS_actual,kernel = "gaussian"), lwd = 3, col = 4, yaxt="n", xaxt = "n", main = "")
par(new=TRUE)

malawi2<-na.omit(malawi)
sm.density.compare(malawi2$logFCS, malawi2$logFCS_clust_hat_3, xlab="Species",na.rm=TRUE)

dev.off() 

plot(density(logFCS_ipczone_hat_3,na.rm=TRUE), lwd = 3, col = 8, xlab="",yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(logFCS_TA_hat_3,na.rm=TRUE), lwd = 3, col = 2,xlab="", yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(logFCS_clust_hat_3,na.rm=TRUE), lwd = 3, col = 3, xlab="",yaxt="n", xaxt = "n", main = "")
par(new=TRUE)
plot(density(logFCS,na.rm=TRUE), lwd = 3, col = 4,xlab="", main = "")
par(new=TRUE)

library(car)
densityPlot(logFCS,na.rm=TRUE,normalize=TRUE)
par(new=TRUE)
densityPlot(logFCS_ipczone_hat_3,na.rm=TRUE,normalize=FALSE,col=3,xlab="")
par(new=TRUE)
densityPlot(logFCS_TA_hat_3,na.rm=TRUE,normalize=FALSE,col=2,xlab="")
par(new=TRUE)
densityPlot(logFCS_clust_hat_3,na.rm=TRUE,normalize=FALSE,col=4,xlab="")


##### r square plots #########
r2<-read.csv("r2.csv",header=FALSE,sep="")
colnames(r2)<-c("model","rsquare")

r2$rsquare[1]<- 0.17874373
r2<-r2[,1:2]
subset<-r2

subset$level<-"Cluster"
subset$level[4:6]<-"TA"
subset$level[16:18]<-"TA"
subset$level[25:27]<-"TA"

subset$level[7:9]<-"IPC zone"
subset$level[19:21]<-"IPC zone"
subset$level[28:30]<-"IPC zone"


subset$level[10:12]<-"Household"
subset$level[31:36]<-"Household"

subset$m<-1
three_indexes<-seq(1,nrow(subset),3)
subset$m[three_indexes]<-3
subset$m[three_indexes+1]<-2

subset$measure<-"HDDS"
subset$measure[34:36]<-"logFCS"

subset$measure[10:21]<-"RCSI"
subset$measure[21:30]<-"logFCS"



ord <- c("IPC zone","TA","Cluster","Household")
subset$level <- factor(subset$level,levels=ord)

subset$models <- factor(subset$m)

rplot<-ggplot(subset, aes(x =level, y = rsquare,colour =models,shape = measure))+ geom_point(size=5) 
rplot<-rplot + labs(title =" R-squares of different models by levels and food security measures", x = "Geo-spatial level", y = "adjusted (Pseudo) R-squares")
print(rplot + scale_colour_manual(values = c("Blue", "Red", "Green")))

ggplot(subset, aes(x =level, y = rsquare,colour = as.factor(m),)) + geom_boxplot() + facet_grid(~measure)+ggtitle("R-squares of different models by levels and food security measures")


ggplot(subset, aes(x =level, y = rsquare)) + geom_boxplot() + facet_grid(~measure)+ggtitle("R-squares of different models by levels and food security measures")


dev.off()

######## kernel density plots #########
summary(logFCS,na.rm = TRUE)
summary(logFCS_clust_hat_3,na.rm = TRUE)
summary(logFCS_TA_hat_3,na.rm = TRUE)
summary(logFCS_ipczone_hat_3,na.rm = TRUE)

require(grid)
dev.off()





FCS_data<-cbind(malawi$logFCS_ipczone_hat_3,malawi$logFCS_TA_hat_3,malawi$logFCS_clust_hat_3,malawi$logFCS_ind_hat_3,malawi$logFCS)
colnames(FCS_data)<-c("IPC","TA","cluster","hh","actual")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
ggplot(as.data.frame(long_fcs),aes(x=logFCS, color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.8) + geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)
#ggplot(long_fcs,aes(x=logFCS, fill=level)) + geom_histogram(alpha=0.25)
ggplot(long_fcs,aes(x=level,y= logFCS,fill=level)) +  geom_boxplot() + geom_hline(yintercept = log(28),linetype=2) + geom_hline(yintercept = log(42),linetype=2)







RCSI_data<-cbind(malawi$RCSI_ipczone_hat_3,malawi$RCSI_TA_hat_3,malawi$RCSI_clust_hat_3,malawi$RCSI_ind_hat_3,malawi$RCSI)
colnames(RCSI_data)<-c("IPC","TA","cluster","hh","actual")
long_RCSI<- melt(RCSI_data,na.rm = TRUE)

long_RCSI$RCSI[long_RCSI$RCSI<0]<-0

colnames(long_RCSI)<-c("no","level","RCSI")

ggplot(as.data.frame(long_RCSI),aes(x=RCSI, color=level)) + geom_density(alpha=0.25)+xlim(0, 30)+ geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=9,linetype=2) +  geom_vline(xintercept=19,linetype=2) 
ggplot(long_RCSI,aes(x=level,y= RCSI,fill=level)) +  geom_boxplot()+ geom_hline(yintercept=3,linetype=2) + geom_hline(yintercept=9,linetype=2) +  geom_hline(yintercept=19,linetype=2)




HDDS_data<-cbind(malawi$HDDS_ipczone_hat_3,malawi$HDDS_TA_hat_3,malawi$HDDS_clust_hat_3,malawi$HDDS_ind_hat_3,malawi$HDDS)
colnames(HDDS_data)<-c("IPC","TA","cluster","hh","actual")
long_HDDS<- melt(HDDS_data,na.rm = TRUE)
colnames(long_HDDS)<-c("no","level","HDDS")
ggplot(as.data.frame(long_HDDS),aes(x=HDDS, color=level)) + geom_density(alpha=0.25,show.legend = TRUE) +xlim(1.5, 7.6) + geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=6,linetype=2)
ggplot(long_HDDS,aes(x=level,y= HDDS,fill=level)) +  geom_boxplot()+ geom_hline(yintercept = 3,linetype=2) + geom_hline(yintercept =6,linetype=2)



malawi <- read.csv("malawi_data_tobit.csv")
RCSI_data<-cbind(malawi$RCSI_ipczone_tobit3,malawi$RCSI_TA_tobit3,malawi$RCSI_clust_tobit3,malawi$RCSI_ind_tobit_3,malawi$RCSI)
RCSI_data[RCSI_data<0]<-0
colnames(RCSI_data)<-c("IPC","TA","cluster","hh","actual")
long_RCSI<- melt(RCSI_data,na.rm = TRUE)
colnames(long_RCSI)<-c("no","level","RCSI")
ggplot(as.data.frame(long_RCSI),aes(x=RCSI, color=level)) + geom_density(alpha=0.25)+xlim(0, 30) + geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=10,linetype=2)

ggplot(long_RCSI,aes(x=level,y= RCSI,fill=level)) +  geom_boxplot()+ geom_hline(yintercept =4 ,linetype=2) + geom_hline(yintercept =10,linetype=2)




 