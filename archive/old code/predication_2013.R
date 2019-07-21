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


# logFCS <- read.csv("data/logFCS_predict_table.csv")
# HDDS <- read.csv("data/HDDS_predict_table.csv")
# RCSI <- read.csv("data/RCSI_predict_table.csv")
logFCS <- read.csv("data/all_predict/clust_logFCS_predict_clust_m3.csv")

logFCS_hh <- read.csv("data/logFCS_predict_table_hh.csv")

HDDS <- read.csv("data/all_predict/clust_HDDS_predict_clust_m3.csv")
RCSI <- read.csv("data/all_predict/clust_RCSI_predict_clust_m3.csv")


colnames(logFCS_hh)<-c("logFCS","logFCS_predict")
colnames(HDDS)<-c("HDDS","HDDS_predict")
colnames(RCSI)<-c("RCSI","RCSI_predict")

#"Poor_predict","Borderline_predict","Acceptable_predict"
# Poor_actual","Borderline_actual","Acceptable_actual"

logFCS$cat_logFCS<-cut(logFCS$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS$cat_logFCS_predict<-cut(logFCS$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))

confusionMatrix(logFCS$cat_logFCS_predict,logFCS$cat_logFCS)



logFCS_hh$cat_logFCS<-cut(logFCS_hh$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS_hh$cat_logFCS_predict<-cut(logFCS_hh$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))

confusionMatrix(logFCS_hh$cat_logFCS_predict,logFCS_hh$cat_logFCS)

plot(logFCS$logFCS,logFCS$logFCS_predict)
abline(0,1)
rmse_logFCS = sqrt( sum( (logFCS$logFCS_predict - logFCS$logFCS)^2 , na.rm = TRUE ) / nrow(logFCS) )



# HDDS 3 6 
HDDS$cat_HDDS<-cut(HDDS$HDDS, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
HDDS$cat_HDDS_predict<-cut(HDDS$HDDS_predict, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))

confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS)

plot(HDDS$HDDS,HDDS$HDDS_predict)
abline(0,1)
rmse_HDDS = sqrt( sum( (HDDS$HDDS_predict - HDDS$HDDS)^2 , na.rm = TRUE ) / nrow(HDDS) )




# rcsi P1 < 4
#P2 = 4-17
#P3 = 17-42
#P4/5 > 42



RCSI$cat_RCSI<-cut(RCSI$RCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
RCSI$cat_RCSI_predict<-cut(RCSI$RCSI_predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))

confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI)

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)

RCSI$RCSI_predict[RCSI$RCSI_predict<0] <-0

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)

rmse_RCSI = sqrt( sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ) / nrow(RCSI) )


R2_RCSI <- 1- (sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ))  / sum((RCSI$RCSI-mean(RCSI$RCSI, na.rm = TRUE))^2, na.rm = TRUE )

R2_RCSI <- 1- (sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ))  / sum((RCSI$RCSI-mean(RCSI$RCSI, na.rm = TRUE))^2, na.rm = TRUE )


############################################
###########cluster level averages
############################################


logFCS <- read.csv("data/logFCS_predict_CLUST.csv")
HDDS <- read.csv("data/HDDS_predict_CLUST.csv")
RCSI <- read.csv("data/RCSI_predict_CLUST.csv")
RCSI_predict_CLUST_IPC

#"Poor_predict","Borderline_predict","Acceptable_predict"
# Poor_actual","Borderline_actual","Acceptable_actual"

plot(logFCS$clust_logFCS,logFCS$clust_logFCS_predict)
abline(0,1)
rmse_logFCS = sqrt( sum( (logFCS$clust_logFCS_predict - logFCS$clust_logFCS)^2 , na.rm = TRUE ) / nrow(logFCS) )

R2_logFCS <- 1- (sum( (logFCS$clust_logFCS_predict - logFCS$clust_logFCS)^2 , na.rm = TRUE ))  / sum((logFCS$clust_logFCS-mean(logFCS$clust_logFCS, na.rm = TRUE))^2, na.rm = TRUE )



# HDDS 3 6 

plot(HDDS$clust_HDDS,HDDS$clust_HDDS_predict)
abline(0,1)
rmse_HDDS = sqrt( sum( (HDDS$clust_HDDS_predict - HDDS$clust_HDDS)^2 , na.rm = TRUE ) / nrow(HDDS) )
R2_HDDS <- 1- (sum( (HDDS$clust_HDDS_predict - HDDS$clust_HDDS)^2 , na.rm = TRUE ))  / sum((HDDS$clust_HDDS-mean(HDDS$clust_HDDS, na.rm = TRUE))^2, na.rm = TRUE )
rmse_HDDS
R2_HDDS




plot(RCSI$clust_RCSI,RCSI$clust_RCSI_predict)
abline(0,1)
rmse_RCSI = sqrt( sum( (RCSI$clust_RCSI_predict - RCSI$clust_RCSI)^2 , na.rm = TRUE ) / nrow(RCSI) )
R2_RCSI <- 1- (sum( (RCSI$clust_RCSI_predict - RCSI$clust_RCSI)^2 , na.rm = TRUE ))  / sum((RCSI$clust_RCSI-mean(RCSI$clust_RCSI, na.rm = TRUE))^2, na.rm = TRUE )
rmse_RCSI
R2_RCSI


postResample(pred = RCSI$clust_RCSI_predict, obs = RCSI$clust_RCSI)
postResample(pred = HDDS$clust_HDDS_predict, obs = HDDS$clust_HDDS)
postResample(pred = logFCS$clust_logFCS_predict, obs = logFCS$clust_logFCS)

# rcsi P1 < 4
#P2 = 4-17
#P3 = 17-42
#P4/5 > 42



RCSI$cat_RCSI<-cut(RCSI$RCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
RCSI$cat_RCSI_predict<-cut(RCSI$RCSI_predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))

confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI)

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)

RCSI$RCSI_predict[RCSI$RCSI_predict<0] <-0

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)

rmse_RCSI = sqrt( sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ) / nrow(RCSI) )


R2_RCSI <- 1- (sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ))  / sum((RCSI$RCSI-mean(RCSI$RCSI, na.rm = TRUE))^2, na.rm = TRUE )

R2_RCSI <- 1- (sum( (RCSI$RCSI_predict - RCSI$RCSI)^2 , na.rm = TRUE ))  / sum((RCSI$RCSI-mean(RCSI$RCSI, na.rm = TRUE))^2, na.rm = TRUE )

postResample(pred = RCSI$clust_RCSI_predict, obs = RCSI$clust_RCSI)
 
postResample(pred = HDDS$clust_HDDS_predict, obs = HDDS$clust_HDDS)
 
postResample(pred = logFCS$clust_logFCS_predict, obs = logFCS$clust_logFCS)



############################################
###########cluster level averages using IPC value only 
############################################

library(caret)

logFCS_IPC <- read.csv("data/logFCS_predict_CLUST_IPC.csv")
HDDS_IPC <- read.csv("data/HDDS_predict_CLUST_IPC.csv")
RCSI_IPC <- read.csv("data/RCSI_predict_CLUST_IPC.csv")

postResample(pred = RCSI_IPC$clust_RCSI_predict_ipc, obs = RCSI_IPC$clust_RCSI)

postResample(pred = HDDS_IPC$clust_HDDS_predict_ipc, obs = HDDS_IPC$clust_HDDS)

postResample(pred = logFCS_IPC$clust_logFCS_predict_ipc, obs = logFCS_IPC$clust_logFCS)

apply(logFCS_IPC$clust_logFCS_predict_ipc, 1, postResample, obs = logFCS_IPC$clust_logFCS)

hist(RCSI_IPC$clust_RCSI_predict_ipc)

data<-cbind(RCSI_IPC$clust_RCSI_predict_ipc,HDDS_IPC$clust_HDDS_predict_ipc,logFCS_IPC$clust_logFCS_predict_ipc)
data<-as.data.frame(data)
colnames(data)<-c("RCSI","HDDS","logFCS")
library(plyr)
library(plotrix)
multhist(data)

long_data<- melt(data,na.rm = TRUE)
long_data

colnames(long_data)

ggplot(as.data.frame(long_data),aes(x=value, color=variable)) + geom_bar() 



b + geom_bar()(alpha=0.25,show.legend = TRUE) 

pp <- qplot(value, data=mpgstack) + facet_wrap(~variable, scales="free")


