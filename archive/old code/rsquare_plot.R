
##### 
rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")

setwd("~/Box Sync/Research/Malawi_FewS/")
malawi <- read.csv("malawi_data_new.csv")
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



######## kernel density plots #########
summary(logFCS,na.rm = TRUE)
summary(logFCS_clust_hat_3,na.rm = TRUE)
summary(logFCS_TA_hat_3,na.rm = TRUE)
summary(logFCS_ipczone_hat_3,na.rm = TRUE)

require(grid)
dev.off()






