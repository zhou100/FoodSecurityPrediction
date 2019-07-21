rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")
library(gtable)
library(grid)

malawi <- read.csv("data/malawi_data_new.csv")

# start here 
data<-cbind(malawi$RCSI,(malawi$FCS),(malawi$HDDS),malawi$month,malawi$ipczone,malawi$TA,malawi$clust)
colnames(data)<-c("RCSI","FCS","HDDS","Month","IPC","TA","Cluster")
data<-as.data.frame(data)

data$month_clust <-interaction(data$Month,data$Cluster)
data$RCSI[data$RCSI>42]<-42
data<-data[data$HDDS!=0,]



#	Maybe a bar chart of variation in FS security measures (month, IPCzone, TA, cluster, month x cluster)




r2 = matrix(NA,3,5)
my_lms <- lapply(1:3, function(x) lm(data[,x] ~ as.factor(Month),data=data))
summaries <- lapply(my_lms, summary)
r2[,1]<-sapply(summaries, function(x) c(r_sq = x$r.squared))


my_lms <- lapply(1:3, function(x) lm(data[,x] ~ as.factor(IPC),data=data))
summaries <- lapply(my_lms, summary)
r2[,2]<-sapply(summaries, function(x) c(r_sq = x$r.squared))


my_lms <- lapply(1:3, function(x) lm(data[,x] ~ as.factor(TA),data=data))
summaries <- lapply(my_lms, summary)
r2[,3]<-sapply(summaries, function(x) c(r_sq = x$r.squared))


my_lms <- lapply(1:3, function(x) lm(data[,x] ~ as.factor(Cluster),data=data))
summaries <- lapply(my_lms, summary)
r2[,4]<-sapply(summaries, function(x) c(r_sq = x$r.squared))



my_lms <- lapply(1:3, function(x) lm(data[,x] ~ as.factor(month_clust),data=data))
summaries <- lapply(my_lms, summary)
r2[,5]<-sapply(summaries, function(x) c(r_sq = x$r.squared))

length(unique(data$Cluster))
length(unique(data$month_clust))


r2 <-as.data.frame(r2)

colnames(r2)<-c("Month","IPC","TA","Cluster","Cluster_by_month")
rownames(r2)<-c("RCSI","FCS","HDDS")
#r2$measure<-c("RCSI","FCS","HDDS")


long_r2<- melt(t(r2),na.rm=TRUE,measure.vars =c("RCSI","FCS","HDDS"))
long_r2<-long_r2[long_r2$Var1!="measure",]

colnames(long_r2)<-c("variation","measure","rsquares")

ar<-as.character(long_r2$rsquares)
long_r2$ar<-as.numeric(ar)

r2
# RCSI_r2_fe =  .22320556
# HDDS_r2_fe =  .30987621
# logFCS_r2_fe =  .33045673


p<-ggplot(long_r2, aes(variation,ar)) +geom_bar(stat = "identity",position = "dodge",aes(fill = measure))

p <- p + labs(y = "R squares",
              x = " ",
              colour = "Food Security Measures",
              shape= " ")

p <- p+ theme_bw() 
p <- p + theme(text = element_text(size=22)) 
p               

ggsave("bar_plot_r2.png", plot = p,device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)

