
library("reshape2")
library("ggplot2")
library("zoo")
##### r square plots #########
r2<-read.csv("data/r2_cluster.csv",header=FALSE,sep="")
colnames(r2)<-c("model","rsquare")

string_list<- strsplit(as.character(r2$model),split="_")
r2_list<- strsplit(as.character(r2$rsquare),split=",")

for (i in 1:nrow(r2)){
  r2$measure[i] <-(string_list[[i]][1])
  r2$level[i] <-(string_list[[i]][2])
  r2$model_num[i] <-(string_list[[i]][4])
  r2$rsquares[i] <-as.numeric(r2_list[[i]][1])
}

class(r2$rsquare)

r2<-r2[,3:ncol(r2)]

r2$level[r2$level=="ipczone"]<-"IPC Zone"
r2$level[r2$level=="clust"]<-"Cluster"
r2$model_num[r2$model_num=="m1"]<-"Model 1"
r2$model_num[r2$model_num=="m2"]<-"Model 2"
r2$model_num[r2$model_num=="m3"]<-"Model 3"



ord <- c("IPC Zone","TA","Cluster")
r2$level <- factor(r2$level,levels=ord)

r2$models <- factor(r2$model_num)


rplot<-ggplot(r2, aes(x =level, y = rsquares,colour =models,shape = measure))+ geom_point(size=7) 
rplot<-rplot + labs( x = "Geo-spatial level", y = "R Squared")

rplot <- rplot+ theme_bw() 
rplot <- rplot + theme(text = element_text(size=22)) 
print(rplot + scale_colour_manual(values = c("Blue", "Red", "Green")))

# ggplot(r2, aes(x =level, y = rsquares,colour = as.factor(model_num),)) + geom_boxplot() + facet_grid(~measure)+ggtitle("R-squares of different models by levels and food security measures")
#ggplot(r2, aes(x =level, y = rsquares)) + geom_boxplot() + facet_grid(~measure)+ggtitle("R-squares of different models by levels and food security measures")


