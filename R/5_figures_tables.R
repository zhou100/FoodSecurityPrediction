# wide to long
library(tidyverse)

library(readxl)
result_tables <- read_excel("output/graphs/result_tables.xlsx", 
                              +     sheet = "Sheet1")


# adjust factor level
# ord_measure <- c("HDDS","rCSI","logFCS")
# r2.df$Measures <- factor(r2.df$Measures,levels=ord_measure)
# 
# ord_level <- c("IPC Zone","TA","Cluster")
# r2.df$level <- factor(r2.df$level,levels=ord_level)
# 
# ord_model <- c("Class 0 (IPC value only)","Class 1 data","Class 1 + Class 2 data","Class 1 + Class 2 + Class 3 data")
# r2.df$Model <- factor(r2.df$Model,levels=ord_model)

 

# figure1_accuracy

figure1_acc<-ggplot(data = result_tables, aes(x = Country, y = Accuracy,colour = Model , shape = Measure )) 
figure1_acc<-figure1_acc + geom_point(size=7)
figure1_acc<-figure1_acc + labs( x = "Country", y = "Accuracy")

figure1_acc <- figure1_acc+ theme_classic()    
figure1_acc <- figure1_acc +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                            axis.title=element_text(size=17,face="bold") ) 


figure1_acc

ggsave("figure1_acc.png", plot = figure1_acc,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)


# Recall

figure1_rec<-ggplot(data = result_tables, aes(x = Country, y = Recall,colour = Model , shape = Measure )) 
figure1_rec<-figure1_rec + geom_point(size=7)
figure1_rec<-figure1_rec + labs( x = "Country", y = "Recall")

figure1_rec <- figure1_rec+ theme_classic()    
figure1_rec <- figure1_rec +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                    axis.title=element_text(size=17,face="bold") ) 


figure1_rec

ggsave("figure1_rec.png", plot = figure1_rec,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)


# F-1_score

figure1_f1<-ggplot(data = result_tables, aes(x = Country, y = `F-1_score`,colour = Model , shape = Measure )) 
figure1_f1<-figure1_f1 + geom_point(size=7)
figure1_f1<-figure1_f1 + labs( x = "Country", y = "F-1 score")

figure1_f1 <- figure1_f1+ theme_classic()    
figure1_f1 <- figure1_f1 +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                  axis.title=element_text(size=17,face="bold") ) 


figure1_f1

ggsave("figure1_f1.png", plot = figure1_f1,device = "png",path = "output/graphs/",
       dpi = 1000, limitsize = TRUE)


### Figure 3 


# wide to long
library(tidyverse)

library(readxl)
result_tables <- read_excel("output/graphs/result_tables.xlsx",sheet = "Figure2")


# figure3_accuracy

figure3_acc<-ggplot(data = result_tables, aes(x = `C-M`, y = Accuracy,colour = Model , shape = sampling )) 
figure3_acc<-figure3_acc + geom_point(size=4)
figure3_acc<-figure3_acc + labs( x = "", y = "Accuracy")

figure3_acc <- figure3_acc+ theme_classic()    
figure3_acc <- figure3_acc +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                    axis.title=element_text(size=17,face="bold") ) 


figure3_acc

ggsave("figure3_acc.png", plot = figure3_acc,device = "png",path = "output/graphs/figure3_3category",
       dpi = 1000, limitsize = TRUE)


# Recall

figure3_rec<-ggplot(data = result_tables, aes(x = `C-M`, y = Recall,colour = Model , shape = sampling)) 
figure3_rec<-figure3_rec + geom_point(size=4)
figure3_rec<-figure3_rec + labs( x = "", y = "Recall")

figure3_rec <- figure3_rec+ theme_classic()    
figure3_rec <- figure3_rec +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                    axis.title=element_text(size=17,face="bold") ) 


figure3_rec

ggsave("figure3_rec.png", plot = figure3_rec,device = "png",path = "output/graphs/figure3_3category",
       dpi = 1000, limitsize = TRUE)


# F-1_score

figure3_f1<-ggplot(data = result_tables, aes(x = `C-M`, y = `F-1_score`,colour = Model , shape = sampling )) 
figure3_f1<-figure3_f1 + geom_point(size=4)
figure3_f1<-figure3_f1 + labs( x = "", y = "F-1 score")

figure3_f1 <- figure3_f1+ theme_classic()    
figure3_f1 <- figure3_f1 +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                  axis.title=element_text(size=17,face="bold") ) 


figure3_f1

ggsave("figure3_f1.png", plot = figure3_f1,device = "png",path = "output/graphs/figure3_3category",
       dpi = 1000, limitsize = TRUE)















