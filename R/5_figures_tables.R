# wide to long
library(tidyverse)

library(readxl)
result_tables_fig1 <- read_excel("output/graphs/result_tables_CV.xlsx", sheet = "Figure1")

# result_tables_fig1 <- read_excel("output/graphs/result_tables_CV_mild.xlsx", sheet = "Sheet1")

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

result_tables_fig1 = result_tables_fig1 %>%
  mutate(CM =  paste(Country, Measure, sep = '-') )


figure1_acc<-ggplot(data = result_tables_fig1, aes(x = CM, y = Accuracy,colour = Model)) 
figure1_acc<-figure1_acc + geom_point(size=4)
figure1_acc<-figure1_acc + labs( x = "", y = "Accuracy")

figure1_acc <- figure1_acc+ theme_classic()    
figure1_acc <- figure1_acc +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                            axis.title=element_text(size=17,face="bold") ) 


figure1_acc

ggsave("figure1_acc.png", plot = figure1_acc,device = "png",path = "output/graphs/figure1_3category",
       dpi = 1000, limitsize = TRUE)


# Recall

figure1_rec<-ggplot(data = result_tables_fig1, aes(x = CM, y = Recall,colour = Model   )) 
figure1_rec<-figure1_rec + geom_point(size=4)
figure1_rec<-figure1_rec + labs( x = "", y = "Recall")

figure1_rec <- figure1_rec+ theme_classic()    
figure1_rec <- figure1_rec +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                    axis.title=element_text(size=17,face="bold") ) 


figure1_rec

ggsave("figure1_rec.png", plot = figure1_rec,device = "png",path = "output/graphs/figure1_3category",
       dpi = 1000, limitsize = TRUE)


figure1_precision<-ggplot(data = result_tables_fig1, aes(x = CM, y = Precision,colour = Model   )) 
figure1_precision<-figure1_precision + geom_point(size=4)
figure1_precision<-figure1_precision + labs( x = "", y = "Precision")

figure1_precision <- figure1_precision+ theme_classic()    
figure1_precision <- figure1_precision +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                                axis.title=element_text(size=17,face="bold") ) 


figure1_precision


# F-1_score

figure1_f1<-ggplot(data = result_tables_fig1, aes(x = CM, y = `F-1_score`,colour = Model )) 
figure1_f1<-figure1_f1 + geom_point(size=4)
figure1_f1<-figure1_f1 + labs( x = "", y = "F-1 score")

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
result_tables <- read_excel("output/graphs/result_tables_CV.xlsx",sheet = "Figure2")


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

# Precision
figure3_precision<-ggplot(data = result_tables, aes(x = `C-M`, y = Precision,colour = Model , shape = sampling)) 
figure3_precision<-figure3_precision + geom_point(size=4)
figure3_precision<-figure3_precision + labs( x = "", y = "Precision")

figure3_precision <- figure3_precision+ theme_classic()    
figure3_precision <- figure3_precision +  theme(plot.title = element_text(size = 12, face = "bold"),legend.title=element_text(size=20), legend.text=element_text(size=18),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                                                axis.title=element_text(size=17,face="bold") ) 


figure3_precision

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











result_xgb = results %>% dplyr::filter(model != "logistic")
result_xgb_log = results %>% dplyr::filter(model == "logistic")


ggplot(data = result_xgb,aes(x=Accuracy,color=Measure,y=Recall)) +
  
  geom_line(size=1.5,show.legend = TRUE)+
  geom_point(data =result_xgb_log ,size=2,show.legend = TRUE)+
  geom_text(label=result_xgb$model,check_overlap = TRUE, angle = 0,hjust=0.5,vjust=0.5) +
  geom_text(data =result_xgb_log ,check_overlap = TRUE, angle = 0,label=result_xgb_log$model,vjust = 1.5)+
  theme_classic()




