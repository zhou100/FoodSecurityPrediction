rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")


csv_list <- list.files(path="data/all_predict_2010", 
                       pattern = "csv$",
                       full.names=TRUE)
dfnames<-csv_list

# remove irregulars in the file names, 
pattern<-c("data/all_predict_2010/","clust_",".csv","_2010")
for (i in 1:length(pattern)) {
  dfnames <- gsub(pattern[i],"", dfnames)
}
dfnames


list2env(
  lapply(setNames(csv_list, make.names(dfnames)), 
         function(i){read.csv(i)}), envir = .GlobalEnv)

predict_df<-cbind(logFCS_predict_m3[1],RCSI_predict_m3[1],HDDS_predict_m3[1])

for (i in 1:length(dfnames)){
  # exclude unneeded cols  
  temp <- get(dfnames[i])
  predict_df<-cbind(predict_df,temp[2])
}



logFCS_IPC <- read.csv("data/all_ipc/logFCS_predict_CLUST_IPC_2010.csv")
HDDS_IPC <- read.csv("data/all_ipc/HDDS_predict_CLUST_IPC_2010.csv")
RCSI_IPC <- read.csv("data/all_ipc/RCSI_predict_CLUST_IPC_2010.csv")


# predict_df$clust_logFCS_clust_predict_m3

# plot_density <- function(data, measure, arg3=2, ...) {
#   newVar <- sin(arg1) + sin(arg2)  # do Some Useful Stuff
#   newVar / arg3   # return value 
# }



FCS_data<-cbind(predict_df$clust_logFCS_ipczone_predict_m3,predict_df$clust_logFCS_TA_predict_m3,predict_df$clust_logFCS_clust_predict_m3,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
plot_long<- long_fcs[long_fcs$level != "IPC_value_only",]
plot_long_ipc<- long_fcs[long_fcs$level == "IPC_value_only",]


ggplot(as.data.frame(plot_long),aes(x=logFCS,color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  stat_density(data=plot_long_ipc,aes(x=logFCS, y=..scaled..*3.5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*28, name = " density (IPC value only) ")) +
  theme_bw()


ggsave("FCS_2010_full.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)


# with only the ipc and actual 
FCS_data<-cbind(predict_df$clust_logFCS_ipczone_predict_m3,predict_df$clust_logFCS_TA_predict_m3,predict_df$clust_logFCS_clust_predict_m3,predict_df$clust_logFCS,logFCS_IPC$clust_logFCS_predict_ipc)
colnames(FCS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_fcs<- melt(FCS_data,na.rm=TRUE)
colnames(long_fcs)<-c("no","level","logFCS")
plot_actual<- long_fcs[long_fcs$level == "Actual",]
plot_long_ipc<- long_fcs[long_fcs$level == "IPC_value_only",]

ggplot(as.data.frame(plot_actual),aes(x=logFCS, color=level)) + geom_density(alpha=0.1) +xlim(2.8, 4.9) +
  geom_vline(xintercept=log(28),linetype=2) + geom_vline(xintercept=log(42),linetype=2)+
  stat_density(data=plot_long_ipc,aes(x=logFCS, y=..scaled..*3.5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*28, name = "density (IPC value only) ")) +
  theme_bw()

ggsave("FCS_2010_ipc_vs_actual.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)


RCSI_data<-cbind(predict_df$clust_RCSI_ipczone_predict_m3,predict_df$clust_RCSI_TA_predict_m3,predict_df$clust_RCSI_clust_predict_m3,predict_df$clust_RCSI,logRCSI_IPC$clust_RCSI_predict_ipc)
colnames(RCSI_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_rcsi<- melt(RCSI_data,na.rm=TRUE)
colnames(long_rcsi)<-c("no","level","RCSI")
plot_long<- long_rcsi[long_rcsi$level != "IPC_value_only",]
plot_long_ipc<- long_rcsi[long_rcsi$level == "IPC_value_only",]


ggplot(as.data.frame(plot_long),aes(x=RCSI, color=level))+ stat_density(data=plot_long_ipc,aes(x=RCSI,y=..scaled../2.3,color=level)) + geom_density(alpha=0.25)+xlim(0, 45) + 
  geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)+
  scale_y_continuous(name= "density",sec.axis = sec_axis(~.*3.125, name = "density (IPC value only) ")) +
  theme_bw()

ggsave("RCSI_2010_full.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 2000, limitsize = TRUE)


# with only the ipc and actual 
RCSI_data<-cbind(predict_df$clust_RCSI_ipczone_predict_m3,predict_df$clust_RCSI_TA_predict_m3,predict_df$clust_RCSI_clust_predict_m3,predict_df$clust_RCSI,RCSI_IPC$clust_RCSI_predict_ipc)
colnames(RCSI_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_rcsi<- melt(RCSI_data,na.rm=TRUE)
colnames(long_rcsi)<-c("no","level","RCSI")
plot_actual<- long_rcsi[long_rcsi$level == "Actual",]
plot_long_ipc<- long_rcsi[long_rcsi$level == "IPC_value_only",]

ggplot(as.data.frame(plot_actual),aes(x=RCSI, color=level)) + geom_density(alpha=0.25)+xlim(0, 45) + 
  geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)+
  stat_density(data=plot_long_ipc,aes(x=RCSI, y=..scaled../5,color=level)) + scale_y_continuous(sec.axis = sec_axis(~.*3.125*2.15, name = "density (IPC value only) ")) +
  theme_bw()



ggsave("RCSI_2010_ipc_vs_actual.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)

# ggplot(as.data.frame(plot_long),aes(x=RCSI, color=level)) + geom_density(alpha=0.25)+xlim(0, 45) + geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)
# 
# ggplot(long_RCSI,aes(x=level,y= RCSI,fill=level)) +  geom_boxplot()+ geom_hline(yintercept =4 ,linetype=2) + geom_hline(yintercept =17,linetype=2) + geom_hline(yintercept =42,linetype=2)


HDDS_data<-cbind(predict_df$clust_HDDS_ipczone_predict_m3,predict_df$clust_HDDS_TA_predict_m3,predict_df$clust_HDDS_clust_predict_m3,predict_df$clust_HDDS,HDDS_IPC$clust_HDDS_predict_ipc)
colnames(HDDS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_HDDS<- melt(HDDS_data,na.rm=TRUE)
colnames(long_HDDS)<-c("no","level","HDDS")
plot_long<- long_HDDS[long_HDDS$level != "IPC_value_only",]
plot_long_ipc<- long_HDDS[long_HDDS$level == "IPC_value_only",]

ggplot(as.data.frame(plot_long),aes(x=HDDS, color=level))+ geom_density(alpha=0.25,show.legend = TRUE) +
  xlim(1.5, 7.6) + geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=6,linetype=2) +
  stat_density(data=plot_long_ipc,aes(x=HDDS,y=..scaled..*1.1,color=level)) +
  scale_y_continuous(name= "density",sec.axis = sec_axis(~.*12, name = "density (IPC value only) ")) 




# ggplot(as.data.frame(plot_long),aes(x=HDDS, color=level))+ stat_density(data=plot_long_ipc,aes(x=HDDS,y=..scaled../2.3,color=level)) + geom_density(alpha=0.25)+xlim(0, 45) + 
#   geom_vline(xintercept=4,linetype=2) + geom_vline(xintercept=17,linetype=2)+ geom_vline(xintercept=42,linetype=2)+
#   scale_y_continuous(name= "density",sec.axis = sec_axis(~.*3.125, name = "density (IPC value only) ")) +
#   theme_bw()



ggsave("HDDS_2010_full.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)


# with only the ipc and actual 
HDDS_data<-cbind(predict_df$clust_HDDS_ipczone_predict_m3,predict_df$clust_HDDS_TA_predict_m3,predict_df$clust_HDDS_clust_predict_m3,predict_df$clust_HDDS,HDDS_IPC$clust_HDDS_predict_ipc)
colnames(HDDS_data)<-c("IPC_zone","TA","cluster","Actual","IPC_value_only")
long_HDDS<- melt(HDDS_data,na.rm=TRUE)
colnames(long_HDDS)<-c("no","level","HDDS")
plot_actual<- long_HDDS[long_HDDS$level == "Actual",]
plot_long_ipc<- long_HDDS[long_HDDS$level == "IPC_value_only",]


ggplot(as.data.frame(plot_actual),aes(x=HDDS, color=level)) + geom_density(alpha=0.25,show.legend = TRUE) +
  xlim(1.5, 7.6) + geom_vline(xintercept=3,linetype=2) + geom_vline(xintercept=6,linetype=2) +
  stat_density(data=plot_long_ipc,aes(x=HDDS, y=..scaled../1.4,color=level)) +
  scale_y_continuous(sec.axis = sec_axis(~.*20, name = "density (IPC value only) ")) +
  theme_bw()



ggsave("HDDS_2010_ipc_vs_actual.png", plot = last_plot(),device = "png",path = "output/figures/",
       dpi = 1000, limitsize = TRUE)
# 
