rm(list=ls())

library("geosphere")
library("reshape2")
library("ggplot2")

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
#read in the prices 
price <- read.csv("maize_price_joined.csv")
# replace the 0's with NAs. 
price[price==0]<-NA 
# exclude data error
price<-price[!(price$date==2013-07-30 & price$date==2013-07-29 & price$date==2013-07-31 & price$date==2013-12-30),]

newdata = data.frame(lcalvol =3, lwieght =2)
########################################
# mkt_thinness measure for each market 
#######################################
# mkt thinness for each market 
# number of missings for each yearmon for each mkt 
# create yearmonn variables
price$yearmon <-strftime(price$date,format = "%Y%m") #save the yearmon
week_count<-as.data.frame(table(unique(price)$yearmo))

# count missings for each yearmon
missings<-aggregate(.~ yearmon, data=price[,5:ncol(price)], function(x) {sum(is.na(x))}, na.action = NULL)
# count how many weeks in each yearmon
#weekscount<-count(price, yearmon)$n

weekscount<-week_count$Freq
# mkt_thinness measure for each mkt: percent of missings for each market
mkt_thinness<-as.data.frame(matrix(NA,113,72))
for (i in 1:nrow(missings)){
  mkt_thinness[i,]<-  missings[i,2:ncol(missings)]/weekscount[i]
}
mkt_thinness<-cbind(missings$yearmon,mkt_thinness)
names(mkt_thinness)<-colnames(missings)

write.csv(mkt_thinness,"mkt_thinness.csv")

# transpose the dataset for merge 
mkt_thinness_transpose = as.data.frame(t(mkt_thinness[,2:ncol(mkt_thinness)]))
colnames(mkt_thinness_transpose)<-t(mkt_thinness$yearmon)
mkt_thinness_transpose$mkt_name<-rownames(mkt_thinness_transpose)

# transform to long format with yearmon
mkt_long <- melt(mkt_thinness_transpose[,1:ncol(mkt_thinness_transpose)], id.vars = "mkt_name")
names(mkt_long)<-c("mkt_name","yearmo","mkt_thinn")
mkt_long$yearmo<-as.character(mkt_long$yearmo)

# read in the imputed prices 
price_yearmo<-read.csv("imputed_price_yearmo.csv")
price$yearmon <-strftime(price$date,format = "%Y%m") #save the yearmon
names(price_yearmo)[1]<-"yearmo"

### merge in the mkt_imputed price 
price_yearmo_t<-t(price_yearmo)

#price_yearmo<-aggregate(.~ yearmon, data=price[,5:ncol(price)], FUN = "median", na.action = rm)
long_price <- melt(price_yearmo,id.vars="yearmo")
names(long_price)<-c("yearmo","mkt_name","imputed_price")
long_price$yearmo<-as.character(long_price$yearmo)

# join prices and mkt thinness 
join<-dplyr::left_join(long_price, mkt_long, by = c("mkt_name","yearmo"))

write.csv(join,"price_thinness_long.csv")



#########################################
# mkt_thinness measure for each household 
#########################################
hh_mkt <- read.csv("hh_near_mkt.csv")
hh_mkt$near_mkt<-toupper(hh_mkt$near_mkt)

# change the names to help with the merge 
colnames(mkt_thinness)[colnames(mkt_thinness)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(mkt_thinness)[colnames(mkt_thinness)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(mkt_thinness)[colnames(mkt_thinness)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"
name_diff<-dplyr::setdiff(hh_mkt$near_mkt,names(mkt_thinness))
name_diff

#name_diff<-dplyr::setdiff(hh_mkt$near_mkt,names(price_yearmo))
#name_diff


### loop: get hh i’s nearest mkt thinness at yearmo j 
mat_hh<-matrix(NA,nrow(hh_mkt),nrow(mkt_thinness))
dim(mat_hh)

for (i in 1:nrow(hh_mkt)){
  for (j in 1:nrow(mkt_thinness)){
    tryCatch(
      {
        mat_hh[i,j]= mkt_thinness[j,as.character(hh_mkt$near_mkt[i])]
      }, 
      error=function(cond){       # Choose a return value in case of warning
        message("Here's the original warning message:")
        message(cond)
      })
    }
}
df_hh<-as.data.frame(mat_hh)
names(df_hh)<-mkt_thinness$yearmon

hh_mkt_thin<-cbind(hh_mkt,df_hh)
hh_long <- melt(hh_mkt_thin[,3:ncol(hh_mkt_thin)], id.vars = c("case_id","near_mkt","near_dist"))
names(hh_long)[names(hh_long)=="value"]<-"mkt_thinn"
names(hh_long)[names(hh_long)=="variable"]<-"yearmo"


write.csv(hh_long,"hh_thinness.csv")



### loop: get hh i’s nearest mkt price at yearmo j 
mat_hh_price<-matrix(NA,nrow(hh_mkt),nrow(price_yearmo))
#dim(mat_hh_price)

for (i in 1:nrow(hh_mkt)){
  for (j in 1:nrow(price_yearmo)){
    tryCatch(
      {
        mat_hh_price[i,j]= price_yearmo[j,as.character(hh_mkt$near_mkt[i])]
      }, 
      error=function(cond){       # Choose a return value in case of warning
        message("Here's the original warning message:")
        message(cond)
      })
  }
}
df_hh_price<-as.data.frame(mat_hh_price)
names(df_hh_price)<-price_yearmo$yearmo

hh_price<-cbind(hh_mkt,df_hh_price)
hh_long_price <- melt(hh_price[,3:ncol(hh_price)], id.vars = c("case_id","near_mkt","near_dist"))
names(hh_long_price)[names(hh_long_price)=="value"]<-"price"
names(hh_long_price)[names(hh_long_price)=="variable"]<-"yearmo"
hh_long_price$lnprice_impu<-log(hh_long_price$price)


write.csv(hh_long_price,"hh_price.csv")



########################################
# mkt_thinness measure for each IPC zone
#######################################
# link markets to each ipc zone by using a population weight

pop_weight<-read.csv("market_thinness/pop_weights_update.csv")
pop_weight$FNID<-as.character(pop_weight$FNID)
popweight2<-pop_weight[order(pop_weight$FNID),]
popweight_recal<- data.frame(fnid=popweight2$FNID,mkt=popweight2$Name,pop=popweight2$pop)

# recalculate the weights 
popweight_recal<-popweight_recal %>% 
  group_by(fnid) %>% 
  mutate(pop.sum = sum(pop,na.rm=TRUE))
popweight_recal$pop.weight<-popweight_recal$pop/popweight_recal$pop.sum

# this weighting changes the way mkts are linked to IPC zones 
popweight_recal$fnid<-as.character(popweight_recal$fnid) # tranform factor to string 

# check names in different datasets 
dif<-dplyr::setdiff(colnames(mkt_thinness),popweight_recal$mkt)
dif 

# change names for match 
colnames(mkt_thinness)[colnames(mkt_thinness)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(mkt_thinness)[colnames(mkt_thinness)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(mkt_thinness)[colnames(mkt_thinness)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"

popweight_recal$pop.weight[is.na(popweight_recal$pop.weight)] <-0

# for MW2012C3010107 there is only one mkt and the population is missing. Will just set the population weight to be 1
popweight_recal$pop.weight[popweight_recal$fnid=="MW2012C3010107"]<-1


mat<-matrix(NA,length(unique(popweight_recal$fnid)),nrow(mkt_thinness))
for (k in 1:nrow(mkt_thinness)){
for (i in 1:length(unique(popweight_recal$fnid))){
 # subset the polygon 
  ipc_name<-unique(popweight_recal$fnid)[i]
  temp<-popweight_recal[grep(ipc_name, popweight_recal$fnid, ignore.case=T),]
  thin = 0
  for (j in 1:nrow(temp)){
    mkt_name<-as.character(temp$mkt[j])
    tryCatch(
      {
        thin =   thin + mkt_thinness[,mkt_name][k] * temp$pop.weight[j]
        }, 
    warning=function(cond){       # Choose a return value in case of warning
      message("Here's the original warning message:")
      message(cond)
    },
    error=function(cond){       # Choose a return value in case of warning
      message("Here's the original error message:")
      message(cond)
    }
    )
  }
  mat[i,k]<-thin 
}
}



ipc_mktthin<-as.data.frame(mat)
names(ipc_mktthin)<-mkt_thinness$yearmon

ipc_mktthin<-cbind(unique(popweight_recal$fnid),ipc_mktthin)
write.csv(ipc_mktthin,"ipc_market_thinness.csv")

# reshape to long 
library(reshape2)
long <- melt(ipc_mktthin, id.vars = "unique(popweight_recal$fnid)")
names(long)<-c("fnid","yearmo","mkt_thinness")
write.csv(long,"ipc_market_thinness_long.csv")


########################
# Not population weighted mkt_thinness
###################
#popweight_recal

mat2<-matrix(NA,length(unique(popweight_recal$fnid)),nrow(mkt_thinness))
for (k in 1:nrow(mkt_thinness)){
  for (i in 1:length(unique(popweight_recal$fnid))){
    # subset the polygon 
    ipc_name<-unique(popweight_recal$fnid)[i]
    temp<-popweight_recal[grep(ipc_name, popweight_recal$fnid, ignore.case=T),]
    thin = 0
    for (j in 1:nrow(temp)){
      mkt_name<-as.character(temp$mkt[j])
      tryCatch(
        {
          thin =   thin + mkt_thinness[,mkt_name][k]
        }, 
        warning=function(cond){       # Choose a return value in case of warning
          message("Here's the original warning message:")
          message(cond)
        },
        error=function(cond){       # Choose a return value in case of warning
          message("Here's the original error message:")
          message(cond)
        }
      )
    }
    mat2[i,k]<-thin/nrow(temp) # a simple average
  }
}

ipc_mktthin_nopop<-as.data.frame(mat2)
names(ipc_mktthin_nopop)<-mkt_thinness$yearmon

ipc_mktthin_nopop<-cbind(unique(popweight_recal$fnid),ipc_mktthin_nopop)
write.csv(ipc_mktthin_nopop,"ipc_market_thinness_nopop.csv")

# reshape to long 
long_nopop <- melt(ipc_mktthin_nopop, id.vars = "unique(popweight_recal$fnid)")
names(long_nopop)<-c("fnid","yearmo","mkt_thinness")

write.csv(long_nopop,"ipc_mkt_thin_nopop_long.csv")
summary(ipc_mktthin_nopop)

####################################
### without using the pop weights based on thiesson polygon##  
## ignore this part, losing a lot of the IPC values
####################################
# for each mkt, there is the ipc zone that it belongs to
# concordance<-read.csv("lpc_mkt.csv")
# c_table<-data.frame(fnid=concordance$FNID,mkt_name=concordance$Name)
# c_table$mkt_name<-as.character(c_table$mkt_name)
# 
# mkt_thinness_transpose$mkt_name<-rownames(mkt_thinness_transpose)
# fnid_mkt_thin<-dplyr::left_join(mkt_thinness_transpose, c_table, by = "mkt_name")
# fnid_mkt_thin<-fnid_mkt_thin[,-(ncol(fnid_mkt_thin)-1)]
# 
# # simple average of the mkts
# fnid_collapse<-aggregate(.~fnid ,data=fnid_mkt_thin, FUN = "mean")
# 
# # this is only 38 fnids 
# 
# long_fnid <- melt(fnid_collapse, id.vars = "fnid")
# 
# 
# write.csv(long_fnid,"ipc_long_nopop.csv")


####################################
### Population weighted imputed prices ##
####################################

colnames(price_yearmo)[colnames(price_yearmo)=="MONKEY.BAY"]<-"MONKEY BAY"
colnames(price_yearmo)[colnames(price_yearmo)=="BEMBEKE.TURN.OFF"]<-"BEMBEKE_TURNOFF"
colnames(price_yearmo)[colnames(price_yearmo)=="TSANGANO.TURN.OFF"]<-"TSANGANO_TURNOFF"


mat_price<-matrix(NA,length(unique(popweight_recal$fnid)),nrow(price_yearmo))
for (k in 1:nrow(price_yearmo)){
  for (i in 1:length(unique(popweight_recal$fnid))){
    # subset the polygon 
    ipc_name<-unique(popweight_recal$fnid)[i]
    temp<-popweight_recal[grep(ipc_name, popweight_recal$fnid, ignore.case=T),]
    price = 0
    for (j in 1:nrow(temp)){
      mkt_name<-as.character(temp$mkt[j])
      tryCatch(
        {
          price =   price + price_yearmo[,mkt_name][k] * temp$pop.weight[j]
        }, 
        warning=function(cond){       # Choose a return value in case of warning
          message("Here's the original warning message:")
          message(cond)
        },
        error=function(cond){       # Choose a return value in case of warning
          message("Here's the original error message:")
          message(cond)
        }
      )
    }
    mat_price[i,k]<-price 
  }
}



ipc_price<-as.data.frame(mat_price)
names(ipc_price)<-price_yearmo$yearmo

ipc_price<-cbind(unique(popweight_recal$fnid),ipc_price)
write.csv(ipc_price,"ipc_price_recal.csv")

# reshape to long 
library(reshape2)
long_price <- melt(ipc_price, id.vars = "unique(popweight_recal$fnid)")
names(long_price)<-c("fnid","yearmo","price")
long_price$lnprice_impu <-log(long_price$price)
write.csv(long_price,"ipc_price_long.csv")


##########

###########
fews<-read.csv("fews_20171016.csv")
unique()
unique(popweight2$FNID)

?substr

zone_12<-substr(unique(popweight2$FNID),9,14)
zone_16<-substr(as.character(fews$FNID),9,14)
dif<-dplyr::setdiff(zone_12,zone_16)
dif2<-dplyr::setdiff(zone_16,zone_12)

dif
dif2
zones<-substr(dif2,5,6)
unique(zones)


#####  
mkt_thinness<-read.csv("mkt_thinness.csv")
mkt_thinness$Average<-rowMeans(mkt_thinness[,3:ncol(mkt_thinness)])
date_list<-seq(as.Date('2007-12-01'),by='month',length=114)
avg<-mkt_thinness$Average
avg<-append(avg, 1, after=107)

date_list<-seq(1,by='month',length=1)

df.plot<-data.frame(yearmon=date_list,mkt_thinness=avg)
df.plot$month<-strftime(df.plot$yearmon,format = "%B")
df.plot$month_num<-strftime(df.plot$yearmon,format = "%m")

aggregate()

require(ggplot2)
require(scales)
df.plot<-df.plot[order(df.plot$month_num),]

ggplot(df.plot[order(df.plot$month_num),], aes(x=month_num, y=mkt_thinness,group = 1,colour = "Market Thinness")) + ggtitle("Average Market Thinness by Month")+ylab("Average market thinness of all markets")+ stat_summary(fun.y="mean", geom="bar")+scale_fill_brewer( palette = "Blues")
ggsave("month.png", width = 10, height = 10)

df.plot$month_num<-as.numeric(df.plot$month_num)

month_list<-rep(month.name, 1)

for (i in 1:12){
  temp<-df.plot[df.plot$month_num==i,]
  ggplot(temp, aes(x=yearmon, y=mkt_thinness,group = 1,colour = "Market Thinness")) + scale_x_date(breaks = date_breaks("1 year"),labels = date_format("%Y"))+ ggtitle(paste("Average Market Thinness in",month_list[i],sep = " "))+ylab("Average market thinness of all markets")+ stat_summary(fun.y="mean", geom="bar")+scale_fill_brewer( palette = "Blues")
  ggsave(paste(month_list[i],"png",sep = "."), width = 10, height = 10)
  }
 
df.plot[df.plot$month_num==1,]
ggplot(df.plot[df.plot$month_num==1,], aes(x=yearmon, y=mkt_thinness,group = 1,colour = "Market Thinness")) + ggtitle("Average Market Thinness by Month")+ylab("Average market thinness of all markets")+ stat_summary(fun.y="mean", geom="bar")+scale_fill_brewer( palette = "Blues")

ggplot(df.plot, aes(x=yearmon, y=mkt_thinness,group = 1,colour = "Market Thinness")) + ggtitle("Average Market Thinness by Month") +  scale_x_date(breaks = date_breaks("6 months"),labels = date_format("%m/%y"))+ylab("Average market thinness of all markets")+ stat_summary(fun.y="mean", geom="bar")+scale_fill_brewer( palette = "Blues")







d <- density(mtcars$mpg) # returns the density data 
plot(d) # plots the results