rm(list=ls())

clust_2010<- read.csv("data/clust_2010.csv")
sum(is.na(clust_2010))



na_check<-lapply(clust_2010,(is.na))
na_check_sum<-lapply(na_check,(sum))
na_check_sum<-na_check_sum[na_check_sum>0]


clust_2013 <- read.csv("data/clust_2013.csv")


na_check13<-lapply(clust_2013,(is.na))
na_check_sum13<-lapply(na_check,(sum))
na_check_sum13<-na_check_sum[na_check_sum>0]

