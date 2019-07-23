rm(list = ls())

require(tidyverse)
library(zoo)


mw.lsms = read.csv("data/clean/household/mw_hh_aggregate.csv",stringsAsFactors = FALSE)

mw.fcs.2010 = mw.lsms %>%
  filter(FS_year == 2010 ) %>%
  select(ea_id,lat_modified,lon_modified,FCS) %>%
  group_by(ea_id,lat_modified,lon_modified) %>%
  summarise( mean_fcs = mean(FCS))

tz.lsms = read.csv("data/clean/household/tz_hh_aggregate.csv",stringsAsFactors = FALSE)

tz.fcs.2010 = tz.lsms %>%
  filter(FS_year == 2010 ) %>%
  select(clusterid,lat_modified,lon_modified,FCS) %>%
  group_by(clusterid,lat_modified,lon_modified) %>%
  summarise( mean_fcs = mean(FCS))

colnames(tz.fcs.2010)[1]="ea_id"

ug.lsms = read.csv("data/clean/household/ug_hh_aggregate.csv",stringsAsFactors = FALSE)

ug.fcs.2010 = ug.lsms %>%
  filter(FS_year == 2010 ) %>%
  select(ea_id,lat_modified,lon_modified,FCS) %>%
  group_by(ea_id,lat_modified,lon_modified) %>%
  summarise( mean_fcs = mean(FCS))


fcs_plot = bind_rows(mw.fcs.2010,tz.fcs.2010)
fcs_plot = bind_rows(fcs_plot,ug.fcs.2010)

fcs_plot = fcs_plot %>% ungroup() %>% select(-ea_id)


fcs_plot$mean_fcs[fcs_plot$mean_fcs==-Inf] = NA
fcs_plot = fcs_plot %>% na.omit()
write.csv(fcs_plot,"output/graphs/fcs_map_2010.csv",row.names = FALSE)

