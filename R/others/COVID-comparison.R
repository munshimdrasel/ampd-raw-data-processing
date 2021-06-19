#COVID comparision 

library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library(measurements)
library(XML)
library(plyr)
library(RCurl)
library(lubridate)
library(ggpubr)
library(mapproj)
library(reshape2)


setwd ("/Users/munshirasel/Google Drive/R/ampd-3")


ampd_raw <- as.data.table(read.fst ("data/ampd_raw.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]



ampd_sym<-   aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                                      SO2..tons.=ampd_raw$SO2..tons.,
                                      CO2..tons.= ampd_raw$CO2..tons.,
                                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)

# 2015 - 2019 avg vs 2020 avg 

years <- c( 2015, 2016, 2017, 2018, 2019, 2020)

ampd_sym_com <- ampd_sym %>% filter (year %in% years) %>% dplyr::select(STATE, month, year, NOx..tons.)

ampd_sym_com <- ampd_sym_com %>% spread(year, NOx..tons.)

ampd_sym_num <- as.character(colnames(ampd_sym_com))

ampd_sym_com <- ampd_sym_com %>% mutate( avg_2015_2019 = rowMeans(ampd_sym_com[,3:7], na.rm=TRUE))

ampd_sym_com <- na.omit(ampd_sym_com)

names(ampd_sym_com)[8] <- "NOx_2020"


ampd_sym_com <- ampd_sym_com %>% mutate (change_NOx_emission = ( avg_2015_2019-NOx_2020)*100/avg_2015_2019, 
                                         ratio = (NOx_2020)/avg_2015_2019)


ampd_sym_com <- ampd_sym_com %>% dplyr::select(STATE, month, NOx_2020, avg_2015_2019, change_NOx_emission, ratio)

melt_ampd_sym_com <- melt (ampd_sym_com, id = c ("STATE", "month", "change_NOx_emission", "ratio"),
                           variable.name = "group", value.name = "NOx.tons.")



#COVID Comparision state wise

melt_ampd_sym_com %>% filter( STATE == "AL") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

  
melt_ampd_sym_com %>% filter( STATE == "AR") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "AZ") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "CA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "CO") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "CT") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "DC") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "DE") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "FL") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "GA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "IA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "ID") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "IL") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "IN") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "KS") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "KY") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "LA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MD") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "ME") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MI") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MN") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MO") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "MO") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MS") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "MT") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NC") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "ND") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NE") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 



melt_ampd_sym_com %>% filter( STATE == "NH") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NJ") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NM") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NV") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "NY") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "OH") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "OK") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "OR") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "PA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "RI") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "SC") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "SD") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "TN") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "TX") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "UT") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "VA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "VT") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "WA") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "WA") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

melt_ampd_sym_com %>% filter( STATE == "WI") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "WV") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


melt_ampd_sym_com %>% filter( STATE == "WY") %>%
  ggplot(aes(month, NOx.tons., color= group, group= group)) +
  geom_point() + geom_line()+ labs(x= "Month",  y = "NOx Emission (tons)", title = "") + 
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + 
  theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
         panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


  
  

  # higher opearation time leads into higher emission in temrs of CO2, NOx, SO2







