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

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")


ampd_raw <- as.data.table(read.fst ("data/ampd_raw.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


#group by fuel category



#fuel types
ampd_raw <- ampd_raw %>% filter (Source.Category== "Electric Utility")


ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                                      SO2..tons.=ampd_raw$SO2..tons.,
                                      CO2..tons.= ampd_raw$CO2..tons.,
                                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)




ampd_raw %>% filter( STATE == "CA") %>%
  ggplot(aes(year, NOx..tons., color= year, group= Fuel.Type..Primary.)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 



coal <- ampd_raw %>% filter (Fuel.Type..Primary.== "Coal")




coal_ampd_ym <-  aggregate(list (NOx..tons.=coal$NOx..tons., 
                                 SO2..tons.=coal$SO2..tons.,
                                 CO2..tons.= coal$CO2..tons.,
                                 Gross.Load..MW.h.=coal$Gross.Load..MW.h.,
                                 Steam.Load..1000lb.= coal$Steam.Load..1000lb.,
                                 HEAT.INPUT= coal$HEAT.INPUT
), by=list( year=coal$year,
            month=coal$month), FUN=sum)


coal_ampd_sym <- aggregate(list (NOx..tons.=coal$NOx..tons., 
                                 SO2..tons.=coal$SO2..tons.,
                                 CO2..tons.= coal$CO2..tons.,
                                 Gross.Load..MW.h.=coal$Gross.Load..MW.h.,
                                 Steam.Load..1000lb.= coal$Steam.Load..1000lb.,
                                 HEAT.INPUT= coal$HEAT.INPUT
), by=list(STATE=coal $ STATE,  year=coal$year,
           month=coal$month), FUN=sum)






pipe.nat.gas <- ampd_raw %>% filter (Fuel.Type..Primary.== "Pipeline Natural Gas")

png_ampd_ym <-  aggregate(list (NOx..tons.=pipe.nat.gas$NOx..tons., 
                                SO2..tons.=pipe.nat.gas$SO2..tons.,
                                CO2..tons.= pipe.nat.gas$CO2..tons.,
                                Gross.Load..MW.h.=pipe.nat.gas$Gross.Load..MW.h.,
                                Steam.Load..1000lb.= pipe.nat.gas$Steam.Load..1000lb.,
                                HEAT.INPUT= pipe.nat.gas$HEAT.INPUT
), by=list( year=pipe.nat.gas$year,
            month=pipe.nat.gas$month), FUN=sum)

png_ampd_sym <-  aggregate(list (NOx..tons.=pipe.nat.gas$NOx..tons., 
                                 SO2..tons.=pipe.nat.gas$SO2..tons.,
                                 CO2..tons.= pipe.nat.gas$CO2..tons.,
                                 Gross.Load..MW.h.=pipe.nat.gas$Gross.Load..MW.h.,
                                 Steam.Load..1000lb.= pipe.nat.gas$Steam.Load..1000lb.,
                                 HEAT.INPUT= pipe.nat.gas$HEAT.INPUT
), by=list( STATE=pipe.nat.gas$STATE, year=pipe.nat.gas$year,
            month=pipe.nat.gas$month), FUN=sum)




#coal and pipleline natural gas boxplot year wise 



plot1 <-  coal_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, NOx..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " NOx Emission (tons)",
                                                               title= "Primary Fuel is Coal")  

plot2 <- png_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, NOx..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  +labs( x= "Year", y= " NOx Emission (tons)",
                                                               title= "Primary Fuel is Pipeline natural gas")  

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)


#S02

plot1 <-  coal_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, SO2..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " SO2 Emission (tons)",
                                                               title= "Primary Fuel is Coal")  

plot2 <- png_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, SO2..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  +labs( x= "Year", y= " SO2 Emission (tons)",
                                                               title= "Primary Fuel is Pipeline natural gas")  

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)

#CO2

plot1 <-  coal_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, CO2..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " CO2 Emission (tons)",
                                                               title= "Primary Fuel is Coal")  

plot2 <- png_ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, CO2..tons., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  +labs( x= "Year", y= " CO2 Emission (tons)",
                                                               title= "Primary Fuel is Pipeline natural gas")  

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)

