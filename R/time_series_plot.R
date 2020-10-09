#time series plot of AMPD data 
#AMPD data were collected from EPA FTP site
#

# 
# install.packages("mapproj")
# 
# install.packages("ggpubr")
# install.packages("fiftystater")

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


ampd_raw <- read.fst ("data/ampd_raw.fst")


#Glossary of ampd data
# glossary <- read.csv("data/datadefinition_09-01-2020_170204640.csv")

# Facility ID (ORISPL)	The unique six-digit facility identification number, also called an ORISPL, assigned by the Energy Information Administration.
# Unit ID	Unique identifier for each unit at a facility.
# Unit	A fossil fuel-fired combustion device.
# SO2 (tons)	Sulfur dioxide emissions in short tons.
# SO2 Phase	Title IV of the Clean Air Act SO2 Phase. Phase I started in 1995; Phase II started in 2000.
# Steam Load (1000lb)	Total steam pressure produced by a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# SO2 Control(s)	Method or equipment used by the combustion unit to minimize production or emission of sulfur dioxide (SO2).
# PM Control(s)	Method or equipment used by the combustion unit to minimize production or emission of particulate matter (PM).
# Operating Time	Any part of an hour in which a unit combusts any fuel.
# Operating Status	An indication of the present condition of a unit (planned, operating, shutdown, etc.).
# NOx Phase	Group 1 boilers are divided into two mutually exclusive groups: Phase I and Phase II. The Phase a Group 1 boiler is associated with determines (in part) the Acid Rain NOx limit a boiler is subject to. There is no Phase I-Phase II bifurcation of Group 2 boilers.
# NOx (tons)	Nitrogen oxide emissions in short tons.
# NOx Control(s)	Method or equipment used by the combustion unit to minimize production or emission of nitrogen oxides (NOx).
# Max Hourly HI Rate (MMBtu/hr)	The design heat input capacity (in MMBtu/hr) for the unit or the highest hourly heat input rate observed in the past five years, whichever is greater.
# Hg Control(s)	Method or equipment used by the combustion unit to minimize production or emission of mercury (Hg).
# Heat Input (MMBtu)	The measure of utilization that is calculated by multiplying the quantity of fuel by the fuels heat content.
# Gross Load (MW-h)	Total electrical generation of a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# Fuel Type (Primary)	The primary type of fuel combusted by the unit.
# Fuel Type (Secondary)	The secondary type of fuel combusted by the unit.





#sorting AMPD data by state, year and month

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
                      ), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
                                 month=ampd_raw $month), FUN=sum)


#sum of all states

ampd_ym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)

# names(ampd_harvard_sym)
names(ampd_sym)



#Yearly NOx emission:
names(ampd_ym)
plot1<- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx Emission (tons)",
                                                              title = "NOx Emission (~2020June)")

plot2 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, SO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission (tons)",
                                                              title = "SO2 Emission (~2020June)")

plot3 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, CO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "CO2 Emission (tons)",
                                                              title = "CO2 Emission (~2020June)")

ggarrange (plot1, plot2, plot3, labels= c("A", "B", "C"), ncol=1, nrow=3)


plot4 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, Gross.Load..MW.h., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Gross Load",
                                                              title = "Gross Load (~2020June)")
plot5 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, HEAT.INPUT, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Heat Input",
                                                              title = "Heat Input (~2020June)")

plot6 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, Steam.Load..1000lb., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Steam Load",
                                                              title = "Steam Load (~2020June)")


plot4
plot5
plot6





# Random Plot

ampd_ym %>%
  ggplot(aes( HEAT.INPUT, NOx..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Gross.Load..MW.h., NOx..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Steam.Load..1000lb., NOx..tons., color= year)) + geom_point() +geom_smooth(aes(color=year))

ampd_ym %>%
  ggplot(aes( HEAT.INPUT, SO2..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Gross.Load..MW.h., SO2..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Steam.Load..1000lb., SO2..tons., color= year)) + geom_point() 

  



#Monthly comparision of emission data in the US


names(ampd_ym)


years <- c (2018, 2019, 2020)

plot1 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year )) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "NOx Emission  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot2 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, Gross.Load..MW.h., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Gross Load Emission (tons)",
                                        title = "Gross Load Emission  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot3 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, Steam.Load..1000lb., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Steam Load",
                                        title = "Steam Load  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot4 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, HEAT.INPUT, color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Heat Input",
                                        title = "Heat Input") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

ggarrange(plot1, plot2, plot3, plot4, labels = c ("A", "B", "C", "D"), nrow=4, ncol= 1)



#Comparing NOx emission vs. month state wise
#########################

# NOx emission 

states <- c ("GA", "TX", "FL",  "PA")

plot1<- ampd_sym %>% filter(STATE %in% states, year == 2020) %>%
  ggplot(aes(month, NOx..tons., color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot1







#looking into emissions (year variable, state fixed)

years <- c ( 2019, 2020)

ampd_sym %>% filter(year %in% years, STATE == "NY") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 1500)


ampd_sym %>% filter(year %in% years, STATE == "NY") %>%
  ggplot(aes(month, SO2..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission (tons)",
                                   title = "Texas") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 40000)


ampd_sym %>% filter(year %in% years, STATE == "TX", month==7)






#monthly SO2 emission year wise

ampd_ym %>% filter( year >= 2018) %>%
  ggplot(aes(month, SO2..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "SO2 Emission (tons)",
                                        title = "Emission SO2 Emission from 2018 to 2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 


#monthly NOx emission year wise

ampd_ym %>% filter( year >= 2018) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "SO2 Emission (tons)",
                                        title = "Emission SO2 Emission from 2018 to 2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 



#fuelt types




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



#Sum op time
ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, SUM_OP_TIME, group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Operation time (hours)",
                                                               title= "Operating time")  

#Gross Load

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,Gross.Load..MW.h., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Gross load (MW-hr)",
                                                               title= "Gross Load MW-hr")  

#Steam Load

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,Steam.Load..1000lb., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Steam.Load..1000lb.",
                                                               title= "Steam.Load..1000lb.")  

#Heat Input

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,HEAT.INPUT, group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " HEAT.INPUT",
                                                               title= "HEAT.INPUT")  
       




fuel <-  c ("Coal", "Pipeline Natural Gas")
coal.natgas <-  ampd_raw %>% filter (Fuel.Type..Primary. %in% fuel)

coal.natgas_ym <-  aggregate(list (NOx..tons.=coal.natgas $NOx..tons., 
                SO2..tons.=coal.natgas $SO2..tons.
     
), by=list(  Fuel.Type..Primary.= coal.natgas$ Fuel.Type..Primary., year=coal.natgas $year), FUN=sum)



coal.natgas_ym %>%
  ggplot() + geom_line(aes(year, NOx..tons., color= Fuel.Type..Primary.)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  

coal.natgas_ym %>%
  ggplot() + geom_line(aes(year, SO2..tons., color= Fuel.Type..Primary.)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  



# ================================================
#CO2
ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, CO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "CO2 Emission (tons)",
                                                              title = "CO2 Emission (~2020June)")

#Heat Input
ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,HEAT.INPUT, group=year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " HEAT.INPUT (MMBtu)",
                                                               title= "HEAT.INPUT")  



#Normalizing data with respect to heat and CO2



ampd_sym <-  drop_na(  mutate (ampd_sym, NOx.to.Heat = NOx..tons./ HEAT.INPUT, 
                     NOx.to.CO2 = NOx..tons./ CO2..tons.,
                     SO2.to.Heat = SO2..tons./ HEAT.INPUT ,
                     SO2.to.CO2 = SO2..tons./ CO2..tons. )) #per million

ampd_ym <-  drop_na(mutate (ampd_ym, NOx.to.Heat = NOx..tons./ HEAT.INPUT, 
                     NOx.to.CO2 = NOx..tons./ CO2..tons. ,
                     SO2.to.Heat = SO2..tons./ HEAT.INPUT ,
                     SO2.to.CO2 = SO2..tons./ CO2..tons. ) )





#boxplot year wise of normalized data

#NOx

plot1<- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, NOx.to.Heat, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx Emission/Heat Input (tons/MMBtu)",
                                                              title = "NOx Emission/Heat Input (tons/MMBtu) (~2020June)")

plot2 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, NOx.to.CO2, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx.to.CO2 Emission (tons/tons)",
                                                              title = "NOx/CO2 Emission (tons/tons) (~2020June)") 


#SO2, year wise emission of Normalized data (Heat/CO2)


plot3<- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, SO2.to.Heat, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission/Heat Input (tons/MMBtu)",
                                                              title = "SO2 Emission/Heat Input (tons/MMBtu) (~2020June)")

plot4 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, SO2.to.CO2, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2/CO2 Emission (tons/tons)",
                                                              title = "SO2/CO2 Emission (tons/tons) (~2020June)")



ggarrange (plot1, plot3, labels= c("A", "B"), ncol=1, nrow=2)
ggarrange (plot2, plot4, labels= c("A", "B"), ncol=1, nrow=2)




# load the map data
states = map_data("state")
str(states)

## MAKE THE PLOT ####

library(fiftystater)
data("fifty_states")
library(dslabs)
data(murders)
state.abb <- murders %>% select (state, abb) 
colnames(state.abb)[2] <- "STATE"

ampd_sym <-  merge(ampd_sym, state.abb, by="STATE", all.x=T)

ampd_sym$state.lower <-  tolower(ampd_sym$state)



# NOx over Heat
years <-  c (2017)
ampd_sym %>% filter (year %in% years) %>% ggplot( aes(map_id = state.lower)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = NOx.to.Heat), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "NOx Emission/Heat Input (tons/MMBtu)", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +   facet_grid(.~ year)


#SO2 over Heat input

years <-  c (2019)
ampd_sym %>% filter (year %in% years) %>% ggplot( aes(map_id = state.lower)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = SO2.to.Heat), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "SO2/Heat Input (tons/MMBtu)", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +   facet_grid(.~ year) 



#Comparing NOx emission vs. month state wise
#########################

# NOx emission 

states <- c ("TX", "UT", "NM",  "PA")

plot1<- ampd_sym %>% filter( STATE %in% states, year == 2020) %>% 
  ggplot(aes(month, NOx.to.Heat, color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission/Heat Input (tons/MMBtu)",
                                   title = "2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot1

states <- c ("TX", "UT", "NM",  "PA")

plot2<- ampd_sym %>% filter( STATE %in% states, year == 2020) %>% 
  ggplot(aes(month, SO2.to.Heat, color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission/Heat Input (tons/MMBtu)",
                                   title = "2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot2



#looking into emissions (year variable, state fixed)

years <- c (2008, 2011, 2014, 2017, 2019)

ampd_sym %>% filter(year %in% years, STATE == "NM") %>%
  ggplot(aes(month, NOx.to.Heat, color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission/Heat Input(tons/MMBtu)",
                                   title = "New Mexico") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  

ampd_sym %>% filter(year %in% years, STATE == "NM") %>%
  ggplot(aes(month, NOx.to.CO2, color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx/CO2  (tons/tons)",
                                   title = "New Mexico") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  










