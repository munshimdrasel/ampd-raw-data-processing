#time series plot of AMPD data

install.packages("ggpubr")

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

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")


ampd_raw <- read.fst ("data/ampd_raw.fst")

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

names(ampd_harvard)
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 

# ampd <- as.data.table( ampd_raw %>% filter (year == 2015 :2020, month == 1:6))

# str(ampd)

# half.year.emission<- setDT(ampd)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
#                            NOx..tons. = sum(NOx..tons., na.rm=TRUE),
#                            CO2..tons. = sum(CO2..tons., na.rm=TRUE),
#                            SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
#                            Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
#                            Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
#                            HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)), 
#                        by = .(STATE, year, month)]
# 
# half.year.emission %>% filter ( STATE=="VA")
# 
# states <- c("CA","VA", "FL")
# 
# half.year.emission %>% as_tibble()



# dfz<- ampd %>% group_by(month, year, STATE) %>% summarise_all(sum)

# names(ampd_raw)

#sorting AMPD data by state, year and month

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT
                      ), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
                                 month=ampd_raw $month), FUN=sum)


#sum of all states

ampd_ym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT
), by=list( year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)

names(ampd_harvard_sym)
names(ampd_sym)

#Changing column names of ampd_harvard data to match  column names of  ampd_raw data







ampd_harvard_sym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                                   SO2..tons.=ampd_harvard$SO2..tons.,
                                   CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                   Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                   Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                   Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list( State= ampd_harvard$ State.x, Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)


colnames(ampd_harvard_sym)[1] <- "STATE"

colnames(ampd_harvard_sym)[2] <- "year"

colnames(ampd_harvard_sym)[3] <- "month"

colnames(ampd_harvard_sym)[4] <- "NOx..tons."

colnames(ampd_harvard_sym)[5] <- "SO2..tons."

colnames(ampd_harvard_sym)[6] <- "CO2..tons."

colnames(ampd_harvard_sym)[7] <- "Gross.Load..MW.h."

colnames(ampd_harvard_sym)[8] <- "Steam.Load..1000lb."

colnames(ampd_harvard_sym)[9] <- "HEAT.INPUT"



ampd_harvard_ym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                                   SO2..tons.=ampd_harvard$SO2..tons.,
                                   CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                   Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                   Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                   Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list(  Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)


names(ampd_harvard_ym)



colnames(ampd_harvard_ym)[1] <- "year"

colnames(ampd_harvard_ym)[2] <- "month"

colnames(ampd_harvard_ym)[3] <- "NOx..tons."

colnames(ampd_harvard_ym)[4] <- "SO2..tons."

colnames(ampd_harvard_ym)[5] <- "CO2..tons."

colnames(ampd_harvard_ym)[6] <- "Gross.Load..MW.h."

colnames(ampd_harvard_ym)[7] <- "Steam.Load..1000lb."

colnames(ampd_harvard_ym)[8] <- "HEAT.INPUT"





#combining two plot for data validation

#yearly NOx emission in the US

x <- ampd_ym %>% filter ( year <= 2015)
y <- ampd_harvard_ym  %>% filter (year <= 2015)

ggplot() + geom_boxplot(data= x, aes( year, NOx..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, NOx..tons.,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  


#yearly SO2 emission in the US

names(ampd_ym)
x <- ampd_ym %>% filter ( year <= 2015)
y <- ampd_harvard_ym  %>% filter (year <= 2015)


ggplot() + geom_boxplot(data= x, aes( year, SO2..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, SO2..tons.,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  


x <- ampd_ym %>% filter ( year <= 2015)
y <- ampd_harvard_ym  %>% filter (year <= 2015)


ggplot() + geom_boxplot(data= x, aes( year, CO2..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, CO2..tons.,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 




# plotting year wise emission in two graphs and then combined together

# NOx emission

plot1<- ampd_ym%>% filter (year <= 2015) %>%
  ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx Emission (tons)",
                                                              title = "NOx Emission_updated data") 

plot2 <- ampd_harvard_ym%>% filter (year <= 2015) %>%
  ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                            y = "NOx Emission (tons)",
                                                            title = "NOx Emission_Harvard data")

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)


#SO2 emission
plot1<- ampd_ym%>% filter (year <= 2015) %>%
  ggplot(aes( year, SO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission (tons)",
                                                              title = "SO2 Emission_updated data") 

plot2 <- ampd_harvard_ym%>% filter (year <= 2015) %>%
  ggplot(aes( year, SO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission (tons)",
                                                              title = "SO2 Emission_Harvard data")

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)



# Few notes:

  # 1. 1995, 1977 We shouldn't consider because the AMPD website doesn't have the data from all States
#2. Harvard data of 2015 is not right since it doesn't has data from June to December of 2015(see the plot below)

years<- c ( 2015)


plot1<- ampd_ym %>% filter ( year %in% years) %>% ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_path() + geom_line() + scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  labs(x= "month",  y = "NOx Emission (tons)", title = "NOx Emission_updated data") +
  ylim (0, 300000)

plot2 <- ampd_harvard_ym  %>% filter ( year %in% years)%>% ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_path() + geom_line() + scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  labs(x= "month",  y = "NOx Emission (tons)", title = "NOx Emission_Harvard data") +
  ylim (0, 300000)

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)



# where is NOx harvard data for months >6?

ampd_harvard_ym  %>% filter ( year==2015) 

# it seems they don't have data from July to december of 2015




#Combining two plots to see year wise difference

# not sure how to use facet wrap command for two different dataframes and how to color different lines

#which one is which?

# 2006, 2007 and from 1997 to 2000 the match is good

years<- c ( 2001)

x <- ampd_ym %>% filter ( year %in% years)
y <- ampd_harvard_ym  %>% filter ( year %in% years)

ggplot() + geom_point(data= x, aes( month, NOx..tons.,color= year, group= year)) + 
  geom_line( data= x, aes( month, NOx..tons.,color= year, group= year )) + 
  geom_point (data=y, aes( month, NOx..tons. ,color= year, group= year)) +
  geom_line( data= y, aes( month, NOx..tons.,color= year, group= year)) + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  labs(x= "Year",  y = "NOx Emission (tons)", title = "NOx Emission data")



# From this point onward I'll just used my updated AMPD data until 2020 
# just to mention 2020 data is upto June, 2020 as of August-23-2020
#later I'll update the data when data will be available in the AMPD website

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


plot7 <- ampd_harvard_ym  %>% filter ( year <= 2015)%>% ggplot(aes( year, Steam.Load..1000lb., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Steam Load",
                                                              title = "Steam Load (~2020June) Harvard data")

plot7

ggarrange (plot6, plot7, labels= c("A", "B"), ncol=1, nrow=2)


# Random Plot

ampd_ym %>%
  ggplot(aes( HEAT.INPUT, NOx..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Gross.Load..MW.h., NOx..tons., color= year)) + geom_point()

ampd_ym %>%
  ggplot(aes( Steam.Load..1000lb., NOx..tons., color= year)) + geom_point() +geom_smooth()

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

states <- c ("GA", "TX", "VA",  "PA")

plot1<- ampd_sym %>% filter(STATE %in% states, year == 2020) %>%
  ggplot(aes(month, NOx..tons., color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot1

#################### next ???###############













# plot2<- ampd_harvard_sym %>% filter(STATE %in% states, year == 2014) %>%
#   ggplot(aes(month, NOx..tons., color= STATE, group= STATE)) +
#   geom_point() + geom_line()+ labs(x= "Month", 
#                                    y = "NOx Emission (tons)",
#                                    title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)
# 
# ggarrange (plot1, plot2, labels= c ("A", "B"), nrow=2, ncol=1)

#texas not matching with harvard data



#looking into emissions (year variable, state fixed)

years <- c (2017, 2018, 2019, 2020)

ampd_sym %>% filter(year %in% years, STATE == "TX") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)


ampd_sym %>% filter(year %in% years, STATE == "TX") %>%
  ggplot(aes(month, SO2..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 30000)



# years <- c (2011, 2012, 2013)
# 
# ampd_harvard_sym %>% filter(Year %in% years, State == "WV") %>%
#   ggplot(aes(Month, SO2..tons., color= Year, group= Year)) +
#   geom_point() + geom_line()+ labs(x= "Month", 
#                                    y = "SO2 Emission (tons)",
#                                    title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 25000)
# 
# ampd_sym %>% filter(year %in% years, STATE == "WV") %>%
#   ggplot(aes(month, SO2..tons., color= year, group= year)) +
#   geom_point() + geom_line()+ labs(x= "Month", 
#                                    y = "SO2 Emission (tons)",
#                                    title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 25000)









# ampd_harvard_sym %>% filter(Year %in% years, State == "TX") %>%
#   ggplot(aes(Month, NOx..tons., color= Year, group= Year)) +
#   geom_point() + geom_line()+ labs(x= "Month", 
#                                    y = "NOx Emission (tons)",
#                                    title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)

###########


# states <- c ("TX", "VA", "FL")
# 
# ampd_6 %>% filter(STATE %in% states, year == 2020 ) %>%
#   ggplot(aes(month, NOx..tons., group = STATE, color= STATE)) + 
#   geom_line()+ geom_point() + labs(x= "Month", 
#                                    y = "NOx Emission (tons)",
#                                    title = "Emission") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))



#heat input

# states <- c ("TX")
# 
# ampd_6 %>% filter(STATE %in% states, year == 2020 ) %>%
#   ggplot(aes(month, HEAT.INPUT, group = STATE, color= STATE)) + 
#   geom_line()+ geom_point() + labs(x= "Month", 
#                                    y = "HEAT.INPUT",
#                                    title = "Emission") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))






years<- c (2010, 2011, 2012)
ampd_harvard_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 









ampd_ym %>% filter( year >= 2018) %>%
  ggplot(aes(month, SO2..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "SO2 Emission (tons)",
                                        title = "Emission O2 Emission from 2018 to 2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 






#NOX and SO2 emission year wise

ampd_ym %>% filter (year >= 1995) %>%
  ggplot(aes(year, NOx..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))


ampd_harvard_ym %>% filter( year >= 1995) %>%
  ggplot(aes(year, NOx..tons., group= year )) +geom_boxplot() + labs(x= "Month", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission Emission ") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 










#SO2


ampd_ym %>% filter (year >= 1995) %>%
  ggplot(aes(year, SO2..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                                                    y = "SO2 Emission (tons)",
                                                                    title = "SO2 Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))

ampd_ym %>% filter (year <= 2015) %>%
  ggplot(aes(year, SO2..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                                                    y = "SO2 Emission (tons)",
                                                                    title = "SO2 Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))


#check
ampd_harvard_ym %>% filter( Year >= 1995) %>%
  ggplot(aes(Year, SO2..tons., group= Year )) +geom_boxplot() + labs(x= "Month", 
                                                                     y = "SO2 Emission (tons)",
                                                                     title = "SO2 Emission Emission ") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 


#looking into emissions (state variable, year fixed)


#combining two plots (Harvard_AMPD data + My AMPD data to compare)


























dfz %>% filter (STATE== "VA", month <= 6, year>= 2018)


ampd %>% group_by(month, year, STATE) 



TX_county<- ampd_raw %>% filter (STATE=="TX") %>% select (County)
names (TX_county)

TX_county %>% unique(County)
as.character(unique(unlist(TX_county)))

# https://eaglefordshale.com/counties

ampd_raw %>% filter (STATE=="TX", County == "Milam County")


       