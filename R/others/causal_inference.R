#Causal analysis

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
library(data.table)
library(panelView)
library(stringi)

#install.packages(c("panelView", "gsynth"))

#causal impact
library(gsynth)
library(CausalImpact)

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")

#getting daily data

ampd_daily <- read.fst ("data/ampd_daily_emission.fst")





#source category

unique (ampd_daily$Source.Category)


#ampd power plants data filtering

ampd_daily_ec <- ampd_daily %>% filter( Source.Category== "Electric Utility" ) 
 
#Filtering state data

ampd_daily_va <- ampd_daily_ec %>% filter (STATE=="VA" & year >=1997)

ampd_daily_va <- as.data.table(ampd_daily_va)


unique(ampd_daily_va$ID)

ampd_daily_va_unit <- ampd_daily_va

va_ampd <- ampd_daily_va_unit




va_ampd<- setDT( va_ampd )[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                               NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                               CO2..tons. = sum(CO2..tons., na.rm=TRUE),
                               SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
                               Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
                               Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
                               HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)),
                           by = .(year,month,day)]



va_ampd <-  va_ampd %>% filter (month <=4 & year>= 1997) %>% dplyr::select (year, month, day, NOx..tons., HEAT.INPUT,
                                                              SUM_OP_TIME, Gross.Load..MW.h., CO2..tons.)



va_ampd[is.na(va_ampd)] <-  0


va_ampd <-  as.data.table(va_ampd)

va_ampd$day <- va_ampd[ , stri_pad_left(va_ampd$day, pad="0", width=2)]

#making NOx emissions as categorical variable: 2020 March and April as 1, all others as 0

va_ampd <- va_ampd[, inputed := 0]

va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==3 ] <- 1
va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==4 ] <- 1


#combining month and day together
va_ampd <- va_ampd [, time := paste(month, day, sep = "")]

# va_ampd <- va_ampd%>% dplyr::select (year, time, NOx..tons., HEAT.INPUT,
#                                                 SUM_OP_TIME, Gross.Load..MW.h., inputed,
#                                      Fuel.Type..Primary.)

va_ampd <- va_ampd%>% dplyr::select (year, time, NOx..tons., HEAT.INPUT,
                                     SUM_OP_TIME, Gross.Load..MW.h., CO2..tons., inputed)

#Observing control and treated values
panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"),by.timing = TRUE)

panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"), type= "outcome")


#Running gsynth 

out <- gsynth(NOx..tons. ~ inputed+  HEAT.INPUT + SUM_OP_TIME + Gross.Load..MW.h. + CO2..tons.  , 
              data=va_ampd,
              index = c("year","time"), na.rm=T, force = "two-way",
              CV = TRUE, seed =  123, estimator = "mc", min.T0 = 10, inference = "parametric", 
              parallel = TRUE)


print(out)




plot(out, theme.bw = TRUE)

plot(out, type = "counterfactual", raw = "none", main="VA") + 
  scale_x_continuous(breaks = seq(0, 120, by = 10)) 


#Considering one unit id, NOx control and Fuel type 


ampd_daily_va_2 <- ampd_daily_va %>%
  mutate(fuel.primary = factor(Fuel.Type..Primary.,
                               levels = c("Coal", "Natural Gas", "Diesel Oil", "Pipeline Natural Gas", ""),
                               labels = c(0, 1, 2, 3, 4)),
         nox.control= factor(NOx.Control.s.,
                             levels= c ("Overfire Air<br>Selective Non-catalytic Reduction", "Overfire Air", "",  "Steam Injection", "Other", "Dry Low NOx Burners<br>Water Injection<br>Selective Catalytic Reduction", "Selective Catalytic Reduction<br>Steam Injection", "Low NOx Burner Technology w/ Overfire Air", "Dry Low NOx Burners<br>Selective Catalytic Reduction<br>Steam Injection", "Dry Low NOx Burners<br>Water Injection", "Water Injection", "Selective Catalytic Reduction", "Dry Low NOx Burners<br>Selective Catalytic Reduction", "Selective Non-catalytic Reduction"),
                             labels= c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)))

unique(ampd_daily_va$ID)


# works ok

ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "56807-1A" )


#Issues
ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "3797-7" ) #CO2 tons data not there


ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "3809-1" ) #no 2020 data


ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "7838-4" ) #very low NOx emission/ doesn't operate long


va_ampd <- ampd_daily_va_unit


unique(va_ampd$fuel.primary)

unique(va_ampd$nox.control)

#units are not having different fuel types or different nox control (time when didn't have control)

va_ampd <-  va_ampd %>% filter (month <=4 & year >=2010) %>% dplyr::select (year, month, day, NOx..tons., HEAT.INPUT,
                                                              SUM_OP_TIME, Gross.Load..MW.h., CO2..tons., fuel.primary,
                                                              nox.control)



va_ampd[is.na(va_ampd)] <-  0


va_ampd <-  as.data.table(va_ampd)

va_ampd$day <- va_ampd[ , stri_pad_left(va_ampd$day, pad="0", width=2)]

#making NOx emissions as categorical variable: 2020 March and April as 1, all others as 0

va_ampd <- va_ampd[, inputed := 0]

va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==3 ] <- 1
va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==4 ] <- 1


#combining month and day together
va_ampd <- va_ampd [, time := paste(month, day, sep = "")]



va_ampd <- va_ampd%>% dplyr::select (year, time, NOx..tons., HEAT.INPUT,
                                     SUM_OP_TIME, Gross.Load..MW.h., inputed, CO2..tons., fuel.primary, nox.control )

#Observing control and treated values
panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"),by.timing = TRUE)

panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"), type= "outcome")


#Running gsynth 

out <- gsynth(NOx..tons. ~ inputed+  HEAT.INPUT  + Gross.Load..MW.h.+ CO2..tons. , 
              data=va_ampd,
              index = c("year","time"), na.rm=T, force = "two-way",
              CV = TRUE, seed =  123, estimator = "mc", min.T0 = 10, inference = "parametric", 
              parallel = TRUE)




plot(out, theme.bw = TRUE)

plot(out, type = "counterfactual", raw = "none", main="") + 
  scale_x_continuous(breaks = seq(0, 120, by = 10)) 

# unique (ampd_daily_va$NOx.Control.s.)





# ampd_daily_va_2 <- ampd_daily_va_2[, inputed.fuel := 0]
# 
# ampd_daily_va_2$inputed.fuel[ampd_daily_va_2$inputed.fuel=="Natural Gas"] <- 1




#categorical based on fuel type
# ampd_daily_va <- ampd_daily_va[, fuel.inputed := 0]
# ampd_daily_va$fuel.inputed[ampd_daily_va$Fuel.Type..Primary.== "Coal"] <- 1
# unique (ampd_daily_va$ID)
# 

# 
# 
# unique(ampd_daily_va$NOx.Control.s.)

#combining ORISPL and UNITID as one



# va_ampd$inputed.fuel <- as.factor(va_ampd$inputed.fuel)
#aggregating emission values with respect to year, month and day 

# va_ampd<- setDT( va_ampd )[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
#                             NOx..tons. = sum(NOx..tons., na.rm=TRUE),
#                             CO2..tons. = sum(CO2..tons., na.rm=TRUE),
#                             SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
#                             Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
#                             Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
#                             HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE),
#                             Fuel.Type..Primary. = fuel.inputed),
#                         by = .(year,month,day)]


#va_coal_unit <- as_tibble(va_coal %>% filter(ID == "7213-1" ) )


# va_ampd <-  va_ampd %>% filter (month <=4) %>% dplyr::select (year, month, day, NOx..tons., HEAT.INPUT, 
#                                                               SUM_OP_TIME, Gross.Load..MW.h., inputed.fuel, ID)



#seeting NA values as 0 indication 0 emissions, 0 input, 0 output on those days




# va_coal_unit <- as.data.frame(na.omit (va_coal_unit))
# 
# 
# #time.points <-  seq.Date(as.Date("1997-01-01"), by=1, length.out=8766)
# 
# #pre and post intervention
# 
# 
# pre.period <-  as.Date(c("1997-01-01", "2019-12-31"))
# post.period <- as.Date(c("2020-01-01", "2020-12-31"))
# 
# 
# #Causal Impact
# 
# impact <- CausalImpact(va_coal_unit, pre.period, post.period)
# 
# plot(impact) + scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2020-12-11")))
# 


#Generalized synthetic Control Method

# https://yiqingxu.org/software/gsynth/gsynth_examples.html



ampd_raw <- as.data.table(read.fst ("data/ampd_raw.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

# ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
#                                       SO2..tons.=ampd_raw$SO2..tons.,
#                                       CO2..tons.= ampd_raw$CO2..tons.,
#                                       Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
#                                       Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
#                                       HEAT.INPUT= ampd_raw$HEAT.INPUT,
#                                       SUM_OP_TIME= ampd_raw$SUM_OP_TIME
# ), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
#            month=ampd_raw $month), FUN=sum)
# 
# 
# ampd_sy<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
#                                       SO2..tons.=ampd_raw$SO2..tons.,
#                                       CO2..tons.= ampd_raw$CO2..tons.,
#                                       Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
#                                       Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
#                                       HEAT.INPUT= ampd_raw$HEAT.INPUT,
#                                       SUM_OP_TIME= ampd_raw$SUM_OP_TIME
# ), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year), FUN=sum)
# 
# 
# 
# ampd_sy <- ampd_sy %>% filter(year >=1997)


#virginia
# 
# ampd_sy_va <- ampd_raw %>% filter( STATE== "VA" & year >= 2015 ) 
# 
# 
# 
# ampd_sy_va$month <- ampd_sy_va[, stri_pad_left(ampd_sy_va$month, pad="0", width=2)]
# 
# 
# 
# ampd_sy_va <- ampd_sy_va  %>% dplyr::select ( ID, year, month, NOx..tons., Gross.Load..MW.h.,  HEAT.INPUT, SUM_OP_TIME)
# 
# 
# ampd_sy_va <- as.data.table(na.omit(ampd_sy_va))
# 
# 
# ampd_sy_va2 <- ampd_sy_va[, inputed := 0]
# 
# 
# 
# ampd_sy_va2$inputed[ampd_sy_va2$year==2020] <- 1
# 
# ampd_sy_va3 <- ampd_sy_va2 [, time := paste(year, month, sep = "")]
# 
# 
# # ampd_sy_va3$time =  str_sub(ampd_sy_va3$time,-4)
# 
# ampd_sy_va3$time <- as.numeric(ampd_sy_va3$time)
# 
# 
# 
# ampd_sy_va3 <- ampd_sy_va3 %>% dplyr::select ( ID,  NOx..tons., HEAT.INPUT, SUM_OP_TIME, time, inputed)
# 
# 
# time_period <- as.data.frame(read.csv("/Users/munshirasel/Google Drive/R/ampd-3/data/time-period.csv"))
# 
# 
# ampd_va <- merge(ampd_sy_va3, time_period, by= "time" )
# 
# ampd_va <-  ampd_va %>% dplyr::select ( ID,  NOx..tons., HEAT.INPUT, SUM_OP_TIME, period, inputed)
# 
# panelView(NOx..tons.  ~ inputed, data = ampd_va,  index = c("ID","period"),by.timing = TRUE) 
# 
# panelView(NOx..tons.  ~ inputed, data = ampd_va,  index = c("ID","period"), type= "outcome") 
# 
# #out <- gsynth(lm((NOx..tons. ~ inputed+ Gross.Load..MW.h. + HEAT.INPUT + SUM_OP_TIME),data=ampd_sy_va),
#         #      data = ampd_sy_va, index = c("month","year"), force = "two-way", 
#          #     CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE)
# 
# 
# 
# out <- gsynth(NOx..tons. ~ inputed+  HEAT.INPUT + SUM_OP_TIME, data=ampd_va, 
#     index = c("ID","period"), force = "two-way", 
#               CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, 
#     parallel = TRUE, cores=4)


#out <- gsynth(lm((NOx..tons. ~ inputed+ Gross.Load..MW.h. + HEAT.INPUT + SUM_OP_TIME),data=ampd_sy_va),
#      data = ampd_sy_va, index = c("month","year"), force = "two-way", 
#     CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE)






#ampd_sy_va$year <- as.Date(as.character(ampd_sy_va$year), format = "%Y")
# 
# ampd_sy_va <- ampd_sy %>% filter( STATE== "VA")
# 
# 
# ampd_sy_va <- ampd_sy_va  %>% dplyr::select ( NOx..tons., Gross.Load..MW.h.,  SUM_OP_TIME)
# 
# 
# ampd_sy_va <- as.data.table(na.omit(ampd_sy_va))
# 
# pre.period <- c (1, 23)
# post.period <-c (24, 24)
# 
# 
# impact <- CausalImpact(ampd_sy_va, pre.period, post.period)
# 
# plot(impact)
# 
# summary(impact)
# 
# 
# 
# 
# ampd_raw %>% filter (STATE== "VA" )
# 



# ===========================================================================

# gsynth

# ============================================================================

#Filtering state data

ampd_daily_va <- ampd_daily_ec %>% filter (STATE=="NY" & year >=1997)


va_ampd <-  ampd_daily_va

#aggregating emission values with respect to year, month and day 

va_ampd<- setDT( va_ampd )[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                               NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                               CO2..tons. = sum(CO2..tons., na.rm=TRUE),
                               SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
                               Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
                               Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
                               HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)), 
                           by = .(year,month,day)]

#va_coal_unit <- as_tibble(va_coal %>% filter(ID == "7213-1" ) )


va_ampd <-  va_ampd %>% filter (month <=4) %>% dplyr::select (year, month, day, NOx..tons., HEAT.INPUT, 
                                                              SUM_OP_TIME, Gross.Load..MW.h.)

#seeting NA values as 0 indication 0 emissions, 0 input, 0 output on those days

va_ampd[is.na(va_ampd)] <-  0


va_ampd <-  as.data.table(va_ampd)

va_ampd$day <- va_ampd[ , stri_pad_left(va_ampd$day, pad="0", width=2)]

#making NOx emissions as categorical variable: 2020 March and April as 1, all others as 0

va_ampd <- va_ampd[, inputed := 0]

va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==3] <- 1
va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==4] <- 1


#combining month and day together
va_ampd <- va_ampd [, time := paste(month, day, sep = "")]

va_ampd <- va_ampd%>% dplyr::select (year, time, NOx..tons., HEAT.INPUT, 
                                     SUM_OP_TIME, Gross.Load..MW.h., inputed)


#Observing control and treated values
panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"),by.timing = TRUE)

panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"), type= "outcome")


#Running gsynth 

out <- gsynth(NOx..tons. ~ inputed+  HEAT.INPUT + SUM_OP_TIME + Gross.Load..MW.h., data=va_ampd, 
              index = c("year","time"), force = "two-way", 
              CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, 
              parallel = TRUE, cores=4)


print(out)
out$est.att
out$est.avg
out$est.beta

plot(out)

plot(out, theme.bw = TRUE)

plot(out, type = "gap", ylim = c(-70,70), xlab = "Period", main = "NY")

plot(out, type = "raw", theme.bw = TRUE)

plot(out,type = "raw", legendOff = TRUE, ylim=c(0,500), main="")

plot(out, type = "counterfactual", raw = "none", main="NY") + 
  scale_x_continuous(breaks = seq(0, 120, by = 10)) 

plot(out, type = "ct", raw = "none", main = "", shade.post = FALSE)

plot(out, type = "counterfactual", raw = "band", xlab = "Time", ylim = c(-5,35), theme.bw = TRUE)

plot(out, type = "counterfactual", raw = "all")

plot(out, type = "counterfactual", year = 2000)

plot(out, type = "counterfactual", id = 104, raw = "band", ylim = c(-10, 30))

plot(out, type = "counterfactual", id = 105, raw = "all", legendOff = TRUE)

plot(out, type = "factors", xlab = "Time")

plot(out, type = "loadings")




# ===========================================================================

# gsynth US
 
# ============================================================================

#Filtering state data

ampd_daily<- ampd_daily %>% filter (year >=1997)



dfy<- as.data.table( ampd_daily)

# Checking if we have repeatated values or not

dfz<- unique( dfy, by = c ("ORISPL_CODE", "UNITID", "date", "STATE"))
# 
# dfz <- dfz%>% filter (Fuel.Type..Primary.=="Coal")
# 

#combining ORISPL and UNITID as one

va_ampd <- dfz[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


#aggregating emission values with respect to year, month and day 

va_ampd<- setDT( va_ampd )[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                               NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                               CO2..tons. = sum(CO2..tons., na.rm=TRUE),
                               SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
                               Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
                               Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
                               HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)), 
                           by = .(year,month,day)]

#va_coal_unit <- as_tibble(va_coal %>% filter(ID == "7213-1" ) )


va_ampd <-  va_ampd %>% filter (month <=4) %>% dplyr::select (year, month, day, NOx..tons., HEAT.INPUT, 
                                                              SUM_OP_TIME, Gross.Load..MW.h.)

#seeting NA values as 0 indication 0 emissions, 0 input, 0 output on those days

va_ampd[is.na(va_ampd)] <-  0


va_ampd <-  as.data.table(va_ampd)

va_ampd$day <- va_ampd[ , stri_pad_left(va_ampd$day, pad="0", width=2)]

#making NOx emissions as categorical variable: 2020 March and April as 1, all others as 0

va_ampd <- va_ampd[, inputed := 0]

va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==3] <- 1
va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==4] <- 1


#combining month and day together
va_ampd <- va_ampd [, time := paste(month, day, sep = "")]

va_ampd <- va_ampd%>% dplyr::select (year, time, NOx..tons., HEAT.INPUT, 
                                     SUM_OP_TIME, Gross.Load..MW.h., inputed)


#Observing control and treated values
panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"),by.timing = TRUE)

panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("year","time"), type= "outcome")


#Running gsynth 

out <- gsynth(NOx..tons. ~ inputed+  HEAT.INPUT + SUM_OP_TIME + Gross.Load..MW.h., data=va_ampd, 
              index = c("year","time"), force = "two-way", 
              CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, 
              parallel = TRUE, cores=4)


print(out)
out$est.att
out$est.avg
out$est.beta

plot(out)

plot(out, theme.bw = TRUE)

plot(out, type = "gap", ylim = c(-4000,4000), xlab = "Period", main = "US")

plot(out, type = "raw", theme.bw = TRUE)

plot(out,type = "raw", legendOff = TRUE, ylim=c(0,500), main="")

plot(out, type = "counterfactual", raw = "none", main="US") + 
  scale_x_continuous(breaks = seq(0, 120, by = 10)) 

plot(out, type = "ct", raw = "none", main = "", shade.post = FALSE)

plot(out, type = "counterfactual", raw = "band", xlab = "Time", ylim = c(-5,35), theme.bw = TRUE)

plot(out, type = "counterfactual", raw = "all")

plot(out, type = "counterfactual", year = 2000)

plot(out, type = "counterfactual", id = 104, raw = "band", ylim = c(-10, 30))

plot(out, type = "counterfactual", id = 105, raw = "all", legendOff = TRUE)

plot(out, type = "factors", xlab = "Time")

plot(out, type = "loadings")

