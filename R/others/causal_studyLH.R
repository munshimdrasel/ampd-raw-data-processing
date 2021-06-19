library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
library(parallel)
# library(sf)
library(viridis)
library(ggplot2)
library(stringi)
library(panelView)

# setwd ("~/Dropbox/GeorgeMason/Advising/Group/MunshiRasel/Code")

#getting daily data

ampd_daily <- read.fst ("/projects/HAQ_LAB/data/ampd-data/ampd_daily_emission.fst", as.data.table = T)

#source category

unique (ampd_daily$Source.Category)


#ampd power plants data filtering

ampd_daily_ec <- ampd_daily[Source.Category== "Electric Utility"]

#Filtering state data

ampd_daily_va <- ampd_daily_ec[STATE=="VA" & year >=1997]

ampd_daily_va <- as.data.table(ampd_daily_va)



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


# #Issues
# ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "3797-7" ) #CO2 tons data not there
# 
# 
# ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "3809-1" ) #no 2020 data
# 
# 
# ampd_daily_va_unit <- ampd_daily_va_2 %>% filter (ID== "7838-4" ) #very low NOx emission/ doesn't operate long


va_ampd <- ampd_daily_va_2


unique(va_ampd$fuel.primary)

unique(va_ampd$nox.control)

#units are not having different fuel types or different nox control (time when didn't have control)

va_ampd <-  va_ampd %>% filter (month <=4 ) %>% dplyr::select (year, month, day, ID, NOx..tons., HEAT.INPUT,
                                                               SUM_OP_TIME, Gross.Load..MW.h., CO2..tons., 
                                                               Fuel.Type..Primary., NOx.Control.s.,
                                                               fuel.primary, nox.control)



va_ampd[is.na(va_ampd)] <-  0


va_ampd <-  as.data.table(va_ampd)

va_ampd$day <- va_ampd[ , stri_pad_left(va_ampd$day, pad="0", width=2)]

#making NOx emissions as categorical variable: 2020 March and April as 1, all others as 0

va_ampd <- va_ampd[, inputed := 0]

va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==3 ] <- 1
va_ampd$inputed[va_ampd$year==2020 & va_ampd$month==4 ] <- 1


#combining month and day together
va_ampd <- va_ampd [, time := paste(month, day, sep = "")]
va_ampd <- va_ampd [, id := paste(year, ID, sep = "_")]


#Observing control and treated values
panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("id","time"),by.timing = TRUE)

panelView(NOx..tons.  ~ inputed, data = va_ampd,  index = c("id","time"), type= "outcome")


#Running gsynth 

out <- gsynth(NOx..tons. ~ inputed + Fuel.Type..Primary. + NOx.Control.s., 
              data=va_ampd,
              index = c("id","time"), na.rm=T, force = "two-way",
              CV = TRUE, seed =  123, estimator = "mc", min.T0 = 10, inference = "parametric", 
              parallel = FALSE)




plot(out, theme.bw = TRUE)

plot(out, type = "counterfactual", raw = "none", main="") + 
  scale_x_continuous(breaks = seq(0, 120, by = 10)) 
