#Part 2: AMPD-Raw data processing starts here

#Script 1: Converting daily emission data into monthly emission data

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

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")

df <- read.fst ("data/emissions-raw.fst")

#checked if there are any duplicated rows in the data
df2<- df %>% distinct()

rm(df2)



head(df)

#separating dates in different columns
df<- df %>% mutate(date = mdy(OP_DATE)) %>% mutate_at(vars(date), funs(year, month, day))

#merging two columns (SO2_Mass (not sure about unit guesing as tons)+ SO2_Mass..tons); similarly others

#Dataset were divided under different columns  after certain years

df$Gross.Load..MW.h. = coalesce(df$GLOAD,df$GLOAD..MWh.)
df$SO2..tons. = coalesce(df$ SO2_MASS, df$ SO2_MASS..tons.)
df$Steam.Load..1000lb. = coalesce(df$SLOAD,df$SLOAD..1000.lbs.)

df$NOx..tons. = coalesce(df$NOX_MASS,df$NOX_MASS..tons.)
df$CO2..tons. = coalesce(df$CO2_MASS,df$CO2_MASS..tons.)

df$SO2.RATE = coalesce(df$SO2_RATE,df$SO2_RATE..lbs.mmBtu.)

df$NOx.RATE = coalesce(df$NOX_RATE,df$NOX_RATE..lbs.mmBtu.)

df$CO2.RATE = coalesce(df$CO2_RATE,df$CO2_RATE..tons.mmBtu.)

df$HEAT.INPUT = coalesce(df$HEAT_INPUT,df$HEAT_INPUT..mmBtu.)

head (df, 10)

names(df)



df_subset <- as.data.table (subset (df, select = c ("STATE",
  "FACILITY_NAME",
  "ORISPL_CODE",
  "UNITID",
  "date",
  "year",
  "month",
  "day",
  "SUM_OP_TIME",
  "COUNT_OP_TIME",
  "FAC_ID",
  "UNIT_ID",
  "Gross.Load..MW.h.",
  "Steam.Load..1000lb.",
  "SO2..tons.",
  "NOx..tons.",
  "CO2..tons.",
  "SO2.RATE",
  "NOx.RATE",
  "CO2.RATE",
  "HEAT.INPUT")))

str(df_subset)

names(df_subset)





dfx<- setDT(df_subset)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
                           NOx..tons. = sum(NOx..tons., na.rm=TRUE),
                           CO2..tons. = sum(CO2..tons., na.rm=TRUE),
                           Avg.SO2.RATE = mean (SO2.RATE, na.rm=TRUE),
                           Avg.NOx.RATE = mean (NOx.RATE, na.rm=TRUE),
                           Avg.CO2.RATE = mean (CO2.RATE, na.rm=TRUE),
                           SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
                           Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
                           Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
                           HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)), 
                       by = .(STATE, FACILITY_NAME, ORISPL_CODE, UNITID, year, month)]





write.fst(dfx, "data/ampd_monthly.fst")


