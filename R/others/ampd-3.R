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

 df <- read.fst ("emissions-raw.fst")
# 
# df3 <- df2 [-c (50000: 99999), ] 
# 
# write.fst(df3, "df3.fst")

#first I would like to work on a small dataset since running big data set takes time.

# rm(list=ls())

# df <- read.fst ("df3.fst")
# head (df)

# as.Date(df$ OP_DATE)
# 
# df %>%
#   dplyr::mutate(year = lubridate::year(OP_DATE), 
#                 month = lubridate::month(OP_DATE), 
#                 day = lubridate::day(OP_DATE))


df<- df %>% 
  mutate(date = mdy(OP_DATE)) %>% 
  mutate_at(vars(date), funs(year, month, day))

#merging two columns (SO2_Mass (not sure about unit guesing as tons)+ SO2_Mass..tons); similarly others

#Dataset were divided under different columns  after 1998.

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

# rm (df) #removed df since vector memory were completely full and was giving error Error: vector memory exhausted (limit reached?)




# Aggregate over week number and climate division
dfx<- aggregate(SO2..tons.~month+ORISPL_CODE+UNITID+year,
                FUN=sum, data=df_subset, na.rm=FALSE)

dfx %>% filter ( year == 1997, month == 1, ORISPL_CODE == 3,
                 UNITID==1)
# Output
#   Week Climate_Division       Rain
# 1   27                1 1.07288622
# 2   29                1 0.05555556
# 3   30                1 0.12500000
# 4   27                2 1.07288622
# 5   29                2 0.05555556
# 6   30                2 0.12500000




# rm (df_subset)

# str(df_subset)

# write.fst(df_subset2, "df_subset.fst")

#aggregating daily data set into monthly dataset
# df_subset<- read.fst("df_subset.fst")

df_month <- df_subset [, .(year,
                           STATE,
                           FACILITY_NAME,
                           SUM_OP_TIME= sum(SUM_OP_TIME, na.rm = TRUE),
                           COUNT_OP_TIME= sum(COUNT_OP_TIME, na.rm = TRUE),
                           FAC_ID,
                           UNIT_ID,
                           Gross.Load..MW.h.= sum(Gross.Load..MW.h., na.rm = TRUE),
                           Steam.Load..1000lb.= sum(Steam.Load..1000lb., na.rm = TRUE),
                           HEAT.INPUT= sum(HEAT.INPUT, na.rm = TRUE),
                           Avg.SOx.RATE..lbs.mmBtu.= mean(SO2.RATE, na.rm= TRUE),
                           Avg.NOx.RATE..lbs.mmBtu.= mean(NOx.RATE, na.rm= TRUE),
                           Avg.CO2.RATE..tons.mmBtu.= mean(CO2.RATE, na.rm= TRUE),
                           
                           SOx..tons. = sum(SO2..tons., na.rm = TRUE),
                           CO2..tons. = sum(CO2..tons., na.rm = TRUE),
                           NOx..tons. = sum(NOx..tons., na.rm = TRUE)),
                           by = c("ORISPL_CODE", "UNITID", "month")]

# head (df_month, 100)
# rm(df_month)
# 
# #all the variable along with date data is showing. I'll get unique dataset
# 
# write.fst(df_month, "df_month.fst")
# 
# rm(df_subset2)

# df_month <- unique(df_month, by = c("ORISPL_CODE", "UNITID", "month"))
df_month2 <- unique(df_month, by = c("ORISPL_CODE", "UNITID", "month", "year", "STATE") ) 

# rm (df_month)

df_month2 <- df_month2[order(year, month),]

write.fst (df_month2, "df_month.fst")

length (unique(df_month$ month))

df_month2 %>% filter (FACILITY_NAME== "Barry" , year == 2015, ORISPL_CODE == 3 ) 
#value is not matching with  AMPD dataset


df_barry <- df_subset2 %>% filter (FACILITY_NAME== "Barry" , year == 1997, month == 1, ORISPL_CODE == 3,
                  UNITID==1) %>% 
  head (32)
sum( df_barry $ SO2..tons.) # 549.668 value same as d_ampd from units-data prep.R file same as AMPD-RASEL.R SO2 tons value for barry facility

# this means our code has some problem




