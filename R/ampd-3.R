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

#separating dates in different columns
df<- df %>% 
  mutate(date = mdy(OP_DATE)) %>% 
  mutate_at(vars(date), funs(year, month, day))

#merging two columns (SO2_Mass (not sure about unit guesing as tons)+ SO2_Mass..tons); similarly others

#Dataset were divided under different columns  after .

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

rm(df2)


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



# str(dfx)
# dfx %>% filter ( year == 1997, month == 1, ORISPL_CODE == 3,
#                                  UNITID=="1")
# 
# dfx %>% filter ( year == 2020, month == 1, ORISPL_CODE == 55284,
#                  UNITID=="GS04")
# 
# 
# 
# 
# dfx %>% filter ( year == 2014, month == 1, ORISPL_CODE == 880100,
#                  UNITID=="BLR004")
# #this data matches with ampd-3.R file where I downloaded data automatically using url. It means
# #data validation is ok.
# 
# 
# 
# 
# 
# 
# df %>% filter ( year == 2020, month == 1, ORISPL_CODE == 55284,
#                  UNITID=="GS04")
# 
# df_subset %>% filter ( year == 1995, month == 1, ORISPL_CODE == 47,
#                        UNITID=="5") 
# 
# 
# 
# df %>% filter ( ORISPL_CODE==880041)






# rm (df_subset)

# str(df_subset)

# write.fst(df_subset2, "df_subset.fst")

#aggregating daily data set into monthly dataset
# df_subset<- read.fst("df_subset.fst")



# head (df_month, 100)
# rm(df_month)
# 
# #all the variable along with date data is showing. I'll get unique dataset
# 
# write.fst(df_month, "df_month.fst")
# 
# rm(df_subset2)

# df_month <- unique(df_month, by = c("ORISPL_CODE", "UNITID", "month"))
# df_month2 <- unique(df_month, by = c("ORISPL_CODE", "UNITID", "month", "year", "STATE") ) 
# 
# # rm (df_month)
# 
# df_month2 <- df_month2[order(year, month),]
# 
# write.fst (df_month2, "df_month.fst")
# 
# length (unique(df_month$ month))
# 
# df_month2 %>% filter (FACILITY_NAME== "Barry" , year == 2015, ORISPL_CODE == 3 ) 
# #value is not matching with  AMPD dataset
# 
# 
# df_barry <- df_subset2 %>% filter (FACILITY_NAME== "Barry" , year == 1997, month == 1, ORISPL_CODE == 3,
#                   UNITID==1) %>% 
#   head (32)
# sum( df_barry $ SO2..tons.) # 549.668 value same as d_ampd from units-data prep.R file same as AMPD-RASEL.R SO2 tons value for barry facility
# 
# # this means our code has some problem




