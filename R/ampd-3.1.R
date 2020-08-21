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

df$Gross.Load..MW.h. = coalesce(df$GLOAD,df$GLOAD..MWh.)
df$SO2..tons. = coalesce(df$ SO2_MASS, df$ SO2_MASS..tons.)
df$Steam.Load..1000lb. = coalesce(df$SLOAD,df$SLOAD..1000.lbs.)

df$NOx..tons. = coalesce(df$NOX_MASS,df$NOX_MASS..tons.)
df$CO2..tons. = coalesce(df$CO2_MASS,df$CO2_MASS..tons.)

df$SO2.RATE = coalesce(df$SO2_RATE,df$SO2_RATE..lbs.mmBtu.)

df$NOx.RATE = coalesce(df$NOX_RATE,df$NOX_RATE..lbs.mmBtu.)

df$CO2.RATE = coalesce(df$CO2_RATE,df$CO2_RATE..tons.mmBtu.)

df$HEAT.INPUT = coalesce(df$HEAT_INPUT,df$HEAT_INPUT..mmBtu.)


OrganizeDate <- function(ncol.time, fun) {
  names.col <- colnames(data.x)
  if (is.null(ncol.time) == TRUE) {
    if (lubridate::is.POSIXt(data.x[ , ncol.date]) == TRUE) {
      col.time <-  match.fun(fun)(data.x[ , ncol.date])
      data.x[ ,(ncol(data.x) + 1)] <- col.time
      colnames(data.x) <- c(names.col, fun)
    } else {
      data.x[ , ncol.date] <- parse_date_time(x = data.x[ , ncol.date], 
                                              orders = orders)
      col.time <- match.fun(fun)(data.x[ , ncol.date])
      data.x[ ,(ncol(data.x) + 1)] <- col.time
      colnames(data.x) <- c(names.col, fun)
    } 
  } else {
    col.time <- data.x[ , ncol.time]
    colnames(data.x)[ncol.time] <- fun 
  }
  data.x <- data.x
}
#  
# Run internal function to data
#
data.x <- OrganizeDate(ncol.time = ncol.day, fun = "day")
data.x <- OrganizeDate(ncol.time = ncol.month, fun = "month")
data.x <- OrganizeDate(ncol.time = ncol.year, fun = "year")
data.x[ , ncol.date] <- as.Date(paste(data.x$year, data.x$month, data.x$day, sep = "-"))
return(data.x)
}

dfx <- OrganizeDateColumns(data.x = df, ncol.date = 5, orders = "mdy", ncol.day = NULL, ncol.month = NULL, ncol.year = NULL)


df_subset <- as.data.table(subset (df, select = c ("STATE",
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

