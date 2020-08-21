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

ampd_monthly<- read.fst ("data/ampd_monthly.fst")

facility_attributes <- fread ("data/facility_attributes.csv")

names(facility_attributes)
names (ampd_monthly)

variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Year",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
  "Associated Stacks",
  "Program(s)",
  "EPA Region",
  "NERC Region",
  "County",
  "County Code",
  "FIPS Code",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "SO2 Phase",
  "NOx Phase",
  "Unit Type",
  "Fuel Type (Primary)",
  "Fuel Type (Secondary)",
  "SO2 Control(s)",
  "NOx Control(s)",
  "PM Control(s)",
  "Hg Control(s)",
  "Commercial Operation Date" ,
  "Operating Status",
  "Max Hourly HI Rate (MMBtu/hr)",
  "NA"
))

setnames(facility_attributes, variables)

colnames(facility_attributes)[3] <- "ORISPL_CODE"

colnames(facility_attributes)[4] <- "UNITID"


#adding two dataframe

dfy <- merge(ampd_monthly,facility_attributes,by=c("ORISPL_CODE","UNITID"))

dfy<- as.data.table(dfy)

names(dfy)

str(dfy)


dfz<- unique( dfy, by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE"))


ampd_raw <- dfz[, list(ORISPL_CODE, UNITID, STATE, year,
                   month, SO2..tons., NOx..tons., CO2..tons.,
                   Avg.SO2.RATE, Avg.NOx.RATE, Avg.CO2.RATE,
                   SUM_OP_TIME, Gross.Load..MW.h., Steam.Load..1000lb.,
                   HEAT.INPUT, FIPS.Code, County, County.Code, Source.Category,
                   Facility.Latitude, Facility.Longitude, SO2.Phase, NOx.Phase,
                   Fuel.Type..Primary.,Fuel.Type..Secondary., SO2.Control.s.,
                   NOx.Control.s., PM.Control.s., Hg.Control.s., Max.Hourly.HI.Rate..MMBtu.hr. )]

write.fst(ampd_raw, "data/ampd_raw.fst")

str(ampd_raw)



#data validation check with Harvard data

url <- "https://dataverse.harvard.edu/api/access/datafile/3086908?gbrecs=true"

directory<- "/Users/munshirasel/Google Drive/R/ampd-3/data"
file <- file.path(directory, 'AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv'
)
# if file does not exist, download it
if (!file.exists(file)) {
  download.file(url = url, destfile = file)
  print("data downloaded")
}


ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

names(ampd_harvard)
as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 







##### Data validate #####

ampd_raw %>% filter ( year == 1998, month == 8, ORISPL_CODE == 3,
                      UNITID=="1") %>% select (SO2..tons., NOx..tons., Gross.Load..MW.h., HEAT.INPUT)

# SO2..tons. NOx..tons. Gross.Load..MW.h. HEAT.INPUT
# 1:    586.879    213.358             99237   904854.2

ampd_harvard %>% filter ( Year == 1998, Month == 8, Facility.ID..ORISPL. == 3,
                          Unit.ID=="1") %>% 
  select (SO2..tons., NOx..tons., Gross.Load..MW.h., Heat.Input..MMBtu.)

# SO2..tons. NOx..tons. Gross.Load..MW.h. Heat.Input..MMBtu.
# 1:     586.88    213.363             99237           904854.2





ampd_raw %>% filter ( year == 2004, month == 8, ORISPL_CODE == 981,
                      UNITID=="4") %>% select (SO2..tons., NOx..tons., Gross.Load..MW.h., HEAT.INPUT)


ampd_harvard %>% filter ( Year == 2004, Month == 8, Facility.ID..ORISPL. == 981,
                          Unit.ID=="4") %>% 
  select (SO2..tons., NOx..tons., Gross.Load..MW.h., Heat.Input..MMBtu.)




ampd_all_harvard<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                           SO2..tons.=ampd_harvard$SO2..tons.,
                           CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                           Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                           Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                           Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list( Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)

#plotting 

ampd_all_harvard %>% filter (Year >= 1995) %>%
  ggplot(aes(Year, NOx..tons., group= Year)) + geom_boxplot()+ labs(x= "Year", 
                                                                    y = "NOx Emission (tons)",
                                                                    title = "Emission  to 2020") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 2))

ampd_all_harvard %>% filter (Year == 2007)






