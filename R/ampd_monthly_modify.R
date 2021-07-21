#Script 3: Merging facility attributes with ampd monthly emission data 
#here I've used Facility attributes from this bookmark:  https://ampd.epa.gov/ampd/#?bookmark=28623  


rm(list = ls())

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
library(dplyr)
library(stringr)



#on local machine
setwd ("/Users/munshirasel/Google_Drive/R/ampd-raw-data-processing")


#on hopper cluster use this working directory:
#setwd ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing")


#see ampd-daily-to-monthly.R file for data preparation on this dataset
ampd_monthly<- as.data.table(read.fst ("data/ampd_monthly.fst"))

# https://ampd.epa.gov/ampd/#?bookmark=28623  (facility attributes)

facility_attributes <- fread ("data/facility_attributes_updated.csv")


variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Associated Stacks",
  "Year",
  "Program(s)",
  "EPA Region",
  "NERC Region",
  "County",
  "County Code",
  "FIPS Code",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
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

colnames(facility_attributes)[1] <- "STATE"


length(unique(ampd_monthly$ORISPL_CODE))
length(unique(facility_attributes$ORISPL_CODE))

#my emission data is short of 58 facilities comparing to facility attributes files. missing facilities (example: ORISPL CODE: 10784, 913)

sort(unique(facility_attributes$Year))


#checking if data has duplicated rows
ampd_monthly <- as_tibble(ampd_monthly %>% distinct())
facility_attributes <- as_tibble((facility_attributes %>% distinct()))

#removing 0's from in front of UNITID in monthly emission datasets and facility attributes to match same UNITID in both datasets
ampd_monthly$UNITID <-  str_replace(ampd_monthly$UNITID, "^0+" ,"")
facility_attributes$UNITID <- str_replace(facility_attributes$UNITID, "^0+" ,"")


#Year is activity year. In that year may be facility changed fuel types/ or implemented new regulations etc.
ampd_monthly$Year <- ampd_monthly$year

#merging
dfy <- merge(ampd_monthly,facility_attributes, by=c("STATE", "ORISPL_CODE","UNITID", "Year" ), 
             all.x = T, allow.cartesian = T) 

#dfy observation must match ampd_monthly observations 

dfy<- unique( as.data.table(dfy), by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE" )) 

dfy <- dfy %>% dplyr::select(-"NA.")

dfy<- as.data.table(dfy)

dfy[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

dfy[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
dfy[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

dfy[, Fuel2.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Secondary.))]
dfy[Fuel.Type..Primary. == "", Fuel2.IsCoal := NA]

###trying Has.SO2.scrubber variable 
dfy <- dfy[, Has.SO2.Scrub := 1]
dfy[is.na(SO2.Control.s.), SO2.Control.s. := ""]
dfy$Has.SO2.Scrub <- ifelse(dfy$SO2.Control.s. == "", 0, 1)


write.fst(dfy, "data/ampd_monthly_all.fst")

# ==============validating scrubber information with AMPD harvard dataset=========================
ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL]
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
ampd_harvard <- ampd_harvard[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]

length(unique(dfy$ORISPL_CODE [dfy$year==2014 & dfy$Has.SO2.Scrub==1]))
length(unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Has.SO2.Scrub==1]))

x <- unique(dfy$ORISPL_CODE [dfy$year==2014 & dfy$Has.SO2.Scrub==1])
y <- unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Has.SO2.Scrub==1])

x %in% y

#facility 883-8 has no SO2 scrubber info in AMPD harvard dataset but Has.SO2.Scrubber variable was mentioned as 1.

dfy_883 <- dfy %>% filter (ORISPL_CODE==883 & year== 2014)

ampd_harvard_883 <- ampd_harvard %>% filter (Facility.ID..ORISPL.==883 & Unit.ID=="8" & Year==2014)
tail(ampd_harvard_883,10)

#trying operating status


dfy <- dfy %>% mutate (Status = ifelse(dfy$Operating.Status %in% c ("Retired"), "Retired",
                         ifelse(dfy$Operating.Status %in% c ("Long-term Cold Storage"), "Long-term Cold Storage",
                                ifelse(dfy$Operating.Status %in% "Retired (Retired 01/01/1999)", "Retired", "Operating"
                                        ))))

length(unique(dfy$ORISPL_CODE [dfy$year==2014 & dfy$Status=="Operating"]))
length(unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Status=="Operating"]))

x <- unique(dfy$ORISPL_CODE [dfy$year==2014 & dfy$Status=="Operating"])
y <- unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Status=="Operating"])

x %in% y

#facility 2362, 880087 doesn't have observations in ampd_harvard dataset

dfy_880087 <- dfy %>% filter (ORISPL_CODE== 880087& year== 2014)

ampd_harvard_880087 <- ampd_harvard %>% filter (Facility.ID..ORISPL.==880087 & Year==2014)



# =============================================================================================

# Data validation with AMPD Harvard datasets after merging Facility attributes information


ampd_raw <- as.data.table(read.fst ("data/ampd_monthly_all.fst"))
ampd_updated <- ampd_raw %>%  filter (year >=1997 & year <=2020)
ampd_updated_coal <- ampd_raw %>%  filter (Fuel1.IsCoal==1 & year >=1997 & year <=2020)

ampd_harvard <- ampd_harvard %>% filter ( Year >=1997 & Year <=2020)
ampd_harvard_coal <- ampd_harvard %>% filter (Fuel1.IsCoal==1 & Year >=1997 & Year <=2020)

length(unique(ampd_updated$ORISPL_CODE [ampd_updated$year==2014 & ampd_updated$Has.SO2.Scrub==1]))
length(unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Has.SO2.Scrub==1]))

length(unique(ampd_updated$ORISPL_CODE [ampd_updated$year==2014 & ampd_updated$Status=="Operating"]))
length(unique(ampd_harvard$Facility.ID..ORISPL. [ampd_harvard$Year==2014 & ampd_harvard$Status=="Operating"]))

years <- (1997:2020)

dfm <- as.data.frame(matrix(nrow=24, ncol = 3))

for (i in 1:length(years)) {
  yr <- ampd_updated %>%  filter (year== years[i])
  operation <- length(unique(yr$ORISPL_CODE [yr$year==years[i] & yr$Status=="Operating"]))
  had.so2.scrubber <- length(unique(yr$ORISPL_CODE [yr$year==years[i] & yr$Has.SO2.Scrub==1]))
  dfm [i,1] <- years[i]
  dfm [i,2] <- operation
  dfm [i,3] <- had.so2.scrubber
}

colnames(dfm)[1] <- "year"
colnames(dfm)[2] <- "facility.operation"
colnames(dfm)[3] <- "had.so2.scrubbers"

dfm %>% ggplot(aes (x=year)) + geom_point(aes(y=facility.operation)) + geom_line(aes(y=facility.operation))


length(unique(ampd_updated_coal$UNITID [ampd_updated_coal$year==2014 & ampd_updated_coal$Status=="Operating"]))
length(unique(ampd_harvard_coal$Unit.ID [ampd_harvard_coal$Year==2014 & ampd_harvard_coal$Status=="Operating"]))

ampd_unit_LH <- fread ("/Users/munshirasel/Downloads/AMPD_Unit.csv")


###comapring with AMPD_unit.csv file
length(unique(ampd_unit_LH$Unit.ID [ampd_unit_LH$Year==2014 & ampd_unit_LH$Has.SO2.Scrub==1]))
length(unique(ampd_updated$UNITID [ampd_updated$year==2014 & ampd_updated$Has.SO2.Scrub==1]))

length(unique(ampd_unit_LH$Facility.ID..ORISPL. [ampd_unit_LH$Year==2014 & ampd_unit_LH$Has.SO2.Scrub==1]))
length(unique(ampd_updated$ORISPL_CODE [ampd_updated$year==2014 & ampd_updated$Has.SO2.Scrub==1]))




