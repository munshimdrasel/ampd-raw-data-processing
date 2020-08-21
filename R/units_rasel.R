# Here i followed untis data preparation from disperseR vignette
#http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Units_Preparation.html



#this file is to create units data file for disperseR package using PP.units.monthly 1995 to 2020 data

library(disperseR) # our package
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


disperseR::create_dirs("/Users/munshirasel/Google Drive/R/ampd-3")

url <-
  "ftp://newftp.epa.gov/air/emismod/2014/v2/2014fd/emissions/2014fd_inputs_point.zip"
directory <- proc_dir
file <- file.path(directory, '2014fd_inputs_point.zip')

if (!file.exists(file)) {
  # if file does not exist, download it
  download.file(url = url, destfile = file)
}
unzip(file, exdir = directory) # unzip the file

file <- file.path( directory, "2014fd_cb6_14j", "inputs", "ptegu",
                   "ptegu_2014NEIv2_POINT_20171103_final_21dec2017_nf_v2.csv")

d_nei <- data.table::fread(file, skip = 18)


#AMPD data attaching

file <- load ("data/PP.units.monthly1995_2020.RData")

d_ampd <- PP.units.monthly1995_2020

# d_ampd <- transform(PP.units.monthly1995_2020, Year = as.numeric(Year))




d_ampd2 <- d_ampd


#Manipulation

d_nei_unique <- unique(d_nei[, .(
  facility_name,
  Facility.ID..ORISPL. = oris_facility_code,
  Unit.ID = oris_boiler_id,
  stkhgt,
  stkdiam,
  stktemp,
  stkvel,
  latitude,
  longitude
)])

d_nei_unique <- d_nei_unique[Facility.ID..ORISPL. != "" & Unit.ID != ""]
d_nei_unique <- d_nei_unique[, Facility.ID..ORISPL. := as.numeric(d_nei_unique$Facility.ID..ORISPL.)]


PP.units.monthly1995_2020_2 <- unique(PP.units.monthly1995_2020, by = c("Facility.ID..ORISPL.", "Unit.ID", "Month"))


# we can use different fuel type. Here I'm considering Coal fuel type for further analysis

##----- Coal-burning units

d_ampd[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
d_ampd[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

d_ampd[, Fuel2.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Secondary.))]
d_ampd[Fuel.Type..Primary. == "", Fuel2.IsCoal := NA]

d_ampd <- d_ampd[, list(Facility.ID..ORISPL., Unit.ID,
                        Year, Month,
                        State, County, FIPS,
                        Facility.Latitude, Facility.Longitude,
                        SO2..tons., NOx..tons., Avg..NOx.Rate..lb.MMBtu., CO2..short.tons.,
                        Heat.Input..MMBtu., Gross.Load..MW.h., Steam.Load..1000lb.,
                        Operating.Time,
                        
                        Source.Category,
                        Fuel.Type..Primary., Fuel1.IsCoal,
                        Fuel.Type..Secondary., Fuel2.IsCoal
                    )]

d_ampd2 <-d_ampd


#AMPD data manipulation to prepare unit data set
get_units_data <- function(year, d_ampd) {
  
  d_ampd <- d_ampd[Year == year][, V1 := NULL]
  
  d_ampd_subset <- d_ampd[Fuel1.IsCoal == 1][, .(
    Facility.ID..ORISPL.,
    Unit.ID,
    Year,
    Month,
    # Initial.Year.of.Operation,
    # Sulfur.Content,
    #Program.s.,
    #SO2.Phase,
    #NOx.Phase,
    # EPA.Region,
    # NERC.Region,
    Source.Category,
    State,
    Facility.Latitude,
    Facility.Longitude,
    # Has.SO2.Scrub,
    SO2..tons.,
    # Has.NOx.Scrub,
    NOx..tons.,
    CO2..short.tons.,
    Heat.Input..MMBtu.,
    Gross.Load..MW.h.,
    Steam.Load..1000lb.)]
  
  d_ampd_annual <- d_ampd_subset[, .(
    Facility.Latitude,
    Facility.Longitude,
    SO2..tons. = sum(SO2..tons., na.rm = TRUE),
    CO2..short.tons. = sum(CO2..short.tons., na.rm = TRUE),
    NOx..tons. = sum(NOx..tons., na.rm = TRUE)
  ),
  by = c("Facility.ID..ORISPL.", "Unit.ID", "Year")]
  
  d_ampd_annual <- unique(d_ampd_annual, by = c("Facility.ID..ORISPL.", "Unit.ID"))
  d_ampd_annual <- d_ampd_annual[, Facility.ID..ORISPL. :=
                                   as.numeric(d_ampd_annual$Facility.ID..ORISPL.)]
  
  ampd <- merge(d_ampd_annual,
                d_nei_unique,
                by = c("Facility.ID..ORISPL.", "Unit.ID"),
                all.x = TRUE)
  
  ampd[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
  ampd <- ampd[, .(
    ID = ID,
    Latitude = Facility.Latitude,
    Longitude = Facility.Longitude,
    SOx = SO2..tons.,
    CO2 = CO2..short.tons.,
    NOx = NOx..tons.,
    Height = conv_unit(stkhgt, "ft", "m"),
    Diam = conv_unit(stkdiam, "ft", "m"),
    Velocity = conv_unit(stktemp, "ft_per_sec", "m_per_sec"),
    Temp = conv_unit(stkvel, "F", "K")
  )]
  
  ampd <- ampd[, inputed := 0]
  ampd$inputed[is.na(ampd$Height)] <- 1
  
  # impute
  ampd$Height[is.na(ampd$Height)] <- mean(ampd$Height, na.rm = T)
  ampd <- unique(ampd, by = "ID")
  
  # add year variable 
  ampd <- ampd[, year := year]
  ampd <- ampd %>% dplyr::select(-c(Diam, Velocity, Temp))
}


vector_years<-c(1995:2020)



units <- data.table::setDF(data.table::rbindlist(lapply(vector_years, 
                                                        get_units_data, 
                                                        d_ampd=d_ampd2)))

units <- units %>%
  mutate(uID=gsub("-", ".", ID))

save(units, file = "data/units_rasel.rda")



# 
# # Aggregate over week number and climate division
# dfx<- aggregate(SO2..tons.~month+ORISPL_CODE+UNITID+year,
#                 FUN=sum, data=df_subset, na.rm=FALSE)
# 
# dfx %>% filter ( year == 1997, month == 1, ORISPL_CODE == 3,
#                  UNITID==1)