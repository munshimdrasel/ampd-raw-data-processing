#units data preparation from disperseR vignette

# http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Units_Preparation.html


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

disperseR::create_dirs("/Users/munshirasel/Google_Drive/R/ampd-3")

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

d_ampd <- data.table::fread("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

d_ampd2 <- d_ampd


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


get_units_data <- function(year, d_ampd) {
  
  d_ampd <- d_ampd[Year == year][, V1 := NULL]
  
  d_ampd_subset <- d_ampd[Fuel1.IsCoal == 1][, .(
    Facility.ID..ORISPL.,
    Unit.ID,
    Year,
    Month,
    Initial.Year.of.Operation,
    Sulfur.Content,
    Program.s. = Program.s..x,
    SO2.Phase = SO2.Phase.y,
    NOx.Phase = NOx.Phase.y,
    EPA.Region,
    NERC.Region,
    Source.Category = Source.Category.y,
    State = State.x,
    Facility.Latitude = Facility.Latitude.x,
    Facility.Longitude = Facility.Longitude.x,
    Has.SO2.Scrub,
    SO2..tons.,
    Has.NOx.Scrub,
    NOx..tons.,
    CO2..short.tons.,
    Heat.Input..MMBtu.,
    Gross.Load..MW.h.,
    Steam.Load..1000lb.,
    Max.Hourly.HI.Rate..MMBtu.hr.)]
  
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

vector_years<-c(1995:2015)

units <- data.table::setDF(data.table::rbindlist(lapply(vector_years, 
                                                        get_units_data, 
                                                        d_ampd=d_ampd2)))

units <- units %>%
  mutate(uID=gsub("-", ".", ID))

save(units, file = "units.Rda")
