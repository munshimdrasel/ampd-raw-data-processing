#Script 6: #This script generates ampd yearly emission data considering  Primary Fuel is only Coal for using in disperseR package


#Here I've included all facilities (not only electric facility) 
# [1] "Electric Utility"     "Cogeneration"         "Small Power Producer" "Industrial Boiler"   
# [5] "Industrial Turbine"   "Pulp & Paper Mill"    "Iron & Steel"         "Institutional"       
# [9] "Petroleum Refinery"   "Cement Manufacturing"

# Here i followed untis data preparation from disperseR vignette
#http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Units_Preparation.html



#this file is to create units data file considering Primary Fuel is Coal
# for disperseR package using PP.units.monthly 1997 to 2021 data
rm(list = ls())

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

# create directory
disperseR::create_dirs("/Users/munshirasel/Google_Drive/R/ampd-raw-data-processing")

#read ampd data

file <- load ("data/PP.units.monthly1997_2021.rda")
d_ampd <- PP.units.monthly1997_2021
d_ampd2 <- d_ampd

# download nei data
url <- "ftp://newftp.epa.gov/air/emismod/2014/v2/2014fd/emissions/2014fd_inputs_point.zip"
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


# we can use different fuel types. Here I'm considering Coal fuel type for further analysis

##----- Coal-burning units

# d_ampd[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
# d_ampd[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]
# 
# d_ampd[, Fuel2.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Secondary.))]
# d_ampd[Fuel.Type..Primary. == "", Fuel2.IsCoal := NA]

d_ampd <- d_ampd[, list(Facility.ID..ORISPL., Unit.ID,
                        Year, Month,
                        State, County, FIPS,
                        Facility.Latitude, Facility.Longitude,
                        SO2..tons., NOx..tons., Avg..NOx.Rate..lb.MMBtu., CO2..short.tons.,
                        Heat.Input..MMBtu., Gross.Load..MW.h., Steam.Load..1000lb.,
                        Operating.Time,
                        Source.Category,
                        Fuel.Type..Primary., Fuel1.IsCoal,
                        Fuel.Type..Secondary., Fuel2.IsCoal,
                        Program.s., SO2.Phase, NOx.Phase, EPA.Region, NERC.Region
                        
                    )]

d_ampd2 <-d_ampd


# define units function
get_units_data <- function(year, d_ampd) {
  
  d_ampd <- d_ampd[Year == year]
  
  d_ampd_subset <- d_ampd[Fuel1.IsCoal == 1][, .(
    Facility.ID..ORISPL.,
    Unit.ID,
    Year,
    Month,
    # Initial.Year.of.Operation,
    # Sulfur.Content,
    Program.s.,
    SO2.Phase,
    NOx.Phase,
    EPA.Region,
    NERC.Region,
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

## run the function
vector_years<-c(1997:2021)
units_updated <- data.table::setDF(data.table::rbindlist(lapply(vector_years, 
                                                        get_units_data, 
                                                        d_ampd=d_ampd2)))



units_updated <-units_updated %>% mutate(uID=gsub("-", ".", ID)) %>% data.table()
save(units_updated, file = "data/units_coal_1997_2021.rda")

## do the monthly data
PP.vars <- c("FacID" = "Facility.ID..ORISPL.",
             "Unit.ID" = "Unit.ID",
             "Latitude" = "Facility.Latitude",
             "Longitude" = "Facility.Longitude",
             "year" = "Year",
             "month" = "Month",
             "SO2.tons" = 'SO2..tons.',
             "NOx.tons" = 'NOx..tons.',
             "HeatIn.MMBtu" = 'Heat.Input..MMBtu.',
             "GrossLoad.MWh" = 'Gross.Load..MW.h.')
units_monthly <- fread("data/AMPD_Unit.csv", select = unname( PP.vars))

setnames( units_monthly, PP.vars, names( PP.vars))
units_monthly[, uID := gsub('_|-|\\*', '.',
                            paste( FacID, Unit.ID, sep = '.'))]

save( units_monthly, file = 'data/units_monthly.rda')



# ==========================Data validation======================================================================


load ("data/units_LH_corrected.Rda")
units_base <- units %>% filter (year>=1997 & year<=2015)

load("data/units_coal_1997_2021.rda")
units_created <- units_updated %>%  filter (year>=1997)

units_base_2018 <- units_base %>% filter (year==2014)
units_created_2018 <- units_created %>% filter (year==2014)

length(unique(units_base_2018$ID))
length(unique(units_created_2018$ID))


length(unique(units_base$ID))
length(unique((units_created$ID)))

# x <- unique(units_base$ID)
# y <-unique((units_created$ID))
# 
# tail(x %in% y, 381)
# 
# x[1262]


units_base_y<- aggregate(list (NOx=units_base$NOx,
                                  SOx=units_base$SOx), 
                         by=list(  year=units_base$year), FUN=sum, na.rm=TRUE)
units_base_y$group <- "emis_disperseR"

units_created_y<- aggregate(list (NOx=units_created$NOx,
                               SOx=units_created$SOx), 
                         by=list(  year=units_created$year), FUN=sum, na.rm=TRUE)
units_created_y$group <- "emis_updated"

units_combined <-  do.call("rbind", list(units_created_y, units_base_y))

#NOx comparison
ggplot() + geom_point(data= units_combined, aes( year, NOx,color= group, group= group)) + 
  geom_line(data= units_combined, aes( year, NOx,color= group, group= group)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  +
  labs(x= "Year",   y = "NOx Emission (tons)", title = "Yearly coal fired power plants emission comparison")

#SO2 comparison
ggplot() + geom_point(data= units_combined, aes( year, SOx,color= group, group= group)) +
  geom_line (data= units_combined, aes( year, SOx,color= group, group= group)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  +
  labs(x= "Year",   y = "SO2 Emission (tons)", title = "Yearly coal fired power plants emission comparison") 



# ========monthly data validation-=====================

PP.units.monthly1995_2017 <- disperseR::PP.units.monthly1995_2017 %>% filter(year>=1997 & year <=2017 )


#updated dataset (rasel)
load("data/units_monthly.rda")



PP.units.monthly1997_2021 <- units_monthly %>% filter(year>= 1997 & year<=2017)


length(unique(PP.units.monthly1995_2017$uID)) #disperseR dataset has 5866 unit id

length(unique(PP.units.monthly1997_2021$uID)) #updated dataset has 5959 unit id.

# #units data check yearly 
# 
# units.data <- disperseR::units %>% filter (year >=1997 & year <=2018)
# 
# length(unique(units.data$ID))
# 
# coal.fuels <- c ("Coal", "Coal, Pipeline Natural Gas", "Coal, Natural Gas", "Coal, Other Gas",
#                  "Coal, Coal Refuse", "Coal Refuse", "Coal, Wood" )
# 
# ampd_coal <- PP.units.monthly1997_2021 %>% filter ( Fuel.Type..Primary. %in% coal.fuels & Year >=1997 & Year <= 2018)
# ampd_coal[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
# 
# 
# length(unique(ampd_coal$ID)) 
# 
# x <- unique(units.data$ID)
# y <- unique(ampd_coal$ID)
# 
# tail(x %in% y, 381)
# 
# x[1262]
# 
# #55245-1 is other gas in my dataset, in harvard dataset it is as coal.
# ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
# ampd_harvard<- as.data.table (ampd_harvard)
# ampd_harvard <- ampd_harvard [ , V1 := NULL]
# ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
# ampd_harvard <- ampd_harvard[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
# 
# ampd_harvard_55245_1 <- ampd_harvard %>%  filter (Facility.ID..ORISPL.==55245 & Unit.ID=="1")
# unique(ampd_harvard_55245_1$Fuel.Type..Primary..x)
# 
# PP.units.monthly1997_2021_55245 <- PP.units.monthly1997_2021 %>% filter (Facility.ID..ORISPL.==55245)
# 
# unique(PP.units.monthly1997_2021$Fuel.Type..Primary.[PP.units.monthly1997_2021$Facility.ID..ORISPL.==55245
#                                                      &PP.units.monthly1997_2021$Unit.ID=="1"])




sum(PP.units.monthly1995_2017$NOx.tons, na.rm=T)

sum(PP.units.monthly1997_2021$NOx.tons, na.rm=T)


#monthly plot of emissions data from disperseR vs. updated data (for all fuel types)
monthly.disperseR_ym <- aggregate(list (NOx..tons.=PP.units.monthly1995_2017$NOx.tons, 
                                        SO2..tons.=PP.units.monthly1995_2017$SO2.tons
), by=list( year=PP.units.monthly1995_2017$year,
            month=PP.units.monthly1995_2017$month), FUN=sum, na.rm=T)

monthly.disperseR_ym$group <- "emis_monthly_disperseR"

monthly.updated_ym <- aggregate(list (NOx..tons.=PP.units.monthly1997_2021$NOx.tons, 
                                      SO2..tons.=PP.units.monthly1997_2021$SO2.tons
), by=list( year=PP.units.monthly1997_2021$year,
            month=PP.units.monthly1997_2021$month), FUN=sum, na.rm=T)

monthly.updated_ym$group <- "emis_monthly_updated"


emission_monthly_combined <- do.call("rbind", list(monthly.disperseR_ym, monthly.updated_ym))


years <- c (2017)
emission_monthly_combined%>% filter (year %in% years) %>% 
  ggplot(aes(month, NOx..tons., group= group, color= group)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month",  y = "NOx (tons)", title = "2017 monthly coal fired power plants emission comparison") 


emission_monthly_combined%>% filter (year %in% years) %>% 
  ggplot(aes(month, SO2..tons., group= group, color= group)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month",  y = "SO2 (tons)", title = "2017 monthly coal fired power plants emission comparison") 



