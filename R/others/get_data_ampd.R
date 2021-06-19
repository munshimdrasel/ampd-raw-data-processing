##----- Data sources
# All 2010-2015
#   http://ampd.epa.gov/ampd/#?bookmark=12264
# All 2000-2009
#   http://ampd.epa.gov/ampd/#?bookmark=12265  
# All 1980-1999
#  http://ampd.epa.gov/ampd/#?bookmark=12266

##----- Load R packages

library(data.table)
library(maps)
library(stringr)

##----- Read and aggregate AMPD data

variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Associated Stacks",
  "Month",
  "Year",
  "Program(s)",
  "SO2 (tons)",
  "Avg. NOx Rate (lb/MMBtu)",
  "NOx (tons)",
  "CO2 (short tons)",
  "Heat Input (MMBtu)",
  "Operating Time",
  "Gross Load (MW-h)",
  "Steam Load (1000lb)",
  "EPA Region",
  "NERC Region",
  "County",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
  "SO2 Phase",
  "NOx Phase",
  "Operating Status",
  "Unit Type",
  "Fuel Type (Primary)",
  "Fuel Type (Secondary)",
  "SO2 Control(s)",
  "NOx Control(s)",
  "PM Control(s)",
  "Hg Control(s)",
  "NA"
))

# Files obtained from: http://ampd.epa.gov/ampd/
#
#   2010-2018 ## LRFH added 2019-01-10
# https://ampd.epa.gov/ampd/?bookmark=12264
# 
#   2010-2015
# http://ampd.epa.gov/ampd/#?bookmark=12264
#   
#   2000-2009
# http://ampd.epa.gov/ampd/#?bookmark=12265
#   
#   1980-1999
# http://ampd.epa.gov/ampd/#?bookmark=12266
#
# as emission_2010-2015.csv, emission_2000-2009.csv, emission_1980-1999.csv
#
# CSV files are opened in MS Excel and saved as CSV with "_2" postfix to
# sort a duplicate rowname issue.
#
# LRFH added emission_2010-2015_3.csv with complete 2015 data 2019-01-10
# LRFH added emission_2016-2018_3.csv with complete 2018 data 2019-07-13

setwd('~/Dropbox/Harvard/ARP/Data_AMPD_EIA')
e1 <- fread("emission_1980-1999_2.csv")
setnames(e1, make.names(names(e1)))
# setnames(e1, c("State", substring(names(e1), 3)[-1]))
e2 <- fread("emission_2000-2009_2.csv")
setnames(e2, make.names(names(e2)))
# setnames(e2, c("State", substring(names(e2), 3)[-1]))
e3 <- fread("emission_2010-2015_3.csv")
setnames(e3, make.names(names(e3)))
# setnames(e3, c("State", substring(names(e3), 3)[-1]))
e4 <- fread("emission_2016-2018_3.csv")
setnames(e4, make.names(names(e4)))
# setnames(e4, c("State", substring(names(e4), 3)[-1]))

e1 <- e1[, by = variables]
e2 <- e2[, by = variables]
e3 <- e3[, by = variables]
e4 <- e4[, by = variables]

emissions <- rbind(e1, e2, e3, e4)

emissions[, Owner := NULL]
emissions[, Operator := NULL]
emissions[, Representative..Primary. := NULL]
emissions[, Representative..Secondary. := NULL]

write.csv(emissions, "emissions_all.csv")

##----- Scrubber

DTU <- copy(emissions)

# Parse scrubber information
source("scrubber_parser.R")

# Parse operating status
# source("parse_operating_status.R")

##----- Add FIPS

data(county.fips)
DTU[, StateCounty:= tolower(paste(state.name[match(State, state.abb)], County, sep=","))]
DTU[, FIPS:= county.fips[match(StateCounty, county.fips$polyname), ]$fips]
DTU[, StateCounty := NULL]

##----- Coal-burning units

DTU[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
DTU[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

DTU[, Fuel2.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Secondary.))]
DTU[Fuel.Type..Primary. == "", Fuel2.IsCoal := NA]

DTUS <- DTU[, list(Facility.Name, Facility.ID..ORISPL., Unit.ID,
                   Year, Month, Program.s.,
                   State, County, FIPS,
                   Facility.Latitude, Facility.Longitude,
                   SO2..tons., NOx..tons., Avg..NOx.Rate..lb.MMBtu., CO2..short.tons.,
                   Heat.Input..MMBtu., Gross.Load..MW.h., Steam.Load..1000lb.,
                   Operating.Time,
                   Operating.Status,
                   Source.Category,
                   Fuel.Type..Primary., Fuel1.IsCoal,
                   Fuel.Type..Secondary., Fuel2.IsCoal,
                   SO2.Phase, NOx.Phase,
                   # SO2.Scrub1, SO2.Scrub2, SO2.Scrub3, SO2.Scrub4,
                   SO2.Control.1, SO2.Control.2, SO2.Control.3, 
                   # NOx.Scrub1, NOx.Scrub2, NOx.Scrub3, NOx.Scrub4,
                   NOx.Control.1, NOx.Control.2, NOx.Control.3, NOx.Control.4, NOx.Control.5, 
                   # PM.Scrub1, PM.Scrub2, PM.Scrub3, PM.Scrub4)]
                   PM.Control.1, PM.Control.2, PM.Control.3, PM.Control.4)]
                   
##----- Scrubber Booleans

DTUS[, Has.SO2.Scrub := as.numeric(SO2.Control.1 != "" | SO2.Control.2 != "" | SO2.Control.3 != ""),
     list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Has.PM.Scrub := as.numeric(PM.Control.1 != "" | PM.Control.2 != "" | PM.Control.3 != "" | PM.Control.4 != ""),
     list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Has.NOx.Scrub := as.numeric(NOx.Control.1 != "" | NOx.Control.2 != "" | NOx.Control.3 != "" | NOx.Control.4 != "" | NOx.Control.4 != ""),
     list(Facility.ID..ORISPL., Year, Month)]

##----- Operating Boolean
DTUS[, Is.Retired:= as.numeric(Operating.Status == "Retired"), list(Facility.ID..ORISPL., Year, Month)]

##----- SO2 program phase
DTUS[, Is.Phase1:= as.numeric(SO2.Phase == "Table 1"), list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Is.Phase2:= as.numeric(SO2.Phase == "Phase 2"), list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Is.OptIn:= as.numeric(SO2.Phase == "Opt-In"), list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Is.Substitution:= as.numeric(SO2.Phase == "Substitution"), list(Facility.ID..ORISPL., Year, Month)]
DTUS[, Is.Compensating:= as.numeric(SO2.Phase == "Compensating"), list(Facility.ID..ORISPL., Year, Month)]

##----- Write unit-level AMPD dataset

write.csv(DTUS, "AMPD_Unit.csv")

##----- Merge with EIA data (from forms EIA-767 and EIA-923, see file "eia.R")

DTUS <- fread("AMPD_Unit.csv")[, V1 := NULL]

# Sulfulr content
D_EAI <- fread("D_EIA.csv")[, V1 := NULL]
D_EAI[, Facility.ID..ORISPL. := as.character(Facility.ID..ORISPL.)]
D_EAI[, Year := as.character(Year)]
D_EAI[, Month := as.character(Month)]

# Initial year of operation
D_EGC <- fread("D_EGC.csv")[, V1 := NULL]
D_EGC[, Facility.ID..ORISPL. := as.character(Facility.ID..ORISPL.)]

setkeyv(D_EAI, c("Facility.ID..ORISPL.", "Unit.ID"))
setkeyv(D_EGC, c("Facility.ID..ORISPL.", "Unit.ID"))

D_EAI <- D_EGC[D_EAI, allow.cartesian = TRUE]

DTUS[, Facility.ID..ORISPL. := as.character(Facility.ID..ORISPL.)]
DTUS[, Year := as.character(Year)]
DTUS[, Month := as.character(Month)]

setkeyv(D_EAI, c("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month"))
setkeyv(DTUS, c("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month"))

DD <- D_EAI[DTUS, allow.cartesian = TRUE]

##----- Write unit-level AMPD dataset with EIA sulfur content

write.csv(DD, "AMPD_Unit_with_Sulfur_Content.csv")

# Regulations PM, SO2, NOx

D_EIA_767_Regulations <- fread("D_EIA_767_Regulations.csv")[, V1 := NULL]
D_EIA_767_Regulations[, Facility.ID..ORISPL. := as.character(Facility.ID..ORISPL.)]
D_EIA_767_Regulations[, Year := as.character(Year)]

setkeyv(DD, c("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month"))
setkeyv(DD, c("Facility.ID..ORISPL.", "Unit.ID", "Year"))
setkeyv(D_EIA_767_Regulations, c("Facility.ID..ORISPL.", "Unit.ID", "Year"))

DDD <- D_EIA_767_Regulations[DD, allow.cartesian = TRUE]

##----- Write unit-level AMPD dataset with EIA sulfur content and regulations

write.csv(DDD, "AMPD_Unit_with_Sulfur_Content_and_Regulations.csv")

##----- Aggregate at facility level

DTC <- copy(DTUS)
setkey(DTC, Facility.ID..ORISPL., Year, Month)

byvar <- c("Facility.Name", "Facility.ID..ORISPL.",
           "Year", "Month",
           "Facility.Latitude", "Facility.Longitude",
           "State", "County", "FIPS", "Source.Category")

DTF <- DTC[, list(Number.Units. = length(unique(Unit.ID)),
                  # Emissions
                  Total.Heat.Input..MMBtu. = sum(as.numeric(Heat.Input..MMBtu., na.rm=TRUE)),
                  Total.SO2..tons. = sum(as.numeric(SO2..tons., na.rm=TRUE)),
                  Total.NA.SO2..tons. = sum(is.na(SO2..tons.)),
                  Total.NOx..tons. = sum(as.numeric(NOx..tons., na.rm=TRUE)),
                  Total.NA.NOx..tons. = sum(is.na(NOx..tons.)),
                  Total.CO2..tons. = sum(as.numeric(CO2..short.tons., na.rm=TRUE)),
                  Total.NA.CO2..tons. = sum(is.na(CO2..short.tons.)),
                  Total.NA.Heat.Input..MMBtu. = sum(is.na(Heat.Input..MMBtu.)),
                  # Operating time and status
                  Total.Operating.Time = sum(as.numeric(Operating.Time, na.rm=TRUE)),
                  Total.NA.Operating.Time = sum(is.na(Operating.Time)),
                  Total.Is.Retired = sum(Is.Retired),
                  # Coal
                  Total.Fuel1.IsCoal = sum(Fuel1.IsCoal, na.rm=TRUE),
                  Total.Fuel2.IsCoal = sum(Fuel2.IsCoal, na.rm=TRUE),
                  # SO2 program phase
                  Total.SO2.Phase1 = sum(Is.Phase1),
                  Total.SO2.Phase2 = sum(Is.Phase2),
                  Total.SO2.OptIn = sum(Is.OptIn),
                  Total.SO2.Substitution = sum(Is.Substitution),
                  Total.SO2.Compensating = sum(Is.Compensating),
                  # Scrubbers
                  Total.SO2.Scrub = sum(Has.SO2.Scrub),
                  Total.PM.Scrub = sum(Has.PM.Scrub),
                  Total.NOx.Scrub = sum(Has.NOx.Scrub)),
           by = byvar]

##----- Write facility-level AMPD dataset

write.csv(DTF, "AMPD_Facility.csv")

##----- Merge with facility attribute information

F1 <- data.table(read.csv("facility_1980-1999_2.csv"))
F2 <- data.table(read.csv("facility_2000-2009_2.csv"))
F3 <- data.table(read.csv("facility_2010-2015_2.csv"))
F4 <- data.table(read.csv("facility_2016-2018_3.csv"))

F_all <- unique(rbind(F1, F2, F3, F4))
F_all[, Facility.ID..ORISPL. := as.numeric(Facility.ID..ORISPL.)]

F_all_dups <- F_all[,.(Facility.ID..ORISPL., Unit.ID, Year)]
F_all <- F_all[!duplicated(F_all_dups),]

# Unit level

AMPD_Unit <- fread("AMPD_Unit.csv")
AMPD_Unit_with_Sulfur_Content <- fread("AMPD_Unit_with_Sulfur_Content.csv")[, V1 := NULL]
AMPD_Unit_with_Sulfur_Content[, Facility.ID..ORISPL. := as.numeric(Facility.ID..ORISPL.)]
AMPD_Unit_with_Sulfur_Content[, Year := as.numeric(Year)]
AMPD_Unit_with_Sulfur_Content_and_Regulations <- fread("AMPD_Unit_with_Sulfur_Content_and_Regulations.csv")[, V1 := NULL]
AMPD_Unit_with_Sulfur_Content_and_Regulations[, Facility.ID..ORISPL. := as.numeric(Facility.ID..ORISPL.)]
AMPD_Unit_with_Sulfur_Content_and_Regulations[, Year := as.numeric(Year)]

AMPD_Unit_with_Facility_Attributes <- merge(AMPD_Unit, F_all, by = c("Facility.ID..ORISPL.", "Unit.ID", "Year"))[, V1 := NULL]
AMPD_Unit_with_Sulfur_Content_with_Facility_Attributes <- merge(AMPD_Unit_with_Sulfur_Content, F_all, by = c("Facility.ID..ORISPL.", "Unit.ID", "Year"))
AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes <- merge(AMPD_Unit_with_Sulfur_Content_and_Regulations, F_all, by = c("Facility.ID..ORISPL.", "Unit.ID", "Year"))

write.csv(AMPD_Unit_with_Facility_Attributes,
          "AMPD_Unit_with_Facility_Attributes.csv")
write.csv(AMPD_Unit_with_Sulfur_Content_with_Facility_Attributes,
          "AMPD_Unit_with_Sulfur_Content_with_Facility_Attributes.csv")
write.csv(AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes,
          "AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
