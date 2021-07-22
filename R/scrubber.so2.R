#this script plots SO2 scrubber using facility over time

library( data.table)
library( fst)
library( parallel)
library( disperseR)
library( cowplot)
library( censusapi)
library( USAboundaries)
library( ggplot2)
library( viridis)
library( scales)
library( raster)
library( sf)
library( fasterize)

setwd ("/Users/munshirasel/Google_Drive/R/ampd-raw-data-processing")

id.control.install <- function( uiD.name,
                                dt.uid = copy( pp.monthly), 
                                Has.colname = 'Fuel.Type..Primary.'){
  print( uiD.name)
  
  dt.use <- copy( dt.uid[ uID == uiD.name])
  dt.use[, yearmo := paste( Year, formatC( Month, flag = '0', width = 2), sep = '_')]
  setnames( dt.use, Has.colname, "Has.colname")
  
  # fill months preceding start of data
  year_mos_all <- data.table( expand.grid( Year = unique( dt.uid$Year), Month = 1:12))
  year_mos_all[, yearmo := paste( Year, formatC( Month, flag = '0', width = 2), sep = '_')]
  dt.use.m <- data.table( merge( year_mos_all, dt.use, by = c( 'Year', 'Month', 'yearmo'), all = T))
  yearmo.min <- min( dt.use$yearmo)
  yearmo.max <- max( dt.use$yearmo)
  
  if( Has.colname == 'Operating.Status'){
    # check if more info on when operations started
    if( TRUE %in% grepl( 'Operating.*Started', dt.use.m$Has.colname)){
      dt.use.edit <- dt.use.m[grepl( '\\(', dt.use.m$Has.colname)]
      date.start <- unique( gsub( '^.* |\\)', '', dt.use.edit$Has.colname))
      date.start <- as.Date( date.start, format = '%m/%d/%Y')
      yearmon.start <- paste( year( date.start), 
                              formatC( month( date.start), width = 2, flag = '0'), sep = '_')
      yearmo.min <- yearmon.start
    }
    
    # check if more info on when operations stopped
    if( TRUE %in% grepl( '\\(Retired', dt.use.m$Has.colname)){
      dt.use.edit <- dt.use.m[grepl( '\\(', dt.use.m$Has.colname)]
      date.start <- unique( gsub( '^.* |\\)', '', dt.use.edit$Has.colname))
      date.start <- as.Date( date.start, format = '%m/%d/%Y')
      yearmon.start <- paste( year( date.start), 
                              formatC( month( date.start), width = 2, flag = '0'), sep = '_')
      yearmo.max <- yearmon.start
    }
    dt.use.m[ , Has.colname := as( Has.colname, 'character')]
    dt.use.m[ yearmo < yearmo.min, Has.colname := 'Unopened']
    dt.use.m[ yearmo > yearmo.max, Has.colname := 'Retired']
    dt.use.m[ is.na( Has.colname), Has.colname := '0']
    dt.use.m[ Has.colname != 'Operating', Has.colname := '0']
    dt.use.m[ Has.colname == 'Operating', Has.colname := '1']
    dt.use.m[, Has.colname := as( Has.colname, 'numeric')]
  }
  
  # if( Has.colname == 'Is.Retired')
  #   dt.use.m[ yearmo < yearmo.min, Has.colname := 'Retired']
  
  # clean up a bit
  name.months.inop <- paste0( Has.colname, '_months.inop')
  dt.slim <- dt.use.m[ ,uID := uiD.name][, .( months.inop = sum( Has.colname)), 
                                         by = .( uID, Year)]
  dt.slim[, Has.colname := ifelse( months.inop > 0, 1, 0)]
  setnames( dt.slim, c( 'months.inop', 'Has.colname'), 
            c( name.months.inop, Has.colname))
  
  return( dt.slim)
}


## -------------------------------------------------------------------- ##
##               Read data, format control starts                       ##
## -------------------------------------------------------------------- ##
# read in pp data, define uID, order by unit, month, and year
# pp.monthly <- fread( "/Users/munshirasel/Downloads/AMPD_Unit.csv")
pp.monthly <- fread( "data/AMPD_Unit.csv")

setnames( pp.monthly, 'Facility.ID..ORISPL.', 'FacID')
pp.monthly[, uID := gsub('_|-|\\*', '.', paste(FacID, Unit.ID, sep = '.'))]

# do Fuel1.IsGas
pp.monthly <- pp.monthly[ Fuel1.IsCoal == 1]

# pick the units
units.use <- unique( pp.monthly$uID)

## -------------------------------------------------------------------- ##
##               ID years in op, controls, etc                          ##
## -------------------------------------------------------------------- ##
# id operating status for all years
op_status <- rbindlist( mclapply( units.use, id.control.install, copy( pp.monthly),
                                  Has.colname = 'Operating.Status'))

## identify changes so2, NOx, pm scrubber
so2_ctr1 <- rbindlist( mclapply( units.use, id.control.install, copy( pp.monthly),
                                 Has.colname = 'Has.SO2.Scrub'))

## -------------------------------------------------------------------- ##
##               back out facilities from  units                        ##
## -------------------------------------------------------------------- ##
# count number of units in each facility by year
units.fac <- unique( pp.monthly[, .( uID, FacID, Year,
                                     Facility.Name, Facility.Latitude, Facility.Longitude, State)])
units.fac.counts <- units.fac[, .( count = .N), by = .( FacID, Year,
                                                        Facility.Name, Facility.Latitude, 
                                                        Facility.Longitude, State)]

# sum so2 emissions, load & heat input
fac_so2 <- pp.monthly[, .( so2 = sum( SO2..tons., na.rm = T),
                           heat = sum( Heat.Input..MMBtu., na.rm = T),
                           load = sum( Gross.Load..MW.h., na.rm = T)),
                      by = .( FacID, Year)]

# count units with scrubbers
so2_ctr1_fac <- merge( so2_ctr1, units.fac, by = c( 'uID', 'Year'))
so2_ctr1.counts <- so2_ctr1_fac[, .( scrubbbers = sum( Has.SO2.Scrub, na.rm = T)), 
                                by = .( FacID, Year)]

# count units in operation
op_status_fac <- merge( op_status, units.fac, by = c( 'uID', 'Year'))
op_status.counts <- op_status_fac[, .( operation = sum( Operating.Status, na.rm = T)), 
                                  by = .( FacID, Year)]

## -------------------------------------------------------------------- ##
##               put the datasets together                              ##
## -------------------------------------------------------------------- ##
fac_data1 <- merge( units.fac.counts, so2_ctr1.counts,
                    by = c( 'FacID', 'Year'))
fac_data2 <- merge( fac_data1, op_status.counts,
                    by = c( 'FacID', 'Year'))
fac_data <- merge( fac_data2, fac_so2,
                   by = c( 'FacID', 'Year'))
setnames( fac_data, "Year", "year")

plot( fac_data)

write.fst( fac_data,
           'data/facility_operating_scrubbbers.fst')




## ================================================= ##
#  look at fraction of load with so2 controls & total 
## ================================================= ##
fac_data <- read.fst( 'data/facility_operating_scrubbbers.fst', as.data.table = TRUE)

# facilities can only have as many scrubbers as units in operations
fac_data[ scrubbbers > operation, scrubbbers := operation]
fac_data[, sum( operation), by = year]
fac_data[, sum( scrubbbers), by = year]

# define all facilities at all years
fac_data_ex <- expand.grid( FacID = unique( fac_data$FacID),
                            year = unique( fac_data$year)) %>%
  as.data.table

fac_data_full <- merge( fac_data_ex, fac_data, all.x = TRUE, by = c( 'FacID', 'year'))
fac_data_full[ is.na( count), count := 0]
fac_data_full[ is.na( scrubbbers), scrubbbers := 0]
fac_data_full[ is.na( operation), operation := 0]
fac_data_full[ is.na( so2), so2 := 0]
fac_data_full[ is.na( heat), heat := 0]
fac_data_full[ is.na( load), load := 0]

# get the maximum numbers of units in operation at a given time
fac_data_full[, max_operation := 0]
for( y in min( fac_data$year):max( fac_data$year)){
  fac_data_full[ year <= y, max_y := max( operation), by = FacID]
  fac_data_full[ year == y, max_operation := max( operation, max_y), by = FacID]
}

# calculate closed
fac_data_full[, `:=` ( closed = max_operation - operation,
                       noscrub = operation - scrubbbers)]
plot( fac_data_full$year, fac_data_full$closed)

# mean number of units closed per year after 2010
mean( diff( fac_data_full[ year > 2008, sum( closed), by = year]$V1))
fac_data_full[ , sum( scrubbbers), by = year]

#melt on counts
fac_data_units.m <- melt( fac_data_full, id.vars = c( 'FacID', 'year'),
                          measure.vars = c( 'closed', 'noscrub', 'scrubbbers'),
                          variable.factor = TRUE)
fac_data_units.m[, variable := factor( variable,
                                       levels = c( 'closed' , 'scrubbbers', 'noscrub' ))]

ggplot( fac_data_units.m[year >= 1997 & year <=2020],
        aes( x = year, y = value, fill = variable)) +
  geom_col() + 
  scale_fill_brewer( palette = "Set2",
                     name = 'Number of units that were...',
                     labels = c( 'retired' = 'closed',
                                 'scrubbbers' = 'operating with a scrubber',
                                 'noscrub' = 'operating without a scrubbers')) + 
  theme_minimal() + 
  theme( axis.text = element_text( size = 12),
         axis.title = element_blank(),
         legend.background = element_rect( color = NA),
         legend.position = c( .25, .25))


# fac_data_units.m[ variable == 'scrubbbers', sum( value), by = year]
