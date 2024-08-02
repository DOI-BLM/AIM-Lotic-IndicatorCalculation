#############################################################################

######                 Specify the database source                     ######   

#############################################################################

# Set working directory
setwd(".../folder")
# Set path for output files
OutPath<-"...foider/"

# DEFINESOURCE DATABASE - for dbType specify either 'FGDB' or 'SDE' or 'RS' for file geodatabase, spatial database engine or rest service respectively
# Select one, comment out other sources
#dbType <- 'FGDB'
#dbType <- 'SDE'
 dbType <-'RS' #Rest Service


fgdbPath<-'.../LoticAIMdata.gdb'

# URL path to rest service
RSPath<-"https://......FeatureServer"


# Set up SQL Server connection using ODBC Data Source administrator 23 bit on your PC
# type in the database name, description (same as name) and server
# source database connection string if SDE is being used.  Otherwise specify an empty string ''
dbPrefixString <- 'ilmocAIMLoticPub.ILMOCAIMPUBDBO' #'ilmocAIMPubDBO'

# # connect to the database if it's SDE
 if (dbType == 'SDE') {
   sourceDB = RODBC::odbcConnect(dsn = "", rows_at_time = 1)
 } else {
   sourceDB = NULL
 }


#############################################################################

######                 Specify additional data sources                     ######   

#############################################################################

#Source Admin Units database for Reach Info and Sinuosity script. Use static copy not official database.
AdminUnits<-sf::st_read("...adminunits.gdb")
#project the layer to USGS equal area conic (points and admin unit boundaries must be in same projection for join below to work)
AdminUnits<-sf::st_transform(AdminUnits,crs=5070)

# ReachInfoSpatial
# Used in Reach Info and Sinuosity
# read in F_sampled reaches separately because ReadTable function strips off the spatial data
if(dbType=="FGDB"){
  ReachInfoSpatial<-sf::st_read(fgdbPath,layer=sampledReaches)
}else{
  ReachInfoSpatial<-arc_select(get_layer(arc_open(RSPath),id=2,))
}  
# project the layer to USGS equal area conic
ReachInfoSpatial<-sf::st_transform(ReachInfoSpatial,crs=5070)



# Ecoregion
# Used in final part of Step 2 - to attribute ecoregional groups to each reach
if(dbType=="FGDB"){
  EcoregionSpatial<-sf::st_read(fgdbPath,layer=ecoregion)
}else{
  EcoregionSpatial<-arc_select(get_layer(arc_open(RSPath),id=4,))
}
EcoregionSpatial<-sf::st_transform(EcoregionSpatial,crs=5070)

# All Indicators
# Used to compare indicators across years. Only necessary if computing historic indicators
allindicators<-arc_select(get_layer(arc_open(RSPath), id=0))




#############################################################################

######          Specify the year, point, protocol and project           ######   

#############################################################################

# #c() indicates all data will be included for a given filter 
years <- c()
pointIDs <- c()
protocolTypes <- c()
projects <- c()


# years <- c()
# pointIDs <- c()
# protocolTypes <- c()
# projects <- c()

# Example
# years <- c('2019')
# pointIDs <- c('CN-SS-63770')
# protocolTypes <- c('Wadeable')
# projects <- c('ID_ChallisFO')