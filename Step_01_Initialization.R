###############################################################################

###### Load required packages - there will be an error if one is missing ######

###############################################################################
# Load in data manipulation packages
library('tidyr')
# used to pivot data into wider or longer formats (canopy cover and wood - since we had to compromise on back end table structure)
library('purrr')
# used to join data frames together any time there are more than 2
library('dplyr')
# used to summarize and select columns
# note that aggregate is still used in many places instead of summarize just because it was too time consuming to change all code.
# the advantage to summarize is that it can name output columns, aggregate multiple columns at once, and get samples sizes too in the same code line and data frame
# this prevents having to merge a bunch of data frames back together. however one needs to be carefull with how null data is treated and in some cases, data frames need to be seperated out to properly remove NULL data
library('plyr')
# ideally remove plyr package in the future because it conflicts with dplyr and dplyr is a newer version.  
# however, there is no good replacement for the mapvalues function in dplyr so I left plyr in only for that one function. 
# map values function is used throughout most scripts and is used to convert side channel transect letters to main transect letters
# for any new code writing you MUST preface all functions with dplyr:: or plyr:: so R does not get confused. 
# in particular "count" and "summarize" functions are found in both packages and throughout all scripts.

#Load the following packages to read in Rest Service database
#Load sf to read tables in file geodatabase
library('sf')
library('arcgisbinding')
arc.check_product()
library('arcgis')
# token<-auth_binding()
# set_arc_token()
##############################################################################

######        Specify the location of all the subsequent R scripts      ######

##############################################################################

#load the database source and filters
source('config.R', echo = TRUE)
# load the function that reads in all data
if(dbType=="SDE"|dbType=="FGDB"){
  source('FNC_ReadTable_RODBC.R')
}else{
  source('FNC_ReadTable_RestService.R')
}



##################################################################################################################

######   Table names can be edited below if schema changes are made or edit vs. pub locations are changed   ######   

##################################################################################################################
# Specify table names so that table names can change as needed and only this script will need changed if tables names change
# field names however, appear throughout scripts
ecoregion<-'AIM_Lotic__LU_Ecoregion'
points<-'AIM_Lotic__D_Point'
sampledReaches <- 'AIM_Lotic__F_SampledReaches_W_B'
nonSampledReaches<- 'AIM_Lotic__F_NotSampledReaches_W_B' 
channelDimensions_W <- 'AIM_Lotic__F_ChannelDimensions_W'
channelDimensions_B <- 'AIM_Lotic__F_ChannelDimensions_B'
canopyCover_W <- 'AIM_Lotic__F_CanopyCover_W'
canopyCover_B <- 'AIM_Lotic__F_CanopyCover_B'
vegComplexity <- 'AIM_Lotic__F_VegComplexity_W_B'
vegSpeciesWB <- 'AIM_Lotic__F_VegSpecies_W_B'
speciesMetadata <- 'AIM_Lotic__LU_SpeciesMetadata'
stateSpeciesList<- 'AIM_Lotic__LU_StateSpeciesList'
WQMacroInvert <- 'AIM_Lotic__F_WaterQualityMacroInvert_W_B'
slope_W <- 'AIM_Lotic__F_Slope_W'
pools_w <- 'AIM_Lotic__F_Pools_W'
largeWood_w_b <- 'AIM_Lotic__F_LargeWood_W_B'
streambedParticles_w <- 'AIM_Lotic__F_StreambedParticles_W'
PoolTailFines_W <- 'AIM_Lotic__F_PoolTailFines_W'
bank_w <- 'AIM_Lotic__F_Bank_W'
bank_b <- 'AIM_Lotic__F_Bank_B'
FishCover_W_B <- 'AIM_Lotic__F_FishCover_W_B'
FloodProneWidth <- 'AIM_Lotic__F_FloodproneWidth_W_B'
SlopePoolSummary <- 'AIM_Lotic__F_SlopePoolSummary_W'
HumanInfluence <- 'AIM_Lotic__F_HumanInfluence_W_B'
ThalwegW <- 'AIM_Lotic__F_Thalweg_W'
ThalwegSBP <- 'AIM_Lotic__F_Thalweg_StreambedParticles_B'
indicators <- 'AIM_Lotic__I_Indicators'
photos<-"AIM_Lotic__F_Photos_W_B"

# Specify table names so that table names can change as needed and only this script will need changed if tables names change
# field names however, appear throughout scripts
# if using rest Service, use the id
if (dbType == 'RS'){
  ecoregion<-4
  #points<-'D_Point'
  sampledReaches <- 2
  nonSampledReaches<- 3 
  channelDimensions_W <- 10
  channelDimensions_B <- 9
  canopyCover_W <- 8
  canopyCover_B <- 7
  vegComplexity <- 24
  vegSpeciesWB <- 25
  speciesMetadata <- 27
  stateSpeciesList<- 28
  WQMacroInvert <- 26
  slope_W <- 19
  pools_w <- 17
  largeWood_w_b <- 15
  streambedParticles_w <- 21
  PoolTailFines_W <- 18
  bank_w <- 6
  bank_b <- 5
  FishCover_W_B <- 12
  FloodProneWidth <- 13
  SlopePoolSummary <- 20
  HumanInfluence <- 14
  ThalwegW <- 23
  ThalwegSBP <- 22
  indicators <- 0
  #photos<-"F_Photos_W_B"
}




# if SDE is in use, use the prefix string on all the table names
if (dbType == 'SDE'){
  sampledReaches <- paste(dbPrefixString, '.', sampledReaches,"_evw", sep='')
  nonSampledReaches<- paste(dbPrefixString, '.', nonSampledReaches,"_evw", sep='')
  channelDimensions_W <- paste(dbPrefixString, '.', channelDimensions_W,"_evw", sep='')
  channelDimensions_B <- paste(dbPrefixString, '.', channelDimensions_B,"_evw", sep='')
  canopyCover_W <- paste(dbPrefixString, '.', canopyCover_W,"_evw", sep='')
  canopyCover_B <- paste(dbPrefixString, '.', canopyCover_B,"_evw", sep='')
  points <- paste(dbPrefixString, '.', points,"_evw", sep='')
  vegComplexity <- paste(dbPrefixString, '.', vegComplexity,"_evw", sep='')
  vegSpeciesWB <- paste(dbPrefixString, '.', vegSpeciesWB,"_evw", sep='')
  stateSpeciesList<- paste(dbPrefixString, '.', stateSpeciesList,"_evw", sep='')
  speciesMetadata<- paste(dbPrefixString, '.', speciesMetadata,"_evw", sep='')
  WQMacroInvert <- paste(dbPrefixString, '.', WQMacroInvert,"_evw", sep='')
  slope_W <- paste(dbPrefixString, '.', slope_W,"_evw", sep='')
  pools_w <- paste(dbPrefixString, '.', pools_w,"_evw", sep='')
  largeWood_w_b <- paste(dbPrefixString, '.', largeWood_w_b,"_evw", sep='')
  streambedParticles_w <- paste(dbPrefixString, '.', streambedParticles_w,"_evw", sep='')
  PoolTailFines_W <- paste(dbPrefixString, '.', PoolTailFines_W,"_evw", sep='')
  bank_w <- paste(dbPrefixString, '.', bank_w,"_evw", sep='')
  bank_b <- paste(dbPrefixString, '.', bank_b,"_evw", sep='')
  FishCover_W_B <- paste(dbPrefixString, '.', FishCover_W_B,"_evw", sep='')
  FloodProneWidth <- paste(dbPrefixString, '.', FloodProneWidth,"_evw", sep='')
  SlopePoolSummary <- paste(dbPrefixString, '.', SlopePoolSummary,"_evw", sep='')
  HumanInfluence <- paste(dbPrefixString, '.', HumanInfluence,"_evw", sep='')
  ThalwegW <- paste(dbPrefixString, '.', ThalwegW,"_evw", sep='')
  ThalwegSBP <- paste(dbPrefixString, '.', ThalwegSBP,"_evw", sep='')
  indicators <- paste(dbPrefixString, '.', indicators,"_evw", sep='')
  photos <- paste(dbPrefixString, '.', photos,"_evw", sep='')
}



#############################################################################

######                 Get Desired EvaluationIDs                       ######   

#############################################################################

#Get Unique EvaluationIDs from sampled Reaches feature class to query rest of data from other tables by
DF_ReachInfo <- ReadTable(TableName=sampledReaches, PointIDs=pointIDs, Years=years, Projects=projects, ProtocolTypes=protocolTypes)
# get a list of unique Evaluation IDs from ReachInfoDF
uniqueEvalIDs = as.list(unique(DF_ReachInfo['EvaluationID']))[[1]]


#############################################################################

######                 Specify additional data sources                     ######   

#############################################################################


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
#allindicators<-arc.select(arc.open("\\\\blm\\dfs\\loc\\GBP\\OC\\projects\\AIM\\DataAndTools\\SDE\\oc_pub_osa.sde\\ILMOCDO.AIM_Lotic__I_Indicators_evw"))




