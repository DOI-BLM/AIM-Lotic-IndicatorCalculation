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

#Load the following packages to read in SDE database
#Load sf to read tables in file geodatabase
library('sf')
library('arcgisbinding')
arc.check_product()
library('arcgis')

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
ecoregion<-'LU_Ecoregion'
points<-'D_Point'
sampledReaches <- 'F_SampledReaches_W_B'
nonSampledReaches<- 'F_NotSampledReaches_W_B' 
channelDimensions_W <- 'F_ChannelDimensions_W'
channelDimensions_B <- 'F_ChannelDimensions_B'
canopyCover_W <- 'F_CanopyCover_W'
canopyCover_B <- 'F_CanopyCover_B'
vegComplexity <- 'F_VegComplexity_W_B'
vegSpeciesWB <- 'F_VegSpecies_W_B'
speciesMetadata <- 'LU_SpeciesMetadata'
stateSpeciesList<- 'LU_StateSpeciesList'
WQMacroInvert <- 'F_WaterQualityMacroInvert_W_B'
slope_W <- 'F_Slope_W'
pools_w <- 'F_Pools_W'
largeWood_w_b <- 'F_LargeWood_W_B'
streambedParticles_w <- 'F_StreambedParticles_W'
PoolTailFines_W <- 'F_PoolTailFines_W'
bank_w <- 'F_Bank_W'
bank_b <- 'F_Bank_B'
FishCover_W_B <- 'F_FishCover_W_B'
FloodProneWidth <- 'F_FloodproneWidth_W_B'
SlopePoolSummary <- 'F_SlopePoolSummary_W'
HumanInfluence <- 'F_HumanInfluence_W_B'
ThalwegW <- 'F_Thalweg_W'
ThalwegSBP <- 'F_Thalweg_StreambedParticles_B'
indicators <- 'I_Indicators'
photos<-"F_Photos_W_B"

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

