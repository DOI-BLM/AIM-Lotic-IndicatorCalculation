###################################################################################################################

##############                    Source Each Script Below to Compute Indicators                 #################

###################################################################################################################
# Note: When reading from FGDB it is normal to get Warning message: no simple feature geometries present: returning a data.frame or tbl_df
# warnings are fine but if an error occurrs running one of the scripts below, your will get a red error message in the console and the rest of that specific script will not run
# additionally errors should be obvious because the dataframe will be missing from the final list of objects in the environment and in the output files
source('Reach Info and Sinuosity.R', echo = TRUE)
source('Veg Complexity.R', echo = TRUE)
source('Canopy Cover.R', echo = TRUE)
source('Priority Noxious and Native.R', echo = TRUE)
source('Water Quality.R', echo = TRUE)

source('Pools.R', echo = TRUE)
source('Large Wood.R', echo = TRUE)
source('Streambed Particles.R', echo = TRUE)
source('Pool Tail Fines.R', echo = TRUE)

source('Bank Stability and Cover and Angle.R', echo = TRUE)
source('Channel Dimensions Thalweg and Slope.R', echo = TRUE)
source('Floodprone Width.R', echo = TRUE)

source('Human Influence.R', echo = TRUE)

###################################################################################################################

##############                         Combine All Indicators Into One File                 #################

###################################################################################################################
# ###combine all objects that exist in workspace that contain indicator values####
IndicatorCheckJoin=DF_ReachInfo
IndicatorList=c("DF_Sinuosity","DF_SideChannel",
                "DF_VegComplexity","DF_VegComplexityWoody","DF_VegComplexityUnderstoryGround","DF_VegComplexityBareGround",
                "DF_CanopyCover",
                "DF_Noxious_And_Native",
                "DF_WQ",
                "DF_Pools",
                "DF_LargeWood",
                "DF_StreambedParticles",
                "DF_PoolTailFines",
                "DF_BankStabilityCover","DF_BankAngle",
                "DF_Slope","DF_Heights","DF_BankfullWidth","DF_WettedWidth","DF_Depths","DF_FloodplainConnectivity","DF_Thalweg",
                "DF_Entrenchment",
                "DF_FishCover",
                "DF_HumanInfluence")
for (s in 1:length(IndicatorList)) {
  if(exists(IndicatorList[s])==TRUE){IndicatorCheckJoin=purrr::reduce(list(IndicatorCheckJoin,as.data.frame(get(IndicatorList[s]))),by="EvaluationID",left_join)}
  else {IndicatorCheckJoin=IndicatorCheckJoin}
}

#To remove all of the metrics and only get the indicators subset by EvaluationID and all those columns ending in "CHECK". Hmm..not really sure what the $ is doing here, the code works without it, but all the examples I've looked at keep the $ so I kept it too... 
IndicatorCheck=IndicatorCheckJoin[,c("EvaluationID",grep("CHECK", colnames(IndicatorCheckJoin),value=TRUE))]


# #write out all data frames for QC or to troubleshoot scripts
# for (s in 1:length(IndicatorList)){
#   if(exists(IndicatorList[s])==TRUE){write.csv(as.data.frame(get(IndicatorList[s])),paste0('/',Sys.Date(),IndicatorList[s],".csv"),row.names=FALSE)}
# }
  
#####################################################################################################

##############                    Default Benchmark Group                         ###############

#####################################################################################################
#create the EcoregionStreamSize  field (changed from DefaultBenchmarkGroup in 2023)

# create stream size class from avg bankfull width
IndicatorCheck$BNK_THRESH=ifelse(as.numeric(IndicatorCheck$BankfullWidthAvg_CHECK)>10,"LargeWadeable","SmallWadeable")

# attribute with ecoregion
# EcoregionSpatial and reachInfoSpatial called in in config script
PointsEcoregion<-sf::st_join(ReachInfoSpatial,EcoregionSpatial)
Ecoregion<-sf::st_drop_geometry(PointsEcoregion[,c("EvaluationID","Ecoregion")])
#join ecoregion to indicators
IndicatorCheck<- dplyr::left_join(IndicatorCheck,Ecoregion, by='EvaluationID')

#join ecoregions, size class, and protocol info
IndicatorCheck$BNK_THRESH=ifelse(IndicatorCheck$ProtocolType_CHECK=='Boatable',"Boatable",IndicatorCheck$BNK_THRESH)
IndicatorCheck$EcoregionStreamSize=paste(IndicatorCheck$Ecoregion,IndicatorCheck$BNK_THRESH, sep="_")
IndicatorCheck$EcoregionStreamSize=ifelse(IndicatorCheck$EcoregionStreamSize=="NorthernRockies_Boatable"|IndicatorCheck$EcoregionStreamSize=="SouthernRockies_Boatable"|IndicatorCheck$EcoregionStreamSize=="NorthernXericBasin_Boatable","NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable",
                                        ifelse(IndicatorCheck$EcoregionStreamSize=="EasternXericBasin_Boatable"|IndicatorCheck$EcoregionStreamSize=="SouthernXericBasin_Boatable", "EasternXericBasinSouthernXericBasin_Boatable", IndicatorCheck$EcoregionStreamSize))
IndicatorCheck$EcoregionStreamSize=ifelse(IndicatorCheck$EcoregionStreamSize=="XericCalifornia_SmallWadeable","PacificNorthwestXericCalifornia_SmallWadedable",
                                          ifelse(IndicatorCheck$EcoregionStreamSize=="XericCalifornia_LargeWadeable","PacificNorthwestXericCalifornia_LargeWadedable", 
                                                 ifelse(IndicatorCheck$EcoregionStreamSize=="XericCalifornia_Boatable","PacificNorthwestXericCalifornia_Boatable", IndicatorCheck$EcoregionStreamSize)))

## Code below can be uncommented out and used to get shorter abbreviations for the default benchmark if needed for graphing purposes or to match EPA reference data set ecoregion designations
# IndicatorCheck$THRESH=paste(IndicatorCheck$ECO10,IndicatorCheck$BNK_THRESH, sep="_")#2011-2015#works with new design database as well
# IndicatorCheck$THRESH3=as.factor(IndicatorCheck$THRESH)
# levels(IndicatorCheck$THRESH3) <- list( XE_SOUTH_SmallWadeable="XE_SOUTH_SmallWadeable",XE_SOUTH_LargeWadeable="XE_SOUTH_LargeWadeable",
#                                     MT_SWEST_SmallWadeable="MT_SWEST_SmallWadeable",MT_SWEST_LargeWadeable="MT_SWEST_LargeWadeable",
#                                     XE_EPLAT_SmallWadeable="XE_EPLAT_SmallWadeable",XE_EPLAT_LargeWadeable="XE_EPLAT_LargeWadeable",
#                                     MT_PNW_SmallWadeable="MT_PNW_SmallWadeable", MT_PNW_LargeWadeable="MT_PNW_LargeWadeable",MT_PNW_BOATABLE="MT_PNW_BOATABLE",
#                                     PL_NCULT_SmallWadeable="PLN_CULT_SmallWadeable", PL_NCULT_LargeWadeable="PLN_CULT_LargeWadeable",PL_NCULT_BOATABLE="PLN_CULT_BOATABLE",
#                                     PL_RANGE_SmallWadeable="PL_RANGE_SmallWadeable",PL_RANGE_LargeWadeable="PL_RANGE_LargeWadeable", PL_RANGE_BOATABLE="PL_RANGE_BOATABLE",
#                                     MT_SROCK_SmallWadeable="MT_SROCK_SmallWadeable",MT_SROCK_LargeWadeable="MT_SROCK_LargeWadeable",
#                                     MT_NROCK_SmallWadeable="MT_NROCK_SmallWadeable", MT_NROCK_LargeWadeable="MT_NROCK_LargeWadeable",
#                                     XE_NORTH_SmallWadeable="XE_NORTH_SmallWadeable",XE_NORTH_LargeWadeable="XE_NORTH_LargeWadeable",
#                                     Other=c( "AK_SmallWadeable","AK_LargeWadeable"),
#                                     MT_ROCK_BOATABLE=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),
#                                     XE_SEPLAT_BOATABLE=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
# )
# levels(IndicatorCheck$THRESH3) <- list( SouthernXericBasin_SmallWadeable="XE_SOUTH_SmallWadeable",SouthernXericBasin_LargeWadeable="XE_SOUTH_LargeWadeable",
#                                    SouthwestMountains_SmallWadeable="MT_SWEST_SmallWadeable",SouthwestMountains_LargeWadeable="MT_SWEST_LargeWadeable",
#                                    EasternXericBasin_SmallWadeable="XE_EPLAT_SmallWadeable",EasternXericBasin_LargeWadeable="XE_EPLAT_LargeWadeable",
#                                    PacificNorthwest_SmallWadeable="MT_PNW_SmallWadeable", PacificNorthwest_LargeWadeable="MT_PNW_LargeWadeable",PacificNorthwest_Boatable="MT_PNW_BOATABLE",
#                                    NorthernCultivatedPlains_SmallWadeable="PL_NCULT_SmallWadeable", NorthernCultivatedPlains_LargeWadeable="PL_NCULT_LargeWadeable",NorthernCultivatedPlains_Boatable="PL_NCULT_BOATABLE",
#                                    RangelandPlains_SmallWadeable="PL_RANGE_SmallWadeable",RangelandPlains_LargeWadeable="PL_RANGE_LargeWadeable", RangelandPlains_Boatable="PL_RANGE_BOATABLE",
#                                    SouthernRockies_SmallWadeable="MT_SROCK_SmallWadeable",SouthernRockies_LargeWadeable="MT_SROCK_LargeWadeable",
#                                    NorthernRockies_SmallWadeable="MT_NROCK_SmallWadeable", NorthernRockies_LargeWadeable="MT_NROCK_LargeWadeable",
#                                    NorthernXericBasin_SmallWadeable="XE_NORTH_SmallWadeable",NorthernXericBasin_LargeWadeable="XE_NORTH_LargeWadeable",
#                                    NorthernRockiesSouthernRockiesNorthernXericBasin_Boatable=c("MT_NROCK_BOATABLE", "MT_SROCK_BOATABLE","XE_NORTH_BOATABLE"),
#                                    EasternXericBasinSouthernXericBasin_Boatable=c( "XE_EPLAT_BOATABLE" ,"XE_SOUTH_BOATABLE")
# )
# IndicatorCheck$THRESH2=IndicatorCheck$THRESH
# IndicatorCheck$THRESH2=ifelse(IndicatorCheck$THRESH2=="PL_RANGE_BOATABLE"|IndicatorCheck$THRESH2=="PLN_CULT_BOATABLE"|IndicatorCheck$THRESH2=="MT_PNW_BOATABLE"|IndicatorCheck$THRESH2=="MT_NROCK_BOATABLE"|IndicatorCheck$THRESH2=="MT_SROCK_BOATABLE"|IndicatorCheck$THRESH2=="XE_NORTH_BOATABLE"|IndicatorCheck$THRESH2=="XE_EPLAT_BOATABLE"|IndicatorCheck$THRESH2=="XE_SOUTH_BOATABLE","ALL_BOATING",IndicatorCheck$THRESH2)
# IndicatorCheck$THRESH4=ifelse(IndicatorCheck$ECO10=="XE_EPLAT"|IndicatorCheck$ECO10=="XE_SOUTH"|IndicatorCheck$ECO10=="PLN_CULT"|IndicatorCheck$ECO10=="PL_RANGE","lowstab","highstab")


#write.csv(IndicatorCheck,paste0('IndicatorCheck',Sys.Date(),'.csv'),na="",row.names=FALSE)


###################################################################################################################

##############                        Additional Formatting for Final Output                #################

###################################################################################################################


# remove sample size info and any other undesired columns by running IndicatorXwalk Script
# this is also currently used to remove "_CHECK" from column names. 
# keeping this xwalk script allows for us to change indicator names without having to change the code. simply change the names in the data.xwalk csv below.
source('indicatorXwalk.R')
data.xwalk=read.csv("indicatorXwalk_2023.csv")

data.input=IndicatorCheck
IndicatorsFinal=indicatorXwalk(data.input,data.xwalk)

IndicatorsFinal<-data.frame(lapply(IndicatorsFinal, as.character), stringsAsFactors=FALSE)
write.csv(IndicatorsFinal,paste0(OutPath,'I_Indicators',"_",Sys.Date(),'.csv'),na="",row.names=FALSE)

###################################################################################################################

##############                        Historic Data             #################

###################################################################################################################

# Add in historic data for revisited reaches to compare indicator values across visits
# Join existing indicators for these same points to this new file
# select only historic points where pointIDs match pointIDs in the current dataset
# allindicators defined in config script

pointIDs <- DF_ReachInfo %>% tidyr::separate(EvaluationID, c('PointID', 'Date'), sep = '_')
uniquePointIDs <- unique(pointIDs['PointID'])

#create table of historic indicators with only those points in the DF-ReachInfo table for  comparison of data of sites across years
historicIndicators <- allindicators[allindicators$PointID %in% uniquePointIDs[,1],] 

#combine historic and indicators final. Y=old and x=new
#Run this for comparison of historic and new calculations
#IndicatorsFinal<-dplyr::left_join(IndicatorsFinal, historicIndicators,by="EvaluationID")

# select only columns that are included in the current indicator output
columns<- colnames(IndicatorsFinal)
historicIndicators <- subset(historicIndicators, select = columns)
# append historic data to current computed indicators
IndicatorsFinal=rbind.fill(historicIndicators,IndicatorsFinal)
# sort result by project and evaluationID
IndicatorsFinal=IndicatorsFinal[order(IndicatorsFinal$Project,IndicatorsFinal$EvaluationID),]

#IndicatorsFinal<-data.frame(lapply(IndicatorsFinal, as.character), stringsAsFactors=FALSE)
write.csv(IndicatorsFinal,paste0(OutPath,'I_Indicators',"_",Sys.Date(),"_plushistoric",'.csv'),na="",row.names=FALSE)
