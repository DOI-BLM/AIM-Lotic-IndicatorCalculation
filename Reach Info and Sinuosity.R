###########################################################
#                Reach Descriptors                        #
###########################################################

# SampleReach Table already read in in the step 1 initialization script

# rename fields in DF_ReachInfo data frame to indicate if they're being kept
sampledReachesChkFields <- c("BeaverFlowMod","BeaverSigns","FieldEvalDate","EvaluationID_OLD", "PhotoLink","WaterWithdrawals","SampledMidLatitude","BottomReachLatitude","TopReachLatitude","StreamName","SampledMidLongitude","BottomReachLongitude","TopReachLongitude","Project","ProtocolType","ProtocolVersion","PointID","FieldStatus","ProtocolReachLength")
newFieldNames = c()
for (fld in names(DF_ReachInfo)){
  if (fld %in% sampledReachesChkFields){
    addField <- paste(fld, "_CHECK", sep="")
  } else {
    addField <- fld
  }
  newFieldNames <- append(newFieldNames, addField)
}
DF_ReachInfo <- setNames(DF_ReachInfo, newFieldNames)
#DF_ReachInfo<-subset(DF_ReachInfo,select = -c(GDB_GEOMATTR_DATA) )
#write.csv(DF_ReachInfo,'DF_ReachInfo.csv')


###########################################################
#                BLM Admin Unit Boundaries                #
###########################################################
#subset records so that the filters applied to all other tables get applied to this one
ReachInfoSpatial<-subset(ReachInfoSpatial,EvaluationID %in% uniqueEvalIDs)
# do a spatial join between points and admin boundaries to get points attributed with district and field office
PointsAttributed<-sf::st_join(ReachInfoSpatial,AdminUnits)
# map the points by state just as a gut check
plot(PointsAttributed['ADMIN_ST'],pch=19)
#add CHECK to the end of the fields and change name to match indicator name
names(PointsAttributed)[names(PointsAttributed)=="PARENT_NAME"]<-"District_CHECK"
names(PointsAttributed)[names(PointsAttributed)=="ADMU_NAME"]<-"FieldOffice_CHECK"
names(PointsAttributed)[names(PointsAttributed)=="ADMIN_ST"]<-"BLM_AdminState_CHECK"
# subset data and remove the spatial component
DF_Points<-sf::st_drop_geometry(PointsAttributed[,c("EvaluationID","BLM_AdminState_CHECK","District_CHECK","FieldOffice_CHECK")])
#remove duplicate rows due to national conservation area polygons in  admin unit that overlap field office boundaries
#Lines 48 and 49 will remove some known duplicates but breaks if that value is not present, so not good for ingests. Leaving in case needed. Run line 50 instead.
#DF_Points<-DF_Points[-c(grep('NCA',DF_Points$FieldOffice_CHECK)),]
#DF_Points<-DF_Points[-c(grep('VERMILION CLIFFS NM',DF_Points$FieldOffice_CHECK)),]
DF_Points %>% distinct()
#join back in to main reach info table
DF_ReachInfo<-dplyr::left_join(DF_ReachInfo,DF_Points, by='EvaluationID')
#remove trailing zeroes that come in when pulling from the SDE
cols= "FieldEvalDate_CHECK"
DF_ReachInfo[cols] <- lapply(DF_ReachInfo[cols], as.POSIXct, tz = "UTC")


##########################################################
#                    Side Channels                       #
##########################################################
#Presence or Absence of side channels anywhere within the reach for a given EvaluationID

#Get Side Channel info from ChannelDimensions_W table for data from 2016 and on
side <- ReadTable(TableName=channelDimensions_W, EvaluationIDs=uniqueEvalIDs)
#Get Side Channel info from Thalweg table for data prior to 2016
sideThalweg<-ReadTable(TableName=ThalwegW, EvaluationIDs=uniqueEvalIDs)

# Calculate the number of transects (2016 and on) or thalweg measurements (2013-2015) with side channels present
# Then change count to simply presence or absence of side channels for each EvaluationID for current indicator reporting
# could eventually report length of side channels or number of side channels for each EvaluationID if assumptions were made about where side channel started and stopped in relation to transects

# Convert SideChannelType values to 1 if in (major, dry, minor) and 0 if absent. map all other values to "NA"
side$TypeNum <- ifelse(side$SideChannelType=="Major", 1,
                       ifelse(side$SideChannelType=="Dry", 1,
                              ifelse(side$SideChannelType=="Minor", 1,
                                     ifelse(side$SideChannelType=="Absent", 0,
                                            NA)))) # all other values map to NA

sideThalweg$SideNum<-ifelse(sideThalweg$SideChannelPresent=="Yes",1,
                            ifelse(sideThalweg$SideChannelPresent=="No",0,
                                   NA))

# Sum this result for a given EvaluationID to get a count of the number of transects or thalweg measurments with side channels and the number of total transects sampled
# Make sure to remove NA (na.rm=T) so that any NAs dont screw up the sum; 
#need to remove NAs if some other values are present but if no values are present at all for a reach then it should be NA. currently it is creating a 0 and absent designation if that is the case
#could instead count the number of transects with "X" in front for the channel dimensions table once null data is removed from 2021 or current field season
side_typeCount<- side %>%
  dplyr::group_by(EvaluationID) %>%
  dplyr::summarize(nSideChannelTransect= sum(TypeNum,na.rm=TRUE), nTransect=length(EvaluationID))

side_numCount<- sideThalweg %>%
  dplyr::group_by(EvaluationID) %>%
  dplyr::summarize(nSideChannelThalweg= sum(SideNum,na.rm=TRUE), nThalweg=length(EvaluationID))

#join two types of side channel counts together keeping all records from both tables
DF_SideChannel<-dplyr::full_join(side_typeCount,side_numCount,by='EvaluationID')

#2013-2015 data will have side channel counts for both thalweg and main transects, newer data will only have counts for main transects
#sum side channel counts across thalweg and main transects so that we get one column we can use for presence absence
DF_SideChannel$SideSum<- rowSums(cbind(DF_SideChannel$nSideChannelThalweg,DF_SideChannel$nSideChannelTransect), na.rm=TRUE)

# Then if the result is >0 make the value Present, if the result=0, then set to Absent
DF_SideChannel$SideChannels_CHECK <- ifelse(DF_SideChannel$SideSum > 0, 'Present',
                                        ifelse(DF_SideChannel$SideSum==0, 'Absent',
                                               NA)) # all other values map to NA
#write.csv(DF_SideChannel,'DF_SideChannel.csv')



##########################################################
#                    Sinuosity                           #
##########################################################
#Sinuosity= Distance along the thalweg (protocol reach length)/ Straight line distance between top and bottom of reach coordinates

#use data from DF_ReachInfo which has been read in first script and name changes to it have been made in the first section of this script above
#Get top and bottom of reach coordinates and use the formula below to calculate the distance in meters between coordinates that are in decimal degrees
DF_ReachInfo$straightline_CHECK=acos(sin(DF_ReachInfo$BottomReachLatitude_CHECK*3.141593/180)*sin(DF_ReachInfo$TopReachLatitude_CHECK*3.141593/180) + cos(DF_ReachInfo$BottomReachLatitude_CHECK*3.141593/180)*cos(DF_ReachInfo$TopReachLatitude_CHECK*3.141593/180)*cos(DF_ReachInfo$TopReachLongitude_CHECK*3.141593/180-DF_ReachInfo$BottomReachLongitude_CHECK*3.141593/180)) * 6371000

#create a new data frame without coordinates so that those are not repeated in the final data frame when all "DF" objects are joined together
sinuosity <- DF_ReachInfo %>% dplyr::select(c('EvaluationID', 'straightline_CHECK', 'FieldStatus_CHECK','ProtocolReachLength_CHECK'))

#Sinuosity= Distance along the thalweg (protocol reach length)/ Straight line distance between top and bottom of reach coordinates, round to 2 decimal places because 3 significant digits for reach length
sinuosity$Sinuosity_CHECK=round(as.numeric(sinuosity$ProtocolReachLength_CHECK)/as.numeric(sinuosity$straightline_CHECK),digits=2)

# Due to GPS errors and errors in measuring thalweg distances in the field. you can get values less than 1 (values can commonly be as low as 0.8)
# we used to exclude these values less than 1 but in 2019 we changed indicator values to be rounded to 1 if values are <1 
sinuosity$Sinuosity_CHECK=ifelse(sinuosity$Sinuosity_CHECK<1,1,sinuosity$Sinuosity_CHECK)

#remove data from partial sites because GPS accuracy is not good enough to warant calcs for these smaller distances AND we have no good record of actual reach length sampled compared to protocol reach length
sinuosity$Sinuosity_CHECK=ifelse(sinuosity$FieldStatus_CHECK=="Partial Reach"|sinuosity$FieldStatus_CHECK=="Interrupted Flow and Partial Reach",NA,sinuosity$Sinuosity_CHECK)

#create a new data frame with just sinuosity so that other "_CHECK" columns are not repeated in the final data frame when all "DF" objects are joined together
DF_Sinuosity <- sinuosity[,c('EvaluationID','Sinuosity_CHECK')]
#write.csv(DF_Sinuosity,'DF_Sinuosity.csv')

