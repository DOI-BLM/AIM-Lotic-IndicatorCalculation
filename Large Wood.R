#########################################################
#                   Get Large Wood Data                 #
#########################################################
# read in LargeWood_W_B table 
LwdWB <- ReadTable(TableName = largeWood_w_b, EvaluationIDs = uniqueEvalIDs)
# remove the ObjectID field
LwdWB <- LwdWB[, !(names(LwdWB) == 'OBJECTID'|names(LwdWB) == 'RecordID'|names(LwdWB)=='GlobalID')]
LwdWB<-subset(LwdWB,EvaluationID!="BA-TR-1180_2021-08-08")
# need reach length to compute frequency of pieces/volume of wood per km
#To get the reach length for which LWD was accesssed divide the total reach length by 10 to get the transect spacing and then multiply times the number of LWD transects sampled
#This is different than the EPA's reach length. The EPA determines reach length by approximating the number of intended thalweg stations
#They take the greater of either the max number of stations occurring at each transect or the station "mode" occurring at a site
#This seems overly complex....particularly the mode part why not get an accurate number of stations per transect? 
#Our method could over estimate wood though...crews may have evaluated sections of the thalweg but not recorded a wood value because they forgot or something else
#However not all crews collecting thalweg in future and may not be able to get thalweg depths where you could get wood...we probably could use another parameter that is collected at all thalweg stations (side channel presence- but did not collect this in 2016) 
TRCHLEN <-ReadTable(TableName = sampledReaches, TableFields = c('EvaluationID','ProtocolReachLength'),EvaluationIDs = uniqueEvalIDs)
#TRCHLEN<-subset(TRCHLEN, select = -c(geometry))

# wood size classes differ between boatable and wadeable protocols so need protocol type
protocols_evalIDs <-ReadTable(TableName = sampledReaches, TableFields = c('EvaluationID','ProtocolType'),EvaluationIDs = uniqueEvalIDs)
#protocols_evalIDs<-subset(protocols_evalIDs, select = -c(geometry))
#protocols_evalIDs$ProtocolType=ifelse(protocols_evalIDs$EvaluationID %in% c('FA-RV-10930_2021-07-10','GJ-RV-10189_2021-07-12',	'MN-RO-1090_2021-07-27',	'CR-RV-10109_2021-07-13',	'RG-RV-10082_2021-07-07',	'TA-RV-10994_2021-06-05',	'TA-RV-12514_2021-07-09',	'KR-RV-12809_2021-07-14',	'MS-RM-3073_2021-05-30',	'MN-RO-1083_2021-07-26',	'CR-TR-11754_2021-07-14',	'TA-RV-11746_2021-07-08',	'TA-RV-10210_2021-07-08',	'UC-TR-13848_2021-07-11',	'FA-RV-13554_2021-07-11'),"Boatable",'Wadeable')
# add protocol to LwdWB
LwdWB <- merge(LwdWB, protocols_evalIDs, by='EvaluationID')
LwdWB<- LwdWB[,!(names(LwdWB) %in% c('created_date','created_user','last_edited_user','last_edited_date'))]
LwdWB$WoodLocation<- ifelse(LwdWB$WoodLocation=="BridgingAboveBankfullChannel","Bridging Above Bankfull Channel",ifelse(LwdWB$WoodLocation=="WithinBankfullChannel","Within Bankfull Channel",LwdWB$WoodLocation))
#LwdWB<- LwdWB %>% select (1:24)# use this if getting error with geometry


### Additional Wood data formating
# much easier to pivot data into one column for all wood counts rather than suming across columns and rows
Lwd_pivot<-tidyr::pivot_longer(LwdWB,!c(EvaluationID,Transect,WoodLocation,ProtocolType))
#remove nulls
Lwd_pivot<- subset(Lwd_pivot,is.na(Lwd_pivot$value)==FALSE)
# side channel wood is just summed at a transect and across a site. Reach lengths don't differ if side channels are present (i.e. reach length is not doubled - only one linear feature is considered in frequency)
# To prevent confusion and get the correct number of transects and subsequent reach length calcs, we remap side channel transects (prefixed with an X) to main transects and remove the X
Lwd_pivot$Transect=plyr::mapvalues(Lwd_pivot$Transect, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
#########################################################
#                    Large Wood Indicators              #
#########################################################
#### LargeWoodCount ####

# sum the resulting rowSum per EvaluationID to get total pieces of wood per evaluationID
LargeWood_per_EvalID <- setNames(aggregate(value ~ EvaluationID+WoodLocation, data=Lwd_pivot, FUN=sum,na.rm=TRUE), c('EvaluationID','WoodLocation','LargeWoodCount_CHECK'))
# get the sample size, note side channel transects have not been combined yet so they will add to the sample size
Largewood_sample_size__per_EvalID <- Lwd_pivot %>% dplyr::count(EvaluationID,WoodLocation,name='nLargeWood_CHECK')
#merge counts and sample sizes
merged_largewood <- merge(LargeWood_per_EvalID, Largewood_sample_size__per_EvalID, by=c('EvaluationID','WoodLocation'))


#### Large Wood Frequency ####
# LargeWoodFreq= EPA C1WM100- (Cummulative count of LWD in bankfull channel across all size classes)/(Reach Length) units are pieces/100m

# Corrected these calculations in December 2024 to address differences in boatable versus wadeable. 

# Merge counts with the reach length
LargeWood_per_EvalID <- merge(merged_largewood, TRCHLEN, by='EvaluationID')
# get the number of transects per EvaluationID for which large wood was collected (CountTran)
# get the number of unique EvaluationID+Transect - transect names may appear twice if an X transect was present
unique_eval_transects <- unique(Lwd_pivot[, c('EvaluationID', 'Transect', 'ProtocolType')])
CountTran <- unique_eval_transects %>% dplyr::count(EvaluationID, name='CountTran')
# merge the CountTran in - needed for frequency computation
LargeWood_per_EvalID <- merge(LargeWood_per_EvalID, CountTran, by='EvaluationID')
# Compute large wood reach length. For wadeable, there are 10 transects so divide the total reach length by 10 and then multiply by the number of transects in the dataset
# For boatable the length measured = 110 (11 transects x 10 meter plots)
LargeWood_per_EvalID$LWD_ReachLength<- ifelse(LargeWood_per_EvalID$ProtocolType=="Wadeable",LargeWood_per_EvalID$CountTran * (LargeWood_per_EvalID$ProtocolReachLength/10),
                                              ifelse(LargeWood_per_EvalID$ProtocolType=="Boatable",LargeWood_per_EvalID$CountTran * (110/11), NA))
# compute the frequency - pieces of wood divided by the length of the reached assessed for wood and round to 3 digits multiply by 100 to get pieces per 100 m reach
LargeWood_per_EvalID$LargeWoodFreq_CHECK <- round(100 * (LargeWood_per_EvalID$LargeWoodCount_CHECK / LargeWood_per_EvalID$LWD_ReachLength),3)



#### LargeWoodVol ####
#EPA - V1WM100
#MUST run LargeWoodFrequency code prior to this code so that the TRCHLEN (reach length) code works. 
#Based on Jawson Law's Code and communication with Phil Kauffman I have assigned a max or upper end value to those categories which were >some number

# get min and max of size class bins
LWD_wood_sizestt <- textConnection(
  "ProtocolType name  Min_DIAMETER  Max_DIAMETER	Min_LENGTH	Max_LENGTH
  Boatable LargeDiamLargeLen	0.8	1	30	75
  Boatable LargeDiamMediumLen	0.8	1	15	30
  Boatable LargeDiamCombinedSmallLen	0.8	1	5	15
  Boatable MediumDiamLargeLen	0.6	0.8	30	75
  Boatable MediumDiamMediumLen	0.6	0.8	15	30
  Boatable MediumDiamCombinedSmallLen	0.6	0.8	5	15
  Boatable SmallDiamLargeLen	0.3	0.6	30	75
  Boatable SmallDiamMediumLen	0.3	0.6	15	30
  Boatable SmallDiamCombinedSmallLen	0.3	0.6	5	15
  Boatable XLargeDiamLargeLen	1	2	30	75
  Boatable XLargeDiamMediumLen	1	2	15	30
  Boatable XLargeDiamCombinedSmallLen	1	2	5	15
  Wadeable LargeDiamLargeLen	0.6	0.8	15	30
  Wadeable LargeDiamMediumLen	0.6	0.8	5	15
  Wadeable LargeDiamCombinedSmallLen	0.6	0.8	1.5	5
  Wadeable MediumDiamLargeLen	0.3	0.6	15	30
  Wadeable MediumDiamMediumLen	0.3	0.6	5	15
  Wadeable MediumDiamCombinedSmallLen	0.3	0.6	1.5	5
  Wadeable SmallDiamLargeLen	0.1	0.3	15	30
  Wadeable SmallDiamMediumLen	0.1	0.3	5	15
  Wadeable SmallDiamCombinedSmallLen	0.1	0.3	1.5	5
  Wadeable XLargeDiamLargeLen	0.8	2	15	30
  Wadeable XLargeDiamMediumLen	0.8	2	5	15
  Wadeable XLargeDiamCombinedSmallLen	0.8	2	1.5	5
  Wadeable LargeDiamSmallLen	0.6	0.8	3	5
  Wadeable MediumDiamSmallLen	0.3	0.6	3	5
  Wadeable SmallDiamSmallLen	0.1	0.3	3	5
  Wadeable XLargeDiamSmallLen	0.8	2	3	5
  Wadeable SmallDiamXSmallLen	0.1	0.3	1.5	3
  Wadeable XLargeDiamXSmallLen	0.8	2	1.5	3
  Wadeable MediumDiamXSmallLen	0.3	0.6	1.5	3
  Wadeable LargeDiamXSmallLen	0.6	0.8	1.5	3"
)


# Create tables of the volume value that corresponds to the size category.
LWD_wood_sizes=read.table(LWD_wood_sizestt, header = TRUE, stringsAsFactors = FALSE)
#calculate the volume of 1 piece of wood for each size class
#Equation found in Kauffman 99 (pg 31 of Kauffman 1999 and he cites Robison 1998) is wrong, first noticed because it does not produce values equal to those found in aquamet. Jason Law has the correct code, confirmed by Phil Kauffman and also produces the same values found in Aquamet. Aquamet code only uses calculated values from the equation but does not contain the equation itself
LWD_wood_sizes$VOLUME=pi*((0.5*(LWD_wood_sizes$Min_DIAMETER+((LWD_wood_sizes$Max_DIAMETER-LWD_wood_sizes$Min_DIAMETER)/3)))^2)*(LWD_wood_sizes$Min_LENGTH+((LWD_wood_sizes$Max_LENGTH-LWD_wood_sizes$Min_LENGTH)/3))
#join volumes with countsof wood 
LargeWood_Volume=dplyr::left_join(Lwd_pivot,LWD_wood_sizes, by=c('ProtocolType','name'))
#get the total volume of wood per size class and transect by multiplying the volume of each size class by the number of pieces of wood in the size class
LargeWood_Volume$VOLcalc=LargeWood_Volume$VOLUME*LargeWood_Volume$value
# sum volumes across size classes and transects per EvaluationID
LargeWood_volume_per_EvalID <- LargeWood_Volume %>% dplyr::group_by(EvaluationID,WoodLocation) %>% dplyr::summarize(LargeWoodVolSum=sum(VOLcalc, na.rm=TRUE))
#merge total counts with total volumes
LargeWood_per_EvalID <- merge(LargeWood_per_EvalID, LargeWood_volume_per_EvalID, by=c('EvaluationID','WoodLocation'))
#calculate volume as a frequency to scale it to the length of the reach assessed for wood and multiply by 100 so it is volume of wood per 100 m reach
LargeWood_per_EvalID$LargeWoodVol_CHECK<-round((LargeWood_per_EvalID$LargeWoodVolSum/LargeWood_per_EvalID$LWD_ReachLength)*100,3)

# add protocol back in and add a year column
#LargeWood_per_EvalID<- merge(LargeWood_per_EvalID,protocols_evalIDs, by='EvaluationID')
date_split <- data.frame(do.call('rbind', strsplit(as.character(LargeWood_per_EvalID$EvaluationID), '_', fixed=TRUE)))
LargeWood_per_EvalID$Year <- format(as.Date(date_split$X2, format='%Y-%m-%d'), '%Y')


# Excluded all indicator values if they do not meet the minumum sample size required:
LargeWood_per_EvalID$LargeWoodVol_CHECK=ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=64&LargeWood_per_EvalID$Year>='2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodVol_CHECK,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                               ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$Year<'2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodVol_CHECK,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                      ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$ProtocolType=='Boatable',LargeWood_per_EvalID$LargeWoodVol_CHECK,NA))) # For boatable sites collected in any Year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
LargeWood_per_EvalID$LargeWoodFreq_CHECK=ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=64&LargeWood_per_EvalID$Year>='2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodFreq_CHECK,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                                               ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$Year<'2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodFreq_CHECK,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                                      ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$ProtocolType=='Boatable',LargeWood_per_EvalID$LargeWoodFreq_CHECK,NA))) # For boatable sites collected in any Year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
LargeWood_per_EvalID$LargeWoodCount_CHECK=ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=64&LargeWood_per_EvalID$Year>='2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodCount_CHECK,# For wadeable sites collected in 2014 or later, total data points for entire reach=160 (16 per transect, 10 transects), 5 transects of data would be 80, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 64
                                                ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$Year<'2014'&LargeWood_per_EvalID$ProtocolType!='Boatable',LargeWood_per_EvalID$LargeWoodCount_CHECK,# For wadeable sites collected in 2013, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
                                                       ifelse(LargeWood_per_EvalID$nLargeWood_CHECK>=48&LargeWood_per_EvalID$ProtocolType=='Boatable',LargeWood_per_EvalID$LargeWoodCount_CHECK,NA))) # For boatable sites collected in any Year, total data points for entire reach=120, 5 transects of data would be 60, but because large wood is collected between transects Jennifer and Nicole decided to allow for 4 transects of data which is 48
# subset columns so that the pivot below comes out clean
largewoodsubset=subset(LargeWood_per_EvalID,select=c(EvaluationID,WoodLocation,LargeWoodVol_CHECK,LargeWoodFreq_CHECK,LargeWoodCount_CHECK,nLargeWood_CHECK))
largewoodsubset$WoodLocation<-ifelse(largewoodsubset$WoodLocation=="Bridging Above Bankfull Channel"|largewoodsubset$WoodLocation=="BridgingAboveBankfullChannel","AboveChan","InChan")
# pivot the data such that wood location is now appended to each indicator column
DF_LargeWood=tidyr::pivot_wider(largewoodsubset,names_from=WoodLocation,values_from=c(LargeWoodVol_CHECK,LargeWoodFreq_CHECK,LargeWoodCount_CHECK,nLargeWood_CHECK))
#rename indicators
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodVol_CHECK_AboveChan"]<-"LgWoodAboveChanVol_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodVol_CHECK_InChan"]<-"LgWoodInChanVol_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodCount_CHECK_AboveChan"]<-"LgWoodAboveChanCount_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodCount_CHECK_InChan"]<-"LgWoodInChanCount_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodFreq_CHECK_AboveChan"]<-"LgWoodAboveChanFreq_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="LargeWoodFreq_CHECK_InChan"]<-"LgWoodInChanFreq_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="nLargeWood_CHECK_AboveChan"]<-"n_LgWoodAboveChan_CHECK"
names(DF_LargeWood)[names(DF_LargeWood)=="nLargeWood_CHECK_InChan"]<-"n_LgWoodInChan_CHECK"


#write.csv(DF_LargeWood,'DF_LargeWood.csv')
 
