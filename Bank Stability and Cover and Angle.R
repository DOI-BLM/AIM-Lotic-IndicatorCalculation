##########################################################
#     Get  Bank Stability and Cover and Angle Data        #
##########################################################

#read in Bank_W and Bank_B table
BankTab_w <- ReadTable(TableName = bank_w, EvaluationIDs = uniqueEvalIDs)
BankTab_b <- ReadTable(TableName = bank_b, EvaluationIDs = uniqueEvalIDs)
# merge both tables into one
if (nrow(BankTab_b)>0 & nrow(BankTab_w)>0){BankStab <- merge(BankTab_w, BankTab_b, all = TRUE)
} else if (nrow(BankTab_w)>0) {BankStab<-BankTab_w
} else BankStab<-BankTab_b

# Get SideChannelLocation from ChannelDimensions_W
ChannelDim_w <- ReadTable(TableName = channelDimensions_W, EvaluationIDs = uniqueEvalIDs)
ChannelDim_w <- ChannelDim_w %>% select(c('EvaluationID', 'Transect', 'SideChannelLocation'))
ChannelDim_w<-subset(ChannelDim_w,!(is.na(SideChannelLocation)))
#create a copy of the dataframe for use below
ChannelDim_w2<-ChannelDim_w

# need protocol to deal with sample sizes appropriately
# need to add protocol in after data has been aggregated so add in from sampledReaches
Protocol<- ReadTable(TableName=sampledReaches,TableFields = c("EvaluationID","ProtocolType"),EvaluationIDs = uniqueEvalIDs)

##################################################################################################
#  subsetting data to properly deal with side channels for both bank stability and bank angle    #
##################################################################################################

##### properly select only outside banks for 2016 and after data ####
#changed in 2019 NOT to average across all side channel and main channel banks.
##PIBO takes only the outside banks but for pre-2016 data we can't determine which are the outside banks after the fact so we averaged across all banks that were collected.
# for data before 2016 this section can be run and just doesnt alter the data, all banks are used and averaged across
#for 2016 and beyond take only data from outside banks... run above code plus the code below for 2016+ data
#get which side of the main channel the side channel was on to determine outside banks
#if side channel on right bank need to use right bank for X transect data and use left bank data for main transect
#if side channel on left bank need to use left bank for X transect data and use right bank data for main transect
#Specify to use the opposite bank data if main transect
ChannelDim_w$SelectBank<-ifelse(ChannelDim_w$SideChannelLocation=='Left','Right',ifelse(ChannelDim_w$SideChannelLocation=='Right','Left',ChannelDim_w$SideChannelLocation))
# Copy dataset and create a row corresponding to side channel row
ChannelDim_w2$Transect <- plyr::mapvalues(ChannelDim_w2$Transect, c("A","B","C","D","E","F","G","H","I","J","K"), c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK"))
#Specify to use same bank if side channel transect
ChannelDim_w2$SelectBank<-ChannelDim_w$SideChannelLocation
#create a new data frame with a row for each main and corresponding side channel   
SelectBanks<-rbind(ChannelDim_w,ChannelDim_w2)
# left join the BankStab and ChannelDim_w
BankStab <- dplyr::left_join(BankStab, SelectBanks, by = c('EvaluationID', 'Transect'))
# select only correct banks if side channel data is present or if no side channel is present/ side channel location was not recorded ("select bank"=NA) use all data
Banks<-subset(BankStab,SelectBank==Bank|is.na(SelectBank)==TRUE)




##########################################################
#          Bank Stability and Cover Indicators            #
##########################################################
#Decided in 2019 not to remove depositonal banks! to match MIM calcs

##### 1. create columns designating if banks are covered (1), stable (1), or uncovered (0), unstable (0). Then get one column that has 1 if banks are covered and stable and 0 if not ####
### a. banks that are covered are assigned a value of 1 ###
Banks$CoverValueBasal=as.numeric(ifelse(Banks$CoveredBankOld=='No',"0",ifelse(Banks$CoveredBankOld=='Yes',"1",NA)))
Banks$CoverValueFoliar=as.numeric(ifelse(Banks$CoveredBankMIM=='No',"0",ifelse(Banks$CoveredBankMIM=='Yes',"1",NA)))

### b. banks that are stable are assigned a value of 1 
# for erosional banks stable= (Absent) and Unstable is (Fracture, slump, slough, eroding)
# for depositional banks only banks that are covered are considered stable
# first remove banks where bank type is not filled in. in these cases stability can not be calculated
BanksNA=subset(Banks,is.na(BankType))
if (nrow(BanksNA>0)){BanksNA$StableValue=NA}
# get stability for depositonal banks that had basal cover collected
BanksDepositionalBasal=subset(Banks,BankType=='Depositional'& is.na(Banks$CoverValueFoliar)==TRUE)
BanksDepositionalBasal$StableValue=as.numeric(ifelse(BanksDepositionalBasal$CoverValueBasal=='1', "1",
                                                              ifelse(BanksDepositionalBasal$CoverValueBasal=='0',"0","NA")))
# get stability for depositional banks that had foliar cover collected
BanksDepositionalFoliar=subset(Banks,BankType=='Depositional'& is.na(Banks$CoverValueFoliar)==FALSE)
BanksDepositionalFoliar$StableValue=as.numeric(ifelse(BanksDepositionalFoliar$CoverValueFoliar=='1',"1",
                                                      ifelse(BanksDepositionalFoliar$CoverValueFoliar=='0',"0","NA")))
# get stability for erosional banks
BanksErosional=subset(Banks,BankType=='Erosional')  
BanksErosional$StableValue=as.numeric(ifelse(BanksErosional$ErosionalFeature %in% c('Slump','Eroding','Slough','Fracture'),"0",
                                             ifelse(BanksErosional$ErosionalFeature=='Absent',"1","NA")))
# merge all banks back together
# may need to collapse this table for a given visit if both foliar and basal were collected at the same site! we dont have two stability fields in the indicator table
BanksAll=rbind(BanksDepositionalBasal,BanksDepositionalFoliar,BanksErosional,BanksNA)

#### c. banks that are stable and covered are assigned a value of 1 ###
BanksAll$PctBankCoveredStableMIM=as.numeric(ifelse((BanksAll$CoverValueFoliar+BanksAll$StableValue)<2,0,1))
BanksAll$PctBankCoveredStableOld=as.numeric(ifelse((BanksAll$CoverValueBasal+BanksAll$StableValue)<2,0,1))


##### 2. sumarize cover and stability across all banks by calculating the proportion of banks covered or stable or both covered and stable, also compute the number of banks assessed #####
# the math works such that taking the mean of 1s and 0s = the proportion of covered or stable banks
BnkAll<- BanksAll %>% group_by(EvaluationID) %>% dplyr::summarize (PctBankCoveredMIM_CHECK=mean(CoverValueFoliar, na.rm=TRUE),
                                                               PctBankStable_CHECK=mean(StableValue, na.rm=TRUE),
                                                               PctBankCoveredStableMIM_CHECK=mean(PctBankCoveredStableMIM, na.rm=TRUE),
                                                               PctBankCoveredOld_CHECK=mean(CoverValueBasal, na.rm=TRUE),
                                                               PctBankCoveredStableOld_CHECK=mean(PctBankCoveredStableOld, na.rm=TRUE),
                                                               nBnkCover_CHECK=n())
# convert above proportions to percent and round
BnkAll$PctBankCoveredOld_CHECK=round(BnkAll$PctBankCoveredOld_CHECK*100,digits=0)
BnkAll$PctBankCoveredStableOld_CHECK=round(BnkAll$PctBankCoveredStableOld_CHECK*100,digits=0)
BnkAll$PctBankCoveredMIM_CHECK=round(BnkAll$PctBankCoveredMIM_CHECK*100,digits=0)
BnkAll$PctBankStable_CHECK=round(BnkAll$PctBankStable_CHECK*100,digits=0)
BnkAll$PctBankCoveredStableMIM_CHECK=round(BnkAll$PctBankCoveredStableMIM_CHECK*100,digits=0)


##### 3. dont calculate indicator when sample sizes are two low ####
# sample sizes depend on protocol (wadeable vs. boatable) so join that in and subset data into two datasets
BnkAll=left_join(BnkAll,Protocol, by="EvaluationID")
BnkAllBoatable=subset(BnkAll,BnkAll$ProtocolType=="Boatable")
BnkAllWadeable=subset(BnkAll,BnkAll$ProtocolType=="Wadeable")

# boatable - 22 total, min= 5 transects 2 banks=10
BnkAllBoatable$PctBankCoveredStableOld_CHECK=ifelse(BnkAllBoatable$nBnkCover_CHECK<10,NA,BnkAllBoatable$PctBankCoveredStableOld_CHECK) 
BnkAllBoatable$PctBankCoveredOld_CHECK=ifelse(BnkAllBoatable$nBnkCover_CHECK<10,NA,BnkAllBoatable$PctBankCoveredOld_CHECK)  
BnkAllBoatable$PctBankCoveredMIM_CHECK=ifelse(BnkAllBoatable$nBnkCover_CHECK<10,NA,BnkAllBoatable$PctBankCoveredMIM_CHECK) 
BnkAllBoatable$PctBankCoveredStableMIM_CHECK=ifelse(BnkAllBoatable$nBnkCover_CHECK<10,NA,BnkAllBoatable$PctBankCoveredStableMIM_CHECK)  
BnkAllBoatable$PctBankStable_CHECK=ifelse(BnkAllBoatable$nBnkCover_CHECK<10,NA,BnkAllBoatable$PctBankStable_CHECK) 

# Wadeable - 42 total 2 banks at 21 transects (11 main and 10 intermediate), min= 5 main and 4 intermediate transects=9*2 banks=18
BnkAllWadeable$PctBankCoveredStableOld_CHECK=ifelse(BnkAllWadeable$nBnkCover_CHECK<18,NA,BnkAllWadeable$PctBankCoveredStableOld_CHECK)
BnkAllWadeable$PctBankCoveredOld_CHECK=ifelse(BnkAllWadeable$nBnkCover_CHECK<18,NA,BnkAllWadeable$PctBankCoveredOld_CHECK)  
BnkAllWadeable$PctBankCoveredMIM_CHECK=ifelse(BnkAllWadeable$nBnkCover_CHECK<18,NA,BnkAllWadeable$PctBankCoveredMIM_CHECK) 
BnkAllWadeable$PctBankCoveredStableMIM_CHECK=ifelse(BnkAllWadeable$nBnkCover_CHECK<18,NA,BnkAllWadeable$PctBankCoveredStableMIM_CHECK)  
BnkAllWadeable$PctBankStable_CHECK=ifelse(BnkAllWadeable$nBnkCover_CHECK<18,NA,BnkAllWadeable$PctBankStable_CHECK) 

# merge boatable and wadeable back together
if(exists("BnkAllBoatable")==TRUE & exists("BnkAllWadeable")==TRUE) {
  DF_BankStabilityCover=rbind(BnkAllWadeable,BnkAllBoatable)
} else if (exists("BnkAllWadeable")) {
  DF_BankStabilityCover=BnkAllWadeable
} else {
  DF_BankStabilityCover=BnkAllBoatable
}


#write.csv(DF_BankStabilityCover,'DF_BankStabilityCover.csv')

##########################################################

#                    Bank Angle  Indicators              #

##########################################################
#BankAngleAvg
#PctBanksUndercut


#2016+ data
#need to treat side channels the same as with bank stability and only use the angles from the outer banks
#run the side channel section prior to running this to get SelectBanks
# remove any records where BankAngle is NA
bank_table_subset <- subset(Banks,is.na(BankAngle)==FALSE)
# if a bank angle is less than 45 degrees, set it to 45 degrees
# not sure why we do this but PIBO does so wanted to duplicate their measurements. something related to preventing low values from skewing the mean
bank_table_subset$BankAngle=ifelse(bank_table_subset$BankAngle<45,45,bank_table_subset$BankAngle)
# average bank angles across all banks and transects
bank_angle_means <- aggregate(bank_table_subset$BankAngle, list(bank_table_subset$EvaluationID), mean)
colnames(bank_angle_means) <- c('EvaluationID', 'BankAngleAvg_CHECK')
# round bank angle average to 0 decimals
bank_angle_means$BankAngleAvg_CHECK <- round(bank_angle_means$BankAngleAvg_CHECK, 0)
# count the number of bank angle samples per evaluation ID
bank_angle_samples <- bank_table_subset %>% dplyr::count(EvaluationID, name='nBankAngle_CHECK')
# join the bank angle sample count onto the bank angle means
DF_BankAngle <- merge(bank_angle_means, bank_angle_samples, by = 'EvaluationID')

#calculate percent of banks that are undercut (<90 degrees, Acute)
pct_acute <- bank_table_subset %>% dplyr::group_by(EvaluationID, BankAngleType) %>% dplyr::summarise(n = n()) %>% dplyr::mutate(freq = n / sum(n))
#convert to percent
pct_acute$PctBank<-round(pct_acute$freq*100,digits=1)
#pivot to wide format
pct_acute_pivot <- pct_acute %>% select(EvaluationID, BankAngleType, PctBank) %>% pivot_wider(names_from=BankAngleType, values_from=PctBank, values_fill=0)
#create new column for Percent Banks Undercut
pct_acute_pivot$PctBanksUndercut_CHECK <- pct_acute_pivot$Acute
undercut_banks<-pct_acute_pivot %>% select(EvaluationID,PctBanksUndercut_CHECK )

#merge with Bank angle means
DF_BankAngle <- merge(DF_BankAngle, undercut_banks, by = 'EvaluationID')
# if sample size is less than 10, set bank angle average to NA
DF_BankAngle$BankAngleAvg_CHECK <- ifelse(DF_BankAngle$nBankAngle_CHECK<10, NA, DF_BankAngle$BankAngleAvg_CHECK)
# if sample size is less than 10, set Pct Banks Undercut to NA
DF_BankAngle$PctBanksUndercut_CHECK <- ifelse(DF_BankAngle$nBankAngle_CHECK<10, NA, DF_BankAngle$PctBanksUndercut_CHECK)


#write.csv(DF_BankAngle,"DF_BankAngle.csv")








