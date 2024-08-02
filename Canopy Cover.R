##########################################################################################
###### Get Data for Canopy Cover ######
#########################################################################################

#read in CanopyCover_W and CanopyCover_B tables
#PctOverheadCover and PctBankOverheadCover
#EPA xcdenmid and xcdenbk indicators

# Read CanopyCover_W
densiom_w <- ReadTable(TableName = canopyCover_W, EvaluationIDs = uniqueEvalIDs)

# Read CanopyCover_B
densiom_b <- ReadTable(TableName = canopyCover_B, EvaluationIDs = uniqueEvalIDs)


############################################
#          Canopy Cover Indicators        #
############################################

#### PctOverheadCover ####
#EPA xcdenmid 

if(nrow(densiom_w)>0){
    # get just the densiom_w columns that have 'CanopyCenter' in the column name  because these are the "overhead" cover measurements taken in the center of the stream  
    densw_overhead_cols <- data.frame(densiom_w['EvaluationID'], densiom_w[grepl('CanopyCenter', colnames(densiom_w))])
    # collapse the table so that all values are in one column so that you dont have to take the mean across a matrix
    long_densw_overhead<-tidyr::pivot_longer(densw_overhead_cols,!EvaluationID)
    # remove null values which may be imported from survey123 if ingestion script doesnt properly remove
    long_densw_overhead<-subset(long_densw_overhead,is.na(long_densw_overhead$value)==FALSE)
    # compute the means per EvaluationID
    densw_means <- setNames(stats::aggregate(value~EvaluationID, data=long_densw_overhead, FUN=mean, na.rm=TRUE),c("EvaluationID","MeanOverheadDensiom"))
    #convert to a percent by dividing by total number of possible intersections on the densiometer (17)
    densw_means$PctOverheadCover_CHECK=round(densw_means$MeanOverheadDensiom/17*100,digits=1)
    # count number of measurements collected per Evaluation ID
    nDensSamples<-long_densw_overhead %>% dplyr::count(EvaluationID, name="nPctOverheadCover_CHECK")
    # merge the counts in with the means by Evaluation ID
    DensPvt=merge(densw_means,nDensSamples,by="EvaluationID")
    # Exclude indicator value if less than 5 transect worth of data collected; 4 locations * 5 transects=20
    DensPvt$PctOverheadCover_CHECK=ifelse(DensPvt$nPctOverheadCover_CHECK<20,NA,DensPvt$PctOverheadCover_CHECK)


#### PctBankOverheadCover ####
#EPA xcdenbk
#Wadeable
    # get just the densiom_w columns that are taken facing the banks
    densw_bank_cols <- data.frame(densiom_w[c('EvaluationID','CanopyLeft','CanopyRight')])
    # collapse the table so that all values are in one column so that you dont have to take the mean across a matrix
    long_densw_bank<-tidyr::pivot_longer(densw_bank_cols,!EvaluationID)
    # remove null values which may be imported from survey123 if ingestion script doesnt properly remove
    long_densw_bank<-subset(long_densw_bank,is.na(long_densw_bank$value)==FALSE)
    # compute the means per EvaluationID
    denswbank_means <- setNames(stats::aggregate(value~EvaluationID, data=long_densw_bank, FUN=mean, na.rm=TRUE),c("EvaluationID","MeanBankDensiom"))
    #convert to a percent by dividing by total number of possible intersections on the densiometer (17)
    denswbank_means$PctBankOverheadCover_CHECK=round(denswbank_means$MeanBankDensiom/17*100,digits=1)
    # count number of measurements collected per Evaluation ID
    nDensSamplesBnkW<-long_densw_bank %>% dplyr::count(EvaluationID,name="nPctBankOverheadCover_CHECK")
    # merge the counts in with the means by Evaluation ID
    BnkDensPvtWade=merge(denswbank_means,nDensSamplesBnkW,by="EvaluationID")
    # Exclude indicator value if less than 5 transect worth of data collected; 2 * 5 transects=10
    BnkDensPvtWade$PctBankOverheadCover_CHECK=ifelse(BnkDensPvtWade$nPctBankOverheadCover_CHECK<10,NA,BnkDensPvtWade$PctBankOverheadCover_CHECK)
}


#Boatable
if(nrow(densiom_b)>0){
    # get just the densiom_b columns that have 'Canopy' in the column name
    # changed calculation of boating sites in 2017 to include all 4 measurements taken at a bank
    densb_all_cols <- data.frame(densiom_b['EvaluationID'], densiom_b[grepl('Canopy', colnames(densiom_b))])
    # collapse the table so that all values are in one column so that you dont have to take the mean across a matrix
    long_densb_bank<-tidyr::pivot_longer(densb_all_cols ,!EvaluationID)
    # remove null values which may be imported from survey123 if ingestion script doesnt properly remove
    long_densb_bank<-subset(long_densb_bank,is.na(long_densb_bank$value)==FALSE)
    # compute the means per EvaluationID
    densbbank_means <- setNames(stats::aggregate(value~EvaluationID, data=long_densb_bank, FUN=mean, na.rm=TRUE),c("EvaluationID","MeanBankDensiom"))
    #convert to a percent by dividing by total number of possible intersections on the densiometer (17)
    densbbank_means$PctBankOverheadCover_CHECK=round(densbbank_means$MeanBankDensiom/17*100,digits=1)
    # count number of measurements collected per Evaluation ID
    nDensSamplesBnkB<-long_densb_bank %>% dplyr::count(EvaluationID, name="nPctBankOverheadCover_CHECK")
     # merge the counts in with the means by Evaluation ID
    BnkDensPvtBoat=merge(densbbank_means,nDensSamplesBnkB,by="EvaluationID")
    # Exclude indicator value if less than 5 transect worth of data collected; 4 locations * 5 transects=20
    BnkDensPvtBoat$PctBankOverheadCover_CHECK=ifelse(BnkDensPvtBoat$nPctBankOverheadCover_CHECK<20,NA,BnkDensPvtBoat$PctBankOverheadCover_CHECK)
}

# if both dataframes exist, merge them together, otherwise set BnkDensPvt = the one that does exist
if(exists("BnkDensPvtBoat")==TRUE & exists("BnkDensPvtWade")==TRUE) {
  BnkDensPvt=rbind(BnkDensPvtWade,BnkDensPvtBoat)
} else if (exists("BnkDensPvtWade")) {
    BnkDensPvt=BnkDensPvtWade
} else {
    BnkDensPvt=BnkDensPvtBoat
}

DF_CanopyCover=dplyr::full_join(DensPvt,BnkDensPvt, by='EvaluationID')
#write.csv(DF_CanopyCover,'DF_CanopyCover.csv')
