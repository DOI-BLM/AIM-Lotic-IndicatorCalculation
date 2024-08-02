##########################################################################################
###### Get Data for Veg Complexity Indicators ######
##########################################################################################
#VegComplexity
#EPA XCMG Indicator
#read in VegComplexity table
RipXCMG <- ReadTable(TableName = vegComplexity, EvaluationIDs = uniqueEvalIDs)
vegComplexity_fields <- c('CanopyBigTreeCover','CanopySmallTreeCover','UnderstoryWoodyCover','UnderstoryNonWoodyCover','GroundNonWoodyCover','GroundWoodyCover')
VegComplexityWoody_fields <- c("CanopyBigTreeCover","CanopySmallTreeCover","GroundWoodyCover","UnderstoryWoodyCover")
VegComplexityUnderstoryGround_fields <- c("GroundWoodyCover","UnderstoryWoodyCover")
VegComplexityBareGround_fields <- c("BareGroundCover")

#########################################################
#          Veg Complexity Indicator Computation       #
#########################################################
# In the field, crews assess cover for each height category using five cover classes: 0 = absent 0%, 1 = sparse: <10%, 2 = moderate: 10-40%, 3 = heavy: 40-75%, and 4 = very heavy >75%. 
# For analysis, each cover class is assigned a mid-point (0%, 5%, 25%, 57.5%, and 87.5% respectively) and then converted to proportional cover (0.05, 0.25, 0.575, and 0.875 respectively)
# Proportional cover is then summed across the two vegetation types and three heights, and finally averaged across the left and right banks of 11 transects 

#function to recode numeric rank categories (0-4) to proportional cover from 0-1; median of cover class bins are used
recode_val <- function(the_val){result <- ifelse(the_val == 1, 0.05, ifelse(the_val == 2, 0.25, ifelse(the_val==3, 0.575, ifelse(the_val==4, 0.875, ifelse(the_val==0, 0, NA)))))}

#VegComplexity 
#EPA xcmg

# recode the columns
the_fields <- append('EvaluationID', vegComplexity_fields)
RipXCMG_recoded <- RipXCMG[the_fields] %>% dplyr::mutate_if(is.numeric, recode_val)
# compute the sum and counts of non-NA values per row (bank)
numeric_columns <- dplyr::select_if(RipXCMG_recoded, is.numeric)
RipXCMG_rowSumsCounts <- data.frame(EvaluationID=RipXCMG_recoded['EvaluationID'], Row_Sum=rowSums(numeric_columns, na.rm=TRUE), NumSamples=rowSums(!is.na(numeric_columns)))
# compute the mean of the row sums per Evaluation ID = VegComplexity
RipXCMG_means_per_EvalID <- RipXCMG_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(VegComplexity_CHECK=mean(Row_Sum))
# round the VegComplexity Values to 2 decimal places
RipXCMG_means_per_EvalID$VegComplexity_CHECK=round(RipXCMG_means_per_EvalID$VegComplexity_CHECK,digits=2)
# compute the sample size per EvaluationID
RipXCMG_numSamples_per_EvalID <- RipXCMG_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(nVegComplexity_CHECK=sum(NumSamples))
# remove data with insufficent sample size (6 cover types/strata (canopy-big trees, small trees,understory- woody, nonwoody,groundcove- woody, nonwoody) * 2 banks * minimum of 5 transects=60)
DF_VegComplexity <- merge(RipXCMG_means_per_EvalID, RipXCMG_numSamples_per_EvalID, by='EvaluationID')
DF_VegComplexity$VegComplexity_CHECK=ifelse(DF_VegComplexity$nVegComplexity_CHECK<60,NA,DF_VegComplexity$VegComplexity_CHECK)


#VegComplexityWoody
#EPA xcmgw
#xcmgw=XC+XMW+XGW: However, this is not how aquamet is calculating it, the order of operation would give different results if XC was caluclated and then added to XMW and XMG
#More true to aquamet calculation: XCMG=XCL+XCS+XMW+XGW
#Need to just calculate it by transect side first then average at an entire site.
##XC=XCL+XCS (Small Canopy trees (CANSTRE) + Large Canopy trees(CANBTRE))
##XMW=Understory woody aka UNDWDY
##MGW= ground cover woody GCWDY

# recode the columns
the_fields <- append('EvaluationID', VegComplexityWoody_fields)
RipWW_recoded <- RipXCMG[the_fields] %>% dplyr::mutate_if(is.numeric, recode_val)
# compute the sum and counts of non-NA values per row (bank)
numeric_columns <- dplyr::select_if(RipWW_recoded, is.numeric)
RipWW_rowSumsCounts <- data.frame(EvaluationID=RipWW_recoded['EvaluationID'], Row_Sum=rowSums(numeric_columns, na.rm=TRUE), NumSamples=rowSums(!is.na(numeric_columns)))
# compute the mean of the row sums per Evaluation ID = VegComplexityWoody
RipWW_means_per_EvalID <- RipWW_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(VegComplexityWoody_CHECK=mean(Row_Sum))
# round the VegComplexityWoody Values to 2 decimal places
RipWW_means_per_EvalID$VegComplexityWoody_CHECK=round(RipWW_means_per_EvalID$VegComplexityWoody_CHECK,digits=2)
# compute the sample size per EvaluationID
RipWW_numSamples_per_EvalID <- RipWW_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(nVegComplexityWoody_CHECK=sum(NumSamples))
# remove data with insufficent sample size (4 types of cover * 2 banks * minimum of 5 transects=40)
DF_VegComplexityWoody <- merge(RipWW_means_per_EvalID, RipWW_numSamples_per_EvalID , by='EvaluationID')
DF_VegComplexityWoody$VegComplexityWoody_CHECK=ifelse(DF_VegComplexityWoody$nVegComplexityWoody_CHECK<40,NA,DF_VegComplexityWoody$VegComplexityWoody_CHECK)


#VegComplexityUnderstoryGround

# recode the columns
the_fields <- append('EvaluationID', VegComplexityUnderstoryGround_fields)
RipUND_recoded <- RipXCMG[the_fields] %>% dplyr::mutate_if(is.numeric, recode_val)
# compute the sum and counts of non-NA values per row (bank)
numeric_columns <- dplyr::select_if(RipUND_recoded, is.numeric)
RipUND_rowSumsCounts <- data.frame(EvaluationID=RipUND_recoded['EvaluationID'], Row_Sum=rowSums(numeric_columns, na.rm=TRUE), NumSamples=rowSums(!is.na(numeric_columns)))
# compute the mean of the row sums per Evaluation ID = VegComplexityUnderstoryGround
RipUND_means_per_EvalID <- RipUND_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(VegComplexityUnderstoryGround_CHECK=mean(Row_Sum))
# round the VegComplexityUnderstoryGround Values to 2 decimal places
RipUND_means_per_EvalID$VegComplexityUnderstoryGround_CHECK=round(RipUND_means_per_EvalID$VegComplexityUnderstoryGround_CHECK,digits=2)
# compute the sample size per EvaluationID
RipUND_numSamples_per_EvalID <- RipUND_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(nVegComplexityUnderstoryGround_CHECK=sum(NumSamples))
# remove data with insufficent sample size (2 types of cover * 2 banks * minimum of 5 transects=20)
DF_VegComplexityUnderstoryGround <- merge(RipUND_means_per_EvalID, RipUND_numSamples_per_EvalID , by='EvaluationID')
DF_VegComplexityUnderstoryGround$VegComplexityUnderstoryGround_CHECK=ifelse(DF_VegComplexityUnderstoryGround$nVegComplexityUnderstoryGround_CHECK<20,NA,DF_VegComplexityUnderstoryGround$VegComplexityUnderstoryGround_CHECK)


######## other EPA veg complexity variations ###########
#VegComplexityBareGround
#EPA XGB

# recode the columns
the_fields <- append('EvaluationID', VegComplexityBareGround_fields)
RipBG_recoded <- RipXCMG[the_fields] %>% dplyr::mutate_if(is.numeric, recode_val)
# compute the sum and counts of non-NA values per row (bank)
numeric_columns <- dplyr::select_if(RipBG_recoded, is.numeric)
RipBG_rowSumsCounts <- data.frame(EvaluationID=RipBG_recoded['EvaluationID'], Row_Sum=rowSums(numeric_columns, na.rm=TRUE), NumSamples=rowSums(!is.na(numeric_columns)))
# compute the mean of the row sums per Evaluation ID = VegComplexityUnderstoryGround
RipBG_means_per_EvalID <- RipBG_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(VegComplexityBareGround_CHECK=mean(Row_Sum))
# round the VegComplexityBareGround Values to 2 decimal places
RipBG_means_per_EvalID$VegComplexityBareGround_CHECK=round(RipBG_means_per_EvalID$VegComplexityBareGround_CHECK,digits=2)
# compute the sample size per EvaluationID
RipBG_numSamples_per_EvalID <- RipBG_rowSumsCounts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(nVegComplexityBareGround_CHECK=sum(NumSamples))
# remove data with insufficent sample size (1 types of cover * 2 banks * minimum of 5 transects=10)
DF_VegComplexityBareGround <- merge(RipBG_means_per_EvalID, RipBG_numSamples_per_EvalID  , by='EvaluationID')
DF_VegComplexityBareGround$VegComplexityBareGround_CHECK=ifelse(DF_VegComplexityBareGround$nVegComplexityBareGround_CHECK<10,NA,DF_VegComplexityBareGround$VegComplexityBareGround_CHECK)

DF_JoinedVegComplexityDF=purrr::reduce(list(DF_VegComplexity,DF_VegComplexityWoody,DF_VegComplexityUnderstoryGround,DF_VegComplexityBareGround), by='EvaluationID',left_join)
#write.csv(DF_JoinedVegComplexity,'DF_JoinedVegComplexity.csv')
