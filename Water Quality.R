###############################################################################################
###### Read In Water Quality Data ######
###############################################################################################
#read in WaterQualityMacroInvert table
WQ <- ReadTable(TableName = WQMacroInvert, EvaluationIDs = uniqueEvalIDs)

WQMacroTurbAvgFields <- c('TurbidityReading1','TurbidityReading2','TurbidityReading3')
WQMacroNitrAvgFields <- c('TotalNitrogenOriginal','TotalNitrogenDuplicate')
WQMacroPhosAvgFields <- c('TotalPhosphorusOriginal','TotalPhosphorusDuplicate')

# QAQC number of ids and compare it with what is observed
# WQ evaluation IDs can easily get duplicates due to repeat visits so make sure to QC this data if you are getting too many rows in the final datasets
n_distinct(WQ$EvaluationID, na.rm = TRUE) 

###############################################################################################
###### Water Quality Computations ######
###############################################################################################

################ Turbidity ######################
# create TurbidityAvg field by averaging across the 3 measurements taken; 
#note some devices average this in the field and in this case the same value (average) is entered for all three readings
#### We are selecting and performing averages from columns: 'TurbidityReading1','TurbidityReading2','TurbidityReading3'
Turbidity <- WQ %>% dplyr::select(all_of(WQMacroTurbAvgFields))
TurbidityDF <- data.frame(EvaluationID=WQ['EvaluationID'], TurbidityAvg_CHECK=round(rowMeans(Turbidity,na.rm=TRUE),digits=2))

############### Total Nitrogen ########################
# changed in 2021 to NOT  average across the original and duplicate samples if duplicate water quality samples were taken, 
#### We are selecting and performing averages from columns: 'TotalNitrogenOriginal','TotalNitrogenDuplicate'
#TotalNitrogen<- WQ %>% dplyr::select(all_of(WQMacroNitrAvgFields))
#TotalNitrogenDF <- data.frame(EvaluationID=WQ['EvaluationID'], TotalNitrogen_CHECK=round(rowMeans(TotalNitrogen,na.rm=TRUE),digits=1))
WQ$TotalNitrogen=round(WQ$TotalNitrogenOriginal,digits=2)
names(WQ)[names(WQ) == 'TotalNitrogen'] <- 'TotalNitrogen_CHECK'


############### Total Phosphorous ########################
# changed in 2021 to NOT  average across the original and duplicate samples if duplicate water quality samples were taken, 
#### We are selecting and performing averages from columns: 'TotalPhosphorusOriginal','TotalPhosphorusDuplicate'
#TotalPhosphorous <- WQ %>% dplyr::select(all_of(WQMacroPhosAvgFields))
#TotalPhosphorousDF <- data.frame(EvaluationID=WQ['EvaluationID'], TotalPhosphorous_CHECK=round(rowMeans(TotalPhosphorous,na.rm=TRUE),digits=1))
WQ$TotalPhosphorous=round(WQ$TotalPhosphorusOriginal,digits=2)
names(WQ)[names(WQ) == 'TotalPhosphorous'] <- 'TotalPhosphorous_CHECK'


############### pH, Specific Conductance, and Instant Temp ##########
#rename  and round fields
# Change the name of SpecificConductance field and round to 2 sig figs
WQ$SpecificConductance=round(WQ$SpecificConductance,digits=2)
names(WQ)[names(WQ) == 'SpecificConductance'] <- 'SpecificConductance_CHECK'

# Change the name of pH field
names(WQ)[names(WQ) == 'pH'] <- 'pH_CHECK'

# Change the name of InstantTemp field
names(WQ)[names(WQ) == 'InstantTemp'] <- 'InstantTemp_CHECK'

DF_WQ<-purrr::reduce(list(WQ,TurbidityDF),by="EvaluationID",left_join)
#write.csv(DF_WQ,'WQ.csv')


