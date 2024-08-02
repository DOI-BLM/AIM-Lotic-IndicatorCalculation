##########################################################
#                Get Flood Prone Width Data             #
##########################################################
#read in FloodproneWidth_W
#Floodprone width
FloodWidth <- ReadTable(TableName = FloodProneWidth, EvaluationIDs = uniqueEvalIDs)
FloodWidth <- FloodWidth %>% dplyr::select(c('EvaluationID', 'Riffle', 'FloodproneWidth', 'FloodproneBankfullWidth'))


##########################################################
#         Flood Prone Width Calculations                 #
##########################################################

#Entrenchment for Riffle = 1
riffle1 <- FloodWidth[FloodWidth$Riffle == 1, ]
riffle1$EntrenchmentRiffle1_CHECK=round(riffle1$FloodproneWidth/riffle1$FloodproneBankfullWidth, digits=2)
# round to 1 if less than 1 or 3 if more than 3
# in the future consider not rounding to 3 because field offices would like to see the full floodprone width but that might be better at the raw data scale
riffle1$EntrenchmentRiffle1_CHECK=ifelse(riffle1$EntrenchmentRiffle1_CHECK<1,1,ifelse(riffle1$EntrenchmentRiffle1_CHECK>3,3,riffle1$EntrenchmentRiffle1_CHECK))
riffle1 <- riffle1 %>% dplyr::select(c('EvaluationID', 'EntrenchmentRiffle1_CHECK'))
# Entrenchment for Riffle = 2
riffle2 <- FloodWidth[FloodWidth$Riffle == 2, ]
riffle2$EntrenchmentRiffle2_CHECK=round(riffle2$FloodproneWidth/riffle2$FloodproneBankfullWidth, digits=2)
# round to 1 if less than 1 or 3 if more than 3
# in the future consider not rounding to 3 because field offices would like to see the full floodprone width but that might be better at the raw data scale
riffle2$EntrenchmentRiffle2_CHECK=ifelse(riffle2$EntrenchmentRiffle2_CHECK<1,1,ifelse(riffle2$EntrenchmentRiffle2_CHECK>3,3,riffle2$EntrenchmentRiffle2_CHECK))
riffle2 <- riffle2 %>% dplyr::select(c('EvaluationID', 'EntrenchmentRiffle2_CHECK'))
# join the two back together on EvaluationID
DF_Entrenchment <- dplyr::full_join(riffle1, riffle2, by = 'EvaluationID')
#write.csv(DF_Entrenchment,'DF_Entrenchment.csv')
