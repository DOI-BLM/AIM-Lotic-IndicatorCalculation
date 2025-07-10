##########################################################
#                Get Flood Prone Width Data             #
##########################################################
#read in FloodproneWidth_W
#Floodprone width
FloodWidth <- ReadTable(TableName = FloodProneWidth, EvaluationIDs = uniqueEvalIDs)
FloodWidth <- FloodWidth %>% dplyr::select(c('EvaluationID', 'Riffle', 'FloodproneWidth', 'FloodproneBankfullWidth','FloodproneBankfullHeight', 'FloodproneMaxWaterDepth'))


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


##########################################################
#             Width: Depth                               #
##########################################################
# This method was added in September 2024.
# This calculated the W:D at the transects where the two floodprone width measurements were taken. this ensures the measurement was taken at a riffle.
# This is different from Rosgen in that we don't measure an average water depth at the transect, only a max depth, so our W:D will be lower than Rosgen's
# FPW measurements are supposed to be taken at representative riffles at the top and bottom of the reach which makes them great candidates for W:D
# The other option would have been to use transect data and try to exclude transects with pools, but we wouldn't be sure we were getting riffle segments ro that the transects were representatve of the reach.

# W:D for Riffle  1
WD1 <- FloodWidth[FloodWidth$Riffle == 1, ]
WD1$WidthToDepthRiffle1_CHECK=WD1$FloodproneBankfullWidth/(WD1$FloodproneBankfullHeight+WD1$FloodproneMaxWaterDepth)
WD1<- WD1 %>% dplyr::select(c('EvaluationID', 'WidthToDepthRiffle1_CHECK'))

# W:D for Riffle  2
WD2 <- FloodWidth[FloodWidth$Riffle == 2, ]
WD2$WidthToDepthRiffle2_CHECK=WD2$FloodproneBankfullWidth/(WD2$FloodproneBankfullHeight+WD2$FloodproneMaxWaterDepth)
WD2<- WD2 %>% dplyr::select(c('EvaluationID', 'WidthToDepthRiffle2_CHECK'))



