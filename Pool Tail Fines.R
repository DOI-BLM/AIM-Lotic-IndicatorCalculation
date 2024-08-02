#########################################################
#       Get Data for Pool Tail Fines Indicators         #
#########################################################

#read in PoolTailFines_W table
#Pool Tail Fines
PoolFines <- ReadTable(TableName = PoolTailFines_W, EvaluationIDs = uniqueEvalIDs)


#########################################################
#                Pool Tail Fines Indicators             #
#########################################################
#PctPoolFinesLessThan2mm and PctPoolFinesLessThan6mm

# compute percentage of particles <2mm for each grid
# total possible particles is gotten by taken the 50 grid intersections and subtracting the particles that could not be measured (organic matter or boulders)
# unclear why these are not included in the calculation; PIBO only is assessing of the particles that were possible spawning matterial what is fines?
PoolFines$PoolTailFinesLessThan2mm <- (PoolFines$PoolTailFinesLessThan2mm / (50 - PoolFines$PoolTailOrgMatterBoulder)) * 100
PoolFines$PoolTailFinesLessThan2mm <- ifelse(is.nan(PoolFines$PoolTailFinesLessThan2mm), NA, PoolFines$PoolTailFinesLessThan2mm)
# compute percentage of particles <6mm for each grid
PoolFines$PoolTailFinesLessThan6mm <- (PoolFines$PoolTailFinesLessThan6mm / (50 - PoolFines$PoolTailOrgMatterBoulder)) * 100
PoolFines$PoolTailFinesLessThan6mm <- ifelse(is.nan(PoolFines$PoolTailFinesLessThan6mm), NA, PoolFines$PoolTailFinesLessThan6mm)
# average the PoolTailFinesLessThan2mm across all 3 grids at a pool
PoolFines2_per_pool <- PoolFines %>% dplyr::group_by(EvaluationID,PoolNum) %>% dplyr::summarize(PoolFines2_per_pool=mean(PoolTailFinesLessThan2mm, na.rm=TRUE))
# average the PoolTailFinesLessThan6mm across all 3 grids at a pool
PoolFines6_per_pool <- PoolFines %>% dplyr::group_by(EvaluationID,PoolNum) %>% dplyr::summarize(PoolFines6_per_pool=mean(PoolTailFinesLessThan6mm, na.rm=TRUE))
# average the PoolTailFinesLessThan2mm for all pools within the reach
PoolFines2_per_EvalID <- PoolFines2_per_pool %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(PctPoolTailFinesLessThan2mm_CHECK=mean(PoolFines2_per_pool, na.rm=TRUE))
PoolFines2_per_EvalID$PctPoolTailFinesLessThan2mm_CHECK <- round(PoolFines2_per_EvalID$PctPoolTailFinesLessThan2mm_CHECK, 0)
# average the PoolTailFinesLessThan6mm for all pools within the reach
PoolFines6_per_EvalID <- PoolFines6_per_pool %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(PctPoolTailFinesLessThan6mm_CHECK=mean(PoolFines6_per_pool, na.rm=TRUE))
PoolFines6_per_EvalID$PctPoolTailFinesLessThan6mm_CHECK <- round(PoolFines6_per_EvalID$PctPoolTailFinesLessThan6mm_CHECK, 0)
# make the output data frame
DF_PoolTailFines <- merge(PoolFines2_per_EvalID, PoolFines6_per_EvalID, by='EvaluationID', all=TRUE)
#sample sizes were checked on raw data- if only 1 replicate in a pool data was inactivated and if more than 50% of remaining data missing data all data for site inactivated
#write.csv(DF_PoolTailFines,'DF_PoolTailFines.csv')


