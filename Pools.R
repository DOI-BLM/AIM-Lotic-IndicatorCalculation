#########################################################
#       Get Data for Pool Indicators              #
#########################################################

#read in SlopePoolSummary_W and Pools_W tables
#Pools_w
pool_length <- ReadTable(TableName = pools_w, EvaluationIDs = uniqueEvalIDs)

# slopePoolSummary_w
reach_length <- ReadTable(TableName = SlopePoolSummary, EvaluationIDs = uniqueEvalIDs)

#RP100 (residual pools as defined by EPA more than 100cm deep) and other EPA pool metrics 
#see seperate R script called EPA_Pools

#########################################################
#        PIBO Pool Indicator Computations              #
#########################################################

#PctPools

# Sum the pool length values per EvaluationID
PoolLength_per_EvalID <- pool_length %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(Pool_Length_Sum=if (all(is.na(PoolLength))) NA else sum(PoolLength, na.rm=TRUE))
# join the pool length per Eval ID and reach length tables
poolsmerge <- dplyr::full_join(PoolLength_per_EvalID, reach_length, by='EvaluationID')
# compute the PctPools where both values are not NA
calc_pct_rows <- poolsmerge[!is.na(poolsmerge$Pool_Length_Sum) & !is.na(poolsmerge$PoolReachLength),]
calc_pct_rows$PctPools_CHECK <- round((calc_pct_rows$Pool_Length_Sum/calc_pct_rows$PoolReachLength)*100,digits=2)
calc_pct_rows <- dplyr::select(calc_pct_rows, c('EvaluationID','PctPools_CHECK'))


#ResPoolDepth - residual pool depth (PoolDepth is exactly the same data as pool_length)
PoolDepth <- pool_length %>% dplyr::select(c('EvaluationID','PoolTailDepth','PoolMaxDepth'))
#residual depth = maxdepth-pool tail depth
PoolDepth$ResPoolDepthAvg_CHECK=(PoolDepth$PoolMaxDepth-PoolDepth$PoolTailDepth)
#average across all pools for a give evaluationID
RPD=setNames(aggregate(PoolDepth$ResPoolDepthAvg_CHECK,list(EvaluationID=PoolDepth$EvaluationID),mean, na.rm=TRUE),c("EvaluationID","ResPoolDepthAvg_CHECK"))#converted to m
#round to 2 digits
RPD$ResPoolDepthAvg_CHECK=round(RPD$ResPoolDepthAvg_CHECK,digits=2)

# find the number of pools per EvaluationID = PoolCount
pool_length <- subset(pool_length, !is.na(PoolTailDepth) | !is.na(PoolMaxDepth) | !is.na(PoolLength))
pool_counts <- pool_length %>% dplyr::count(EvaluationID,name="PoolCount_CHECK")

# Join length, depth, and counts
DF_Pools <- purrr::reduce(list(poolsmerge,pool_counts,calc_pct_rows,RPD), by="EvaluationID",left_join)
#compute pools per km by dividing number of pools at the site by the length of the reach assessed for pools and multiplying by 1000m
DF_Pools$PoolFreq_CHECK <- round((DF_Pools$PoolCount_CHECK/DF_Pools$PoolReachLength)*1000,digits=1)

# record 0 if there were no pools present and properly designate pools as NA if no flow
DF_Pools$PctPools_CHECK <- ifelse(DF_Pools$PoolsCollected=='Collected No Pools Present',0,DF_Pools$PctPools_CHECK)
DF_Pools$PctPools_CHECK <- ifelse(DF_Pools$PoolsCollected=='No Flow Not Collected',NA,DF_Pools$PctPools_CHECK)
DF_Pools$PoolFreq_CHECK <- ifelse(DF_Pools$PoolsCollected=='Collected No Pools Present',0,DF_Pools$PoolFreq_CHECK)
DF_Pools$PoolFreq_CHECK <- ifelse(DF_Pools$PoolsCollected=='No Flow Not Collected',NA,DF_Pools$PoolFreq_CHECK)
DF_Pools$ResPoolDepthAvg_CHECK <- ifelse(DF_Pools$PoolsCollected=='Collected No Pools Present',0,DF_Pools$ResPoolDepthAvg_CHECK)
DF_Pools$ResPoolDepthAvg_CHECK <- ifelse(DF_Pools$PoolsCollected=='No Flow Not Collected',NA,DF_Pools$ResPoolDepthAvg_CHECK)
DF_Pools$PoolCount_CHECK <- ifelse(DF_Pools$PoolsCollected=='Collected No Pools Present',0,DF_Pools$PoolCount_CHECK)
DF_Pools$PoolCount_CHECK <- ifelse(DF_Pools$PoolsCollected=='No Flow Not Collected',NA,DF_Pools$PoolCount_CHECK)

#write.csv(DF_Pools,'DF_Pools.csv')


