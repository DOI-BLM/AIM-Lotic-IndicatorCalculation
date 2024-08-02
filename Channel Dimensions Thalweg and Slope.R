#########################################################################################################
###### Get Channel Dimensions, Thalweg, and Slope Data ######
#########################################################################################################
#get ChannelDimensions_W and ChannelDimensions_B
channel_dim_w <- ReadTable(TableName = channelDimensions_W, EvaluationIDs = uniqueEvalIDs)
channel_dim_b <- ReadTable(TableName = channelDimensions_B, EvaluationIDs = uniqueEvalIDs)
# if both wadeable and boatable sites exist, combine these two tables, keep all common columns, if boatable doesnt exist use wadeable. if wadeable not in dataset use boatable
if(nrow(channel_dim_b)>0 & nrow(channel_dim_w)>0){channel_dim_both <- merge(channel_dim_w, channel_dim_b, all.x = TRUE, all.y = TRUE)
} else if (nrow(channel_dim_w)>0){channel_dim_both <-channel_dim_w
} else channel_dim_both <-channel_dim_b


#get Thalweg data
# get ThalwegStreambedParticles_B and Thalweg_W 
thalweg_stream_particles <- ReadTable(TableName = ThalwegSBP, EvaluationIDs = uniqueEvalIDs)
thalweg_w <- ReadTable(TableName = ThalwegW, EvaluationIDs = uniqueEvalIDs)
# combine the two thalweg data frame, combine these two tables, keep all common columns, if boatable doesnt exist use wadeable. if wadeable not in dataset use boatable
if(nrow(thalweg_stream_particles)>0 & nrow(thalweg_w)>0){thalweg_data <- merge(thalweg_w, thalweg_stream_particles, all.x = TRUE, all.y = TRUE)
} else if (nrow(thalweg_stream_particles)>0){thalweg_data<-thalweg_stream_particles
} else thalweg_data<-thalweg_w

# get thalweg spacing, number of measurements, and other reach level info
# this data is used to set  appropriate sample size criteria
# it could also be used to create longitudinal profile plots 
thalweg_set_up <- ReadTable(TableName = sampledReaches, EvaluationIDs = uniqueEvalIDs)
thalweg_set_up <- thalweg_set_up %>% select(c('EvaluationID','ProtocolReachLength','ThalwegSpacing','NumThalwegsPerTransect','FieldStatus'))

# year is needed to properly handle differences in how floodplain connectivity is measured at dry transects across the years
eval_dates <- ReadTable(TableName=sampledReaches, TableFields=c('EvaluationID','FieldEvalDate'))
#remove trailing zeroes that come in when pulling from the SDE
cols= "FieldEvalDate"
eval_dates[cols] <- lapply(eval_dates[cols], as.POSIXct, tz = "UTC")
eval_dates$Year <- format(eval_dates$FieldEvalDate, format="%Y")

# read in SlopePoolSummary_W
SlopeSummary <- ReadTable(TableName = SlopePoolSummary, EvaluationIDs = uniqueEvalIDs)
SlopeRaw<-ReadTable(TableName =slope_W,EvaluationIDs = uniqueEvalIDs)


##########################################################
#                Channel Dimensions                     #
##########################################################
# WettedWidthWithBarAvg (wetted width including any bars. This was originally called WettedWidthAvg. Changed name in 2023 and added WettedWidthAvg to mean without bars)
# WettedWidthAvg (wetted width without bar)
# EPA XWIDTH
# note the EPA combines all side channel, main and intermediate transects and then takes the max wetted width for a total of 10 values (excluding transect K?) instead of 21. This is likely an artifact of them adding intermediate transects halway through the project
# We are deviating from the EPA's method of calculating wetted width. We take the max value for main vs. side channels (should in theory always be main channel); then we average across all 21 values (main and intermediate)
# in 2019 we deviated from the above guidance and changed to take the sum at a given transect and then average across transects
# copy the channel dimensions data frame - this has both wadeable and boatable data
Widths <- data.frame(channel_dim_both)
# remove all NAs because na.rm within dplyr does not properly remove them. instead they are getting converted to 0s ...not sure why
Widths <-subset(Widths,is.na(Widths$WettedWidth)==FALSE)
#Create a column calculating the wetted width minus the bar width
Widths$WettedWidth_NoBar <- Widths$WettedWidth - Widths$BarWidth
# change all side channels transect letters to main transect letters so that we can sum across main and side channels below
Widths$Transect=plyr::mapvalues(Widths$Transect, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
# sum both variables (wetted width with and without) across side channels and main transects
WettedWidthSumTransect <- Widths %>% dplyr::group_by(EvaluationID, Transect) %>% dplyr::summarise_at(c("WettedWidth", "WettedWidth_NoBar"), sum)
# average all transects per EvaulationID and get sample size
DF_WettedWidth <- WettedWidthSumTransect %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(WettedWidthWithBarAvg_CHECK=mean(WettedWidth,na.rm=TRUE),WettedWidthAvg_CHECK=mean(WettedWidth_NoBar,na.rm=TRUE),nWettedWidthAvg_CHECK=n())
# if WettedWidthAvg is less than 9 (5 main transects and 4 intermediate) set it to NA
DF_WettedWidth$WettedWidthAvg_CHECK=ifelse(DF_WettedWidth$nWettedWidthAvg_CHECK<9,NA,DF_WettedWidth$WettedWidthAvg_CHECK)
# if WettedWidthWithBarAvg is less than 9 (5 main transects and 4 intermediate) set it to NA
DF_WettedWidth$WettedWidthWithBarAvg_CHECK=ifelse(DF_WettedWidth$nWettedWidthAvg_CHECK<9,NA,DF_WettedWidth$WettedWidthWithBarAvg_CHECK)
# round values to 2 digits
DF_WettedWidth$WettedWidthAvg_CHECK=round(DF_WettedWidth$WettedWidthAvg_CHECK,digits=2)
DF_WettedWidth$WettedWidthWithBarAvg_CHECK=round(DF_WettedWidth$WettedWidthWithBarAvg_CHECK,digits=2)
#write.csv(DF_WettedWidth,'DF_WettedWidth.csv')





#BankfullWidthAvg
# EPA XBKF_W
# changed all side channels to normal transects above already
# remove all NAs because na.rm within dplyr does not properly remove them. instead they are getting converted to 0s ...not sure why
BWidths <-subset(channel_dim_both,is.na(channel_dim_both$BankfullWidth)==FALSE)
# change all side channels transect letters to main transect letters so that we can sum across main and side channels below
BWidths$Transect=plyr::mapvalues(BWidths$Transect, c("XA", "XB","XC","XD","XE","XF","XG","XH","XI","XJ","XK" ),c("A", "B","C","D","E","F","G","H","I","J","K"))
# sum across side channels and main transects
BankfullWidSumTransect <- BWidths %>% dplyr::group_by(EvaluationID, Transect) %>% dplyr::summarize(sum_bankfull_width=sum(BankfullWidth))
#average all transects per EvaulationID and get sample size
DF_BankfullWidth <- BankfullWidSumTransect %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(BankfullWidthAvg_CHECK=mean(sum_bankfull_width,na.rm=TRUE), nBankfullWidthAvg_CHECK=n())
# if BankfullWidthAvg is less than 5 set it to NA
DF_BankfullWidth$BankfullWidthAvg_CHECK=ifelse(DF_BankfullWidth$nBankfullWidthAvg_CHECK<5,NA,DF_BankfullWidth$BankfullWidthAvg_CHECK)
# round values to 2 digits
DF_BankfullWidth$BankfullWidthAvg_CHECK=round(DF_BankfullWidth$BankfullWidthAvg_CHECK,digits=2)
#write.csv(DF_BankfullWidth,'DF_BankfullWidth.csv')




##########################################################
#          Floodplain Connectivty Indicators            #
##########################################################
# ChannelIncision, BenchHeightAvg, BankfullHeightAvg
# EPA LINCIS_H 
# 2019 changed to not collect this data at side channels in 2019  historic side channel data was permanently removed from the database so dont need to exclude here
# prior to 2019 we from took the lowest of the two sides if side channel data was present, note EPA takes max of the two sides
# I couldnt figure out how to remove NAs in a count verb so had to seperate bankfull and bench height into two dataframes and then apply summarize and count functions.
Bankfull <-channel_dim_both %>% select(c('EvaluationID','Transect','BankfullHeight'))
Bankfull <-subset(Bankfull, is.na(BankfullHeight)==FALSE)
Bench <-channel_dim_both %>% select(c('EvaluationID','Transect','BenchHeight'))
Bench <- subset(Bench, is.na(BenchHeight)==FALSE)
# compute means and sample sizes per evaluation id per indicator
AvgBankfullHt<- Bankfull %>% group_by(EvaluationID) %>% dplyr::summarize(BankfullHeightAvg_CHECK=mean(BankfullHeight,na.rm=TRUE),nBankfullHeight_CHECK=n())
AvgBenchHt <- Bench %>% group_by(EvaluationID) %>% dplyr::summarize(BenchHeightAvg_CHECK=mean(BenchHeight,na.rm=TRUE),nBenchHeight_CHECK=n())
# join bankfull and bench heights
DF_Heights <- merge(AvgBankfullHt, AvgBenchHt, by='EvaluationID', all=TRUE)
# if sample sizes are less than 5, set to NA
DF_Heights$BankfullHeightAvg_CHECK <- ifelse(DF_Heights$nBankfullHeight_CHECK<5, NA, DF_Heights$BankfullHeightAvg_CHECK)
DF_Heights$BenchHeightAvg_CHECK <- ifelse(DF_Heights$nBenchHeight_CHECK<5, NA, DF_Heights$BenchHeightAvg_CHECK)
#compute EPA's channel incision from mean bankfull and bench heights
# note that mathematically it does not matter whether you take the mean bankfull height and subtract it from the mean bench height or if you take the paired bankfull height and incision height differences and then calculate a mean if N is same
DF_Heights$ChannelIncision_CHECK <- round(log10(DF_Heights$BenchHeightAvg_CHECK-DF_Heights$BankfullHeightAvg_CHECK+0.1),digits=2)
DF_Heights$BankfullHeightAvg_CHECK <- round(DF_Heights$BankfullHeightAvg_CHECK, digits=2)
DF_Heights$BenchHeightAvg_CHECK <- round(DF_Heights$BenchHeightAvg_CHECK, digits=2)
#write.csv(DF_Heights,'DF_Heights.csv')


#FloodplainConnectivity 
# this is also known as Rosgen's bank height ratio
# bench and bankfull heights are taken from waters surface so we need to get thalweg data at a given transect to account for water depth and get a clear picture of channel dimensions
# isolate records with ThalwegMeasNum = 1 because these are only the measurements taken directly at the transect
thalweg_transect_data <- subset(thalweg_data,ThalwegMeasNum==1)
# starting in 2017 bankfull and incision heights measured from thalweg at dry transect but any data before that needs omitted if thalweg=0
# join in the year data
thalweg_transect_data <- left_join(thalweg_transect_data, eval_dates, by = 'EvaluationID')
# select all records where Year > 2016 or (Year < 2017 and ThalwegDepth > 0)
thalweg_transect_data_dry_adj <- subset(thalweg_transect_data, Year > 2016 | (Year < 2017 & ThalwegDepth > 0))
# join channel dimensions data to thalweg_data
thalweg_chan_dim <- merge(thalweg_transect_data_dry_adj, channel_dim_both, by=c('EvaluationID', 'Transect'))
# subset to only needed columns to make the data more manageable
thalweg_chan_dim <- dplyr::select(thalweg_chan_dim, c('EvaluationID','Transect','ThalwegDepth', 'BankfullHeight', 'BenchHeight'))
# subset data set so that it only has complete records that have bench height, bankfull height, and thalweg depth
transects_subset <- thalweg_chan_dim %>% subset(!is.na(ThalwegDepth) & !is.na(BankfullHeight) & !is.na(BenchHeight))
# compute the FloodplainConnectivity per Transect as the ratio of total bench height to total bankfull height 
transects_subset$BnkHeightRatio <- (transects_subset$BenchHeight + transects_subset$ThalwegDepth)/(transects_subset$BankfullHeight + transects_subset$ThalwegDepth)
# average across transects and get sample size
DF_FloodplainConnectivity <- transects_subset %>% group_by(EvaluationID) %>% dplyr::summarize(FloodplainConnectivity_CHECK=mean(BnkHeightRatio),nFloodplainConnectivity_CHECK=n())
#remove values where sample size is < 5 transects
DF_FloodplainConnectivity$FloodplainConnectivity_CHECK=ifelse(DF_FloodplainConnectivity$nFloodplainConnectivity_CHECK<5,NA,DF_FloodplainConnectivity$FloodplainConnectivity_CHECK)
#  round to two decimal places
DF_FloodplainConnectivity$FloodplainConnectivity_CHECK <- round(DF_FloodplainConnectivity$FloodplainConnectivity_CHECK, 2)
#write.csv(DF_FloodplainConnectivity,'DF_FloodplainConnectivity.csv')


# BankfullDepth and BenchDepth
# Bankfull Height +thalweg detpth; Bench Height +thalweg depth at each transect, then averaged across a reach

# Bench Depth
# isolate records with ThalwegMeasNum = 1 because these are only the measurements taken directly at the transect
thalweg_transect_data <- subset(thalweg_data,ThalwegMeasNum==1)
bench_transect_data<- channel_dim_both %>% select(c('EvaluationID','Transect','BenchHeight'))
Thalweg_plus_bench<-merge(thalweg_transect_data, bench_transect_data, by=c('EvaluationID', 'Transect')) %>% select(c('EvaluationID','Transect','BenchHeight','ThalwegDepth'))
# subset data set so that it only has complete records that have bench height and thalweg depth
t_ben_subset <- Thalweg_plus_bench %>% subset(!is.na(ThalwegDepth)  & !is.na(BenchHeight))
# Sum bench+thalweg  by transect, equals NA if either measurement not taken
t_ben_depths <- t_ben_subset %>% rowwise() %>% 
  dplyr::mutate(BenchDepth = sum(BenchHeight, ThalwegDepth) )
#Take the average for BenchDepth across all transects in a reach
Bench_Depths <- t_ben_depths  %>% dplyr::group_by(EvaluationID) %>% 
  dplyr::summarize(BenchDepthAvg_CHECK=mean(BenchDepth,na.rm=TRUE),nBenchDepthAvg_CHECK=n())
# if fewer than 5 transects collected, set to NA
Bench_Depths$BenchDepthAvg_CHECK=ifelse(Bench_Depths$nBenchDepthAvg_CHECK<5,NA,Bench_Depths$BenchDepthAvg_CHECK)
#round to 2 decimal places
Bench_Depths$BenchDepthAvg_CHECK <- round(Bench_Depths$BenchDepthAvg_CHECK, digits=2)


# Bankfull Depth
# isolate records with ThalwegMeasNum = 1 because these are only the measurements taken directly at the transect
thalweg_transect_data <- subset(thalweg_data,ThalwegMeasNum==1)
Bankfull_transect_data<- channel_dim_both %>% select(c('EvaluationID','Transect','BankfullHeight'))
Thalweg_plus_Bankfull<-merge(thalweg_transect_data, Bankfull_transect_data, by=c('EvaluationID', 'Transect')) %>% select(c('EvaluationID','Transect','BankfullHeight','ThalwegDepth'))
# subset data set so that it only has complete records that have Bankfull height and thalweg depth
t_bank_subset <- Thalweg_plus_Bankfull %>% subset(!is.na(ThalwegDepth)  & !is.na(BankfullHeight))
# Sum Bankfull+thalweg  by transect, equals NA if either measurement not taken
t_bank_depths <- t_bank_subset %>% rowwise() %>% 
  dplyr::mutate(BankfullDepth = sum(BankfullHeight, ThalwegDepth) )
#Take the average for BankfullDepth across all transects in a reach
Bankfull_Depths <- t_bank_depths  %>% dplyr::group_by(EvaluationID) %>% 
  dplyr::summarize(BankfullDepthAvg_CHECK=mean(BankfullDepth,na.rm=TRUE),nBankfullDepthAvg_CHECK=n())
# if fewer than 5 transects collected, set to NA
Bankfull_Depths$BankfullDepthAvg_CHECK=ifelse(Bankfull_Depths$nBankfullDepthAvg_CHECK<5,NA,Bankfull_Depths$BankfullDepthAvg_CHECK)
# round to 2 decimal places
Bankfull_Depths$BankfullDepthAvg_CHECK <- round(Bankfull_Depths$BankfullDepthAvg_CHECK, digits=2)

# merge two depth files together
DF_Depths <- merge(Bankfull_Depths, Bench_Depths, by="EvaluationID")

##########################################################
#         Thalweg Mean, CV and Pct Dry  Indicators       #
##########################################################
# Thalweg Mean and CV
# Compute the mean and standard deviation of thalweg depth per EvaluationID also get sample size
thalweg_depths_no_na <- subset(thalweg_data, !is.na(ThalwegDepth))
thalweg_depth <- thalweg_depths_no_na %>% group_by(EvaluationID) %>% dplyr::summarize(ThalwegDepthAvg_CHECK=mean(ThalwegDepth),ThalwegDepth_SD=sd(ThalwegDepth),Num_Thalweg_Depths=n()) 
# Compute the ThalwegDepthCV and round appropriately
thalweg_depth$ThalwegDepthCV_CHECK <- thalweg_depth$ThalwegDepth_SD/thalweg_depth$ThalwegDepthAvg_CHECK
thalweg_depth$ThalwegDepthAvg_CHECK <- round(thalweg_depth$ThalwegDepthAvg_CHECK, 2)
thalweg_depth$ThalwegDepthCV_CHECK <- round(thalweg_depth$ThalwegDepthCV_CHECK, 2)

#Pct Dry
# Get the number of thalweg depths that = 0 per EvaluationID
# decided to use number of thalweg depth=0 because more spatially explict...this means this indicator can't be calc if thalweg not collected
thalweg_depths_zero <- subset(thalweg_data, ThalwegDepth==0)
thalweg_dzero_count <- thalweg_depths_zero %>% dplyr::count(EvaluationID,name='Zero_Count')
# join that number into the main data set if thalweg 
thalweg_depth <- left_join(thalweg_depth, thalweg_dzero_count, by = 'EvaluationID')
# if no zero depths set to 0
thalweg_depth$Zero_Count <- ifelse(is.na(thalweg_depth$Zero_Count), 0, thalweg_depth$Zero_Count)
# Compute Percent Dry
thalweg_depth$PctDry_CHECK <- (thalweg_depth$Zero_Count/thalweg_depth$Num_Thalweg_Depths) * 100
thalweg_depth$PctDry_CHECK <- round(thalweg_depth$PctDry_CHECK, 1)
# merge with expected sample sizes and compute percent complete
DF_Thalweg<- merge(thalweg_depth, thalweg_set_up, by = 'EvaluationID')
DF_Thalweg$PercentComplete <- (DF_Thalweg$Num_Thalweg_Depths/(DF_Thalweg$NumThalwegsPerTransect * 10)) * 100
# if partial site must have more than 40% data if full site must have more than 80%
DF_Thalweg$ThalwegDepthAvg_CHECK <- ifelse(DF_Thalweg$FieldStatus %in% c('Partial Reach','Interrupted Flow and Partial Reach'),
                                          ifelse(DF_Thalweg$PercentComplete<40|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$ThalwegDepthAvg_CHECK),
                                          ifelse(DF_Thalweg$PercentComplete<80|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$ThalwegDepthAvg_CHECK))#changed from 100 to 80 in 2019
DF_Thalweg$ThalwegDepthCV_CHECK <- ifelse(DF_Thalweg$FieldStatus %in% c('Partial Reach','Interrupted Flow and Partial Reach'),
                                         ifelse(DF_Thalweg$PercentComplete<40|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$ThalwegDepthCV_CHECK),
                                         ifelse(DF_Thalweg$PercentComplete<80|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$ThalwegDepthCV_CHECK))#changed from 100 to 80 in 2019
# omit CV if there are any dry thalwegs
DF_Thalweg$ThalwegDepthCV_CHECK <- ifelse(DF_Thalweg$PctDry_CHECK>0,NA,DF_Thalweg$ThalwegDepthCV_CHECK)
# if partial site must have more than 40% data if full site must have more than 80%
DF_Thalweg$PctDry_CHECK <- ifelse(DF_Thalweg$FieldStatus %in% c('Partial Reach','Interrupted Flow and Partial Reach'),
                                 ifelse(DF_Thalweg$PercentComplete<40|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$PctDry_CHECK),
                                 ifelse(DF_Thalweg$PercentComplete<80|DF_Thalweg$NumThalwegsPerTransect==1,NA,DF_Thalweg$PctDry_CHECK))#changed from 100 to 80 in 2019
#write.csv(DF_Thalweg,'DF_Thalweg.csv')

#################################################
#                 Pct Slope                     #
#################################################
# recalculate the elevation change for each shot in case data has been edited
SlopeRaw$ElevationChangeRecalc=SlopeRaw$StartHeight-SlopeRaw$EndHeight
SlopeRaw$ElevationChangeRecalc=ifelse(is.na(SlopeRaw$ElevationChangeRecalc)==TRUE,SlopeRaw$ElevationChange,SlopeRaw$ElevationChangeRecalc)
# recalculate the total elevation change for each pass by summing all shots and taking the absolute value of that sum
slopeavg<- SlopeRaw %>% dplyr::group_by(EvaluationID, Pass) %>% dplyr::summarize(TotalElevationChange=abs(sum(ElevationChangeRecalc)))
# pivot the passes into columns so that the calculations below are easier
slopeavg_wide<-tidyr::pivot_wider(slopeavg,names_from=Pass,values_from=TotalElevationChange, names_prefix="TotalElevationChangePassRecalc")
# create columns with 1 (within 10%) or 0 (not within 10%) to designate which passes are within 10% of one another
slopeavg_wide$is1within10of2=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc2-slopeavg_wide$TotalElevationChangePassRecalc1)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc1==0,1,slopeavg_wide$TotalElevationChangePassRecalc1)<=.10,1,0)
slopeavg_wide$is3within10of1=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc3-slopeavg_wide$TotalElevationChangePassRecalc1)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc1==0,1,slopeavg_wide$TotalElevationChangePassRecalc1)<=.10,1,0)
slopeavg_wide$is3within10of2=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc3-slopeavg_wide$TotalElevationChangePassRecalc2)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc2==0,1,slopeavg_wide$TotalElevationChangePassRecalc2)<=.10,1,0)
# create columns designating which passes are within 20% of one another
slopeavg_wide$is1within20of2=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc2-slopeavg_wide$TotalElevationChangePassRecalc1)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc1==0,1,slopeavg_wide$TotalElevationChangePassRecalc1)<=.20,1,0)
slopeavg_wide$is3within20of1=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc3-slopeavg_wide$TotalElevationChangePassRecalc1)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc1==0,1,slopeavg_wide$TotalElevationChangePassRecalc1)<=.20,1,0)
slopeavg_wide$is3within20of2=ifelse(abs(slopeavg_wide$TotalElevationChangePassRecalc3-slopeavg_wide$TotalElevationChangePassRecalc2)/ifelse(slopeavg_wide$TotalElevationChangePassRecalc2==0,1,slopeavg_wide$TotalElevationChangePassRecalc2)<=.20,1,0)

# average the passes that are within 10% of one another. If only one pass is present use that pass, if no passes within 10% NA
slopeavg_wide$AvgTotalElevationChangeRecalc=ifelse(is.na(slopeavg_wide$TotalElevationChangePassRecalc1)==FALSE & is.na(slopeavg_wide$TotalElevationChangePassRecalc2)==TRUE & is.na(slopeavg_wide$TotalElevationChangePassRecalc3)==TRUE,
                                                   slopeavg_wide$TotalElevationChangePassRecalc1,
                                                   ifelse(slopeavg_wide$is1within10of2==1,rowMeans(slopeavg_wide[,c(2,3)]),
                                                          ifelse(slopeavg_wide$is3within10of1==1 & slopeavg_wide$is3within10of2==1,rowMeans(slopeavg_wide[,c(2,3,4)]),
                                                                 ifelse(slopeavg_wide$is3within10of2==1, rowMeans(slopeavg_wide[,c(3,4)]),
                                                                        ifelse(slopeavg_wide$is3within10of1==1, rowMeans(slopeavg_wide[,c(2,4)]), NA)))))
# if no passes were within 10% see if any where within 20% and if so average those, if not leave as NA
slopeavg_wide$AvgTotalElevationChangeRecalc=ifelse(is.na(slopeavg_wide$AvgTotalElevationChangeRecalc)==TRUE,
                                                   ifelse(slopeavg_wide$is1within20of2==1,rowMeans(slopeavg_wide[,c(2,3)]),
                                                          ifelse(slopeavg_wide$is3within20of1==1 & slopeavg_wide$is3within20of2==1,rowMeans(slopeavg_wide[,c(2,3,4)]),
                                                                 ifelse(slopeavg_wide$is3within20of2==1, rowMeans(slopeavg_wide[,c(3,4)]),
                                                                        ifelse(slopeavg_wide$is3within20of1==1, rowMeans(slopeavg_wide[,c(2,4)]), NA)))),
                                                   slopeavg_wide$AvgTotalElevationChangeRecalc)

# join to summary table to get slope reach length
DF_Slope<- dplyr::left_join(SlopeSummary,slopeavg_wide, by='EvaluationID')                                     
# To DO decide if we should remove % slope from table and just calculate here. Need to QC historic data first
# names(slopeavg_wide)[names(slopeavg_wide) == 'PctSlope']<- "PctSlope_CHECK" 
# slopeavg_wide$PctSlope_CHECK=round(slopeavg_wide$PctSlope_CHECK, digits = 2)
# calculate slope in one line here if not just pulling from the database
DF_Slope$PctSlope_CHECK=round(DF_Slope$AvgTotalElevationChangeRecalc/DF_Slope$SlopeReachLength*100,digits=2)
