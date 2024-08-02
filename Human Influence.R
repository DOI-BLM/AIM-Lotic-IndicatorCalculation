##########################################################################################################
##### Other metrics still being worked on or old metrics no longer used #####
##########################################################################################################
#read in HumanInfluence_W_B
Human_Influ <- ReadTable(TableName = HumanInfluence, EvaluationIDs = uniqueEvalIDs)


####################################################################################################
# Get list of all human influence types present and populate HUmanInfluence indicator with that list

human=tidyr::pivot_longer(Human_Influ,cols=Buildings:Fire,names_to=c('Type'),values_to='Present')
#convert data to presence absence rather than proximity
human$Present=ifelse(human$Present=='Absent',0,1)

#subset to get sites with some human influence to summarize those influences
humanSum<- human %>% 
  dplyr::group_by(EvaluationID, Type) %>% 
  dplyr::summarize(numBanks=sum(Present,na.rm=TRUE)) %>% 
  filter(numBanks>0)
#get list of all human influences for each site in one cell
humanSum1<-humanSum %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(HumanInfluence=paste0(list(unique(Type)),collapse=''))

#set sites with no human influence to "None"
humanSum2<-human %>% 
  dplyr::group_by(EvaluationID) %>%
  dplyr::summarize(HumanInfluence=sum(Present,na.rm=TRUE)) %>%
  dplyr::filter(HumanInfluence==0)%>%
  dplyr::mutate(HumanInfluence=if_else(HumanInfluence==0,"None",""))

#merge all sites together
DF_HumanInfluence=rbind(humanSum1,humanSum2)
DF_HumanInfluence$HumanInfluence_CHECK=DF_HumanInfluence$HumanInfluence
#remove string concatenation formatting; note " and () are special characters so they have to be escaped using \ and () has to be double escaped \\
DF_HumanInfluence$HumanInfluence_CHECK=gsub("^c()","",DF_HumanInfluence$HumanInfluence_CHECK)
DF_HumanInfluence$HumanInfluence_CHECK=gsub("\"","",DF_HumanInfluence$HumanInfluence_CHECK)
DF_HumanInfluence$HumanInfluence_CHECK=gsub("\\(","",DF_HumanInfluence$HumanInfluence_CHECK)
DF_HumanInfluence$HumanInfluence_CHECK=gsub("\\)","",DF_HumanInfluence$HumanInfluence_CHECK)



#EPA W1_HALL

# EPA Proximity weighted disturbance indicator. the closer to the stream the higher the weight
# values are averaged within a disturbance types and then summed across types
# only a subset of distrubance fields are used below to match EPA but BLM may want to use all disturbance types collected
# NRSA values for Present are used below
# Reclass values so text values are changed to numeric values
# Be careful EMAP_WEST uses Present=0.667 according to documenation,or 0.6667 in the aquamet code says 0.6667.
# if there ends up being a lot of P's in the data this makes a difference!!! 

recode_human_val <- function(the_val){result <- ifelse(the_val=="Within Streambed", 1.5, ifelse(the_val=="Contained In Plot", 1.0, ifelse(the_val=="Present Outside Plot", 0.5, 0.0)))}
#select only EPA fields
mutate_fields <- c('Buildings', 'LoggingOperations', 'Mining', 'ParksLawns', 'PastureHayFence', 'PavementClearedLot', 'Pipes', 'RoadRailroadCulvert', 'RowCrops', 'LandfillTrash', 'WallDikeRipRap')
#change values from categories to weights
Human_Influ_val <- Human_Influ %>% dplyr::mutate_at(mutate_fields, recode_human_val)
#add evaluationID back in
Human_Influ_table <- data.frame('EvaluationID'=Human_Influ_val['EvaluationID'], Human_Influ_val %>% select(all_of(mutate_fields)))
# collapse data table so it is easier to work with
long_Human_Influ<- tidyr::pivot_longer(Human_Influ_table,!EvaluationID,names_to=c('Type'))
# compute the means per EvaluationID and disturbance type
Human_Influence_means <- long_Human_Influ %>% dplyr::group_by(EvaluationID, Type) %>% dplyr::summarize(avgvalue=mean(value, na.rm=TRUE))
# sum across disturbance types
Human_Influence_Sum<-  Human_Influence_means %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(W1_HALL_CHECK=mean(avgvalue,na.rm=TRUE))


# compute the number of samples per Evaluation ID - not sure if this will be needed
HumanInfl_row_sample_counts <- data.frame(EvaluationID=Human_Influ['EvaluationID'], NumSamples=rowSums(!is.na(Human_Influ %>% select(all_of(mutate_fields)))))
Human_Infl_samples_per_eval <- HumanInfl_row_sample_counts %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(nW1_HALL_CHECK=sum(NumSamples))

DF_HumanInfluence<-purrr::reduce(list(DF_HumanInfluence,Human_Influence_Sum,Human_Infl_samples_per_eval),by='EvaluationID',left_join)

# 11 human influence types collected at 2 banks and 5 transects =110


