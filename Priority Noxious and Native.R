##################################################################
# Get Priority Native and Noxious Frequency of Occurence Data    #
##################################################################

# need lat and long because final product is a spatial feature class
latlong  <-ReadTable(TableName = sampledReaches, TableFields=c('EvaluationID','SampledMidLatitude','SampledMidLongitude'), EvaluationIDs =uniqueEvalIDs)

#read in VegSpecies_W_B table which has species plot observations
VegSpec_WB <- ReadTable(TableName = vegSpeciesWB, EvaluationIDs = uniqueEvalIDs)

# Replace 2022 verbiage with 2021 verbiage so code works
VegSpec_WB$CommonName[VegSpec_WB$CommonName== "no noxious sedge rush equisetum"]<-"no sedge or rush or equisetum and no noxious woody or noxious herbaceous" 

# read in species metadata list lookup tables
LU_SpeciesMetadata <- ReadTable(TableName = speciesMetadata, EvaluationIDs = uniqueEvalIDs)
LU_StateSpeciesList <- ReadTable(TableName = stateSpeciesList, EvaluationIDs = uniqueEvalIDs)
    #messy 2021 import fixes
    VegSpec_WB$CommonName<- gsub("_"," ",VegSpec_WB$CommonName)
    LU_SpeciesMetadata$WoodyHerb<-gsub("herb ","herb",LU_SpeciesMetadata$WoodyHerb)
#then join two tables together to get noxious info and get list of all species looked for in a given year
species<- dplyr::left_join(LU_SpeciesMetadata,LU_StateSpeciesList, by="CommonName")

# reduce the LU_SpeciesMetadata table so that only one row per CommonName is present
#### note if a species is noxious in one state and not another this should create 2 records here but will likely cause issues later in the script, will need to deal with this when it arises
speciessub <- species[,c('CommonName', 'ScientificName', 'Noxious', 'WoodyHerb')]
LU_SpeciesUnique <- speciessub[!duplicated(speciessub), ]

VegSpec_WB <- dplyr::left_join(VegSpec_WB, LU_SpeciesUnique, by = c('CommonName'))

###############################################################
#                     Calc Veg Indicators                     #
###############################################################

##### General Approach ######

## % noxious woody, % noxious herb, % native woody, % sedge rush, % equisetum

#  1.	Count number of banks with noxious woody etc.
#        Subset dataset so that only noxious woody (etc.) species are present then sum all species at a given bank. If result >1 change to 1. Then sum across banks.
#  2.	Get total number of banks assessed for
#       2021 and on- 
#         Count total number of unique banks present in the dataset
#             “No noxious or sedge rush or equisetum” species records are needed so that a bank appears in the dataset
#             	Native woody are treated differently. No other categories are lumped in with it so the 2020-2019 approach below works fine for it.
#                 Additionally, it must be treated this way because native woody is not always collected if noxious is. Only rows with “no native woody” present or native woody species should be included in the calc. 
#       2020-2019 
#       	Cant handle the same as 2021 because we can’t assume that if native were collected that noxious were or that sedge rush were
#        	If “no noxious woody” or “no noxious herb” etc. change data to 0 and then count the number of nonnull banks to get total number of banks 
#             Cant do this for 2021 data because a bank might have had sedge rush present but no noxious. 
#                 If treated this way then the bank would not get counted as sampled.
#       -   note that in 2021 we recalculated historic indicator values to not be based on presence absences but rather based on actual species info. 
#       -  For example If noxious woody were recorded as present but no specific noxious woody species were recorded. this bank would be treated as not collected
#       --also note that for 2020 and 2019 data the code that calculated total number of banks for 2021 needs commented out and the line above it needs uncommented to properly get total number of banks for 2019-2020
#       2016-2018
#           kept raw presence absence data in database but removed indicators from indcator table and no code below exists to calculate these indicators
#  3. Calculate % by dividing values from 1 by values from 2 and multiply by 100
#  4. Joint the 5 sampleframes and modify data by year where data were not collected (0 to NA)

############################
# Create columns that records if veg species of interest was recorded.
# create a new column that contains a 1 if the species recorded was noxious woody
VegSpec_WB$Nox_Woody_Count <-ifelse(VegSpec_WB$Noxious=='Yes' & VegSpec_WB$WoodyHerb=='woody',1, NA)
# change NA values to 0 if the bank was assessed but no noxious woody found
# only needed for 2019 and 2020 data so that the total number of banks is counted correctly. it likely is NOT correct for 2021 do to how categories are combined but added 2021 veribage in as testing code
VegSpec_WB$Nox_Woody_Count <-ifelse(VegSpec_WB$CommonName=='no noxious woody'|VegSpec_WB$CommonName=='no sedge or rush or equisetum and no noxious woody or noxious herbaceous',0,VegSpec_WB$Nox_Woody_Count)
# create a new column that contains a 1 if the species recorded was native woody
VegSpec_WB$Nat_Woody_Count <-ifelse(VegSpec_WB$Noxious=='No' & VegSpec_WB$WoodyHerb=='woody',1, NA)
# change NA values to 0 if the bank was assessed but no noxious woody found
# need for BOTH 2021 and historic data!!! (see general approach above)
VegSpec_WB$Nat_Woody_Count <-ifelse(VegSpec_WB$CommonName=='no native woody',0,VegSpec_WB$Nat_Woody_Count)
VegSpec_WB$Nox_Herb_Count <-ifelse(VegSpec_WB$Noxious=='Yes' & VegSpec_WB$WoodyHerb=='herb',1, NA)
# change NA values to 0 if the bank was assessed but no noxious herb found
# only needed for 2019 and 2020 data so that the total number of banks is counted correctly. it likely is NOT correct for 2021 do to how categories are combined but added 2021 veribage in as testing code
VegSpec_WB$Nox_Herb_Count <-ifelse(VegSpec_WB$CommonName=='no noxious herbaceous'|VegSpec_WB$CommonName=='no sedge or rush or equisetum and no noxious woody or noxious herbaceous',0,VegSpec_WB$Nox_Herb_Count)
VegSpec_WB$Sedge_Rush_Count <-ifelse(VegSpec_WB$CommonName=='sedge or rush',1, NA)
# change NA values to 0 if the bank was assessed but no sedge rush found
# only needed for 2019 and 2020 data so that the total number of banks is counted correctly. This line is not needed for 2021 data and MAY NOT be correct for 2021 because of combining it with equisteum 
VegSpec_WB$Sedge_Rush_Count <-ifelse(VegSpec_WB$CommonName %in% c('no sedge or rush','no sedge rush','no sedge or rush or equisetum and no noxious woody or noxious herbaceous','equisetum'),0,VegSpec_WB$Sedge_Rush_Count)
# create a new column that contains a 1 if the species recorded was equisetum
VegSpec_WB$Equisetum_Count <-ifelse(VegSpec_WB$CommonName=='equisetum',1, NA)
# change NA values to 0 if the bank was assessed but no equisetum found
# only needed for 2019 and 2020 data so that the total number of banks is counted correctly. This line is not needed for 2021 data and MAY NOT be correct for 2021 because of combining it with sedge rush 
VegSpec_WB$Equisetum_Count <-ifelse(VegSpec_WB$CommonName %in% c('no sedge or rush or equisetum and no noxious woody or noxious herbaceous','sedge or rush'),0,VegSpec_WB$Equisetum_Count)

###################EDITS
# Add in 0s for noxious woody if currently NA and veg was collected. Be careful with data before 2020. Equisetum was not collected in those years.
VegSpec_WB$Nox_Herb_Count <-ifelse(is.na(VegSpec_WB$Nox_Herb_Count) & VegSpec_WB$Sedge_Rush_Count>=0,0,VegSpec_WB$Nox_Herb_Count)
VegSpec_WB$Nox_Herb_Count <-ifelse(is.na(VegSpec_WB$Nox_Herb_Count) & VegSpec_WB$Equisetum_Count>=0,0,VegSpec_WB$Nox_Herb_Count)
VegSpec_WB$Nox_Herb_Count <-ifelse(is.na(VegSpec_WB$Nox_Herb_Count) & VegSpec_WB$Nox_Woody_Count>=0,0,VegSpec_WB$Nox_Herb_Count)
VegSpec_WB$Nox_Herb_Count <-ifelse(is.na(VegSpec_WB$Nox_Herb_Count) & VegSpec_WB$Nat_Woody_Count>=0,0,VegSpec_WB$Nox_Herb_Count)

VegSpec_WB$Nox_Woody_Count <-ifelse(is.na(VegSpec_WB$Nox_Woody_Count) & VegSpec_WB$Sedge_Rush_Count>=0,0,VegSpec_WB$Nox_Woody_Count)
VegSpec_WB$Nox_Woody_Count <-ifelse(is.na(VegSpec_WB$Nox_Woody_Count) & VegSpec_WB$Equisetum_Count>=0,0,VegSpec_WB$Nox_Woody_Count)
VegSpec_WB$Nox_Woody_Count <-ifelse(is.na(VegSpec_WB$Nox_Woody_Count) & VegSpec_WB$Nox_Herb_Count>=0,0,VegSpec_WB$Nox_Woody_Count)
VegSpec_WB$Nox_Woody_Count <-ifelse(is.na(VegSpec_WB$Nox_Woody_Count) & VegSpec_WB$Nat_Woody_Count>=0,0,VegSpec_WB$Nox_Woody_Count)

VegSpec_WB$Sedge_Rush_Count <-ifelse(is.na(VegSpec_WB$Sedge_Rush_Count) & VegSpec_WB$Nox_Woody_Count>=0,0,VegSpec_WB$Sedge_Rush_Count)
VegSpec_WB$Sedge_Rush_Count <-ifelse(is.na(VegSpec_WB$Sedge_Rush_Count) & VegSpec_WB$Equisetum_Count>=0,0,VegSpec_WB$Sedge_Rush_Count)
VegSpec_WB$Sedge_Rush_Count <-ifelse(is.na(VegSpec_WB$Sedge_Rush_Count) & VegSpec_WB$Nox_Herb_Count>=0,0,VegSpec_WB$Sedge_Rush_Count)
VegSpec_WB$Sedge_Rush_Count <-ifelse(is.na(VegSpec_WB$Sedge_Rush_Count) & VegSpec_WB$Nat_Woody_Count>=0,0,VegSpec_WB$Sedge_Rush_Count)

VegSpec_WB$Equisetum_Count <-ifelse(is.na(VegSpec_WB$Equisetum_Count) & VegSpec_WB$Nox_Woody_Count>=0,0,VegSpec_WB$Equisetum_Count)
VegSpec_WB$Equisetum_Count <-ifelse(is.na(VegSpec_WB$Equisetum_Count) & VegSpec_WB$Sedge_Rush_Count>=0,0,VegSpec_WB$Equisetum_Count)
VegSpec_WB$Equisetum_Count <-ifelse(is.na(VegSpec_WB$Equisetum_Count) & VegSpec_WB$Nox_Herb_Count>=0,0,VegSpec_WB$Equisetum_Count)
VegSpec_WB$Equisetum_Count <-ifelse(is.na(VegSpec_WB$Equisetum_Count) & VegSpec_WB$Nat_Woody_Count>=0,0,VegSpec_WB$Equisetum_Count)



# ----- PctNoxiousWoody ------

### 1. count number of banks with noxious woody

# remove all rows that are now left as NA
Nox_Woody<-VegSpec_WB[is.na(VegSpec_WB$Nox_Woody_Count)==FALSE,]
# sum per bank 
Nox_Woody_Bank<-aggregate(Nox_Woody_Count~ EvaluationID+Transect+Bank, data=Nox_Woody, FUN=sum, na.rm=TRUE)
# if >1 set=1 
Nox_Woody_Bank$Nox_Woody_Count=ifelse(Nox_Woody_Bank$Nox_Woody_Count>1,1,Nox_Woody_Bank$Nox_Woody_Count)
# sum across banks
Nox_Woody_Reach <- aggregate(Nox_Woody_Count~ EvaluationID, data=Nox_Woody_Bank, FUN=sum, na.rm=TRUE)

### 2. count total number of banks and reuse this field below for other indicators
# TO DO add year above and then turn this into an if statement so dont have to uncomment out? but wait till calculations are changed to deal with historic presence absence data
#if after 2021
# remove any not collected records if these accidently got ingested (should not be ingested at all!!)
Sampled_Banks<-subset(VegSpec_WB,CommonName!="Not Collected")
# subset to only 3 columns
Sampled_Banks<-VegSpec_WB[,c("EvaluationID","Transect","Bank")]
# remove multiple rows per bank if multiple species recorded per bank
Unique_Sampled_Banks<-Sampled_Banks[!duplicated(Sampled_Banks),]

### sum change object to specific named object for this indicator
### would ideally keep generic but data prior to 2021 needs to be specific so leave specific object for ease of switching between years

#uncomment out first line and comment out second line if prior to 2021 
#Tot_Nox_Woody_Count <- Nox_Woody_Bank %>% dplyr::count(EvaluationID, name='Tot_Nox_Woody')
Tot_Nox_Woody_Count <- Unique_Sampled_Banks %>% dplyr::count(EvaluationID, name='Tot_Nox_Woody')

### 3. Calc %
# join the counts together
Pct_Nox_Woody <- dplyr::left_join(Nox_Woody_Reach, Tot_Nox_Woody_Count, by = 'EvaluationID')
# compute Nox_Woody count/Tot_Nox_Woody count * 100 = PctNoxiousWoody
# dont calculate for visits with less than min sample size
# min sample size is 2 banks recorded at 5 transects so must be >=10
Pct_Nox_Woody$PctNoxiousWoodySpecies_CHECK <- ifelse(Pct_Nox_Woody$Tot_Nox_Woody>9,round((Pct_Nox_Woody$Nox_Woody_Count / Pct_Nox_Woody$Tot_Nox_Woody) * 100, 0),NA)


# ------ PctNativeWoody ------
### 1. count number of banks with noxious woody
# remove all rows that are now left as NA
Nat_Woody<-VegSpec_WB[is.na(VegSpec_WB$Nat_Woody_Count)==FALSE,]
# sum per bank
Nat_Woody_Bank<-aggregate(Nat_Woody_Count~ EvaluationID+Transect+Bank, data=Nat_Woody, FUN=sum, na.rm=TRUE)
# if >1 set to 1
Nat_Woody_Bank$Nat_Woody_Count=ifelse(Nat_Woody_Bank$Nat_Woody_Count>1,1,Nat_Woody_Bank$Nat_Woody_Count)
# sum across banks
Nat_Woody_Reach <- aggregate(Nat_Woody_Count~ EvaluationID, data=Nat_Woody_Bank, FUN=sum, na.rm=TRUE)

### 2. count total number of banks by counting all nonull records from step 1
# see general approach above but this step is different than rest of indicators because of how the survey for is designed and because this is a contingent indicator!!!
Tot_Nat_Woody_Count <- Nat_Woody_Bank %>% dplyr::count(EvaluationID, name='Tot_Nat_Woody')

### 3. Calc %
# join the counts together
Pct_Nat_Woody <- dplyr::left_join(Nat_Woody_Reach, Tot_Nat_Woody_Count, by = 'EvaluationID')
# compute Nat_Woody count/Tot_Nat_Woody count * 100 = PctNativeWoody
# dont calculate for visits with less than min sample size
# min sample size is 2 banks recorded at 5 transects so must be >=10
Pct_Nat_Woody$PctNativeWoodySpecies_CHECK <- ifelse(Pct_Nat_Woody$Tot_Nat_Woody>9,round((Pct_Nat_Woody$Nat_Woody_Count / Pct_Nat_Woody$Tot_Nat_Woody) * 100, 0),NA)


# ------ PctNoxiousHerb ------
### 1. count number of banks with noxious herb
# remove all rows that are now left as NA
Nox_Herb<-VegSpec_WB[is.na(VegSpec_WB$Nox_Herb_Count)==FALSE,]
# sum per bank 
Nox_Herb_Bank<-aggregate(Nox_Herb_Count~ EvaluationID+Transect+Bank, data=Nox_Herb, FUN=sum, na.rm=TRUE)
# if >1 set=1
Nox_Herb_Bank$Nox_Herb_Count=ifelse(Nox_Herb_Bank$Nox_Herb_Count>1,1,Nox_Herb_Bank$Nox_Herb_Count)
# sum across banks
Nox_Herb_Reach <- aggregate(Nox_Herb_Count~ EvaluationID, data=Nox_Herb_Bank, FUN=sum, na.rm=TRUE)

### 2. count total number of banks and using unique sampled banks object created in pctnoxious woody
#uncomment out first line and comment out second line if prior to 2021 
#Tot_Nox_Herb_Count <- Nox_Herb_Bank %>% dplyr::count(EvaluationID, name='Tot_Nox_Herb')
Tot_Nox_Herb_Count <- Unique_Sampled_Banks %>% dplyr::count(EvaluationID, name='Tot_Nox_Herb')

### 3. Calc %
# join the counts together
Pct_Nox_Herb <- dplyr::left_join(Nox_Herb_Reach, Tot_Nox_Herb_Count, by = 'EvaluationID')
# compute Nox_Herb count/Tot_Nox_Herb count * 100 = PctNoxiousHerb
# dont calculate for visits with less than min sample size
# min sample size is 2 banks recorded at 5 transects so must be >=10
Pct_Nox_Herb$PctNoxiousHerbSpecies_CHECK <- ifelse(Pct_Nox_Herb$Tot_Nox_Herb>9,round((Pct_Nox_Herb$Nox_Herb_Count / Pct_Nox_Herb$Tot_Nox_Herb) * 100, 0),NA)



# ------- PctSedgeRush -------
### 1. count number of banks with sedge rush
# remove all rows that are now left as NA
Sedge_Rush<-VegSpec_WB[is.na(VegSpec_WB$Sedge_Rush_Count)==FALSE,]
# sum per bank 
Sedge_Rush_Bank<-aggregate(Sedge_Rush_Count~ EvaluationID+Transect+Bank, data=Sedge_Rush, FUN=sum, na.rm=TRUE)
# if >1 set=1
Sedge_Rush_Bank$Sedge_Rush_Count=ifelse(Sedge_Rush_Bank$Sedge_Rush_Count>1,1,Sedge_Rush_Bank$Sedge_Rush_Count)
# sum across banks
Sedge_Rush_Reach <- aggregate(Sedge_Rush_Count~ EvaluationID, data=Sedge_Rush_Bank, FUN=sum, na.rm=TRUE)

### 2. count total number of banks and using unique sampled banks object created in pctnoxious woody
#uncomment out first line and comment out second line if prior to 2021 
#Tot_Sedge_Rush_Count <- Sedge_Rush_Bank %>% dplyr::count(EvaluationID, name='Tot_Sedge_Rush')
Tot_Sedge_Rush_Count <- Unique_Sampled_Banks %>% dplyr::count(EvaluationID, name='Tot_Sedge_Rush')

### 3. Calc %
# join the counts together
Pct_Sedge_Rush <- dplyr::left_join(Sedge_Rush_Reach, Tot_Sedge_Rush_Count, by = 'EvaluationID')
# compute Sedge_Rush count/Tot_Sedge_Rush count * 100 = PctSedgeRush
# dont calculate for visits with less than min sample size
# min sample size is 2 banks recorded at 5 transects so must be >=10
Pct_Sedge_Rush$PctSedgeRushSpecies_CHECK <- ifelse(Pct_Sedge_Rush$Tot_Sedge_Rush>9,round((Pct_Sedge_Rush$Sedge_Rush_Count / Pct_Sedge_Rush$Tot_Sedge_Rush) * 100, 0),NA)



# ------- PctEquisetum -------
### 1. count number of banks with equisetum
# remove all rows that are now left as NA
Equisetum<-VegSpec_WB[is.na(VegSpec_WB$Equisetum_Count)==FALSE,]
# sum per bank
Equisetum_Bank<-aggregate(Equisetum_Count~ EvaluationID+Transect+Bank, data=Equisetum, FUN=sum, na.rm=TRUE)
# if >1 set=1
Equisetum_Bank$Equisetum_Count=ifelse(Equisetum_Bank$Equisetum_Count>1,1,Equisetum_Bank$Equisetum_Count)
# sum across banks
Equisetum_Reach <- aggregate(Equisetum_Count~ EvaluationID, data=Equisetum_Bank, FUN=sum, na.rm=TRUE)

### 2. count total number of banks and using unique sampled banks object created in pctnoxious woody
# not collected before 2021 so no need to comment or uncomment anything here but 0s in output will need set to NULL for 2019-2020 data if recalculating indicators with this code
Tot_Equisetum_Count <- Unique_Sampled_Banks %>% dplyr::count(EvaluationID, name='Tot_Equisetum')

### 3. Calc %
# join the counts together
Pct_Equisetum <- dplyr::left_join(Equisetum_Reach, Tot_Equisetum_Count, by = 'EvaluationID')
# compute Equisetum count/Tot_Equisetum count * 100 = PctEquisetum
# dont calculate for visits with less than min sample size
# min sample size is 2 banks recorded at 5 transects so must be >=10
Pct_Equisetum$PctEquisetumSpecies_CHECK <- ifelse(Pct_Equisetum$Tot_Equisetum>9,round((Pct_Equisetum$Equisetum_Count / Pct_Equisetum$Tot_Equisetum) * 100, 0),NA)



# ------- 4. join the four dataframes -------
DF_Noxious_And_Native <- list(Pct_Nox_Woody, Pct_Nat_Woody, Pct_Nox_Herb, Pct_Sedge_Rush,Pct_Equisetum) %>% purrr::reduce(full_join, by = 'EvaluationID')
# Modifications to account for years when some species were not collected 
# get year from evaluationID
date_split <- data.frame(do.call('rbind', strsplit(as.character(DF_Noxious_And_Native$EvaluationID), '_', fixed=TRUE)))
DF_Noxious_And_Native$Year <- as.numeric(format(as.Date(date_split$X2, format='%Y-%m-%d'), '%Y'))
# If year is 2016-2018, veg values should be NA because no species lists were developed.Equisetum should be NA for all years on or before 2020 as it wasn't collected.
DF_Noxious_And_Native$PctNoxiousWoodySpecies_CHECK <- ifelse(DF_Noxious_And_Native$Year>=2016 & DF_Noxious_And_Native$Year<=2018, NA, DF_Noxious_And_Native$PctNoxiousWoodySpecies_CHECK )
DF_Noxious_And_Native$PctNoxiousHerbSpecies_CHECK <- ifelse(DF_Noxious_And_Native$Year>=2016 & DF_Noxious_And_Native$Year<=2018, NA, DF_Noxious_And_Native$PctNoxiousHerbSpecies_CHECK )
DF_Noxious_And_Native$PctSedgeRushSpecies_CHECK <- ifelse(DF_Noxious_And_Native$Year>=2016 & DF_Noxious_And_Native$Year<=2018, NA, DF_Noxious_And_Native$PctSedgeRushSpecies_CHECK )
DF_Noxious_And_Native$PctEquisetumSpecies_CHECK <- ifelse(DF_Noxious_And_Native$Year>=2016 & DF_Noxious_And_Native$Year<=2020, NA, DF_Noxious_And_Native$PctEquisetumSpecies_CHECK )


#write.csv(DF_Noxious_And_Native,'Noxious_And_NativeDF.csv')


#################################################
#         Calc Species Specific Indicators      #
#################################################

#### General Approach #####
#  1.	Count number of occurrences of a given species including nulls (so that we get 0 for any species that were not identified as present)
#  2.	Get the noxious woody herb etc. status of the species and then use the appropriate total number of banks assessed from above
#  3. Get the state and year specific species list for each evaluationID and join this in to add records for species that were absent at that visit.
#  4. Fill in 0s for absent species and (TO DO-then remove 0s for any sites where noxious or native veg was not collected)

# ------ PercentPlotsPresent ------
### 1. count number of occurrences by species
Num_Rows_CommonName <- VegSpec_WB %>% dplyr::count(EvaluationID,CommonName,name='Num_Banks_Per_Species')

# join the LU_Species onto the counts
Num_Rows_CommonName <- merge(Num_Rows_CommonName, LU_SpeciesUnique, by = "CommonName", all.x = TRUE)
# join the Noxious and Native dataframe
Num_Rows_CommonName <- merge(Num_Rows_CommonName, DF_Noxious_And_Native, by = "EvaluationID", all.x = TRUE)
'%!in%'<- Negate('%in%')
#Other option to do not in    sub <- mydata[!(mydata$group %in% c("A", "B", "E", "G")),]
Num_Rows_CommonName <-subset(Num_Rows_CommonName, Num_Rows_CommonName$CommonName %!in% c('Not Collected','no noxious herbaceous','no noxious woody','no sedge or rush','no sedge rush','no native woody','no native herbaceous','no noxious sedge rush equisetum') )

### 2. Get total number of banks from above
# determine Tot_Num_Banks and only calculate if more than 9 banks present (5 transects and 2 banks per transect)
Num_Rows_CommonName$PercentPlotsPresent <- NA
Num_Rows_CommonName$PercentPlotsPresent <- ifelse(Num_Rows_CommonName$WoodyHerb=='herb' & Num_Rows_CommonName$Noxious=='Yes' & Num_Rows_CommonName$Tot_Nox_Herb>9,
                                                  round(Num_Rows_CommonName$Num_Banks_Per_Species/Num_Rows_CommonName$Tot_Nox_Herb*100, 0), Num_Rows_CommonName$PercentPlotsPresent)
Num_Rows_CommonName$PercentPlotsPresent <- ifelse(Num_Rows_CommonName$WoodyHerb=='woody' & Num_Rows_CommonName$Noxious=='Yes' & Num_Rows_CommonName$Tot_Nox_Woody>9,
                                                        round(Num_Rows_CommonName$Num_Banks_Per_Species/Num_Rows_CommonName$Tot_Nox_Woody*100, 0), Num_Rows_CommonName$PercentPlotsPresent)
Num_Rows_CommonName$PercentPlotsPresent <- ifelse(Num_Rows_CommonName$WoodyHerb=='woody' & Num_Rows_CommonName$Noxious=='No' & Num_Rows_CommonName$Tot_Nat_Woody>9,
                                                        round(Num_Rows_CommonName$Num_Banks_Per_Species/Num_Rows_CommonName$Tot_Nat_Woody*100, 0), Num_Rows_CommonName$PercentPlotsPresent)
Num_Rows_CommonName$PercentPlotsPresent <- ifelse(Num_Rows_CommonName$CommonName=='sedge or rush' & Num_Rows_CommonName$Tot_Sedge_Rush >9,
                                                        round(Num_Rows_CommonName$Num_Banks_Per_Species/Num_Rows_CommonName$Tot_Sedge_Rush*100, 0), Num_Rows_CommonName$PercentPlotsPresent)
Num_Rows_CommonName$PercentPlotsPresent <- ifelse(Num_Rows_CommonName$CommonName=='equisetum' & Num_Rows_CommonName$Tot_Equisetum >9,
                                                  round(Num_Rows_CommonName$Num_Banks_Per_Species/Num_Rows_CommonName$Tot_Equisetum*100, 0), Num_Rows_CommonName$PercentPlotsPresent)

### 3. Get the state and year specific species list for each evaluationID and join this in to add records for species that were absent at that visit. 

#Veg species lists are stored by common name, state, and year so we need to add state and year to the vegspecies data
# Get SpeciesState and other admin unit info
# Points are attributed with latest admin unit layer info in the reach info script
# get species state from admin units and handle oregon specially
DF_ReachInfo$SpeciesState<-ifelse(DF_ReachInfo$District_CHECK %in% c("MEDFORD DISTRICT OFFICE","NW OREGON DISTRICT OFFICE","COOS BAY DISTRICT OFFICE"),"WOR", DF_ReachInfo$BLM_AdminState_CHECK)
DF_ReachInfo$SpeciesState<-ifelse(DF_ReachInfo$SpeciesState=="OR","EOR",DF_ReachInfo$SpeciesState)
# get year from evaluationID
date_split <- data.frame(do.call('rbind', strsplit(as.character(DF_ReachInfo$EvaluationID), '_', fixed=TRUE)))
DF_ReachInfo$Year <- as.numeric(format(as.Date(date_split$X2, format='%Y-%m-%d'), '%Y'))
# add species state and year into veg sample data
Num_Rows_CommonName2  <- dplyr::left_join(Num_Rows_CommonName ,DF_ReachInfo[,c('EvaluationID','SpeciesState','Year')], by="EvaluationID")
# join species lists with a list of evaluationIDs to get which species were assessed for each visit
specieslistpereval<- dplyr::left_join(species,DF_ReachInfo, by=c('SpeciesState','Year'))
# join species list per eval with computed species veg indicators, full join to keep all rows even if they are NA!!
Num_Rows_CommonName3<- dplyr::full_join(Num_Rows_CommonName2,specieslistpereval, by=c('EvaluationID','CommonName','ScientificName','Noxious','WoodyHerb'))
# need to join bank sample sizes back in by evaluationID because join above didnt get that info for all species due to the specifictiy of the join. this adds duplicate columns so use the ".y" one below
Num_Rows_CommonName4<-left_join(Num_Rows_CommonName3,DF_Noxious_And_Native, by='EvaluationID')

#### 4. fill in 0s if no banks were recorded at all for a given species, this should keep NAs if number of banks assessed was less than 9
Num_Rows_CommonName4$PercentPlotsPresent <- ifelse(Num_Rows_CommonName4$WoodyHerb=='herb' & Num_Rows_CommonName4$Noxious=='Yes' & Num_Rows_CommonName4$Tot_Nox_Herb.y>9 & is.na(Num_Rows_CommonName4$Num_Banks_Per_Species)==TRUE,0,Num_Rows_CommonName4$PercentPlotsPresent)
Num_Rows_CommonName4$PercentPlotsPresent <- ifelse(Num_Rows_CommonName4$WoodyHerb=='woody' & Num_Rows_CommonName4$Noxious=='Yes' & Num_Rows_CommonName4$Tot_Nox_Woody.y>9 & is.na(Num_Rows_CommonName4$Num_Banks_Per_Species)==TRUE,0,Num_Rows_CommonName4$PercentPlotsPresent)
Num_Rows_CommonName4$PercentPlotsPresent <- ifelse(Num_Rows_CommonName4$WoodyHerb=='woody' & Num_Rows_CommonName4$Noxious=='No' & Num_Rows_CommonName4$Tot_Nat_Woody.y>9 & is.na(Num_Rows_CommonName4$Num_Banks_Per_Species)==TRUE,0,Num_Rows_CommonName4$PercentPlotsPresent)
Num_Rows_CommonName4$PercentPlotsPresent <- ifelse(Num_Rows_CommonName4$CommonName=='sedge or rush' & Num_Rows_CommonName4$Tot_Sedge_Rush.y >9& is.na(Num_Rows_CommonName4$Num_Banks_Per_Species)==TRUE,0,Num_Rows_CommonName4$PercentPlotsPresent)
Num_Rows_CommonName4$PercentPlotsPresent <- ifelse(Num_Rows_CommonName4$CommonName=='equisetum' & Num_Rows_CommonName4$Tot_Equisetum.y >9& is.na(Num_Rows_CommonName4$Num_Banks_Per_Species)==TRUE,0,Num_Rows_CommonName4$PercentPlotsPresent)

###------- format and write out results --------#
I_VegSpeciesFreqOccurrence <- Num_Rows_CommonName4[,c('EvaluationID','CommonName','ScientificName', 'PercentPlotsPresent','Noxious','WoodyHerb')]
I_VegSpeciesFreqOccurrence$RecordID=paste0(I_VegSpeciesFreqOccurrence$EvaluationID,"_",I_VegSpeciesFreqOccurrence$CommonName)
I_VegSpeciesFreqOccurrence <-dplyr::left_join(I_VegSpeciesFreqOccurrence,latlong, by='EvaluationID')
I_VegSpeciesFreqOccurrence<-subset(I_VegSpeciesFreqOccurrence,is.na(EvaluationID)==FALSE & is.na(PercentPlotsPresent)==FALSE)

#bull thistle has a space for Yes noxious

write.csv(I_VegSpeciesFreqOccurrence,paste0(OutPath,'I_VegSpeciesFreqOccurrence',"_",Sys.Date(),'.csv'),na="",row.names=FALSE)
