#########################################################
#        Get Streambed Particle Data         #
#########################################################

#read in StreambedParticles_W and ThalwegStreambedParticles_B tables
Sed_wade <- ReadTable(TableName = streambedParticles_w, EvaluationIDs = uniqueEvalIDs)
Sed_boat <- ReadTable(TableName = ThalwegSBP, EvaluationIDs = uniqueEvalIDs)

#########################################################
#        Streambed Particle Indicators         #
#########################################################

##### PctFinesLessThan2mm and PctFinesLessThan6mm
# merge wadeable and boatable data
if(nrow(Sed_wade)>0 & nrow(Sed_boat)>0){sed_wb <- merge(Sed_wade, Sed_boat, all.x = TRUE, all.y = TRUE)
} else if (nrow(Sed_wade)>0){sed_wb <-Sed_wade
} else sed_wb <-Sed_boat

# TO DO remove wood and "other" ParticleSizeClass rows in database. Remove ParticleSizeClassNonMeas and consider removing all sediment data if nonmeas was present for a site
# merge categorical data and numeric data into one column for sand and fines less than 2 mm
#this statement works because particle size class is in wadeable and boatable tables. if we ever remove it from the wadeable table, the below code needs altered
sed_wb$SandFines<- ifelse(is.na(sed_wb$ParticleSizeClass)==TRUE,sed_wb$ParticleSize,sed_wb$ParticleSizeClass)

#if only mid dry and wet particles are desired to match EPA calculations subset them here by uncommenting out this line
#sed_wb<-subset(sed_wb, StreambedLocation=="Wet or Dry Middle"|StreambedLocation=="Wet"|StreambedLocation=="Dry Middle")

# get column with 1 if particle is sand or fines, otherwise set to 0
sed_wb$SandFines2<- ifelse(sed_wb$SandFines=="Sand"| sed_wb$SandFines=="Silt or Clay or Muck", 1,ifelse(sed_wb$SandFines=='1'|sed_wb$SandFines=='2',1,0))
# get a column with 1 if particle is less than 6 mm (note this is only for numeric data)
sed_wb$SandFines6 <- ifelse(sed_wb$ParticleSize <= 6, 1,0)

# remove any blank rows so that the particle count below is correct
sed_wb<-subset(sed_wb,is.na(SandFines2)==FALSE)

# calculate the proportion of particles that are sand and fines
# taking a mean of the 0s and 1s is mathematically the same as summing the number of 1s and dividing by the number of particles
# calculating things in this way prevents subsetting the data and then not being able to get 0 percent sand and fines for sites that dont have any
pct_sa_fn<- sed_wb %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(PctFinesLessThan2mm_CHECK=mean(SandFines2,na.rm=TRUE),PctFinesLessThan6mm_CHECK=mean(SandFines6, na.rm=TRUE),nParticles_CHECK=n())

# convert proportion to a percentage and round
pct_sa_fn$PctFinesLessThan6mm_CHECK<-round(pct_sa_fn$PctFinesLessThan6mm_CHECK*100,digits=1)
pct_sa_fn$PctFinesLessThan2mm_CHECK<-round(pct_sa_fn$PctFinesLessThan2mm_CHECK*100,digits=1)


##### get the D50, D16, D84 for Particle Size
# remove blank rows for particles size so that only sites were numeric data were collected are present, 
# the above NA remove statement only removed rows if the join categoical and numeric column was blank
sed_wb_num<-subset(sed_wb,is.na(ParticleSize)==FALSE)
# calc quantiles, note group_modify function is experimental tidyverse life cycle....may need to redo at some point if function changes
quantiles <- sed_wb_num %>% group_by(EvaluationID) %>% dplyr::group_modify(~{quantile(.x$ParticleSize, probs = c(0.50,0.16,0.84), na.rm = TRUE) %>% tibble::enframe()} %>% spread(name, value))
quantiles <- purrr::set_names(quantiles, c('EvaluationID', 'D16_CHECK','D50_CHECK','D84_CHECK'))
#round to 0 decimal places
quantiles$D50_CHECK=round(quantiles$D50_CHECK,digit=0)
quantiles$D16_CHECK=round(quantiles$D16_CHECK,digit=0)
quantiles$D84_CHECK=round(quantiles$D84_CHECK,digit=0)

##### Geometric mean diameter
# needs recalculated for all data. before 2021, function was incorrectly using the log10 rather than using natural log.
# note the function below is equivalent to the EPAs function function (x) {prod(x)^(1/count(x))}
geom_mean <- function(val) {exp(mean(log(val)))}
#particle_size <- select(sed_wb, all_of(c('EvaluationID','ParticleSize')))
sed_GM <- sed_wb_num %>% dplyr::group_by(EvaluationID) %>% dplyr::summarize(GeometricMeanParticleDiam_CHECK = geom_mean(ParticleSize))
sed_GM$GeometricMeanParticleDiam_CHECK=round(sed_GM$GeometricMeanParticleDiam_CHECK,digit=0)



##### merge the data frames together
DF_StreambedParticles <- purrr::reduce(list(pct_sa_fn,quantiles,sed_GM),by='EvaluationID',left_join)

# add in Year, Protocol, and Project so that the correct sample size can be calculated
date_split <- data.frame(do.call('rbind', strsplit(as.character(DF_StreambedParticles$EvaluationID), '_', fixed=TRUE)))
DF_StreambedParticles$Year <- format(as.Date(date_split$X2, format='%Y-%m-%d'), '%Y')
listsites_3fields <- DF_ReachInfo %>% select(c('EvaluationID','ProtocolType_CHECK','Project_CHECK'))
DF_StreambedParticles <- merge(DF_StreambedParticles, listsites_3fields, by='EvaluationID')
# use date_split in other scripts so best to remove it here
remove(date_split)


# for Evaluation IDs with too few samples, set indicators at na
# alaska only collects 5 particles at  main and intermediate transects (21) = total of 105, decided that they needed at least 50 particles for complete data even though 5 particles at 5 main and 4 intermediate transects is 45 particles
# boatable only collects 100 particles (10 thalwegs at 10 transects), 10 particles at 5 transects = 50 particles min sample size
# wadeable collects 210 particles, 10 particles at 5 main and 4 intermediate would be min sample size of 90 but we require min of 100 particles, cant remember why??
DF_StreambedParticles$PctFinesLessThan2mm_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$PctFinesLessThan2mm_CHECK),
                                       ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$PctFinesLessThan2mm_CHECK),
                                              ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$PctFinesLessThan2mm_CHECK)))
DF_StreambedParticles$PctFinesLessThan6mm_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$PctFinesLessThan6mm_CHECK),
                                       ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$PctFinesLessThan6mm_CHECK),
                                              ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$PctFinesLessThan6mm_CHECK)))
DF_StreambedParticles$GeometricMeanParticleDiam_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$GeometricMeanParticleDiam_CHECK),
                            ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$GeometricMeanParticleDiam_CHECK),
                                   ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$GeometricMeanParticleDiam_CHECK)))
DF_StreambedParticles$D50_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D50_CHECK),
                        ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D50_CHECK),
                               ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$D50_CHECK)))
DF_StreambedParticles$D16_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D16_CHECK),
                        ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D16_CHECK),
                               ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$D16_CHECK)))
DF_StreambedParticles$D84_CHECK=ifelse(grepl("^AK", DF_StreambedParticles$Project_CHECK)==TRUE, ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D84_CHECK),
                        ifelse(grepl("^Boatable",DF_StreambedParticles$ProtocolType_CHECK)==TRUE,ifelse(DF_StreambedParticles$nParticles<50,NA,DF_StreambedParticles$D84_CHECK),
                               ifelse(DF_StreambedParticles$nParticles<100,NA,DF_StreambedParticles$D84_CHECK)))
DF_StreambedParticles <-subset(DF_StreambedParticles, select=-c(ProtocolType_CHECK,Project_CHECK))
#write.csv(DF_StreambedParticles,'DF_StreambedParticles.csv')

# ###### Future development #####
# # take categorical size classes and convert them to numeric based on table below. then need to interpolate between these to get D50, D16 etc 
# # diam was determined by taking the geometric mean of the size class bin min and max
# sed_sizes<-read.csv('sed_sizes.csv')
# 
# # "class	min	max	diam
# #  Organic Matter	NA	NA	NA
# #  Other	NA	NA	NA
# #  Wood	NA	NA	NA
# #  Hardpan	4000	8000	5656.854249
# #  Silt or Clay or Muck	0.001	0.06	0.007745967
# #  Sand	0.06	2	0.346410162
# #  Fine Gravel	2	16	5.656854249
# #  Coarse Gravel	16	64	32
# #  Cobble	64	250	126.4911064
# #  Small Boulder	250	1000	500
# #  Large Boulder	1000	4000	2000
# #  Boulder	250	4000	1000
# #  Bedrock Smooth	4000	8000	5656.854249
# #  Bedrock Rough	4000	8000	5656.854249
# #  Bedrock or Hardpan	4000	8000	5656.854249
# #  Gravel	2	64	11.3137085"
# 
# sed_wd<-dplyr::left_join(sed_wb,sed_sizes, by="ParticleSizeClass")
# 
# interpolatePercentile<-
# function (df, classVar, percentile, pctlVar, classBounds)
# {
#   df <- subset(df, !is.na(classVar))
#   classCounts <- aggregate(list(classCount = df[[classVar]]),
#                            list(UID = df$UID, CLASS = df[[classVar]]), count)
#   sampleSizes <- aggregate(list(totalCount = df[[classVar]]),
#                            list(UID = df$UID), count)
#   classPcts <- merge(classCounts, sampleSizes, by = "UID")
#   classPcts$pct <- 100 * classPcts$classCount/classPcts$totalCount
#   classPcts <- merge(classPcts, classBounds, by = "CLASS",
#                      all.x = TRUE)
#   classPcts <- classPcts[order(classPcts$UID, classPcts$min),
#                          ]
#   classPcts$upperPct <- ave(classPcts$pct, classPcts$UID, FUN = cumsum)
#   classPcts <- first(classPcts, "UID", "start")
#   classPcts <- lag(classPcts, "upperPct", "lowerPct")
#   classPcts[classPcts$start, ]$lowerPct <- 0
#   tt <- subset(classPcts, lowerPct < percentile & percentile <=
#                  upperPct)
#   tt[pctlVar] <- with(tt, min + (max - min) * (percentile -
#                                                  lowerPct)/(upperPct - lowerPct))
#   tt <- tt[c("UID", pctlVar)]
#   return(tt)
# }
#  c16 <- interpolatePercentile(sed_wb, "ParticleSizeClass", 16,
# "lsub2d16inor", measurable)
