#' Read table from FGDB or SDE, keep only fields of interest, filter out values
#' This function assumes that the following variables exist:
#' dbType - should be either 'FGDB' or 'SDE'
#' fgdbPath - only used if dbType = 'FGDB'
#' sourceDB - only used if dbType = 'SDE'
#'
#' @param TableName the name of the table to read
#' @param TableFields vector containing the field names (columns) to keep
#' @param PointIDs vector containing the Point IDs to keep
#' @param Years vector containing the Years to keep
#' @param Projects vector containing the Projects to keep
#' @param ProtocolTypes vector containing the ProtocolTypes to keep
#'
#' @return A data frame, wide format of the retrieved data
ReadTable=function(TableName=c(),TableFields=c('*'),EvaluationIDs=c(),PointIDs=c(),Years=c(),Projects=c(),ProtocolTypes=c(), Return_SHAPE=FALSE, Return_OBJECTID=FALSE){
  # read from FGDB or SDE
  if (dbType == 'FGDB'){
    rawTable <- sf::st_read(dsn = fgdbPath, layer = TableName)
    # filter out only the fields of interest
    if (TableFields[1] != '*'){
      rawTable <- rawTable %>% select(all_of(TableFields))
    }
  } else {
    if (TableFields[1] == '*'){
      FieldSTR <- '*'
    } else {
      FieldSTR <- paste(TableFields, collapse = ',')
    }
    SQLstatement <- sprintf("SELECT %s FROM %s", FieldSTR, TableName)
    rawTable <- RODBC::sqlQuery(sourceDB, SQLstatement)
  }
  table_field_names = names(rawTable)
  # if the Year field doesn't exist, try to add it from the Evaluation ID field
  if (!is.null(Years) && 'FieldEvalDate' %in% table_field_names){
    if (dbType == 'FGDB'){
      if (!("Year" %in% table_field_names)) {
        rawTable$Year <- format(as.Date(rawTable$FieldEvalDate, format="%d/%m/%Y"),"%Y")
      }
    } else {
      if (!("Year" %in% table_field_names)) {
        rawTable$Year <- format(as.Date(rawTable$FieldEvalDate, format="%Y-%m-%d %H:%M:%S"),"%Y")
      }
    }
    
    # filter Years
    rawTable <- subset(rawTable, Year %in% Years)
    rawTable <- rawTable[, !(table_field_names == 'Year')]
  }
  
  # filter EvaluationID values
  if (!is.null(EvaluationIDs) && 'EvaluationID' %in% table_field_names){
    rawTable <- rawTable %>%subset(EvaluationID %in% EvaluationIDs)
  }
  
  # filter PointID values
  if (!is.null(PointIDs) && 'PointID' %in% table_field_names){
    rawTable <- rawTable %>% subset(PointID %in% PointIDs)
  }
  
  # filter Projects
  if (!is.null(Projects) && 'Project' %in% table_field_names){
    rawTable <- rawTable %>% subset(Project %in% Projects)
  }
  # filter ProtocolTypes
  if (!is.null(ProtocolTypes) && 'ProtocolType' %in% table_field_names){
    rawTable <- rawTable %>% subset(ProtocolType %in% ProtocolTypes)
  }
  
  # remove the SHAPE field unless instructed to keep it
  if (!Return_SHAPE){
    rawTable <- data.frame(rawTable)
    rawTable <- rawTable[, !(names(rawTable) == 'SHAPE')]
  }
  
  # remove the OBJECTID field unless instructed to keep it
  if (!Return_OBJECTID){
    rawTable <- data.frame(rawTable)
    rawTable <- rawTable[, !(names(rawTable) == 'OBJECTID')]
  }
  
  # return the table
  return(rawTable)
}