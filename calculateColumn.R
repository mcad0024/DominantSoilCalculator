#Calculate dominant attributes for given column.
calculateColumn <- function(dbfTable, newCol, isNum) {
  #Check if column is numeric or categorical and calculate dominant component values.
  if (isNum) {
    #Calculate the weighted average of the given column's values for each POLY_ID 
    #using the percent values of each component.
    colResult <- sqldf(paste0("select POLY_ID, sum(", newCol, " * (PERCENT/100.0)) as ", newCol, " from dbfTable group by POLY_ID"))
  } else {
    #Create name for the dominant attribute's percent column.
    percentColName <- paste0(newCol, "pct")
    #Create table with percentages added up grouped by POLY_ID and the column being calculated.
    initialTable <- sqldf(paste0("select POLY_ID, ", newCol, ", sum(PERCENT) as CMP_PERCENT from dbfTable group by POLY_ID, ", newCol))
    #Create final table with only the rows with the highest percentages for each POLY_ID.
    colResult <- sqldf(paste0("select POLY_ID, ", newCol, ", max(CMP_PERCENT) as ", percentColName, " from initialTable group by POLY_ID"))
  }
  return(colResult)
}
