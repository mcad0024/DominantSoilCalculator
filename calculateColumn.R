#Calculate dominant attributes for given column 
#and return them in a table along with their total percentages if they are ordinal.
calculateColumn <- function(dbfTable, newCol, isNum) {
  #Check if column is numeric or categorical and calculate dominant component values.
  if (isNum) {
    #Calculate the weighted average of the given column's values for each SL 
    #using the percent values of each component.
    colResult <- sqldf(paste0("select SL, sum(", newCol, " * (PERCENT/100.0)) as ", newCol, " from dbfTable group by SL"))
  } else {
    #Create name for the dominant attribute's percent column.
    percentColName <- paste0(newCol, "pct")
    #Create table with percentages added up grouped by SL and the column being calculated.
    initialTable <- sqldf(paste0("select SL, ", newCol, ", sum(PERCENT) as CMP_PERCENT from dbfTable group by SL, ", newCol))
    #Create final table containing only the values with the highest percentages for each SL.
    colResult <- sqldf(paste0("select SL, ", newCol, ", max(CMP_PERCENT) as ", percentColName, " from initialTable group by SL"))
  }
  return(colResult)
}
