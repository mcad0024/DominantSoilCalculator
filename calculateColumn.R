#Calculate dominant attributes for given column.
calculateColumn <- function(dbfTable, newCol, isNum) {
  #Check if column is numeric or categorical and calculate dominant component values.
  if (isNum) {
    colResult <- sqldf(paste0("select SL, sum(", newCol, " * (PERCENT/100.0)) as ", newCol, " from dbfTable group by SL"))
  } else {
    #Create table with percentages added up grouped by SL and by unique values in the column being calculated.
    initialTable <- sqldf(paste0("select SL, ", newCol, ", sum(PERCENT) as CMP_PERCENT from dbfTable group by SL, ", newCol))
    #Create final table with only the rows with the highest percentages for each SL.
    colResult <- sqldf(paste0("select SL, ", newCol, ", max(CMP_PERCENT) as CMP_PERCENT from initialTable group by SL"))
  }
  return(colResult)
}
