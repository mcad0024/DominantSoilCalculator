#Remove NA values from numeric columns.
removeNumNA <- function(dbfTable) {
  #Create list of all numeric columns in table.
  numericCols <- unlist(lapply(dbfTable, is.numeric))
  numCols <- dbfTable[, numericCols]
  #Save the names of the numeric columns.
  numNames <- names(numCols)
  #Remove rows with NA values in the numeric columns.
  newTable <- complete.cases(dbfTable[, numNames])
  #Overwrite initial table with new rows.
  cleanTable <- dbfTable[newTable, ]
  return(cleanTable)
}
