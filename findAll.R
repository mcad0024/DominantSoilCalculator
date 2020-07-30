#TESTING - function to loop through all columns in any table
#and make a dataframe containing all dominant attributes.
findAll <- function(dbfTable) {
  #Create table to update later on.
  resultTable <- sqldf("select distinct SL as SL from dbfTable")
  for (i in 1:ncol(dbfTable)) {
    col_name <- names(dbfTable[i])
    #Avoid calculating columns that don't represent soil attributes.
    if (col_name != "SL" && col_name != "CMP" && col_name != "PERCENT") {
      if (is.numeric(dbfTable[, i])) {
        results <- calculateColumn(dbfTable, col_name, TRUE)
      } else {
        results <- calculateColumn(dbfTable, col_name, FALSE)
      }
      resultTable <- cbind(resultTable, results[, 2])
      resultIndex <- length(resultTable)
      colnames(resultTable)[resultIndex] <- col_name
    }
  }
  return(resultTable)
}
