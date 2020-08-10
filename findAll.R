#Loop through all columns in any table and make a dataframe containing all dominant attributes.
findAll <- function(dbfTable) {
  #Create table to update later on.
  resultTable <- sqldf("select distinct POLY_ID as POLY_ID from dbfTable")
  for (i in 1:ncol(dbfTable)) {
    col_name <- names(dbfTable[i])
    #Avoid calculating columns that don't represent soil attributes.
    if (col_name != "POLY_ID" && col_name != "CMP" && col_name != "PERCENT" && col_name != "CMP_ID") {
      #Check whether the column is numeric or ordinal so the proper calculations can be performed.
      if (is.numeric(dbfTable[, i])) {
        results <- calculateColumn(dbfTable, col_name, TRUE)
      } else {
        results <- calculateColumn(dbfTable, col_name, FALSE)
      }
      #Compile all column results into one table.
      resultTable <- cbind(resultTable, results[, 2])
      #Give each result column its original name.
      resultIndex <- length(resultTable)
      colnames(resultTable)[resultIndex] <- col_name
      #For ordinal columns, add each attribute's percent column into the table.
      if (!is.numeric(dbfTable[, i])) {
        resultTable <- cbind(resultTable, results[, 3])
        #Give each percent column its original name.
        resultIndex <- length(resultTable)
        colnames(resultTable)[resultIndex] <- colnames(results[3])
      }
    }
  }
  return(resultTable)
}
