#Find index of specified column.
findColumn <- function(dbfTable, col1) {
  for (i in 1:ncol(dbfTable)) {
    #Check if column is at current iteration of loop.
    if (colnames(dbfTable)[i] == col1) {
      #Check if column is numeric or ordinal and begin calculations.
      if (is.numeric(dbfTable[, i])) {
        results <- calculateColumn(dbfTable, col1, TRUE)
      } else {
        results <- calculateColumn(dbfTable, col1, FALSE)
      }
    }
  }
  return(results)
}
