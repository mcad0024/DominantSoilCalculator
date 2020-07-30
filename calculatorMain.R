library(foreign)
library(plyr)
library(raster)
library(sqldf)

source("calculateColumn.R")
source("findColumn.R")

#Load dbfs and shapefile.
shape <- shapefile("../CalculatorFiles/PED_AB_SLC_1M_V32.shp")
cmpTable <- read.dbf("../CalculatorFiles/cmp32.dbf")
snfTable <- read.dbf("../CalculatorFiles/snf32.dbf")
#slfTable <- read.dbf("../CalculatorFiles/slf32.dbf")

cat("Dominant Soil Calculator \n1: Calculate CMP table \n2: Calculate SNF table \n")
tableChoice <- readline("Make selection: ")
if (tableChoice == "1") {
  #Interface - prompt user for amount of columns.
  #print(names(cmpTable))
  cat("1: Calculate one column \n2: Calculate all columns \n")
  choice <- readline("Make selection: ")
  #Calculate one column.
  if (choice == "1") {
    col1 <- readline("Enter name of column to calculate: ")
    col1 <- toupper(col1)
    #Check if column is in table.
    if (col1 %in% names(cmpTable)) {
      results <- findColumn(cmpTable, col1)
    } else {
      cat("Error: Invalid column name. \n")
      break
    }
    #Calculate all attribute columns in table.
  } else if (choice == "2") {
    #Create table to update later on.
    resultTableCmp <- sqldf("select distinct SL as SL from cmpTable")
    for (i in 1:ncol(cmpTable)) {
      col_name <- names(cmpTable[i])
      if (col_name != "SL" && col_name != "CMP" && col_name != "PERCENT") {
        if (is.numeric(cmpTable[, i])) {
          results <- calculateColumn(cmpTable, col_name, TRUE)
        } else {
          results <- calculateColumn(cmpTable, col_name, FALSE)
        }
        resultTableCmp <- cbind(resultTableCmp, results[, 2])
        resultIndex <- length(resultTableCmp)
        colnames(resultTableCmp)[resultIndex] <- col_name
      }
    }
  } else {
    cat("Error: Invalid input. \n")
  }
} else if (tableChoice == "2") {
  #SNF calculation
  cmpTable2 <- cmpTable[c("SL", "SOILKEY", "PERCENT")]
  snfAndCmp <- join(cmpTable2, snfTable, by = "SOILKEY", type = "inner")
  #Perform calculations on each record for the specified attribute.
  snfCol1 <- readline("Enter name of column to calculate: ")
  snfCol1 <- toupper(snfCol1)
  #Check if column is in table.
  if (snfCol1 %in% names(snfAndCmp)) {
    snfResults <- findColumn(snfAndCmp, snfCol1)
  } else {
    cat("Error: Invalid column name. \n")
    break
  }
} else {
  cat("Error: Invalid input. \n")
}
