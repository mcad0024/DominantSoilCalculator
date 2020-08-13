#Dominant Soil Calculator
#Create interface for selecting dbf tables and calculate their dominant soil attributes.
#By: Hayden McAdam
#August 2020

library(foreign)
library(plyr)
library(raster)
library(sqldf)

source("calculateColumn.R")
source("findColumn.R")
source("findAll.R")
source("removeNumNA.R")

#Load dbfs.
cmpTable <- read.dbf("../CalculatorFiles/cmp32.dbf")
snfTable <- read.dbf("../CalculatorFiles/snf32.dbf")

#Remove duplicate rows in cmp table.
cmpTable <- cmpTable[!duplicated(cmpTable), ]
#Remove NA values from numeric columns in cmp table.
cmpTable <- removeNumNA(cmpTable)

#Choose table to perform calculations on.
cat("Dominant Soil Calculator \n1: Calculate CMP table \n2: Calculate SNF table \n")
tableChoice <- readline("Make selection: ")
if (tableChoice == "1") {
  #Prompt user for amount of columns.
  #print(names(cmpTable))
  cat("1: Calculate one column \n2: Calculate all columns \n")
  choice <- readline("Make selection: ")
  
  #CMP calculation
  if (choice == "1") {
    #Calculate one column.
    col1 <- readline("Enter name of column to calculate: ")
    col1 <- toupper(col1)
    #Check if column is in table and perform calculations on it.
    if (col1 %in% names(cmpTable)) {
      results <- findColumn(cmpTable, col1)
    } else {
      stop("Error: Invalid column name.")
    }
  } else if (choice == "2") {
    #Calculate all attribute columns in table.
    results <- findAll(cmpTable)
  } else {
    stop("Error: Invalid input.")
  }
} else if (tableChoice == "2") {
  #Remove duplicate rows in snf table.
  snfTable <- snfTable[!duplicated(snfTable), ]
  #Ensure all soilkeys in snf file are unique.
  snfTable <- snfTable[!duplicated(snfTable$SOILKEY), ]
  #Remove NA values from numeric columns in snf table.
  snfTable <- removeNumNA(snfTable)
  #Create cmp table without columns already present in the snf table.
  cmpTableTemp <- cmpTable[, -which(names(cmpTable) %in% names(snfTable))]
  cmpTableTemp <- cbind(cmpTableTemp, cmpTable["SOILKEY"])
  #Join snf and cmp tables on soilkey.
  snfAndCmp <- join(cmpTableTemp, snfTable, by = "SOILKEY", type = "inner")
  
  #Prompt user for amount of columns.
  #print(names(snfAndCmp))
  cat("1: Calculate one column \n2: Calculate all columns \n")
  choice <- readline("Make selection: ")
  
  #SNF calculation
  if (choice == "1") {
    #Calculate one column.
    snfCol1 <- readline("Enter name of column to calculate: ")
    snfCol1 <- toupper(snfCol1)
    #Check if column is in table.
    if (snfCol1 %in% names(snfAndCmp)) {
      results <- findColumn(snfAndCmp, snfCol1)
    } else {
      stop("Error: Invalid column name.")
    }
  } else if (choice == "2") {
    #Calculate all attribute columns in table.
    results <- findAll(snfAndCmp)
  } else {
    stop("Error: Invalid input.")
  }
} else {
  stop("Error: Invalid input.")
}
#Write results into a dbf file.
write.dbf(results, "../CalculatorFiles/Result_Files/soilResults.dbf")
