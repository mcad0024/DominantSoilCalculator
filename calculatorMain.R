#Dominant Soil Calculator
#Calculate dominant soil attributes for the given dbf tables.

library(foreign)
library(plyr)
library(raster)
library(sqldf)

source("calculateColumn.R")
source("findColumn.R")
source("findAll.R")
source("removeNumNA.R")

#TODO:
#Verify snf calculations.
#Add documentation.
#Improve interface.

#Load dbfs and shapefile.
#shape <- shapefile("../CalculatorFiles/PED_AB_SLC_1M_V32.shp")
cmpTable <- read.dbf("../CalculatorFiles/cmp32.dbf")
snfTable <- read.dbf("../CalculatorFiles/snf32.dbf")
#slfTable <- read.dbf("../CalculatorFiles/slf32.dbf")

#Remove duplicate rows in tables.
cmpTable <- cmpTable[!duplicated(cmpTable), ]
snfTable <- snfTable[!duplicated(snfTable), ]
#Ensure all soilkeys in snf file are unique.
snfTable <- snfTable[!duplicated(snfTable$SOILKEY), ]

#Remove NA values from numeric columns in cmp dbf.
cmpTable <- removeNumNA(cmpTable)

#Choose table to perform calculations on.
cat("Dominant Soil Calculator \n1: Calculate CMP table \n2: Calculate SNF table \n")
tableChoice <- readline("Make selection: ")
if (tableChoice == "1") {
  #Prompt user for amount of columns.
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
  } else if (choice == "2") {
    #Calculate all attribute columns in table.
    results <- findAll(cmpTable)
  } else {
    cat("Error: Invalid input. \n")
    break
  }
} else if (tableChoice == "2") {
  #Remove NA values from numeric columns in snf dbf.
  snfTable <- removeNumNA(snfTable)
  #Remove duplicate columns.
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
      cat("Error: Invalid column name. \n")
      break
    }
  } else if (choice == "2") {
    #Calculate all attribute columns in table.
    results <- findAll(snfAndCmp)
  } else {
    cat("Error: Invalid input. \n")
    break
  }
} else {
  cat("Error: Invalid input. \n")
}
#Write results into a dbf file.
write.dbf(results, "../CalculatorFiles/Result_Files/soilResults.dbf")
