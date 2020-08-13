#Dominant Soil Calculator
#Calculate dominant soil attributes for the given dbf tables.
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
cmpTable <- read.dbf("../CalculatorFiles/DSS_AB_Files/dss_v3_ab_cmp.dbf")
snfTable <- read.dbf("../CalculatorFiles/DSS_AB_Files/soil_name_ab_v2.dbf")

#Remove duplicate rows in cmp table.
cmpTable <- cmpTable[!duplicated(cmpTable), ]
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
  #Ensure all soil IDs in snf file are unique.
  snfTable <- snfTable[!duplicated(snfTable$SOIL_ID), ]
  #Remove NA values from numeric columns in snf dbf.
  snfTable <- removeNumNA(snfTable)
  #Remove duplicate columns.
  cmpTableTemp <- cmpTable[, -which(names(cmpTable) %in% names(snfTable))]
  cmpTableTemp <- cbind(cmpTableTemp, cmpTable["SOIL_ID"])
  #Join snf and cmp tables on soil ID.
  snfAndCmp <- join(cmpTableTemp, snfTable, by = "SOIL_ID", type = "inner")
  
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
write.dbf(results, "../CalculatorFiles/Result_Files/soilResultsDSS.dbf")
