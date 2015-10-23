# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
#####################################################
# run_analysis.R
# Johns Hopkins University
# Getting and Cleaning Data Course Project
# Alex Lau
# 10/14/15
#####################################################

# Load libraries
library(plyr)
library(dplyr)


### Step 1: Merge the training and test sets to create one data set

  # Read the test and training data into R, read the associated labels as well
  testDat         <- read.table("test/X_test.txt")
  testLabels      <- read.table("test/y_test.txt")
  trainDat        <- read.table("train/X_train.txt")
  trainLabels     <- read.table("train/y_train.txt")
  featuresLabels  <- read.table("features.txt")
  subTestDat      <- read.table("test/subject_test.txt")
  subTrainDat     <- read.table("train/subject_train.txt")
  activityLabels  <- read.table("./activity_labels.txt")
  
  # Combine the labels into the associated data file
  colnames(testDat)     <- featuresLabels[ , 2]
  testDat               <- cbind(testLabels, testDat)
  colnames(testDat)[1]  <- "TrainingType"
  testDat               <- cbind(subTestDat, testDat)
  colnames(testDat)[1]  <- "SubjectID"
  testDat               <- mutate(testDat, Source = "test")
  
  colnames(activityLabels)  <- c("activityID", "activityName")
  colnames(subTrainDat)     <- c("subjectID")
  
  colnames(trainDat)    <- featuresLabels[ , 2]
  trainDat              <- cbind(trainLabels, trainDat)
  colnames(trainDat)[1] <- "TrainingType"
  trainDat              <- cbind(subTrainDat, trainDat)
  colnames(trainDat)[1] <- "SubjectID"
  trainDat              <- mutate(trainDat, Source = "train")
  
  # Merge the training and test datasets together
  mergedData <- merge(trainDat, testDat, all = TRUE)
  
  # Replace the Activity number label (1...6) with string description
  mergedData$TrainingType <- factor(mergedData$TrainingType, 
                                    levels = activityLabels$activityID, 
                                    labels = activityLabels$activityName)
  
  # Remove data files to clean up memory
  rm(subTestDat, subTrainDat, testDat, testLabels, trainDat, trainLabels)
  
### Step 2: Extract only the mean and std. deviation measurements
  
  # Create a vector of variables that calculate a mean or std. deviation
  extractList <- grep(c("(mean)|(std)"), featuresLabels$V2, value = TRUE)
  
  # Return a data frame containing only these measurements; ignores angle calculations
  # extractedDat <- select(mergedData, matches("(mean)|(std)"), -contains("angle"))
  
  # Return a data frame containing only these measurements
  extractedDat <- select(mergedData, matches("(SubjectID)|(TrainingType)|(mean)|(std)"))
  
### Step 3: Extr
  # Write out name list to file
  write(names(extractedDat), file = "extractedDatNames.txt")

