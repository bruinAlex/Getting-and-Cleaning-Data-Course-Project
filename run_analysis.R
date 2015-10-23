#####################################################
# run_analysis.R
# Johns Hopkins University
# Getting and Cleaning Data Course Project
# Alex Lau
# 10/14/15
#
# Please see README.md and CodeBook.md for further
# information
#####################################################

# Load libraries
library(reshape2)


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
  
  # Return a data frame containing only these measurements
  extractedDat <- select(mergedData, matches("(SubjectID)|(TrainingType)|(mean)|(std)"))
  
### Step 3: Use descriptive activity names instead of numbers
  # Task completed in Step 1 (line 55)
  
### Step 4: Label data set with descriptive names
  # Use the gsub method to make many, many variable name replacements

  names(extractedDat) <- gsub("-mean","-Mean",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("-std","-StdDev",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("-std$","-StdDev",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("^(t)","Time",names(extractedDat), ignore.case = FALSE)
  names(extractedDat) <- gsub("^(f)","Freq",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("(gravity)","Gravity",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("(body)","Body",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("(bodybody)","Body",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("(gyro)","Gyro",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("AccMag","AccMagnitude",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("bodyaccjerkmag","BodyAccJerkMagnitude",names(extractedDat), 
                              ignore.case = TRUE)
  names(extractedDat) <- gsub("JerkMag","JerkMagnitude",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("GyroMag","GyroMagnitude",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("\\()","",names(extractedDat), ignore.case = TRUE)
  names(extractedDat) <- gsub("angle","Angle",names(extractedDat), ignore.case = TRUE)

  # Write out name list to file
  write(names(extractedDat), file = "extractedDatNames.txt")

### Step 5: Export tidy data set with the average of each variable for each activity and each subject.
  # Melt the data into a long-format data set
  meltData <- melt(extractedDat, id = c("SubjectID", "TrainingType"))
  
  # Recast the data while summarizing and calculating the mean
  tidyData <- dcast(meltData, SubjectID + TrainingType ~ variable, mean)
  
  # Write the tidy data set out to a .txt file for uploading
  write.table(tidyData, "tidyData.txt", row.names = FALSE, quote = FALSE)
