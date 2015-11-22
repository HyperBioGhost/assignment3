#Required library
library(reshape2)
library(data.table)

run_analysis <- function(){
      #Read in test data  
      testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
      testData <- read.table("UCI HAR Dataset/test/X_test.txt")
      testLabel <- read.table("UCI HAR Dataset/test/Y_test.txt")
      
      #Read in training data
      trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
      trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
      trainLabel <- read.table("UCI HAR Dataset/train/Y_train.txt")
      
      #Read in features and activities
      features <- read.table("UCI HAR Dataset/features.txt", col.names=c("featureIndex", "featureLabel"))
      activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activityIndex", "activityLabel"))
      activities$activityLabel <- as.character(activities$activityLabel)
      featuresIndices <- grep("-mean\\(\\)|-std\\(\\)", features$featureLabel)
      
      #merge test and training data with descriptive variable names.
      subjects <- rbind(testSubject, trainSubject)
      names(subjects) <- "subjectIndex"
      X <- rbind(testData, trainData)
      XData <- X[, featuresIndices]
      names(XData) <- gsub("\\(|\\)", "", features$featureLabel[featuresIndices])
      YData <- rbind(testLabel, trainLabel)
      names(YData) = "activityIndex"
      activity <- merge(YData, activities, by="activityIndex")$activityLabel
      
      #merge data frames and out put tidy data set
      data <- cbind(subjects, XData, activity)
      write.table(data, "tidyDataSet.txt")
      
      #create a dataset with the average of each variable for each activity and each subject.
      dataTable <- data.table(data)
      calculatedData <- dataTable[, lapply(.SD, mean), by=c("subjectIndex", "activity")]
      write.table(calculatedData, "tidyDataWithAverage.txt", row.names = F)
}