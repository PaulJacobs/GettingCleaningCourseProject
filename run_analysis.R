

runAnalysis <- function()
{
  testLabel <- read.table("test/y_test.txt")
  trainLabel <- read.table("train/y_train.txt")
  labels <- rbind(testLabel, trainLabel)
  
  testMeaurements <- read.table("test/X_test.txt")
  trainMeaurements <- read.table("train/X_train.txt")
  measurements <- rbind(testMeaurements, trainMeaurements)
  
  testSub <- read.table("test/subject_test.txt")
  trainSub <- read.table("train/subject_train.txt")
  subjects <- rbind(testSub, trainSub)
  
  featuresList <- read.table("features.txt", stringsAsFactors = FALSE)
  
  features <- featuresList$V2
  
  ## Isolate only the STD and Mean columns
  useOnlyStdAndMeanColumns <- grepl("(std|mean[^F])", features, perl = TRUE)
  
  ## Resample measures with only the needed columns
  measurements <- measurements[, useOnlyStdAndMeanColumns]
  names(measurements) <- features[useOnlyStdAndMeanColumns]
  
  ## Make the measurement names more readable
  names(measurements) <- gsub("\\(|\\)", "", names(measurements))
  names(measurements) <- tolower(names(measurements))
  
  ## Make the activity names more readable
  activities <- read.table("activity_labels.txt")
  activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))
  
  ## Change the labels to the actual activity name
  labels[,1] = activities[labels[,1], 2]
  
  ## Use activity for the label
  names(labels) <- "activity" 
  
  names(subjects) <- "subject"
  
  ## Merge the measures, labels and subjects
  initialTidiedUpData <- cbind(subjects, labels, measurements)
  
  ## Write the initial tidied up and merged data
  write.table(initialTidiedUpData, "initialTidiedUpData.txt")
  
  ## Summarize by subject and activity
  summarizedMeanForAllMeasurements = ddply(initialTidiedUpData, c("subject","activity"), numcolwise(mean))
  
  ## Write final summarized table
  write.table(summarizedMeanForAllMeasurements, file = "meansBySubjectAndActivity.txt",row.names=FALSE)
}
