tidydata <- function(directory = getwd()) {
  
  #set working directory "UCI HAR Dataset" and extract necessary data
  setwd(directory)
  features <- read.table("features.txt")
  
  #set working directory to test and train directories and extract necessary data
  setwd(paste0(directory, "/test"))
  testraw <- read.table("X_test.txt")
  ytest <- as.data.frame(read.table("y_test.txt"))
  subjecttest <- as.data.frame(read.table("subject_test.txt"))
  
  setwd(paste0(directory, "/train"))
  trainraw <- read.table("X_train.txt")
  ytrain <- as.data.frame(read.table("y_train.txt"))
  subjecttrain <- as.data.frame(read.table("subject_train.txt"))
  
  #add variable name to subject data
  colnames(subjecttest) = c("subject")
  colnames(subjecttrain) = c("subject")
  
  #replace activity numbers with activity descriptions
  colnames(ytest) = c("activity")
  colnames(ytrain) = c("activity")
  ytest$activity = as.character(ytest$activity)
  ytrain$activity = as.character(ytrain$activity)
  
  ytest$activity = gsub("1", "walking", ytest$activity)
  ytest$activity = gsub("2", "walking_upstairs", ytest$activity)
  ytest$activity = gsub("3", "walking_downstairs", ytest$activity)
  ytest$activity = gsub("4", "sitting", ytest$activity)
  ytest$activity = gsub("5", "standing", ytest$activity)
  ytest$activity = gsub("6", "laying", ytest$activity)
  
  ytrain$activity = gsub("1", "walking", ytrain$activity)
  ytrain$activity = gsub("2", "walking_upstairs", ytrain$activity)
  ytrain$activity = gsub("3", "walking_downstairs", ytrain$activity)
  ytrain$activity = gsub("4", "sitting", ytrain$activity)
  ytrain$activity = gsub("5", "standing", ytrain$activity)
  ytrain$activity = gsub("6", "laying", ytrain$activity)
  
  #merge data sets and remove irrelevant data
  subjectall <- rbind(subjecttest, subjecttrain)
  yall <- rbind(ytest, ytrain)
  allraw <- rbind(testraw, trainraw)
  colnames(allraw) = features[,2]
  allwanted <- allraw[,which(grepl("mean()", features[,2], fixed = TRUE) | grepl("std()", features[,2], fixed = TRUE))]
  allfinal <<- cbind(subjectall, yall, allwanted)
  
  #construct mean table
  library(reshape2)
  melt <- melt(allfinal, id=c("subject", "activity"), measure.vars = c(3:68))
  subjectmeans <- dcast(melt, subject ~ variable, mean)
  activitymeans <- dcast(melt, activity ~ variable, mean)
  
  colnames(activitymeans)[1] <- "activity/subject"
  colnames(subjectmeans)[1] <- "activity/subject"
  
  meanstable <<- rbind(activitymeans, subjectmeans)
  return(meanstable)
}
