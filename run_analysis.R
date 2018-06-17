## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for each measurement.
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names.
##5) From the data set in step 4, creates a second, independent tidy data set with the average of each 
##variable for each activity and each subject.

#Load library and data
library(data.table)
library(reshape2)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

#Load activity label and features
activityLabel <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"),
                       col.names = c("classLabel", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt"), 
                  col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)

#Load training datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/y_train.txt"), col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))
train <- cbind( trainSubjects, trainActivities, train)

#Load test dataset
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/y_test.txt"), col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))
test <- cbind( testSubjects, testActivities, test)

#Merge datasets
combined <- rbind(train, test)

#Label combined dataset
combined[["Activity"]] <- factor(combined[, Activity], levels = activityLabel[["classLabel"]], 
                                 labels = activityLabel[["activityName"]])
combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])

#Reshape combined dataset
combined <- reshape2::melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- reshape2::dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)
data.table::fwrite(x = combined, file = "cleanData.txt", quote = FALSE)




