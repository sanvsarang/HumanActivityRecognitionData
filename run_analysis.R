library(dplyr)

## Setting the working directory to current directory
## This would help this script to avoid any manual steps 
setwd("./")

## download the zip file and unzip it
filesource <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(filesource, "./dataset.zip", mode = "wb")
unzip("./dataset.zip")

## assigning file names to variables
filename_features <- "./UCI HAR Dataset/features.txt"
filename_activity_labels <- "./UCI HAR Dataset/activity_labels.txt"
filename_tidy_output <- "tidy_data_set.txt"

filename_X_train <- "./UCI HAR Dataset/train/X_train.txt"
filename_y_train <- "./UCI HAR Dataset/train/y_train.txt"
filename_subject_train <- "./UCI HAR Dataset/train/subject_train.txt"


filename_X_test <- "./UCI HAR Dataset/test/X_test.txt"
filename_y_test <- "./UCI HAR Dataset/test/y_test.txt"
filename_subject_test <- "./UCI HAR Dataset/test/subject_test.txt"


## 1. Merges the training and the test sets to create one data set
features <- read.table(filename_features)
activity_labels <- read.table(filename_activity_labels)

X_train <- read.table(filename_X_train)
y_train <- read.table(filename_y_train)
subject_train <- read.table(filename_subject_train)
train <- cbind(subject_train,y_train,X_train)

X_test <- read.table(filename_X_test)
y_test <- read.table(filename_y_test)
subject_test <- read.table(filename_subject_test)
test <- cbind(subject_test,y_test,X_test)

train_test <- rbind(train,test)

## Part of step 1. Assigning the headers from features file
headers <- c("subject","activity",as.character(features[,2]))
headers <- make.unique(headers)
names(train_test) <- headers


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
headers_mean_std <- grep("(mean\\(\\)|std\\(\\)|subject|activity)",headers, value=TRUE)
HAR_mean_std <- select(train_test,headers_mean_std)

## 3. Uses descriptive activity names to name the activities in the data set
match_index <- match(HAR_mean_std[,2], activity_labels[,1])
HAR_mean_std[,2] <-  activity_labels[match_index,2]

## 4. Appropriately labels the data set with descriptive variable names 
headers <- names(HAR_mean_std)
headers <- gsub("\\(\\)","",headers)
headers <- gsub("X$", "XAxis", headers)
headers <- gsub("Y$", "YAxis", headers)
headers <- gsub("Z$", "ZAxis", headers)
headers <- gsub("std", "StdDeviation", headers)
headers <- gsub("mean", "Mean", headers)
headers <- gsub("Acc", "Accelerometer", headers)
headers <- gsub("Gyro", "Gyroscope", headers)
headers <- gsub("Mag", "Magnitude", headers)
headers <- gsub("-", ".", headers)
headers <- gsub("^t", "Time", headers)
headers <- gsub("^f", "Freq", headers)

names(HAR_mean_std) <- headers


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
HAR_tidy <- group_by(HAR_mean_std, subject, activity) %>% summarize_all(mean)


## Writing the tidy data to output file
write.table(HAR_tidy,file=filename_tidy_output, row.name = FALSE)

print(paste0("Output fle is saved at - ", getwd(),"/",filename_tidy_output))
