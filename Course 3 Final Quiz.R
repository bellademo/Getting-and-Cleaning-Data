install.packages("dyplr")
install.packages("data.table")
install.packages("tidyverse")
library(data.table)
library(dplyr)
library(tidyverse)


filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method = "curl")
}

if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

features <- read.table("UCI HAR Dataset/features.txt", col.names = 
                         c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = 
                       features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names =
                       "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names =
                              "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names =
                        features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names =
                        "code")

#Merge training and test data sets
x_df <- rbind(x_train, x_test)
y_df <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test) 
merge_df <- cbind(subject, y_df, x_df)

#Finding mean and SD for each
tidydf <- merge_df %>% select(subject, code, contains("means"), contains("std"))

#Name activities using descriptive activities
tidydf$code <- activities[tidydf$code, 2]

#Change label names
names(tidydf)[2] = "activity"
names(tidydf) <- gsub("Acc", "Acceleromter", names(tidydf))
names(tidydf) <- gsub("Gyro", "Gyroscope", names(tidydf))
names(tidydf) <- gsub("BodyBody", "Body", names(tidydf))
names(tidydf) <- gsub("Mag", "Magnitude", names(tidydf))
names(tidydf) <- gsub("^t", "Time", names(tidydf))
names(tidydf) <- gsub("^f", "Frequency", names(tidydf))
names(tidydf) <- gsub("tBody", "TimeBody", names(tidydf))
names(tidydf) <- gsub("-mean()", "Mean", names(tidydf), ignore.case = TRUE)
names(tidydf) <- gsub("-std()", "STD", names(tidydf), ignore.case = TRUE)
names(tidydf) <- gsub("-freq()", "Frequency", names(tidydf), ignore.case = TRUE)
names(tidydf) <- gsub("angle", "Angle", names(tidydf))
names(tidydf) <- gsub("gravity", "Gravity", names(tidydf))

#Create independent tidy df w/ average of each variable to each activity/subject
final_df <- tidydf %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(final_df, "FinalData.txt", row.name = FALSE)

#Checking