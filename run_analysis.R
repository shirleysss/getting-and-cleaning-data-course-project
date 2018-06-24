setwd("/Users/SJY/Desktop/SJY_proj/GitHub/datasciencecoursera/course\ 3/proj")

library(dplyr)

# Download the dataset
if(!file.exists("dataset.zip")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile="dataset.zip",method="curl")
}

# Unzip the dataset
if (!file.exists("UCI HAR Dataset")) { 
    unzip("dataset.zip") 
}

# read the training data
subject_train <- read.table(file.path("UCI HAR Dataset","train","subject_train.txt"))
x_train <- read.table(file.path("UCI HAR Dataset","train","X_train.txt"))
y_train <- read.table(file.path("UCI HAR Dataset","train","y_train.txt"))

# read the test data
subject_test <- read.table(file.path("UCI HAR Dataset","test","subject_test.txt"))
x_test <- read.table(file.path("UCI HAR Dataset","test","X_test.txt"))
y_test <- read.table(file.path("UCI HAR Dataset","test","y_test.txt"))


#####################################################################################
# Merges the training and the test sets to create one data set.
train <- cbind(subject_train, x_train, y_train)
test <- cbind(subject_test, x_test, y_test)
merged <- rbind(train, test)

# read the features
feature <- read.table(file.path("UCI HAR Dataset","features.txt"), colClasses = c("character"))[,2]
colnames(merged) <- c("subject", feature, "activity")


#####################################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement.
meanstddata <- merged[,grepl("subject|activity|mean|std", colnames(merged))]


#####################################################################################
# Uses descriptive activity names to name the activities in the data set
# read the activity_labels
activity_label <- read.table(file.path("UCI HAR Dataset","activity_labels.txt"))

# replace the activityID with activity name
meanstddata$activity <- factor(meanstddata$activity, levels = activity_label[,1], labels = activity_label[,2])


#####################################################################################
# Appropriately labels the data set with descriptive variable names.
names(meanstddata) <- gsub("\\()", "", names(meanstddata))
names(meanstddata) <- gsub("^t", "TimeDomain", names(meanstddata))
names(meanstddata) <- gsub("^f", "FrequencyDomain", names(meanstddata))
names(meanstddata) <- gsub("Acc", "Acceleration", names(meanstddata))
names(meanstddata) <- gsub("Gyro", "Gyroscope", names(meanstddata))
names(meanstddata) <- gsub("Mag", "Magnitude", names(meanstddata))
names(meanstddata) <- gsub("Freq", "Frequency", names(meanstddata))
names(meanstddata) <- gsub("mean", "Mean", names(meanstddata))
names(meanstddata) <- gsub("std", "StandardDeviation", names(meanstddata))


#####################################################################################
# From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.
tidydata <- meanstddata %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

# output the tidy data
write.table(tidydata, "Tidy_Data.txt", row.names = FALSE)



