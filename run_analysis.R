# Week 4 Project
# Udaya Tejwani
# 11/20/2018
###########################################

#########################################################################
# Step 1. Merges the training and the test sets to create one data set. #
#########################################################################
setwd("C:\\Users\\Udaya\\Documents\\DataScience\\Course 3\\Week 4\\Project\\UCI HAR Dataset");
activity_labels <- read.table("activity_labels.txt", header = FALSE);
colnames(activity_labels) <- c("activityId","activityType");
features <- read.table("features.txt", header = FALSE);
subject_train <- read.table("./train/subject_train.txt");
colnames(subject_train) <- "subjectId";
X_train <- read.table("./train/X_train.txt", header = FALSE);
colnames(X_train) <- features[,2];
y_train <- read.table("./train/y_train.txt", header = FALSE);
colnames(y_train) <- "activityId";
subject_test <- read.table("./test/subject_test.txt", header = FALSE);
colnames(subject_test) <- "subjectId";
X_test <- read.table("./test/X_test.txt", header = FALSE);
colnames(X_test) <- features[,2];
y_test <- read.table("./test/y_test.txt", header = FALSE);
colnames(y_test) <- "activityId";
all_train <- cbind(subject_train, X_train, y_train);
all_test <- cbind(subject_test, X_test, y_test);
all_data <- rbind(all_train, all_test);

###################################################################################################
# Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. #
###################################################################################################
colNames <- colnames(all_data);
mean_and_std <- (grepl("activityId", colNames) 
                 | grepl("subjectId", colNames) 
                 | grepl("mean..", colNames) 
                 | grepl("std...", colNames));
mean_std_data <- all_data[,mean_and_std == TRUE];

##################################################################################
# Step 3. Uses descriptive activity names to name the activities in the data set #
##################################################################################
mean_std_data <- merge(mean_std_data, activity_labels, by = 'activityId', 
                       all.x = TRUE);

###############################################################################
# Step 4.  Appropriately labels the data set with descriptive variable names. #
###############################################################################
columns <- colnames(mean_std_data);
for (i in 1:length(columns)) {
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
  };
colnames(mean_std_data) <- columns;
##########################################################################################################################################################
# Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. #
##########################################################################################################################################################
tidy_data <- aggregate(. ~subjectId + activityId, mean_std_data, mean);
tidy_data <- tidy_data[order(tidy_data$subjectId, tidy_data$activityId),];
write.table(tidy_data, "tidySet.txt", row.names = FALSE)
