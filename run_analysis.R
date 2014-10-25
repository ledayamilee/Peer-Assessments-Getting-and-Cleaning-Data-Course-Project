#1. Merges the training and the test sets to create one data set.


data1 <- read.table(file="X_train.txt", header=TRUE, sep="\t",)
data2 <- read.table(file="X_test.txt", header=TRUE, sep="\t",)
finalx <- rbind(data1, data2)
data1 <- read.table("train/subject_train.txt")
data2 <- read.table("test/subject_test.txt")
finalsubject <- rbind(data1, data2)
data1 <- read.table("train/y_train.txt")
data2 <- read.table("test/y_test.txt")
finaly <- rbind(data1, data2)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
extractmeasurements <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
finalx <- finalx[, extractmeasurements]
names(finalx) <- features[extractmeasurements, 2]
names(finalx) <- gsub("\\(|\\)", "", names(finalx))
names(finalx) <- tolower(names(finalx))


# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
finaly[,1] = activities[finaly[,1], 2]
names(finaly) <- "activity"



# 4. Appropriately labels the data set with descriptive variable names.

names(finalsubject) <- "subject"
cleaned <- cbind(finalsubject, finaly, finalx)
write.table(cleaned, "README.txt")



# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

eachsubject = unique(finalsubject)[,1]
numsubjects = length(unique(finalsubject)[,1])
numactivities = length(activities[,1])
numcols = dim(cleaned)[2]
result = cleaned[1:(numsubjects*numactivities), ]
row = 1
for (subject in 1:numsubjects) {
  for (activity in 1:numactivities) {
    result[row, 1] = eachsubject[subject]
    result[row, 2] = activities[activity, 2]
    tmp <- cleaned[cleaned$subject==subject & cleaned$activity==activities[activity, 2], ]
    result[row, 3:numcols] <- colmeans(tmp[, 3:numcols])
    row = row+1
  }
}

library(reshape2)
write.table(result, "tidy_data.txt", row.names =FALSE)
