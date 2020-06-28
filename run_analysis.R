#load package from url
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- "getdata-projectfiles-UCI HAR Dataset.zip"
path <- getwd()
#if file doesn't exit, download from url
if(!file.exists(filename)){
  download.file(fileURL, file.path(path, filename))
}
#unzip the file
unzip(filename)

#read activity
activity<- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("activityId", "activity"))

#read Features.
features<- read.table("UCI HAR Dataset/features.txt", header = FALSE, col.names = c("featureId", "feature"))

#read Test Data.
test_subject<- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subjectId")
test_y<-read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = "activityId")
test_x<-read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE, col.names = features[,2])


#read Train Data.
train_subject<- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subjectId")
train_y<- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = "activityId")
train_x<-read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE, col.names = features[,2])

#merge those data using rbind function
test<- cbind(test_subject, test_y, test_x)
train<- cbind(train_subject, train_y, train_x)
dataSet<-rbind(test,train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

dataSet<- dataSet[,grep("subject|activity|mean|std", colnames(dataSet))]

# 3. Uses descriptive activity names to name the activities in the data set

dataSet$activityId<- factor(dataSet$activityId, levels = activity[,1], labels = activity[,2])

# 4. Appropriately labels the data set with descriptive variable names.

dataColNames <- colnames(dataSet)
dataColNames <- gsub("\\.", "", dataColNames)
dataColNames <- gsub("^t", "timeDomain", dataColNames)
dataColNames <- gsub("^f", "frequencyDomain", dataColNames)
dataColNames <- gsub("Acc", "Accelerometer", dataColNames)
dataColNames <- gsub("activityId", "activity", dataColNames)
dataColNames <- gsub("BodyBody", "Body", dataColNames)
dataColNames <- gsub("Freq", "Frequency", dataColNames)
dataColNames <- gsub("Gyro", "Gyroscope", dataColNames)
dataColNames <- gsub("Mag", "Magnitude", dataColNames)
dataColNames <- gsub("mean", "Mean", dataColNames)
dataColNames <- gsub("std", "StandardDeviation", dataColNames)

colnames(dataSet)<- dataColNames

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dataMeans<- dataSet %>% 
      group_by(subjectId, activity) %>% 
      across(funs = mean)

#Generating the text file for submission.
if(!file.exists("tidyData.txt")){
  file.create("tidyData.txt")
  write.table(dataMeans, file = "tidyData.txt", row.names = FALSE)
}
