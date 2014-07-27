library(plyr)
training_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
training_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
training_y <- read.table("./UCI HAR Dataset/train/Y_train.txt")
test_y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

dataset1 <- rbind(training_x,test_x)
tempsubj <- rbind(training_subject,test_subject)
tempy <- rbind(training_y,test_y)
dataset2 <- cbind(dataset1,tempy,tempsubj)

## Providing variable names to the dataset
colnames(dataset2) <- c(as.character(features[,2]),
                        "activityNumber","subjectNumber")

## Using descriptive activity names to name the activites in the dataset
## as a separate column named activity_description
colnames(activity_labels) <- c("activityNumber","activity_description")
mergedData <- join(dataset2,activity_labels)

mergedData <- transform(mergedData,activityNumber=factor(activityNumber),
                      subjectNumber = factor(subjectNumber),
                      activity_description = factor(activity_description))

write.table(meanstdData,file = "combinedData.txt", quote = FALSE, 
            row.names = FALSE, sep = ",")

## Extracting only the mean and standard deviation measurements
n <- names(dataset2)
getmean <- grepl("mean",n)
y <- n[grepl("mean",n)]
z <- n[grepl("std",n)]
meanstd <- c(y,z)
meanstdData <- dataset2[,meanstd]

write.table(meanstdData,file = "meanStdData.txt", quote = FALSE, 
            row.names = FALSE, sep = ",")

## Computing average values for each column for each subject and each activity
avgData <- aggregate(mergedData[,1:561], 
                           by = list(activity = mergedData$activity_description,
                                     subjectNumber = mergedData$subjectNumber), 
                           FUN = mean1 <- function(...){
                               mean(...,na.rm = TRUE)
                           })

write.table(avgData,file = "averageData.txt", quote = FALSE, 
            row.names = FALSE, sep = ",")
