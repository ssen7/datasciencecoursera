library(reshape2)
library(plyr)
## clean environment
rm(list = ls())

## read feature labels
feature_labels <- read.table("./GCDataset/features.txt")

## read activity labels
activity_lab <- read.table("./GCDataset/activity_labels.txt")

## grep and extract mean labels
mean_posn <- grep("mean", feature_labels$V2, ignore.case = TRUE)
mean_labels <- as.character(feature_labels[mean_posn,]$V2)

## grep and extract standard deviation labels
sd_posn <- grep("std()", feature_labels$V2, ignore.case = TRUE)
sd_labels <- as.character(feature_labels[sd_posn, ]$V2)

##get mean and std data together
mean_std_posn <- c(mean_posn, sd_posn)
mean_sd_labels <- c(mean_labels, sd_labels)

## read train and test set
x_train <- read.table("./GCDataset/train/X_train.txt")
x_test <- read.table("./GCDataset/test/X_test.txt")

## extract mean and sd data from x_train and x_test
data_train <- x_train[, mean_std_posn]
data_test <- x_test[, mean_std_posn]

## extract training and test subjects
subject_train <- read.table("./GCDataset/train/subject_train.txt")
#generate training labels
train <- rep("train", nrow(subject_train))
subject_train$SubjectType = train
subject_test <- read.table("./GCDataset/test/subject_test.txt")
#generate test labels
test <- rep("test", nrow(subject_test))
subject_test$SubjectType <- test

## training and test activities
y_train <- read.table("./GCDataset/train/y_train.txt")
y_test <- read.table("./GCDataset/test/y_test.txt")

## find activity names in y_train
y_train_labels <- sapply(y_train$V1, FUN = function(dat) {activity_lab[dat, 2]}, simplify = TRUE)
y_test_labels <- sapply(y_test$V1, FUN = function(dat) {activity_lab[dat, 2]}, simplify = TRUE)
y_train$ActivityName <- y_train_labels
y_test$ActivityName <- y_test_labels
colnames(y_train)[1] <- c("ActivityNum")
colnames(y_test)[1] <- c("ActivityNum")

## combining test and training data
data_final <- rbind(data_train, data_test)
y_final <- rbind(y_train, y_test)
subject_final <- rbind(subject_train, subject_test)

## renaming columns to describe their function
colnames(subject_final)[1] <- c("SubjectNumber")
colnames(data_final) <- mean_sd_labels

## final combining of test and training mean and standard deviation data.
table_final <- cbind(subject_final, y_final, data_final)

## Melting Data Frames according to activity and subject
finalMelt <- melt(table_final, id = c("SubjectNumber", "ActivityName"), measure.vars =  mean_sd_labels)

## Cast the data frame into a 3D array with subject and activity as variables to find mean
finalCast <- acast(finalMelt, SubjectNumber ~ ActivityName ~ variable, mean)

## Convert the casting to a data frame
finalDF <- as.data.frame(finalCast)

## Get the rownames as SubjectNumbers since the rows represents the ID of the subject
finalDF <- cbind(rownames(finalDF), finalDF)
colnames(finalDF)[1] <- c("SubjectNumber")

## Write the data out to a text file
write.table(finalDF, file = "data-gc.csv",sep = ",", row.names = FALSE)