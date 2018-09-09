#loading reshape library
library("reshape")


#1.  Load activity labels + features

#read activity_labels.txt file as table
activityLabels <- read.table("activity_labels.txt")

#converting into two columns based of characters
activityLabels[,2] <- as.character(activityLabels[,2])

#reading features.txt file as table
features <- read.table("features.txt")

#converting into two columns based of characters
features[,2] <- as.character(features[,2])




#2. Extract only the data on mean and standard deviation

#matching .mean and .std using grep function
featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted

#getting just feature names
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names

#using groupsubstitute function, replacing -mean with "Mean"
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names

#using groupsubstitute function, replacing -std with "std"
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names

#using groupsubstitute function, replacing -() with "" or basically removing the brackets
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)
featuresWanted.names




#3.  Load the datasets

#reading X_train.txt file based on featuresWanted
train <- read.table("train/X_train.txt")[featuresWanted]
head(train)

#reading y_train and subject_train files
trainActivities <- read.table("train/Y_train.txt")
trainSubjects <- read.table("train/subject_train.txt")

#binding using columns of all the three files.
train <- cbind(trainSubjects, trainActivities, train)
head(train)

#similary for the test data
test <- read.table("test/X_test.txt")[featuresWanted]
testActivities <- read.table("test/Y_test.txt")
testSubjects <- read.table("test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)




#4.  merge datasets and add labels

#row binding the train and test data
allData <- rbind(train, test)
head(allData)

#adding heading as subject and acitvity to allData
colnames(allData) <- c("subject", "activity", featuresWanted.names)
colnames(allData)




#5.  turn activities & subjects into factors

#factoring activity data as storing them as levels
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$activity

#factoring subject data as well (produces 30 different levels)
allData$subject <- as.factor(allData$subject)
allData$subject

#melting all the data based on subject and acitivity columns so that it creates unique values
allData.melted <- melt(allData, id = c("subject", "activity"))
allData.melted

#taking mean
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)
head(allData.mean)

#writing into tidy.txt as table with values of mean.
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
