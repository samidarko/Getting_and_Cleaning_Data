#' ---
#' output:
#'   md_document:
#'      variant: markdown_github
#' ---
#' ### Prerequisite

#' **library import**
library(data.table)

#' **variables setup**
dataSetFileName <- 'dataset.zip'
dataSetDirName <- 'UCI HAR Dataset'

#' **Dataset download**
if (!file.exists(dataSetFileName)) {
  url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip';
  download.file(url, method = 'curl', destfile = dataSetFileName)
}

#' **Dataset extraction**
if (!file.exists(dataSetDirName)) {
  unzip(dataSetFileName)
}

#+
dir()

#' ### 1) Merges the training and the test sets to create one data set

#' **Merge X**
print('Merge X')
X <- rbind(
  read.table(file.path(dataSetDirName, 'train', 'X_train.txt')), # read X train
  read.table(file.path(dataSetDirName, 'test', 'X_test.txt')) # read X test
  ) # row bind X train and X test to get a single X

#' **Merge y**
print('Merge Y')
y <- rbind(
  read.table(file.path(dataSetDirName, 'train', 'y_train.txt')), # read y train
  read.table(file.path(dataSetDirName, 'test', 'y_test.txt')) # read y test
) # row bind y train and y test to get a single y

#' **Merge subject**
print('Merge subject')
subject <- rbind(
  read.table(file.path(dataSetDirName, 'train', 'subject_train.txt')), # read subject train
  read.table(file.path(dataSetDirName, 'test', 'subject_test.txt')) # read subject test
) # row bind subject train and subject test to get a single subject

#' ### 2) Extracts only the measurements on the mean and standard deviation for each measurement

features <- read.table(file.path(dataSetDirName, 'features.txt'))
selectedMeasurements <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])   # create an index
X <- X[, selectedMeasurements]
names(X) <- features[selectedMeasurements, 2]
names(X) <- gsub("\\(|\\)", "", names(X))   # clean the measurements names (remove "()")

#' ### 3) Uses descriptive activity names to name the activities in the data set

activities <- read.table(file.path(dataSetDirName, 'activity_labels.txt'))
activities[,2] <- as.character(activities[,2])
y[,1] = activities[y[,1], 2]
names(y) <- "activity"

#' ### 4) Appropriately labels the data set with descriptive variable names
names(subject) <- "subject"
tidyDataSet <- cbind(subject, y, X)
write.table(tidyDataSet, "tidy_dataset.txt", row.names = FALSE)


#' ### 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
subjectsList <- sort(unique(tidyDataSet$subject))    # List of all the subjects sorted
activitiesList <- activities$V2
variablesList <- names(X)
tdsa <- data.frame()   # empty dataset

activitiesRow = vector()  # I kept bumping into the following error so I just used this workaround : invalid factor level, NA generated

for (s in subjectsList) {
  for (activity in activitiesList) {
    dataSetRow <- c(s, factor(activity))  # create my subject and activity column
    activitiesRow <- append(activitiesRow, activity)

    indexing <- tidyDataSet$subject == s & tidyDataSet$activity == activity   # for clarity I prepare my indexing here
    dataSetRow <- append(dataSetRow, sapply(variablesList, function(variable) { mean(tidyDataSet[ indexing, variable]) } ))   # I append directly the result of sapply to dataSetRow
    
    tdsa <- rbind(tdsa, dataSetRow)
  }
}

tdsa[, 2] <- activitiesRow # still fixing the "invalid factor level" issue
names(tdsa) <- append(c('subject', 'activity'), variablesList)

write.table(tdsa, "data_set_averages.txt", row.names = FALSE)
