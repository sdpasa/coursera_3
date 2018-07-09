library(dplyr)

data_url <-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip' 
zipFile <- 'UCI HAR Dataset.zip'

if(!file.exists(zipFile)) download.file(data_url, zipFile, mode = 'wb')
filePath <- 'UCI HAR Dataset'
if(!file.exists(filePath)) unzip(zipFile)

# read training data
trainingSubjects <- read.table(file.path(filePath, 'train', 'subject_train.txt'))
trainingValues <- read.table(file.path(filePath, 'train', 'X_train.txt'))
trainingActivity <- read.table(file.path(filePath, 'train', 'y_train.txt'))

# read test data
testSubjects <- read.table(file.path(filePath, 'test', 'subject_test.txt'))
testValues <- read.table(file.path(filePath, 'test', 'X_test.txt'))
testActivity <- read.table(file.path(filePath, 'test', 'y_test.txt'))

features <- read.table(file.path(filePath, 'features.txt'), as.is = TRUE)
activities <- read.table(file.path(filePath, 'activity_labels.txt'))
colnames(activities) <- c('id', 'label')

# Task 1: Merges the training and the test sets to create one data set.
mergedResults <- bind_rows(bind_cols(trainingSubjects,trainingValues,trainingActivity),
                           bind_cols(testSubjects,testValues,testActivity))

names(mergedResults)
colnames(mergedResults) <- c('subject', features[, 2], 'activity')

# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
measurements <- grepl('subject|activity|mean|std', colnames(mergedResults))
activity_measurements <- mergedResults[, measurements]

# Task 3: Uses descriptive activity names to name the activities in the data set
activity_measurements$activity <- factor(activity_measurements$activity, 
                                         levels = activities[, 1], 
                                         labels = activities[, 2])

# Task 4: Appropriately labels the data set with descriptive variable names. 
columnNames <- colnames(activity_measurements)
columnNames <- gsub('[\\(\\)-]', '', columnNames)
columnNames <- gsub('^f', 'frequencyDomain', columnNames)
columnNames <- gsub('BodyBody', 'Body', columnNames)
columnNames <- gsub('^t', 'timeDomain', columnNames)
columnNames <- gsub('Acc', 'Accelerometer', columnNames)
columnNames <- gsub('Gyro', 'Gyroscope', columnNames)
columnNames <- gsub('Freq', 'Frequency', columnNames)
columnNames <- gsub('Mag', 'Magnitude', columnNames)
columnNames <- gsub('mean', 'Mean', columnNames)
columnNames <- gsub('std', 'StandardDeviation', columnNames)

print(columnNames)

#changes names
colnames(activity_measurements) <- columnNames

# Task 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
activity_subject <- activity_measurements %>% 
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

write.table(activity_subject, 'tidy_data.txt', row.names = FALSE, 
            quote = FALSE)

