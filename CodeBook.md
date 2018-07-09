Code Book for Coursera Getting and Cleaning Data Course Project

Introduction

This Code Book describes the data used in this project, the variables used as well as data processing performed to create a tidy data set.

A tidy data set can be described as a data set where:

each variable forms a column;
each observation forms a row; and
each type of observational unit forms a table.
Source Data

The specific data for this project was obtained from:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The above data was based on sensor signals (accelerometers and gyroscopes) from the Samsung Galaxy S smartphones worn on the waists of 30 volunteers performing 6 different activities (walking, walking upstairs, walking downstairs, sitting, standing and laying).

Raw signal data contained in the Inertial Signals folders were not used in this project.

The raw time domain signals were filtered to remove noise. The acceleration signal was then separated into body and gravity acceleration signals and filtered. Subsequently, body linear acceleration and angular velocity were derived in time to obtain Jerk signals. Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm. A Fast Fourier Transform (FFT) was applied to some of these signals (frequency domain signals).

The above signals were used to estimate 561 variables of the feature vector for each pattern. Note X, Y and Z are used to denote 3-axial signals in the X, Y and Z directions.

The obtained dataset was then randomly partitioned into two sets, where 21 of the volunteers was selected for generating the training data and 9 were selected for the test data. The following provides details on the partitioned data sets and related variables:

Training Data

X_train.txt - contains 7,352 observations of 561 features with respect to 21 volunteers in the training group
y_train.txt - contains the ID indicator of the volunteer associated with each of the above 7,352 observations
subject_train.txt - contains the activity indicators associated with each of the above 7,352 observations

Test Data

X_test.text - contains 2,947 observations of 561 features with respect to 9 volunteers in the test group
y_train.txt - contains the ID indicator of the volunteer associated with each of the above 2,947 observations
subject_test.txt - contains the activity indicators associated with each of the above 2,947 observations

Activity Variable

activity_labels.txt contains the activity indicators and can be summarized as follows:
1 - WALKING
2 - WALKING_UPSTAIRS
3 - WALKING_DOWNSTAIRS
4 - SITTING
5 - STANDING
6 - LAYING

Feature Variables

features.txt - contains the variable names pertaining to the 561 elements of each feature vector

Features are normalized and bounded within [-1,1]

features_info.txt - contains addtional information about the features

Additional Information

For more information about the source data please refer to the README.txt.

Data Processing

Data processing is completed using the run_analysis.R script, which:

reads the above text files
merges the training and test data creating a single data.frame containing 10,299 observations of 561 features
merges the 2 activity indicator files creating a single data.frame (dimensions: 10,299 x 1)
merges the 2 ID indicator files creating a single data.frame (dimensions: 10,299 x 1)
adds appropriate column names to to each data.frame
merges the 3 merged data.frames into a new single data.frame (dimensions: 10,299 x 563)
constructs a smaller data.frame by extracting from the merged data.frame only those columns that referenced mean or standard deviation as required for this a project (dimensions: 10,299 x 81)
converts the activity indicators to factors with appropriate labels (WALKING, STANDING, etc.)
modifies the abbreviated column names to be more descriptive and removes any dashes (modified variable names are summarized in the next section)
creates a second, independent tidy data set with the average of each variable for each activity and each subject (dimensions: 180 x 81)
writes the tidy data set to a text file (tidyData.txt)
Variables of Tidy Data Set

The variables of the tidy data set are as follows:

Subject and Activity Variables

Subject : Volunteer ID indicator (integer)
Activity: Activity performed by volunteer (factor with 6 levels "WALKING"", "STANDING", etc. as described above)

Feature Variables

Feature variables are numeric and expressed as the average of the variable for each volunteer for each activity.

Mean Time Domain Body Acceleration in X, Y and Z directions:

timeBodyAccelerometerMean()X
timeBodyAccelerometerMean()Y
timeBodyAccelerometerMean()Z
Standard Deviation of the Time Domain Body Acceleration in the X, Y and Z directions:

timeBodyAccelerometerStandardDeviation()X
timeBodyAccelerometerStandardDeviation()Y
timeBodyAccelerometerStandardDeviation()Z
Mean Time Domain Gravity Acceleration in the X, Y, and Z directions:

timeGravityAccelerometerMean()X
timeGravityAccelerometerMean()Y
timeGravityAccelerometerMean()Z
Standard Deviation of the Time Domain Gravity Acceleration in the X, Y, and Z directions:

timeGravityAccelerometerStandardDeviation()X
timeGravityAccelerometerStandardDeviation()Y
timeGravityAccelerometerStandardDeviation()Z
Mean Time Domain Body Linear Acceleration Jerk Signal in the X, Y and Z directions:

timeBodyAccelerometerJerkMean()X
timeBodyAccelerometerJerkMean()Y
timeBodyAccelerometerJerkMean()Z
Standard Deviation of the Time Domain Linear Acceleration Jerk Signal in the X, Y, and Z directions:

timeBodyAccelerometerJerkStandardDeviation()X
timeBodyAccelerometerJerkStandardDeviation()Y
timeBodyAccelerometerJerkStandardDeviation()Z
Mean Time Domain Body Angular Velocity in the X, Y and Z directions:

timeBodyGyroscopeMean()X
timeBodyGyroscopeMean()Y
timeBodyGyroscopeMean()Z
Standard Deviation of the Time Domain Body Angular Velocity in the X, Y, and Z directions:

timeBodyGyroscopeStandardDeviation()X
timeBodyGyroscopeStandardDeviation()Y
timeBodyGyroscopeStandardDeviation()Z
Mean Time Domain Body Angular Velocity Jerk Signal in the X, Y and Z directions:

timeBodyGyroscopeJerkMean()X
timeBodyGyroscopeJerkMean()Y
timeBodyGyroscopeJerkMean()Z
Standard Deviation of the Time Domain Body Angular Velocity Jerk Signal in the X, Y and Z directions:

timeBodyGyroscopeJerkStandardDeviation()X
timeBodyGyroscopeJerkStandardDeviation()Y
timeBodyGyroscopeJerkStandardDeviation()Z
Mean Time Domain Body Acceleration Magnitude:

timeBodyAccelerometerMagnitudeMean()
Standard Deviation of the Time Domain Body Acceleration Magnitude:

timeBodyAccelerometerMagnitudeStandardDeviation()
Mean Time Domain Gravity Acceleration Magnitude:

timeGravityAccelerometerMagnitudeMean()
Standard Deviation of the Time Domain Gravity Acceleration Magnitude:

timeGravityAccelerometerMagnitudeStandardDeviation()
Mean Time Domain Body Linear Acceleration Jerk Signal Magnitude:

timeBodyAccelerometerJerkMagnitudeMean()
Standard Deviation of the Time Domain Body Linear Acceleration Magnitude:

timeBodyAccelerometerJerkMagnitudeStandardDeviation()
Mean Time Domain Body Angular Velocity Magnitude:

timeBodyGyroscopeMagnitudeMean()
Standard Deviation of the Time Domain Body Angular Velocity Magnitude:

timeBodyGyroscopeMagnitudeStandardDeviation()
Mean Time Domain Body Angular Velocity Jerk Signal Magnitude:

timeBodyGyroscopeJerkMagnitudeMean()
Standard Deviation of the Time Domain Body Angular Velocity Jerk Signal Magnitude:

timeBodyGyroscopeJerkMagnitudeStandardDeviation()
Mean Frequency Domain Body Acceleration in X, Y and Z directions:

frequencyBodyAccelerometerMean()X
frequencyBodyAccelerometerMean()Y
frequencyBodyAccelerometerMean()Z
Standard Deviation of the Frequency Domain Body Acceleration in the X, Y and Z directions:

frequencyBodyAccelerometerStandardDeviation()X
frequencyBodyAccelerometerStandardDeviation()Y
frequencyBodyAccelerometerStandardDeviation()Z
Mean Frequency Domain Body Acceleration Mean Frequency in the X, Y and Z directions:

frequencyBodyAccelerometerMeanFrequency()X
frequencyBodyAccelerometerMeanFrequency()Y
frequencyBodyAccelerometerMeanFrequency()Z
Mean Frequency Domain Body Linear Acceleration Jerk Signal in the X, Y and Z directions:

frequencyBodyAccelerometerJerkMean()X
frequencyBodyAccelerometerJerkMean()Y
frequencyBodyAccelerometerJerkMean()Z
Standard Deviation of the Frequency Domain Body Linear Acceleration Jerk Signal in the X, Y and Z directions:

frequencyBodyAccelerometerJerkStandardDeviation()X
frequencyBodyAccelerometerJerkStandardDeviation()Y
frequencyBodyAccelerometerJerkStandardDeviation()Z
Mean Frequency Domain Body Linear Acceleration Jerk Signal Frequency in the X, Y and Z directions:

frequencyBodyAccelerometerJerkMeanFrequency()X
frequencyBodyAccelerometerJerkMeanFrequency()Y
frequencyBodyAccelerometerJerkMeanFrequency()Z
Mean Frequency Domain Body Angular Velocity in the X, Y and Z directions:

frequencyBodyGyroscopeMean()X
frequencyBodyGyroscopeMean()Y
frequencyBodyGyroscopeMean()Z
Standard Deviation of the Frequency Domain Body Angular Velocity in the X, Y and Z directions:

frequencyBodyGyroscopeStandardDeviation()X
frequencyBodyGyroscopeStandardDeviation()Y
frequencyBodyGyroscopeStandardDeviation()Z
Mean Frequency Domain Body Angular Velocity Frequency in the X, Y and Z directions:

frequencyBodyGyroscopeMeanFrequency()X
frequencyBodyGyroscopeMeanFrequency()Y
frequencyBodyGyroscopeMeanFrequency()Z
Mean Frequency Domain Body Acceleration Magnitude:

frequencyBodyAccelerometerMagnitudeMean()
Standard Deviation of the Frequency Domain Body Acceleration Magnitude:

frequencyBodyAccelerometerMagnitudeStandardDeviation()
Mean Frequency Domain Body Acceleration Magnitude Frequency:

frequencyBodyAccelerometerMagnitudeMeanFrequency()
Mean Frequency Domain Body Linear Acceleration Jerk Signal Magnitude:

frequencyBodyAccelerometerJerkMagnitudeMean()
Standard Deviation of the Frequency Domain Body Linear Acceleration Jerk Signal Magnitude:

frequencyBodyAccelerometerJerkMagnitudeStandardDeviation()
Mean Frequency Domain Body Linear Acceleration Jerk Signal Magnitude Frequency:

frequencyBodyAccelerometerJerkMagnitudeMeanFrequency()
Mean Frequency Domain Body Angular Velocity Magnitude:

frequencyBodyGyroscopeMagnitudeMean()
Standard Deviation of the Frequency Domain Body Angular Velocity Magnitude:

frequencyBodyGyroscopeMagnitudeStandardDeviation()
Mean Frequency Domain Body Angular Velocity Magnitude Frequency:

frequencyBodyGyroscopeMagnitudeMeanFrequency()
Mean Frequency Domain Body Angular Velocity Jerk Signal Magnitude:

frequencyBodyGyroscopeJerkMagnitudeMean()
Standard Deviation of the Frequency Domain Body Angular Velocity Jerk Signal Magnitude:

frequencyBodyGyroscopeJerkMagnitudeStandardDeviation()
Mean Frequency Domain Body Angular Velocity Jerk Signal Magnitude Frequency:

frequencyBodyGyroscopeJerkMagnitudeMeanFrequency()
