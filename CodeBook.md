#CodeBook — UCI Human Activity Recognition Using Smartphones Dataset (Tidy)

After executing the run_analysis.R script, the output tidy data set includes the following 88 variables below.

######Note: This code book only applies to the output from the run_analysis.R script, and is not necessarily applicable to the raw data

Variable | Description 
--- | --- | ---
**SubjectID** | Number assigned to the person subject
**TrainingType** | Six types of activity were tracked: Walking, walking upstairs, walking downstairs, sitting, standing and laying
**tBodyAcc-mean()-X** | Mean body motion acceleration from the accelerometer along the X-axis (time domain signal)
**tBodyAcc-mean()-Y** | Mean body motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tBodyAcc-mean()-Z** | Mean body motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tBodyAcc-std()-X** | Standard deviation of the body motion acceleration from the accelerometer along the X-axis (time domain signal)
**tBodyAcc-std()-Y** | Standard deviation of the body motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tBodyAcc-std()-Z** | Standard deviation of the body motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tGravityAcc-mean()-X** | Mean gravity motion acceleration from the accelerometer along the X-axis (time domain signal)
**tGravityAcc-mean()-Y** | Mean gravity motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tGravityAcc-mean()-Z** | Mean gravity motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tGravityAcc-std()-X** | Standard deviation of the motion acceleration from the accelerometer along the X-axis (time domain signal)
**tGravityAcc-std()-Y** | Standard deviation of the motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tGravityAcc-std()-Z** | Standard deviation of the motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tBodyAccJerk-mean()-X** | Mean body jerk motion acceleration from the accelerometer along the X-axis (time domain signal)
**tBodyAccJerk-mean()-Y** | Mean body jerk motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tBodyAccJerk-mean()-Z** | Mean body jerk motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tBodyAccJerk-std()-X** | Standard deviation of the body jerk motion acceleration from the accelerometer along the X-axis (time domain signal)
**tBodyAccJerk-std()-Y** | Standard deviation of the body jerk motion acceleration from the accelerometer along the Y-axis (time domain signal)
**tBodyAccJerk-std()-Z** | Standard deviation of the body jerk motion acceleration from the accelerometer along the Z-axis (time domain signal)
**tBodyGyro-mean()-X** | Mean body motion acceleration from the gyroscope along the X-axis (time domain signal)
**tBodyGyro-mean()-Y** | Mean body motion acceleration from the gyroscope along the Y-axis (time domain signal)
**tBodyGyro-mean()-Z** | Mean body motion acceleration from the gyroscope along the Z-axis (time domain signal)
**tBodyGyro-std()-X** | Standard deviation of the motion acceleration from the gyroscope along the X-axis (time domain signal)
**tBodyGyro-std()-Y** | Standard deviation of the motion acceleration from the gyroscope along the Y-axis (time domain signal)
**tBodyGyro-std()-Z** | Standard deviation of the motion acceleration from the gyroscope along the Z-axis (time domain signal)
**tBodyGyroJerk-mean()-X** |  (time domain signal)
**tBodyGyroJerk-mean()-Y** | 
**tBodyGyroJerk-mean()-Z** | 
**tBodyGyroJerk-std()-X** |  (time domain signal)
**tBodyGyroJerk-std()-Y** | 
**tBodyGyroJerk-std()-Z** | 
**tBodyAccMag-mean()** |  (time domain signal)
**tBodyAccMag-std()** |  (time domain signal)
**tGravityAccMag-mean()** |  (time domain signal)
**tGravityAccMag-std()** |  (time domain signal)
**tBodyAccJerkMag-mean()** |  (time domain signal)
**tBodyAccJerkMag-std()** |  (time domain signal)
**tBodyGyroMag-mean()** |  (time domain signal)
**tBodyGyroMag-std()** |  (time domain signal)
**tBodyGyroJerkMag-mean()** |  (time domain signal)
**tBodyGyroJerkMag-std()** |  (time domain signal)
**fBodyAcc-mean()-X** |  (frequency domain signal)
**fBodyAcc-mean()-Y** | (frequency domain signal)
**fBodyAcc-mean()-Z** | (frequency domain signal)
**fBodyAcc-std()-X** | (frequency domain signal)
**fBodyAcc-std()-Y** | 
**fBodyAcc-std()-Z** | 
**fBodyAcc-meanFreq()-X** | (frequency domain signal)
**fBodyAcc-meanFreq()-Y** | 
**fBodyAcc-meanFreq()-Z** | 
**fBodyAccJerk-mean()-X** | (frequency domain signal)
**fBodyAccJerk-mean()-Y** | 
**fBodyAccJerk-mean()-Z** | 
**fBodyAccJerk-std()-X** | (frequency domain signal)
**fBodyAccJerk-std()-Y** | 
**fBodyAccJerk-std()-Z** | 
**fBodyAccJerk-meanFreq()-X** | (frequency domain signal)
**fBodyAccJerk-meanFreq()-Y** | 
**fBodyAccJerk-meanFreq()-Z** | 
**fBodyGyro-mean()-X** | (frequency domain signal)
**fBodyGyro-mean()-Y** | 
**fBodyGyro-mean()-Z** | 
**fBodyGyro-std()-X** | (frequency domain signal)
**fBodyGyro-std()-Y** | 
**fBodyGyro-std()-Z** | 
**fBodyGyro-meanFreq()-X** | (frequency domain signal)
**fBodyGyro-meanFreq()-Y** | 
**fBodyGyro-meanFreq()-Z** | 
**fBodyAccMag-mean()** | (frequency domain signal)
**fBodyAccMag-std()** | (frequency domain signal)
**fBodyAccMag-meanFreq()** | (frequency domain signal)
**fBodyBodyAccJerkMag-mean()** | (frequency domain signal)
**fBodyBodyAccJerkMag-std()** | (frequency domain signal)
**fBodyBodyAccJerkMag-meanFreq()** | (frequency domain signal)
**fBodyBodyGyroMag-mean()** | (frequency domain signal)
**fBodyBodyGyroMag-std()** | (frequency domain signal)
**fBodyBodyGyroMag-meanFreq()** | (frequency domain signal)
**fBodyBodyGyroJerkMag-mean()** | (frequency domain signal)
**fBodyBodyGyroJerkMag-std()** | (frequency domain signal)
**fBodyBodyGyroJerkMag-meanFreq()** | (frequency domain signal)
**angle(tBodyAccMean,gravity)** | (frequency domain signal)
**angle(tBodyAccJerkMean),gravityMean)** | (frequency domain signal)
**angle(tBodyGyroMean,gravityMean)** | (frequency domain signal)
**angle(tBodyGyroJerkMean,gravityMean)** | (frequency domain signal)
**angle(X,gravityMean)** | (frequency domain signal)
**angle(Y,gravityMean)** | 
**angle(Z,gravityMean)** | 
