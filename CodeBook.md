#CodeBook — UCI Human Activity Recognition Using Smartphones Dataset (Tidy)

After executing the run_analysis.R script, the output tidy data set includes the following 88 variables below.

######Note: This code book only applies to the output from the run_analysis.R script, and is not necessarily applicable to the raw data

Variable | Description 
--- | --- | ---
**SubjectID** | Number assigned to the person subject
**TrainingType** | Six types of activity were tracked: Walking, walking upstairs, walking downstairs, sitting, standing and laying
**TimeBodyAcc-Mean-X** | Mean body motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeBodyAcc-Mean-Y** | Mean body motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeBodyAcc-Mean-Z** | Mean body motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeBodyAcc-StdDev-X** | Standard deviation of the body motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeBodyAcc-StdDev-Y** | Standard deviation of the body motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeBodyAcc-StdDev-Z** | Standard deviation of the body motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeGravityAcc-Mean-X** | Mean gravity motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeGravityAcc-Mean-Y** | Mean gravity motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeGravityAcc-Mean-Z** | Mean gravity motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeGravityAcc-StdDev-X** | Standard deviation of the motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeGravityAcc-StdDev-Y** | Standard deviation of the motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeGravityAcc-StdDev-Z** | Standard deviation of the motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeBodyAccJerk-Mean-X** | Mean body jerk motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeBodyAccJerk-Mean-Y** | Mean body jerk motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeBodyAccJerk-Mean-Z** | Mean body jerk motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeBodyAccJerk-StdDev-X** | Standard deviation of the body jerk motion acceleration from the accelerometer along the X-axis (time domain signal)
**TimeBodyAccJerk-StdDev-Y** | Standard deviation of the body jerk motion acceleration from the accelerometer along the Y-axis (time domain signal)
**TimeBodyAccJerk-StdDev-Z** | Standard deviation of the body jerk motion acceleration from the accelerometer along the Z-axis (time domain signal)
**TimeBodyGyro-Mean-X** | Mean body motion acceleration from the gyroscope along the X-axis (time domain signal)
**TimeBodyGyro-Mean-Y** | Mean body motion acceleration from the gyroscope along the Y-axis (time domain signal)
**TimeBodyGyro-Mean-Z** | Mean body motion acceleration from the gyroscope along the Z-axis (time domain signal)
**TimeBodyGyro-StdDev-X** | Standard deviation of the motion acceleration from the gyroscope along the X-axis (time domain signal)
**TimeBodyGyro-StdDev-Y** | Standard deviation of the motion acceleration from the gyroscope along the Y-axis (time domain signal)
**TimeBodyGyro-StdDev-Z** | Standard deviation of the motion acceleration from the gyroscope along the Z-axis (time domain signal)
**TimeBodyGyroJerk-Mean-X** | Mean body jerk motion from the gyroscope along the X-axis (time domain signal)
**TimeBodyGyroJerk-Mean-Y** | Mean body jerk motion from the gyroscope along the Y-axis (time domain signal)
**TimeBodyGyroJerk-Mean-Z** | Mean body jerk motion from the gyroscope along the Z-axis (time domain signal)
**TimeBodyGyroJerk-StdDev-X** | Standard deviation of the body jerk motion from the gyroscope along the X-axis (time domain signal)
**TimeBodyGyroJerk-StdDev-Y** | Standard deviation of the body jerk motion from the gyroscope along the Y-axis (time domain signal)
**TimeBodyGyroJerk-StdDev-Z** | Standard deviation of the body jerk motion from the gyroscope along the Z-axis (time domain signal)
**TimeBodyAccMagnitude-Mean** | Mean body accelerometer magnitude (time domain signal)
**TimeBodyAccMagnitude-StdDev** | Standard deviation of the body accelerometer magnitude (time domain signal)
**TimeGravityAccMagnitude-Mean** | Standard deviation of the gravity accelerometer magnitude (time domain signal)
**TimeGravityAccMagnitude-StdDev** | Standard deviation of the gravity accelerometer magnitude (time domain signal)
**TimeBodyAccJerkMagnitudenitude-Mean** |  (time domain signal)
**TimeBodyAccJerkMagnitudenitude-StdDev** |  (time domain signal)
**TimeBodyGyroMagnitude-Mean** |  (time domain signal)
**TimeBodyGyroMagnitude-StdDev** |  (time domain signal)
**TimeBodyGyroJerkMagnitude-Mean** |  (time domain signal)
**TimeBodyGyroJerkMagnitude-StdDev** |  (time domain signal)
**FreqBodyAcc-Mean-X** |  (frequency domain signal)
**FreqBodyAcc-Mean-Y** | (frequency domain signal)
**FreqBodyAcc-Mean-Z** | (frequency domain signal)
**FreqBodyAcc-StdDev-X** | (frequency domain signal)
**FreqBodyAcc-StdDev-Y** | 
**FreqBodyAcc-StdDev-Z** | 
**FreqBodyAcc-MeanFreq-X** | (frequency domain signal)
**FreqBodyAcc-MeanFreq-Y** | 
**FreqBodyAcc-MeanFreq-Z** | 
**FreqBodyAccJerk-Mean-X** | (frequency domain signal)
**FreqBodyAccJerk-Mean-Y** | 
**FreqBodyAccJerk-Mean-Z** | 
**FreqBodyAccJerk-StdDev-X** | (frequency domain signal)
**FreqBodyAccJerk-StdDev-Y** | 
**FreqBodyAccJerk-StdDev-Z** | 
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
