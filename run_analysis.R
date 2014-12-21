run_analysis <- function(){
  # download zip file and extract data sets of interest
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
  
  # get column names for features
  c_names_df <- read.table(unz(temp, "UCI HAR Dataset/features.txt"),stringsAsFactors = NA)
  feature_col_names <- as.vector(c_names_df[,2])
  
  # get out activity mapping for later
  activity_names_df <- read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"),stringsAsFactors = NA,col.names=c('activity_label_id','activity_label'))
    
  # work on training data firstly
  # features
  train_data_x <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"),col.names=feature_col_names)
  # activity label
  train_data_y <- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"),col.names=c('activity_label_id'))
  # subject id
  train_data_subject <- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"),col.names=c('subject_id'))
  # combine into one data set
  train_data <- cbind(train_data_subject, train_data_y, train_data_x)
  
  # now work on test data
  # features
  test_data_x <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"),col.names=feature_col_names)
  # activity label
  test_data_y <- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"),col.names=c('activity_label_id'))
  # subject id
  test_data_subject <- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"),col.names=c('subject_id'))
  # combine into one data set
  test_data <- cbind(test_data_subject, test_data_y, test_data_x)
  # free up temp file
  unlink(temp)
  
  # 1. Merges the training and the test sets to create one data set.
  merged_data <- rbind(train_data,test_data)
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  # use regex to help with processes
  subset_merged_data <- merged_data[,grepl("mean\\.",names(merged_data)) | grepl("std\\.",names(merged_data)) | grepl("subject_id",names(merged_data)) | grepl("activity_label_id",names(merged_data))]
  
  # 3. Uses descriptive activity names to name the activities in the data set
  subset_merged_data_with_activity_names <- merge(subset_merged_data,activity_names_df,by='activity_label_id')
  
  # 4. Appropriately labels the data set with descriptive variable names.
  colnames(subset_merged_data_with_activity_names) <- c("activity_label_id","subject_id","Time Domain - Mean Value of Body Acceleration Signal (X axis)","Time Domain - Mean Value of Body Acceleration Signal (Y axis)"
                                                        ,"Time Domain - Mean Value of Body Acceleration Signal (Z axis)","Time Domain - Standard Deviation of Body Acceleration Signal (X axis)","Time Domain - Standard Deviation of Body Acceleration Signal (Y axis)"
                                                        ,"Time Domain - Standard Deviation of Body Acceleration Signal (Z axis)","Time Domain - Mean Value of Gravity Acceleration Signal (X axis)","Time Domain - Mean Value of Gravity Acceleration Signal (Y axis)"
                                                        ,"Time Domain - Mean Value of Gravity Acceleration Signal (Z axis)","Time Domain - Standard Deviation of Gravity Acceleration Signal (X axis)","Time Domain - Standard Deviation of Gravity Acceleration Signal (Y axis)","Time Domain - Standard Deviation of Gravity Acceleration Signal (Z axis)"
                                                        ,"Time Domain - Mean Value of Body Acceleration Jerk signal (X axis)","Time Domain - Mean Value of Body Acceleration Jerk signal (Y axis)","Time Domain - Mean Value of Body Acceleration Jerk signal (Z axis)"
                                                        ,"Time Domain - Standard Deviation of Body Acceleration Jerk signal (X axis)","Time Domain - Standard Deviation of Body Acceleration Jerk signal (Y axis)","Time Domain - Standard Deviation of Body Acceleration Jerk signal (Z axis)"
                                                        ,"Time Domain - Mean Value of Body Gyro Signal (X axis)","Time Domain - Mean Value of Body Gyro Signal (Y axis)","Time Domain - Mean Value of Body Gyro Signal (Z axis)","Time Domain - Standard Deviation of Body Gyro Signal (X axis)"
                                                        ,"Time Domain - Standard Deviation of Body Gyro Signal (Y axis)","Time Domain - Standard Deviation of Body Gyro Signal (Z axis)","Time Domain - Mean Value of Body Gyro Jerk Signal (X axis)","Time Domain - Mean Value of Body Gyro Jerk Signal (Y axis)"
                                                        ,"Time Domain - Mean Value of Body Gyro Jerk Signal (Z axis)","Time Domain - Standard Deviation of Body Gyro Jerk Signal (X axis)","Time Domain - Standard Deviation of Body Gyro Jerk Signal (Y axis)","Time Domain - Standard Deviation of Body Gyro Jerk Signal (Z axis)"
                                                        ,"Time Domain - Mean Value of Body Accelaration Magnitude","Time Domain - Standard Deviation of Body Accelaration Magnitude","Time Domain - Mean Value of Gravity Accelaration Magnitude","Time Domain - Standard Deviation of Gravity Accelaration Magnitude","Time Domain - Mean Value of Body Acceleration Jerk Magnitude"
                                                        ,"Time Domain - Standard Deviation of Body Acceleration Jerk Magnitude","Time Domain - Mean Value of Body Gyro Magnitude","Time Domain - Standard Deviation of Body Gyro Magnitude","Time Domain - Mean Value of Body Gyro Jerk Magnitude","Time Domain - Standard Deviation of Body Gyro Jerk Magnitude"
                                                        ,"Fast Fourier Transform - Mean Value of Body Acceleration Signal (X axis)","Fast Fourier Transform - Mean Value of Body Acceleration Signal (Y axis)","Fast Fourier Transform - Mean Value of Body Acceleration Signal (Z axis)","Fast Fourier Transform - Standard Deviation of Body Acceleration Signal (X axis)"
                                                        ,"Fast Fourier Transform - Standard Deviation of Body Acceleration Signal (Y axis)","Fast Fourier Transform - Standard Deviation of Body Acceleration Signal (Z axis)","Fast Fourier Transform - Mean Value of Body Acceleration Jerk signal (X axis)","Fast Fourier Transform - Mean Value of Body Acceleration Jerk signal (Y axis)","Fast Fourier Transform - Mean Value of Body Acceleration Jerk signal (Z axis)"
                                                        ,"Fast Fourier Transform - Standard Deviation of Body Acceleration Jerk signal (X axis)","Fast Fourier Transform - Standard Deviation of Body Acceleration Jerk signal (Y axis)","Fast Fourier Transform - Standard Deviation of Body Acceleration Jerk signal (Z axis)","Fast Fourier Transform - Mean Value of Body Gyro Signal (X axis)"
                                                        ,"Fast Fourier Transform - Mean Value of Body Gyro Signal (Y axis)","Fast Fourier Transform - Mean Value of Body Gyro Signal (Z axis)","Fast Fourier Transform - Standard Deviation of Body Gyro Signal (X axis)","Fast Fourier Transform - Standard Deviation of Body Gyro Signal (Y axis)"
                                                        ,"Fast Fourier Transform - Standard Deviation of Body Gyro Signal (Z axis)","Fast Fourier Transform - Mean Value of Body Accelaration Magnitude","Fast Fourier Transform - Standard Deviation of Body Accelaration Magnitude","Fast Fourier Transform - Mean Value of Body Acceleration Jerk Magnitude","Fast Fourier Transform - Standard Deviation of Body Acceleration Jerk Magnitude"
                                                        ,"Fast Fourier Transform - Mean Value of Body Gyro Magnitude","Fast Fourier Transform - Standard Deviation of Body Gyro Magnitude","Fast Fourier Transform - Mean Value of Body Gyro Jerk Magnitude","Fast Fourier Transform - Standard Deviation of Body Gyro Jerk Magnitude","ActivityLabel")
  
  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  # use 'reshape2' for this
  library(reshape2)
  melt_data_set <- melt(subset_merged_data_with_activity_names,id=c("ActivityLabel","subject_id","activity_label_id"))
  second_independent_tidy_data_set <- aggregate(value ~ ActivityLabel + subject_id + variable, data=melt_data_set, FUN=mean)
  second_independent_tidy_data_set
}
