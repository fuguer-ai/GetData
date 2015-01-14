################################################################################
## summarizeData() - Computes the average of the mean() and std() features 
##                   from the accelerometer data by subject and activity.
################################################################################
summarizeData <- function(){
  
  ## Path to the Dataset of interest
  path = "UCI HAR Dataset/"
  
  ## Load Test & Train Data then merge
  test = loadData(path, "test")
  train = loadData(path, "train")
  data = rbind(test, train)
  
  ## Compute Average of Mean() and Std() measurements over subject/activity
  dataSummary = aggregate(data[, 3:dim(data)[2]], by = data[, 1:2], mean)
  
  ## Write results to file
  write.table(x = dataSummary, file = "UCI_HAR_summary.txt", row.name=FALSE)
}


################################################################################
## loadData() - loads the mean() and std() data from the relevant (test or train)
##            - data set.
##
##            - Summary of Files:
##              - features.txt - the labels for the fields in the data set
##
##              - X_<type>.txt - The observations of the features by subject/activity
##
##              - subject_<type>.txt - The subject ID for the rows in the data set
##
##              - activity_labels.txt - the labels for the activities in Y_<type>.txt
##              - Y_<type>.txt - The activty ID for the rows in the data set
################################################################################
loadData <- function(path, type){
  
  ## Load experimental data
  data = read.table(paste(path, type, "/X_", type, ".txt", sep = ""))
  
  ## Assign names to data frame
  names(data) = read.table(paste(path, "features.txt", sep = ""), stringsAsFactors = FALSE)[, 2]
  
  ## Eliminate non mean()/std() fields
  data = data[grep("mean\\(\\)|std\\(\\)", names(data))]
  
  ## Label Observations with Activity
  activityMap = read.table(paste(path, type, "/y_", type, ".txt", sep = ""))
  activityLabels = read.table(paste(path,"activity_labels.txt", sep = ""))  
  activity = sapply(activityMap,FUN = function(x){activityLabels[x,2]})[,1]

  ## Label Observations with Subject
  subject = read.table(paste(path, type, "/subject_", type, ".txt", sep = ""))[,1]

  ## Add Activity/Subject Fields to beginning of dataset
  data = cbind(subject = subject, activity = activity, data)
  
  ## Return the data frame
  return(data)
}

summarizeData()