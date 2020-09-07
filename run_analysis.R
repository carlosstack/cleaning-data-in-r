#'1 Merges the training and the test sets to create one data set.
#'2 Extracts only the measurements on the mean and standard deviation for each measurement.
#'3 Uses descriptive activity names to name the activities in the data set
#'4 Appropriately labels the data set with descriptive variable names.
#'5 From the data set in step 4, creates a second, independent tidy data set with the average 
#'of each variable for each activity and each subject.

#' this fuction run the script and write the file output
#' @param rootpath the path of 'UCI HAR Dataset' folder, by default './UCI HAR Dataset'
run <- function(rootpath = "./UCI HAR Dataset"){
  
  #install.packages("dplyr")
  library(dplyr)
  
  #' This function analize the train or test data set and parse them to data frames
  #' @param type the folder to read can be "test" or "train" 
  analize <- function(type){
    
    # 'activity_labels.txt'file path
    activityLabelsPath <- paste0(rootpath,"/activity_labels.txt")
    # 'features.txt' file path
    featuresPath <- paste0(rootpath,"/features.txt")
    
    # 'test' or 'train' data paths
    folderPath <- paste0(rootpath,paste0("/", type))
    subjectsPath <- paste0(folderPath, paste0("/subject_", type, ".txt"))
    xpath <- paste0(folderPath, paste0("/X_", type, ".txt"))
    ypath <- paste0(folderPath, paste0("/y_", type, ".txt"))
    
    # reading files and put in a variable
    featuresRaw <- read.fwf(featuresPath, widths = c(3,1,35), sep = " ")
    features <- select(featuresRaw, V1,V4)
    names(features) <- c("FeatureIndex","FeatureName")
    
    con <- file(xpath)
    xraw <- readLines(con)
    close(con)
    
    con <- file(ypath)
    yraw <- readLines(con)
    close(con)
    
    activityLabelsRaw <- read.fwf(activityLabelsPath, widths = c(1,1,20))
    
    subjectsRaw <- read.fwf(subjectsPath, widths = c(2), sep = " ")
    
    subjects <- subjectsRaw
    names(subjects) <- c("subject")
    
    #' this function parse the data form 'X_test.txt' file by row
    #' @param x a item of xraw 
    parseX <- function(x){
      row <- strsplit(x, split = " ")
      row <- lapply(row, strtrim, width = 14)
      row <- lapply(row, as.numeric)
      row <- na.omit(row[1][[1]])
      return(row)
    }
    
    xdata <- lapply(xraw, parseX)
    
    #' this function filter the variables with mean or sd in its names 
    #' @param x a item of xdata 
    onlyMeanAndSd <- function(x){
      
      output <- c()
      
      for (i in 1:length(x)){
        # select only that contains mean or sd in its name
        condition <- length(grep("[Mm]ean+|[Ss]d+", paste(features$FeatureName[i]), value = TRUE))>0
        if (condition){
          aux <-  c(list(variable = features$FeatureName[i], value= x[i]))
          output <- rbind(output, aux,deparse.level = 0)
        }
        
      }
      
      return(output)
    }
    
    xdata <- lapply(xdata, onlyMeanAndSd)
    
    # parse yraw to integer and set the colnames as 'ActivityId'
    ydata <- data.frame(as.integer(yraw))
    names(ydata) <- c("ActivityId")
    
    # select the row of activityLabelsRaw ad set names 'ActivityId' and 'ActivityName'
    activityLabels <- select(activityLabelsRaw, V1,V3)
    names(activityLabels) <- c("ActivityId", "ActivityName")
    
    # join ytest and activityLabels by 'ActivityId'
    ydataByLabel <- inner_join(ydata, activityLabels, by="ActivityId")
    
    # bind Subject column in dataset
    dataset <- ydataByLabel
    dataset$Subject = subjects$subject
    
    # bind Measurement column in dataset
    dataset$Measurements = xdata
    
    return(dataset)
  }
  
  # analize 'test' and 'train' data
  print("Analizing test and train data...")
  test <- analize("test");train <- analize("train")
  
  print("Cleannig dataset...")
  # bind 'test' and 'train' 
  dataset <- rbind(test,train)
  
  # remove 'ActivityId' column 
  tidydata <- select(dataset, ActivityName, Subject)
  
  # create a column for each measurement variable 
  for (i in 1:53) {
    aux <- c()
    variable <- dataset$Measurements[1][[1]][i,]$variable
    for (j in 1:dim(dataset)[1]) {
      value <-dataset$Measurements[j][[1]][i,]$value
      aux <- append(aux, value)
    }
    
    tidydata[variable] = aux 
  }
  
  #data set with the average of each variable for each activity and each subject
  tidydata <- tidydata %>% group_by(Subject, ActivityName) %>% summarise_all(list(mean))
  
  print("Writing file...")
  # Write in file
  write.table(tidydata, file = "./tidy_dataset.txt", append = FALSE, quote = TRUE, sep = " ",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE)
  
  print("File created: './tidy_dataset.txt'")
  print("Script finished.")
}

run()










