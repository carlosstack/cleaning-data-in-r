#'1 Merges the training and the test sets to create one data set.
#'2 Extracts only the measurements on the mean and standard deviation for each measurement.
#'3 Uses descriptive activity names to name the activities in the data set
#'4 Appropriately labels the data set with descriptive variable names.
#'5 From the data set in step 4, creates a second, independent tidy data set with the average 
#'of each variable for each activity and each subject.

library(dplyr)
library(data.table)

#root directory
rootpath <- "./UCI HAR Dataset"

activityLabelsPath <- paste0(rootpath,"/activity_labels.txt")
featuresPath <- paste0(rootpath,"/features.txt")

####Features
featuresRaw <- read.fwf(featuresPath, widths = c(3,1,35), sep = " ")
features <- select(featuresRaw, V1,V4)
names(features) <- c("FeatureIndex","FeatureName")

analize <- function(type){
  
  #test directories
  folderPath <- paste0(rootpath,paste0("/", type))
  subjectsPath <- paste0(folderPath, paste0("/subject_", type, ".txt"))
  
  xpath <- paste0(folderPath, paste0("/X_", type, ".txt"))
  ypath <- paste0(folderPath, paste0("/y_", type, ".txt"))
  
  con <- file(xpath)
  xraw <- readLines(con)
  close(con)
  
  con <- file(ypath)
  yraw <- readLines(con)
  close(con)
  
  activityLabelsRaw <- read.fwf(activityLabelsPath, widths = c(1,1,20))


  ####Subject
  subjectsRaw <- read.fwf(subjectsPath, widths = c(2), sep = " ")

  subjects <- subjectsRaw
  names(subjects) <- c("subject")
  
  ####Xtest
  
  parseX <- function(x){
    row <- strsplit(x, split = " ")
    row <- lapply(row, strtrim, width = 14)
    row <- lapply(row, as.numeric)
    row <- na.omit(row[1][[1]])
    return(row)
  }
  
  onlyMeanAndSd <- function(x){
    
    output <- c()
    
    for (i in 1:length(x)){
      
      condition <- length(grep("[Mm]ean+|[Ss]d+", paste(features$FeatureName[i]), value = TRUE))>0
      if (condition){
        aux <-  c(list(variable = features$FeatureName[i], value= x[i]))
        output <- rbind(output, aux,deparse.level = 0)
      }
      
    }
    
    return(output)
  }
  
  xdata <- lapply(xraw, parseX)
  xdata <- lapply(xdata, onlyMeanAndSd)
  
  ####ytest
  ydata <- data.frame(as.integer(yraw))
  names(ydata) <- c("ActivityId")
  
  ####activity labels
  activityLabels <- select(activityLabelsRaw, V1,V3)
  names(activityLabels) <- c("ActivityId", "ActivityName")
  
  ####joining ytest and activityLabels
  ydataByLabel <- inner_join(ydata, activityLabels, by="ActivityId")
  
  ####join an unique dataset
  dataset <- ydataByLabel
  dataset$Subject = subjects$subject
  
  dataset$Measurements = xdata
  
  return(dataset)
}

##read test and train data
test <- analize("test");train <- analize("train")
####merge 
dataset <- rbind(test,train)
####cleanning
tidydata <- select(dataset, ActivityName, Subject)

######Step 4#######
for (i in 1:53) {
  aux <- c()
  variable <- dataset$Measurements[1][[1]][i,]$variable
  for (j in 1:dim(dataset)[1]) {
    value <-dataset$Measurements[j][[1]][i,]$value
    aux <- append(aux, value)
  }
  
  tidydata[variable] = aux 
}

#####grouping for a new dataset
tidydata <- tidydata %>% group_by(Subject, ActivityName) %>% summarise_all(list(mean))

#####Write on a file 
write.table(tidydata, file = "./tidy_dataset.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE); print("Data writed at './tidy_dataset.txt'")

###################










