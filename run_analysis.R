##  This script downloads a zip file containing measurements from wearable computing devices 
##   and extracts the mean and standard deviation for each measurement using the following 
##   steps:
##       1.  Sets working directory
##       2.  Installs libraries of packages needed to perform work
##       3.  Downloads and installs the .zip file to data folder
##           if it doesn't already exist in working directory
##       4.  Adds data folder to working directory
##       5.  Retrieves features to be used as column headings; creates mean/std column filter
##       6.  Retrieves activity descriptions
##       7.  Determines dataset's variable classes
##       8.  Combines test and train subject tables
##       9.  Combines test and train activity labels and adds activity description to labels
##      10.  Combines test and train datasets and adds activity labels and subjects to dataset
##      11.  Creates tidy dataset by doing the following:
##            i. Groups data by feature mesurements
##           ii. Separates feature mesurement column into 3 columns:  
##               feature, axis and measure
##          iii. Groups the data observations by activity, subject, feature, axis and measure
##           iv. Calculates the averages (mean) for the mean and std measurement values
##            v. Moves the average mean and std observations to 2 columns
##      12.  Write the tidy dataset to a .txt file in the working directory
##
##
##  Set the working directory and install libraries of all required packages.
setwd("~/.")
library(dplyr)
library(tidyr)
library(data.table)
##  Loads data if does not already exist.

if(!file.exists("./data")){
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "    
    dir.create("data", recursive=FALSE,showWarnings=TRUE)
    download.file(file_url, destfile = "./data/dataset.zip", mode="wb")
    dateDownloaded <- date()
}
##
##  Set dataset path and load data and combine into single dataset
path <- path.expand("~/data/dataset.zip")
##
## Get descriptive column heading names and activity labels
headings <- read.table(unzip(path,files=c("UCI HAR Dataset/features.txt")),
                       stringsAsFactors=FALSE, col.names=c("head_id","value"))
variables <- c((headings$value %like% "mean"& 
                    !(headings$value %like% "meanFreq"))| 
                   headings$value %like% "std()")
labels <- read.table(unzip(path,files=c("UCI HAR Dataset/activity_labels.txt")),
                     stringsAsFactors=FALSE, col.names=c("id","activity"))
##
##  Determine variable classes, and process data.
##
initial <- read.table(unzip(path,files=c("UCI HAR Dataset/test/X_test.txt")), nrows =1)
classes <- sapply(initial, class)
  
combinedSubjects <- rbind(read.table(unzip(path,files=c("UCI HAR Dataset/test/subject_test.txt")),
                     stringsAsFactors=FALSE, 
                     col.names=c("subject_id")),
                    read.table(unzip(path,files=c("UCI HAR Dataset/train/subject_train.txt")),
                        stringsAsFactors=FALSE, 
                        col.names=c("subject_id")))


combinedLabels <- merge(labels,
                    rbind(read.table(unzip(path,files=c("UCI HAR Dataset/test/y_test.txt")),
                            stringsAsFactors=FALSE, col.names=c("id")),
                          read.table(unzip(path,files=c("UCI HAR Dataset/train/y_train.txt")),
                            stringsAsFactors=FALSE, col.names=c("id"))),by.x="id",by.y="id")   

combinedData <- data.table(cbind(combinedLabels,combinedSubjects,
                      as.data.frame
                          (rbind(read.table(unzip(path,files=c("UCI HAR Dataset/test/X_test.txt")),
                                                colClasses = classes,
                                                stringsAsFactors=FALSE, 
                                                col.names=c(headings$value)),
                                 read.table(unzip(path,files=c("UCI HAR Dataset/train/X_train.txt")),
                                                colClasses = classes,
                                                stringsAsFactors=FALSE,
                                                col.names=c(headings$value))))[,variables]))

combinedDataGroup <- combinedData %>%
                        gather(measurement,value, -id, -activity, -subject_id) %>%
                        separate(measurement,c("feature","measure","axis")) %>%
                        group_by(id, activity,subject_id, feature, measure,axis) %>%      
                        summarize(avg=mean(value)) %>%
                        spread(measure,avg) 

##
##  Create TidyDataSet file in working directory.
##                        
write.table(combinedDataGroup,file="./TidyDataSet.txt", row.names=FALSE)
  
