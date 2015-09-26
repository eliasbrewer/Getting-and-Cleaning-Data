#get files and create file structure
library(httr) 
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
files <- "files.zip"
if(!file.exists(files)){
  download.file(url, files)
}

infolder <- "UCI HAR Dataset"
outfolder <- "results"
if(!file.exists(infolder)){
  unzip(files, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(outfolder)){
  dir.create(outfolder)
} 

#read files into data tables
gettables <- function (filename,cols = NULL){
  f <- paste(infolder,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

features <- gettables("features.txt")

getdata <- function(type, features){
  subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y<-gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x<-gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  return (cbind(subject_data,y,x))
}

test <- getdata("test", features)
train <- getdata("train", features)

#Merges the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
file <- paste(outfolder, "/", "mean_and_std",".csv" ,sep="")
write.csv(mean_and_std,file)

#Uses descriptive activity names to name the activities in the data set
labels <- gettables("activity_labels.txt")

#Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=labels$V1, labels=labels$V2)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
final <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(final)[-c(1:2)] <- paste(colnames(final)[-c(1:2)], "_mean", sep="")
file <- paste(outfolder, "/", "final_dataset",".txt" ,sep="")
write.csv(final,file)
write.table(final,file, row.name=FALSE)


