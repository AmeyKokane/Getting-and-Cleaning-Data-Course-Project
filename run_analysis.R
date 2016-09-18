#loading useful librarys
library(dplyr)
library(tidyr)

# Downloading the file in specified folder

if(!file.exists("~/Data Science Coursera/Data")){dir.create("~/Data Science Coursera/Data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"~/Data Science Coursera/Data/ProjData.zip")

# Unzip downloaded file
unzip(zipfile ="~/Data Science Coursera/Data/ProjData.zip",exdir ="~/Data Science Coursera/Data")

#Listing the files
filepath <- file.path("~/Data Science Coursera/Data","UCI HAR Dataset")

listoffiles <- list.files(filepath,recursive = TRUE)

#Reading Activity data files into variables
ActivityTest <- read.table(file.path(filepath, "test","y_test.txt"),header = FALSE)
ActivityTrain <- read.table(file.path(filepath, "train","y_train.txt"),header = FALSE)

#Reading experiment data files into variables

dataTrain <- read.table(file.path(filepath,"train","X_train.txt"),header = FALSE)
dataTest <- read.table(file.path(filepath,"test","X_test.txt"),header = FALSE)

#Reading Subject files into variables
SubjectTrain <- read.table(file.path(filepath,"train","subject_train.txt"),header = FALSE)
SubjectTest <- read.table(file.path(filepath,"test","subject_test.txt"),header = FALSE)

# Combine Test and Traning files for Activity and Subject to form a single files. We will use rbind function.
#Since there is no variable name present for these files we will assign 'ActivityNum'to Activity dataset
# and 'SubjectNum' to Suject dataset.

ActivityData <- rbind(ActivityTrain,ActivityTest)
names(ActivityData) <- "ActivityNum"

SubjectData <- rbind(SubjectTrain,SubjectTest)
names(SubjectData) <- "SubjectNum"

#Now combine the experiment Test and Train data into single dataset
Data <- rbind(dataTest,dataTrain)

#Renaming variables for combined experiment Data. If you use dim(Data), you will see # of cols = 561. 
#There are 561 col names in features.txt file. So we will read in features.txt file  in R and use it for variable names for experiment data file.

varnames <- read.table(file.path(filepath,"features.txt"),header = FALSE)
names(Data) <- varnames$V2

# Now combine Activity, Subject and Data files vertically to form a single file.
temp <- cbind(ActivityData,SubjectData)
Data_Comb <- cbind(temp,Data)

# Extract measurements for MEAN() and STD() only

varnamesMEANSTD <- varnames$V2[grep( "mean\\(\\)|std\\(\\)",varnames$V2)]

#Subset experiment Data for only MEAN and STD feature variable names

subsetnames <- c("ActivityNum","SubjectNum",as.character(varnamesMEANSTD))

Data <- subset(Data_Comb,select=subsetnames)

# Add Activity Labels from file activity_labels.txt to create 

activitynames <- read.table(file.path(filepath,"activity_labels.txt"),header = FALSE)

Data <- merge(activitynames,Data,by.y  = "ActivityNum",by.x = "V1")

#Renaming variables after merge
names(Data)[1] <- "ActivityNum"
names(Data)[2] <- "ActivityNames"

# Aggregate the data table for average of each Activity for each subject
Data$ActivityNames <- as.character(Data$activityNames)
Data_fin <- aggregate(. ~ SubjectNum - ActivityNames, data = Data, mean)
DataTable<- tbl_df(arrange(Data_fin,SubjectNum,ActivityNum))

##Appropriately label the data set with descriptive variable names
#In the former part, variables activity and subject and names of the activities have been labelled using descriptive names.In this part, Names of Feteatures will labelled using descriptive variable names.

#prefix t is replaced by time
#Acc is replaced by Accelerometer
#Gyro is replaced by Gyroscope
#prefix f is replaced by frequency
#Mag is replaced by Magnitude
#BodyBody is replaced by Body

names(DataTable)<-gsub("std()", "SD", names(DataTable))
names(DataTable)<-gsub("mean()", "MEAN", names(DataTable))
names(DataTable)<-gsub("^t", "time", names(DataTable))
names(DataTable)<-gsub("^f", "frequency", names(DataTable))
names(DataTable)<-gsub("Acc", "Accelerometer", names(DataTable))
names(DataTable)<-gsub("Gyro", "Gyroscope", names(DataTable))
names(DataTable)<-gsub("Mag", "Magnitude", names(DataTable))
names(DataTable)<-gsub("BodyBody", "Body", names(DataTable))


##Write tidy data file to drive

write.table(DataTable, "TidyData.txt", row.name=FALSE)

