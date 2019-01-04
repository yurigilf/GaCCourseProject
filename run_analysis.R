## Download and install libraries, load libraries

install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)

## Download, unzip file and set working directory

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "Data.zip")
ziped <- "Data.zip"
unzip(ziped, exdir = "unziped")
setwd("unziped/UCI HAR Dataset")

## read txt 

ac_labels <- read.table("activity_labels.txt",header = FALSE)
features <- read.table("features.txt", header = FALSE)

train_labels <- read.table("train/y_train.txt",header = FALSE)
train_sets <- read.table("train/x_train.txt",header = FALSE)
train_subjects <- read.table("train/subject_train.txt" ,header = FALSE)

test_labels <- read.table("test/y_test.txt",header = FALSE)
test_sets <- read.table("test/x_test.txt",header = FALSE)
test_subjects <- read.table("test/subject_test.txt" , header = FALSE)


#rename the columns 

colnames(train_labels) <- "ActivityLabel"
colnames(test_labels) <- "ActivityLabel"

colnames(train_subjects) <- "SubjectId"
colnames(test_subjects) <- "SubjectId"

colnames(train_sets) <- features[,2]
colnames(test_sets) <- features[,2]

colnames(ac_labels) <- c('ActivityLabel','Activity')

#merge the ac_labels with the test and train labels for more descriptive name

train_labeled <- merge(ac_labels,train_labels)
test_labeled <- merge(ac_labels,test_labels)

#merge all into a single data set

train_all <- cbind(train_labeled,train_subjects,train_sets)
test_all <- cbind(test_labeled,test_subjects,test_sets)
all <- rbind(train_all,test_all)

#Extracting only mean and standard deviation

colNames <- colnames (all)
MeanStd <-  (grepl("Activity" , colNames) | 
                                grepl("SubjectId" , colNames) | 
                                grepl("mean.." , colNames) | 
                                grepl("std.." , colNames) 
            )

setMeanStd <- all[,MeanStd == TRUE]

##Remove ActivityId 

setMeanStd <- select(setMeanStd,-(ActivityLabel))

##Creating a second tidy data

tidyset <- setMeanStd %>% group_by(SubjectId,Activity) %>% summarise_all(funs(mean))

tidyset <- tidyset [order(tidyset$SubjectId,tidyset$Activity)]

##Create txt file

write.table(tidyset, "tidyset.txt", row.name=FALSE)

