## Download and install libraries, load libraries

install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(stringr)

## Download, unzip file and set working directory

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "Data.zip")
ziped <- "Data.zip"
unzip(ziped, exdir = "unziped")
setwd("unziped/UCI HAR Dataset")

## read txt 

ac_labels <- tbl_df(read.delim("activity_labels.txt",header = FALSE))

train_labels <- tbl_df(read.delim("train/y_train.txt",header = FALSE)) 
train_sets <- tbl_df(read.delim("train/x_train.txt",header = FALSE))
train_subjects <- tbl_df(read.delim("train/subject_train.txt" ,header = FALSE))

test_labels <- tbl_df(read.delim("test/y_test.txt",header = FALSE))
test_sets <- tbl_df(read.delim("test/x_test.txt",header = FALSE))
test_subjects <- tbl_df(read.delim("test/subject_test.txt" , header = FALSE))

#separate activity_labels 

ac_labels <- separate(ac_labels,V1,c("ActivityLabel","Activity"), sep = "\ ")

##transform the data sets to a list of matrices  
#train sets

train_sets <- sapply(train_sets,str_squish)
train_sets <- sapply(train_sets, str_trim)
train_sets <- strsplit(train_sets," ")
aux <- list()
contador <- length(train_sets)

for (i in 1:contador){
    aux[i]<-list(sapply(train_sets[i],as.numeric))
}

train_sets <-aux

#test sets

test_sets <- sapply(test_sets,str_squish)
test_sets <- sapply(test_sets,str_trim)
test_sets <- strsplit(test_sets," ")
aux <- list()
contador <- length(test_sets)

for (i in 1:contador){
    aux[i]<-list(sapply(test_sets[i],as.numeric))
    
}
test_sets <- aux

rm(aux)

#rename the columns on test and train labels

colnames(train_labels) <- "ActivityLabel"
colnames(test_labels) <- "ActivityLabel"

#rename the columns on test and train subjects


colnames(train_subjects) <- "Subject"
colnames(test_subjects) <- "Subject"

#merge the ac_labels with the test and train labels

train_labeled <- merge(ac_labels,train_labels)
test_labeled <- merge(ac_labels,test_labels)

rm(train_labels,test_labels,ac_labels)

# Add the Subject column 

train_labeled<- cbind(train_labeled,train_subjects)
test_labeled <- cbind(test_labeled,test_subjects)

rm(train_subjects,test_subjects)


# Remove the ActivityLabel column add The Test_Train Column
# Add the Set, Mean, StdDev, Measurementes variables, reaarange the variables order

train_data <- train_labeled %>% select(-(ActivityLabel)) %>%  select(Subject,Activity)
train_data <- mutate(train_data,Set = "Train" , Mean =  NA, StdDev = NA)

test_data <- test_labeled %>% select(-(ActivityLabel)) %>% select(Subject,Activity)
test_data <- mutate(test_data,Set = "Test", Mean =  NA, StdDev = NA)

rm(train_labeled,test_labeled)

#add the mean and standard deivation of the sets' measurement to the dataset

contador <- length(train_sets)

for (i in 1:contador){
    train_data[i,4] <- mean(train_sets[[i]])
    train_data[i,5] <- sd(train_sets[[i]])
    
}

contador <- length(test_sets)

for (i in 1:contador){
    test_data[i,4] <- mean(test_sets[[i]])
    test_data[i,5] <- sd(test_sets[[i]])
    
}

rm(contador,i,train_sets,test_sets)

#Create a single data set with both sets

all_data <- rbind(train_data,test_data)

rm(train_data,test_data)



#Create the tidy data set

by_Activity <- aggregate(all_data[,4:5],list(all_data$Activity),mean)

by_subject <- aggregate(all_data[,4:5],list(all_data$Subject),mean)

tidy_data <- rbind(by_Activity,by_subject)

rm(by_Activity,by_subject)

#Save as .txt file

write.table(tidy_data, file = "tidydata.txt",row.names =  FALSE)



