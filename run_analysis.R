
# Coursera Getting and Cleaning Data Course Project 
#####################################################################
# 1. Read data
#   1.1 features.txt
#   1.2 3 train data sets (X_train.txt, y_train.txt, subject_train.txt)
#   1.3 3 test data sets  (X_test.txt,  y_test.txt,  subject_test.txt)
#   1.4 activity_labels.txt
#####################################################################

# 1.1 Read features.txt 
features=read.table("./data/UCI HAR Dataset/features.txt",
      col.names=c("seq","features"))
dim(features)
head(features)

# 1.2 Read data : Train with with variable(feature) names
#                 to give descriptive variables names
X_train=read.table("./data/UCI HAR Dataset/train/X_train.txt", 
      col.names=features$features)
dim(X_train) # 7352  561
y_train=read.table("./data/UCI HAR Dataset/train/y_train.txt",
      col.name="activity")
dim(y_train)
table(y_train)
subject_train=read.table("./data/UCI HAR Dataset/train/subject_train.txt",
    col.name='SubjectID')
dim(subject_train)

# 1.3 Read data : test
X_test=read.table("./data/UCI HAR Dataset/test/X_test.txt", 
         col.names=features$features)
dim(X_test) #  2947  561
y_test=read.table("./data/UCI HAR Dataset/test/y_test.txt",
    col.name="activity")
dim(y_test) # 2947 1

subject_test=read.table("./data/UCI HAR Dataset/test/subject_test.txt",
    col.name='SubjectID')
dim(subject_test) # 2947 1

# 1.4 Read activity_labels.txt
activity_labels=read.table("./data/UCI HAR Dataset/activity_labels.txt",
       col.names=c("seq","activity"))
activity_labels

#########################################################################
# 2. Merges the training and the test sets to create one data set "data".
#    and sort by SubjectID and activity
#########################################################################
train=cbind(subject_train, y_train, X_train)
test =cbind(subject_test, y_test, X_test)
data=rbind(train,test)
data$SubjectID = as.character(data$SubjectID)
data[1:5,1:5]

library(plyr)
data=arrange(data,SubjectID, activity)  # sort by SubjectID and activity

#######################################################################
# 3. Extracts only the measurements on the mean and standard deviation 
#     for each measurement
#######################################################################
# Choose column with  mean() & std()

data.sub=dat[,c(1,2,grep("std", colnames(data)),
                    grep("mean",colnames(data)))]  
dim(data.sub)   # 10299 81

#######################################################################
# 4.  Uses descriptive activity names to name the activities 
#     in the data set
#######################################################################
# Create activity label variable in 'dat' data frame
data.sub$activity=factor(data.sub$activity, 
                     labels=activity_labels$activity)
data.sub[1:5,1:5]

#######################################################################
# 5.  Create tidy data set with average of each variable 
#     for each activity and each subject
#######################################################################

# compute column mean by SubjectID and activity
data.sub$SubjectID=as.numeric(data.sub$SubjectID)
data.tidy = ddply(dat.sub, .(SubjectID, activity),
                 .fun=function(x) {
                     apply(x[,-c(1,2)],2,mean,na.rm=T) }   ) 

write.table(data.tidy,"./data/UCI HAR Dataset/data_tidy.txt",row.name=FALSE)

data.tidy1 <- read.table("./data/UCI HAR Dataset/data_tidy.txt", header = TRUE) 
View(data.tidy1)





