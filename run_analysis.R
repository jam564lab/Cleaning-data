## setwd("/Users/XXXXXX/My Documents/RCourse/project")
## getwd()
library(plyr)
### Read files ###
## Activity_labels.txt = activity labels
## festures.txt  = features labels
## Read test files
## x_test.txt = test sets
## Y_test.txt = test labels
## subject_test.txt = subjects
## Read training files
## x_train.txt = training set
## y_train.txt = training labels
###
activitylabels<-read.table("activity_labels.txt",col.names = c("Id", "Activity"))
featurelabels<-read.table("features.txt",colClasses = c("character"))
xtest<-read.table("./test/X_test.txt")
ytest<-read.table("./test/Y_test.txt")
stest<-read.table("./test/subject_test.txt")
xtrain<-read.table("./train/X_train.txt")
ytrain<-read.table("./train/Y_train.txt")
strain<-read.table("./train/subject_train.txt")
###
## Merges the training and the test sets to create one data set   ##
###
traindata<-cbind(cbind(xtrain, strain), ytrain)
testdata<-cbind(cbind(xtest, stest), ytest)
Mergedata<-rbind(traindata, testdata)
Mergeflabels<-rbind(rbind(featurelabels, c(562, "Subject")), c(563, "Id"))[,2]
names(Mergedata)<-Mergeflabels
###
## Extracts only the measurements on the mean and standard deviation for each measurement  ##
###
Mergedatameanstd <- Mergedata[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(Mergedata))]
###
## Uses descriptive activity names to name the activities in the data set   ##
###
Mergedatameanstd <- join(Mergedatameanstd, activitylabels, by = "Id", match = "first")
Mergedatameanstd <- Mergedatameanstd[,-1]
###
## Appropriately labels the data set with descriptive names ##
###
names(Mergedatameanstd) <- gsub("([()])","",names(Mergedatameanstd))
names(Mergedatameanstd) <- make.names(names(Mergedatameanstd))
###
## From the data set output a file each variable for each activity and each subject
### 
sumdata<-ddply(Mergedatameanstd, c("Activity","Subject"), numcolwise(mean))
newx<- function(x, x2) {
  if (!(x %in% c("Subject","Activity"))) {
    paste(x,x2, sep="")
  }
  else{x
  }
}
exportheaders<-names(sumdata)
exportheaders<-sapply(exportheaders, newx, ".mean")
names(sumdata)<-exportheaders
write.table(sumdata, file = "output-summary.txt", row.name=FALSE)
