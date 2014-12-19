## Working Directory with data
##setwd("C:/Users/Helen/Desktop/Senior Project/Getting and Cleaning Data/Course Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

##Reading in test data

test <- read.table("test/X_test.txt")
testlabels <- read.table("test/y_test.txt")
testsubjects <- read.table("test/subject_test.txt")

##Reading in train data

train <- read.table("train/X_train.txt")
trainlabels <- read.table("train/y_train.txt")
trainsubjects <- read.table("train/subject_train.txt")

#Calculating means and sds
testmeans <- apply(test, 1, mean)
testsds <- apply(test, 1, sd)
trainmeans <- apply(train, 1, mean)
trainsds <- apply(train, 1, mean)

#Concatenating data together
testdata <- cbind(testsubjects, testlabels, testmeans, testsds)
traindata <- cbind(trainsubjects, trainlabels, trainmeans, trainsds)

#Renaming columns
names(testdata) <- c("Subject", "Activity", "Mean Acceleration", "SD Acceleration")
names(traindata) <- c("Subject", "Activity", "Mean Acceleration", "SD Acceleration")

#Averaging across activity and subject
temeans <- data.frame(with(testdata, tapply(testmeans, list(Subject, Activity), mean)))
tesds <- data.frame(with(testdata, tapply(testsds, list(Subject, Activity), mean)))
temeans$Subject <- rownames(temeans)
tesds$Subject <- rownames(tesds)

trmeans <- data.frame(with(traindata, tapply(trainmeans, list(Subject, Activity), mean)))
trsds <- data.frame(with(traindata, tapply(trainsds, list(Subject, Activity), sd)))
trmeans$Subject <- rownames(trmeans)
trsds$Subject <- rownames(trsds)


#Using gather to reshape data
temeandata <- gather(temeans, Activity, count, -Subject)
tesddata <- gather(tesds, Activity, count, -Subject)
temeandata$Group <- "Test"
test <- cbind(temeandata[,c(1,4,2,3)], tesddata[,3])
names(test) <- c("Subject", "Group", "Activity", "Mean", "SD")
trmeandata <- gather(trmeans, Activity, count, -Subject)
trsddata <- gather(trsds, Activity, count, -Subject)
trmeandata$Group <- "Train"
train <- cbind(trmeandata[,c(1,4,2,3)], trsddata[,3])
names(train) <- c("Subject", "Group", "Activity", "Mean", "SD")

#Combining all the data together
activitydata <- rbind(test, train)

#Adding labels
activitydata$Activity <- factor(activitydata$Activity,
                                levels=c("X1","X2","X3","X4","X5","X6"),
                                labels=c("Walking", "Walking Upstairs",
                                "Walking Downstairs", "Sitting", "Standing",
                                "Laying"))

#Writes tiny data set to txt file
write.table(activitydata, "humanactivity.txt", row.name=F)
