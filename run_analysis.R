# GCD-Project
# Requirments:
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# PLan 
#read feature.txt.activity label in  ./UCI HAR Dataset
#read test data in: subject_test.txt, y_test.txt. x_test.txt, 
# read training data in subject_traing.txt,  y_train.txt,x_train.txt,
# 
# cbind(subject (1-30), activity(1-6),x_test)
# cbind(subject (1-30), activity(1-6),x_train)
# rbind (test, and train)
# activity_labels <- read.table(file.choose())

# Step 0: check the files on the OS before loading the files in
# use grep, wc, head, tail to check file contents and size

#Step 1: Read the files into R and check the data set size and contents
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

View(activity_labels)
View(features)

harTrainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt") #./train/subject_train.txt
harTrainActivity <- read.table("./UCI HAR Dataset/train/y_train.txt") #./train/y_train.txt
harTrainMeasure <- read.table("./UCI HAR Dataset/train/X_train.txt")  #./train/X_train.txt
View(harTrainSubject)

harTestSubject <- read.table("./UCI HAR Dataset/test/Subject_test.txt")  #./test/Subject_test.txt
harTestActivity <- read.table("./UCI HAR Dataset/test/y_test.txt") #./test/y_test.txt
harTestMeasure <- read.table("./UCI HAR Dataset/test/X_test.txt")  #./test/X_test.txt

# add 'Train' and 'Test' to the datasets to keep track of the data sources
harTrain <- cbind ('Train',harTrainSubject,harTrainActivity,harTrainMeasure)
harTest <- cbind ('Test', harTestSubject,harTestActivity,harTestMeasure)
names(harTrain)[1:3] <- c("Source", "SubjectID","ActivityID")
names(harTest)[1:3] <- c("Source", "SubjectID","ActivityID")

dim(harTrain)
dim(harTest)

harAll <- rbind ( harTrain,harTest) # To combine both Training and Test Data
dim(harAll)

names(harAll)
names(features)

# to apply the feature names to the combined dataset
names(harAll)[4:564]  <- lapply (features$V2, as.character) 

# Step2. Extracts only the measurements on the mean and standard deviation for each measurement.

# find out the column names features that meet the requirements: he measurements on the mean and standard deviation
filtered_cols <- c(grep("mean()", names(harAll),fixed = TRUE),grep("std()", names(harAll),fixed = TRUE))

#Extract those interested columns
harAllMeanStd <- harAll[,c(1:3,filtered_cols)]
View(harAllMeanStd)

# Step3. Uses descriptive activity names to name the activities in the data set

# Add a new column 'Activity' after the column 'ActivityID'
harAllMeanStdAct <-cbind(harAllMeanStd[1:3],harAllMeanStd[3],harAllMeanStd[4:69])
names(harAllMeanStdAct)[4] <- "Activity"

# install 'car' package to use its built-in function 'recode'
install.packages("car")
library(car)

harAllMeanStdAct$Activity <-recode (harAllMeanStdAct$Activity,"'1'='WALKING'; '2'='WALKING_UPSTAIRS'; '3'='WALKING_DOWNSTAIRS'; '4'='SITTING'; '5'='STANDING'; '6'='LAYING'")

# Step 4. Appropriately labels the data set with descriptive variable names.
# this was done in the end of step1. Just double check. 
View(harAllMeanStdAct)

# Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Install 'plyr' package to use it ddply function
install.packages('plyr')
require(plyr)

# Use ddply and 'numcolwise' to calculate the average for all numerical columns
subjectActMean <-ddply(harAllMeanStdAct, .(SubjectID, Activity), numcolwise(mean))
View(subjectActMean)

# Write the final dataset back to file: GCD_Project_Tidy.txt
write.table(subjectActMean,"GCD_Project_Tidy.txt",row.name=FALSE) #using row.name=FALSE

# Double check the final file readable
final_file <- read.table("GCD_Project_Tidy.txt")
head (final_file)
#end