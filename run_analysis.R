library(dplyr)
library(stringr)

#reads the datasets

x_train <- read.table('UCI HAR Dataset/train/X_train.txt', sep = "")
y_train <- read.table('UCI HAR Dataset/train/y_train.txt', sep ="")

x_test <- read.table('UCI HAR Dataset/test/X_test.txt', sep ="")
y_test <- read.table('UCI HAR Dataset/test/y_test.txt', sep="")

#merges the datasets, by row
data_merged <- rbind(x_train, x_test)
label_merged <- rbind(y_train, y_test)

#reads feature names
feat <- read.table('UCI HAR Dataset/features.txt')

#detects mean() and std() indices
mn <- str_detect(feat$V2,'^(.*)mean')
st <- str_detect(feat$V2, '^(.*)std')

#selects corresponding columns
data_selection <- data_merged[,mn | st]

#putting variable names
names(data_selection) <- feat$V2[mn | st]
variable_names <- names(data_selection)

names(data_selection)<-gsub("Acc", "Accelerometer", names(data_selection))
names(data_selection)<-gsub("Gyro", "Gyroscope", names(data_selection))
names(data_selection)<-gsub("BodyBody", "Body", names(data_selection))
names(data_selection)<-gsub("Mag", "Magnitude", names(data_selection))
names(data_selection)<-gsub("^t", "Time", names(data_selection))
names(data_selection)<-gsub("^f", "Frequency", names(data_selection))
names(data_selection)<-gsub("tBody", "TimeBody", names(data_selection))
names(data_selection)<-gsub("-mean()", "Mean", names(data_selection), ignore.case = TRUE)
names(data_selection)<-gsub("-std()", "STD", names(data_selection), ignore.case = TRUE)
names(data_selection)<-gsub("-freq()", "Frequency", names(data_selection), ignore.case = TRUE)
names(data_selection)<-gsub("angle", "Angle", names(data_selection))
names(data_selection)<-gsub("gravity", "Gravity", names(data_selection))

#attaching activity labels
data_selection <- data_selection %>% mutate(activity = factor(label_merged$V1, labels=c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING')))

#attaching subject identifiers
sub_test <- read.table('UCI HAR Dataset/test/subject_test.txt', sep ="")
sub_train <- read.table('UCI HAR Dataset/train/subject_train.txt', sep ="")
sub <- rbind(sub_train, sub_test)
data_selection <- data_selection %>% mutate(subject = sub)

#summarize according to activity
data_sum <- data_selection %>% group_by(subject, activity) %>% summarize_all(mean)

write.table(data_sum, file='Tidy Dataset Avg.txt', row.names = FALSE)







