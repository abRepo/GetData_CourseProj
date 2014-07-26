###############################################################################
## GetData Course Project
###############################################################################


###############################################################################
## 1. Merges the training and the test sets to create one data set.
###############################################################################

# Read test files
subject_test <- read.table(file = "./data/test/subject_test.txt")
y_test <- read.table(file = "./data/test/y_test.txt")
X_test <- read.table(file = "./data/test/X_test.txt")

# Read train files
subject_train <- read.table(file = "./data/train/subject_train.txt")
y_train <- read.table(file = "./data/train/y_train.txt")
X_train <- read.table(file = "./data/train/X_train.txt")

# Read features file and label colmuns
features <- read.table(file = "./data/features.txt",
                       colClasses = c("numeric", "character"))
names(features) <- c("RowId", "FeatureName")

# Combine train and test files
subject <- rbind(subject_test, subject_train)
names(subject) <- "subjectnum"
rm(subject_test, subject_train)

y <- rbind(y_test, y_train)
names(y) <- "activity"
rm(y_test, y_train)

X <- rbind(X_test, X_train)
names(X) <- features$FeatureName
rm(X_test, X_train)

# Create One Large Data Set
OneDataSet <- cbind(subject, y, X)

###############################################################################
## 2. Extracts only the measurements on the mean and standard deviation.
###############################################################################

# Find mean() and std() in names vector of OneDataSet
findmeanlogi <- grepl("mean", names(OneDataSet))
findstdlogi <- grepl("std", names(OneDataSet))

# Find meanFreq
findmeanFreqlogi <- grepl("meanFreq", names(OneDataSet))

# Combine mean and std using "or" logic
col2keep <- findmeanlogi | findstdlogi
# Keep as long as it is not meanFreq
col2keep <- col2keep & !findmeanFreqlogi

# Keep first two variables: subjectnum and activity
col2keep[c(1,2)] <- TRUE

# Extract first two columns and all columns with mean() or std()
OneDataSet <- OneDataSet[,col2keep]

###############################################################################
## 3. Uses descriptive activity names to name the activities in the data set.
###############################################################################

# activity 1 is walking
OneDataSet[OneDataSet$activity == "1","activity"] <- "walking"
# activity 2 is walking upstairs
OneDataSet[OneDataSet$activity == "2","activity"] <- "walking upstairs"
# activity 3 is walking upstairs
OneDataSet[OneDataSet$activity == "3","activity"] <- "walking downstairs"
# activity 4 is walking upstairs
OneDataSet[OneDataSet$activity == "4","activity"] <- "sitting"
# activity 5 is walking upstairs
OneDataSet[OneDataSet$activity == "5","activity"] <- "standing"
# activity 6 is walking upstairs
OneDataSet[OneDataSet$activity == "6","activity"] <- "laying"


###############################################################################
## 4. Appropriately labels the data set with descriptive variable names. 
###############################################################################

# Substitute "()" - Remove
names(OneDataSet) <- gsub("\\(\\)", "", names(OneDataSet))

# Substitute "-" - Remove
names(OneDataSet) <- gsub("\\-", "", names(OneDataSet))

# Substitute "t..." and  "f..." - Replace with "Time.." and "Freq..."
names(OneDataSet) <- gsub("tBody", "timebody", names(OneDataSet))
names(OneDataSet) <- gsub("fBody", "freqbody", names(OneDataSet))
names(OneDataSet) <- gsub("tGravity", "timegravity", names(OneDataSet))
names(OneDataSet) <- gsub("fGravity", "freqgravity", names(OneDataSet))

# Ensure names are lower case
names(OneDataSet) <- tolower(names(OneDataSet))

# Check Variable Names
# names(OneDataSet)

###############################################################################
## 5. Creates a second, independent tidy data set with the average of each 
##     variable for each activity and each subject.
###############################################################################

# Attach package "dplyr" to assist with summarization
library(dplyr)

# First we define the group by columns
OneDataSet.g <- group_by(OneDataSet, subjectnum, activity)

# Next we compute the means of the non-group columns
tidydata <- summarise_each(OneDataSet.g, funs(mean))

# dplyr changes data frames to tbl_df types.  So we convert back to vanilla df
tidydata <- as.data.frame(tidydata)

# Write data frame to disk
write.csv(tidydata, file = "tidydata.csv")

###############################################################################
## Appendix: Clean up (Remove Un-needed Objects
###############################################################################

rm(OneDataSet.g, 
   X, 
   features, 
   subject, 
   col2keep, 
   y, 
   findmeanFreqlogi,
   findmeanlogi,
   findstdlogi)



str(tidydata)
