#John Tillinghast
#Project for Getting and Cleaning Data course JHU online
#6/16/19

#Instructions

# You should create one R script called run_analysis.R that does the following.
# 
# Line 77 Merges the training and the test sets to create one data set.
# Lines 52-54 Extracts only the measurements on the mean and standard deviation for each measurement.
# Lines 62-63 Uses descriptive activity names to name the activities in the data set
# Lines 86-101 Appropriately labels the data set with descriptive variable names.
# Lines 104-106 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck!

require(tidyverse)
require(readr)
require(stringr)

#0. Reading in both data sets
setwd("~/Documents/Programming/R/JHU Data Science/Getting and Cleaning/Project Data UCI HAR Dataset")
#Read in features.txt first; that will become the column names for both.
feature_list <- read_table2("features.txt", 
                    col_names = c("n", "names"),
                    col_types = 
                    c(n = col_integer(), names = col_character())
                    )
activity_list <- read_table("activity_labels.txt", 
                            col_names = c("Activity.Num", "Activity.Name"),
                            c(number = col_integer(), names = col_character())
                            )
#OK. The next step will actually make a *function* 
#whose input will be either "train" or "test".
#It will read in the appropriate files and link appropriately.

build_dataset <- function(setname) { #setname is either "train" or "test"

  #In both subfolders, there are a lot of files with names such as
  #"X_train.txt" and "y_train.txt".
  get_data <- function(prefix, ...) {
    filename <- "SET/PREFIX_SET.txt" %>% str_replace("PREFIX", prefix)
    filename <- filename %>% str_replace_all("SET", setname)
    data <- read_table(filename, ...)
  }
  #Read in the primary data file for the set. Then select the columns requested.
  
  main_data <- get_data("X", col_names = feature_list$names)

  
  #Instruction: "Extracts only the measurements on the mean and standard deviation for each measurement."
  
  main_data <- select(main_data, 
                    contains("mean()"), 
                    contains("std()"))
  
  #Read in the subject file for the set
  subjects <- get_data("subject", col_names="Subject") 
 
  #Instruction: "Uses descriptive activity names to name the activities in the data set."
  #Read in the activities data, and add the activities names by linking
  #to activity_list
  activities <- get_data("y", col_names="Activity.Num")
  activities <- activities %>% left_join(activity_list, by="Activity.Num") 
                        
  #"Link", i.e. bind, the subject and data files
  joined_output <- bind_cols(subjects, activities, main_data)
  return(joined_output)
} #end get_dataset

#Get both data sets with the subject information
train <- build_dataset("train")
test <- build_dataset("test")

#Instruction: "Merges the training and the test sets to create one data set."

#Combine the two sets with a set indicator
train_and_test <- bind_rows("Train" = train, "Test" = test, .id="Set")

#Check the starts of the train and test subsets of train_and_test.
#Checked by looking at rows 7350:7354 and by checking the unique
#subjects for those with Set=="Train" and Set=="Test".

#OK. Now let's give these guys more reasonable names.


colnames(train_and_test) %>% 
  #Model: tBodyAcc-mean()-Y \\1 is time or freq; \\2 is the quantity;
  #       \\3 is mean or stdev; \\4 is direction (not always present)
        #Start by rearranging.
  str_replace("([tf])([A-Za-z]*)-(mean|std)\\(\\)-*([XYZ]*)" , 
              "\\3\\2\\.\\4\\1" ) %>%
        #Capitalize "mean" and "stdev".
  str_replace("mean", "Mean\\.") %>% 
  str_replace("std", "Stdev\\.")  %>%
        #Clearly indicate Time and Frequency.
  str_replace("([XYZ\\.])t$", "\\1\\.Time") %>%
  str_replace("([XYZ\\.])f$", "\\1\\.Freq") %>%
        #Clean up double periods.
  str_replace("\\.\\.", "\\.") ->
        #Put back into column names.
  colnames(train_and_test)
  
#Make summary data set. (I think it's tidy.)
acc_results <- select(train_and_test, -Activity.Num) %>% 
                  group_by(Set, Subject, Activity.Name) %>%
                  summarize_if(.predicate=is.double, .funs=mean, na.rm=TRUE)

write.table(acc_results, file="Acc_results.txt", row.name=FALSE)