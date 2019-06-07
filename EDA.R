library(tidyverse)
library(caret)

# a function to read all cvs files in a directory
readAll <- function(directory){
  placeholder <- NULL
  path_name <- str_c("./", directory)
  filenames <- list.files(path_name)
  for (filename in filenames){
    loaded_files <- read_csv(str_c(path_name, filename, sep = "/"))
    placeholder <- bind_rows(loaded_files, placeholder)
  }
  return(placeholder)
}

# load all telematics csv files
grabData <- readAll("features")

# load all labels (0: non-dangerous driving, 1: dangerous driving)
labelDF <- read_csv("./labels/part-00000-e9445087-aa0a-433b-a7f6-7f4c19d78ad6-c000.csv")

# a list of booking ID with two labels (0 and 1) at the same time
label_doubled <- labelDF %>% count(bookingID) %>% filter(n == 2) %>% pull(bookingID)

# it is unknown why a trip consists of both labels.
# for the ease of analysis, this booking ID is removed from analysis

# remove the "doubled" bookingID from label
labelDF_cleaned <-
  labelDF %>% filter(!bookingID %in% label_doubled)

# remove the "doubled" bookingID from grabData
grabData_cleaned <-
  grabData %>% filter(!bookingID %in% label_doubled)

# create training, dev and test set
set.seed(100)
train_index <- createDataPartition(labelDF_cleaned$bookingID, p = 0.8, list = FALSE)
trainDF <- labelDF_cleaned[train_index,]
dev_and_test <- labelDF_cleaned[-train_index,]

# create dev and test set
dev_index <- createDataPartition(dev_and_test$label, p = 0.5, list = FALSE)
devDF <- dev_and_test[dev_index,]
testDF <- dev_and_test[-dev_index,]

# keep dev and test set aside
# examine the proportion of 0 and 1 in the data

trainDF %>% filter(label == 1) %>% nrow()
# 4012 out of 15986 is dangerous driving (25.1%)

# divide grabData_cleaned into train, dev and test
grabData_train <-
  grabData_cleaned %>% filter(bookingID %in% trainDF$bookingID)

grabData_dev <-
  grabData_cleaned %>% filter(bookingID %in% devDF$bookingID)

grabData_test <-
  grabData_cleaned %>% filter(bookingID %in% testDF$bookingID)











