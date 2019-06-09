library(tidyverse)
library(caret)
library(gridExtra)

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

# preliminary analysis: look at min, max, mean, median of acceleration
# gyro and speed

grabData_train_sum <-
  grabData_train %>% 
    group_by(bookingID) %>% 
    summarise_at(vars(acceleration_x:gyro_z, Speed), 
                 list(min = min, max = max, mean = mean, med = median))

# join preliminary analysis DF with labels

grabData_train_sum <-
  grabData_train_sum %>% left_join(trainDF, by = "bookingID")


# visualise acceleration x

# function to plot boxplot
boxplot_fn <- function(variables){
    ggplot(grabData_train_sum, aes(x = as.factor(label), y = !!sym(variables))) +
      geom_boxplot()
}

# extracting the name of acceleration x
acceleration_x <- names(grabData_train_sum)[c(2,9,16)]
acceleration_x <- set_names(acceleration_x, c("ax_min", "ax_max", "ax_med"))

# plotting boxplot for all acceleration x parameters in a grid
plots <- map(acceleration_x, ~boxplot_fn(.x))
ggarrange(plotlist = plots)







































