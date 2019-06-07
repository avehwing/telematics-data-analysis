library(tidyverse)

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



























