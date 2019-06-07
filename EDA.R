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

label_doubled <- labelDF %>% count(bookingID) %>% filter(n == 2) %>% pull(bookingID)

