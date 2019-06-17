library(tidyverse)
library(signal)
library(zoo)

# a function to apply bandpass filter on sensor data
fil_fun <- function(ids, column_name, newDF, bw){
  filtered <- NULL
  for(id in ids){
    df <- newDF[newDF$bookingID == id, column_name]
    fil <- filtfilt(bw, df[[column_name]])
    filtered <- c(filtered, fil)
  }
  return(filtered)
}

# a function to apply preprocessing on test set
preprocessing_stage_1 <- function(test_DF){
  # features selection
  newDF <- 
    test_DF %>% 
    select(bookingID, contains("acceleration"), second, Speed) %>% 
    group_by(bookingID) %>% 
    arrange(bookingID, second) %>% 
    ungroup()
  
  # unique id for each trip
  ids <- unique(newDF$bookingID)
  
  # butterworth filter-bandpass filter
  bw <- butter(2, 0.1, type = "pass", plane = "z")
  
  # obtain names of selected features
  column_names <- names(newDF)[c(2,3,4,6)]
  # names of new features (filtered)
  new_col <- c("acc_x_filtered", "acc_y_filtered", "acc_z_filtered", "Speed_filtered")
  
  for(j in 1:length(new_col)){
    newDF[new_col[j]] <- fil_fun(ids, column_names[j], newDF, bw)
  }
  
  # reorientation of acceleration
  # using absolute value
  
  newDF <- 
    newDF %>% 
    mutate(acc_ab = sqrt(acc_x_filtered^2 + acc_y_filtered^2 + acc_z_filtered^2))
  
  # select only filtered features
  newDF <-
    newDF %>%
    select(bookingID, second,contains("filtered"), acc_ab)
  
  return(newDF)
}

































