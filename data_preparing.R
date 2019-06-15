library(tidyverse)
library(signal)
library(zoo)

# load all preprocessed data
load("./preprocessed.RData")

# first attempt for data preparation
# accelerometer data and GPS speed is used for the prediction

grabData_train_1 <- 
  grabData_train %>% 
  select(bookingID, contains("acceleration"), second, Speed) %>% 
  group_by(bookingID) %>% 
  arrange(bookingID, second) %>% 
  ungroup()

# unique id for each trip
ids <- unique(grabData_train_1$bookingID)

# apply butterworth filter (bandpass filter) on all selected data

bw <- butter(2, 0.1, type = "pass", plane = "z")

# a function to apply bandpass filter on sensor data
fil_fun <- function(ids, column_name){
  filtered <- NULL
  for(id in ids){
    df <- grabData_train_1[grabData_train_1$bookingID == id, column_name]
    fil <- filtfilt(bw, df[[column_name]])
    filtered <- c(filtered, fil)
  }
  return(filtered)
}

# visualize the effect of  band pass filter
testing <- grabData_train_1 %>% dplyr::filter(bookingID == grabData_train_1$bookingID[1]) 

ggplot(data = testing)+
  geom_line(aes(x = second, y = acceleration_z)) +
  geom_line(aes(x = second, y = acc_z_filtered), color = "green") +
  xlab("Time (seconds)") +
  ylab("Acceleration") 

# apply bandpass filter on acceleration and speed data

column_names <- names(grabData_train_1)[c(2,3,4,6)]
new_col <- c("acc_x_filtered", "acc_y_filtered", "acc_z_filtered", "Speed_filtered")

system.time(for(j in 1:length(new_col)){
  grabData_train_1[new_col[j]] <- fil_fun(ids, column_names[j])
})

# test rollapplyr with overlapping 50%

a_test <- 1:100
rollapply(a_test, width = 10, mean, by = 4, align = "right")

# reorientation of acceleration
# using absolute value

grabData_train_1 <- 
  grabData_train_1 %>% select(acc_x_filtered:acc_z_filtered) %>% 
  mutate(acc_ab = sqrt(acc_x_filtered^2 + acc_y_filtered^2 + acc_z_filtered^2))
  

# apply sliding windwows on filtered data

grabData_train_preprocessed <-
  grabData_train_1 %>% 
  group_by(bookingID) %>% 
  select(acc_x_filtered, acc_y_filtered, acc_z_filtered, acc_ab, Speed_filtered) %>%
  summarise_at(vars(acc_x_filtered:Speed), rollapply, width = 250, by = 125, aligh = "right")









