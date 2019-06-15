library(tidyverse)

# load all preprocessed data
load("./preprocessed.RData")

# test band pass filter
testing <- grabData_train %>% filter(bookingID == grabData_train$bookingID[100]) %>% 
  select(acceleration_z, second) %>% arrange(second)

ggplot(data = testing)+
  geom_line(aes(x = second, y = acceleration_z))


