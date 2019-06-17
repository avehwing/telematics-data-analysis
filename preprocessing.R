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

preprocessing_stage_2 <- function(newDF){
  # apply sliding windwows on filtered data (75% overlap)
  newDF <-
    newDF %>%
    group_by(bookingID) %>% 
    mutate(acc_x = rollapply(acc_x_filtered, 
                             width = 50, 
                             FUN = mean, 
                             by = 13, 
                             fill = NA),
           acc_y = rollapply(acc_y_filtered,
                             width = 50,
                             FUN = mean,
                             by = 13,
                             fill = NA),
           acc_z = rollapply(acc_z_filtered,
                             width = 50, 
                             FUN = mean, 
                             by = 13, 
                             fill = NA),
           speed = rollapply(Speed_filtered,
                             width = 50, 
                             FUN = mean, 
                             by = 13, 
                             fill = NA),
           acc_ab = rollapply(acc_ab,
                              width = 50, 
                              FUN = mean, 
                              by = 13, 
                              fill = NA))
  
  # filter NA values
  newDF <-
    newDF %>% 
    dplyr::filter(!is.na(acc_x)) %>% 
    select(bookingID, second, acc_x, acc_y, acc_z, acc_ab, speed)
  
  # feature engineering: rotation angle- phi and theta
  newDF <-
    newDF %>% 
    mutate(rot_phi = atan(acc_y/sqrt(acc_x^2 + acc_z^2)),
           rot_theta = atan(-acc_x/acc_z))
  
  # feature engineering: 38 features
  newDF <-
    newDF %>% 
    mutate(acc_ab_p = lead(acc_ab) - acc_ab)
  
  newDF <- 
    newDF %>% 
    group_by(bookingID) %>% 
    summarise(acc_x.mean = mean(acc_x),
              acc_y.mean = mean(acc_y),
              acc_z.mean = mean(acc_z),
              acc_ab.mean = mean(acc_ab),
              phi.mean = mean(rot_phi),
              theta.mean = mean(rot_theta),
              speed.mean = mean(speed),
              acc_x.var = var(acc_x),
              acc_y.var = var(acc_y),
              acc_z.var = var(acc_z),
              phi.var = var(rot_phi),
              theta.var = var(rot_theta),
              speed.var = var(speed),
              acc_x.sd = sd(acc_x),
              acc_y.sd = sd(acc_y),
              acc_z.sd = sd(acc_z),
              speed.sd = sd(speed),
              acc_x.diff = max(acc_x) - min(acc_x),
              acc_y.diff = max(acc_y) - min(acc_y),
              acc_z.diff = max(acc_z) - min(acc_z),
              speed.diff = max(speed) - min(speed),
              cor_xy = cor(acc_x, acc_y, method = "pearson"),
              cor_xz = cor(acc_x, acc_z, method = "pearson"),
              cor_yz = cor(acc_y, acc_z, method = "pearson"),
              acc_x.zc = sum(acc_x == 0, na.rm = TRUE),
              acc_y.zc = sum(acc_y == 0, na.rm = TRUE),
              acc_z.zc = sum(acc_z == 0, na.rm = TRUE),
              acc_x.par = max(acc_x) / mean(acc_x),
              acc_y.par = max(acc_y) / mean(acc_y),
              acc_z.par = max(acc_z) / mean(acc_z),
              speed.par = max(speed, na.rm = TRUE) / mean(speed, na.rm = TRUE),
              acc_x.sma = 1/100 * sum((abs(acc_x) - abs(lead(acc_x))) * (lead(second) - second), na.rm = TRUE),
              acc_y.sma = 1/100 * sum((abs(acc_y) - abs(lead(acc_y))) * (lead(second) - second), na.rm = TRUE),
              acc_z.sma = 1/100 * sum((abs(acc_z) - abs(lead(acc_z))) * (lead(second) - second), na.rm = TRUE),
              acc_ab.sma = 1/100 * sum((abs(acc_ab) - abs(lead(acc_ab))) * (lead(second) - second), na.rm = TRUE),
              acc_ab.dsvm = 1/100 * sum((abs(acc_ab_p) - abs(lead(acc_ab_p))) * (lead(second) - second), na.rm = TRUE),
              acc_ab.svm = 1/n() * sum(sqrt(acc_ab^2)))
  
  return(newDF)
}

preprocessing_stage_3 <- function(newDF, labelDF){
  # join processed features with labels
  newDF <-
    newDF %>% left_join(labelDF, by = "bookingID")
  
  # convert labels into factor
  newDF <-
    newDF %>% 
    mutate(label = case_when(label == 1 ~ "Dangerous",
                             label == 0 ~ "Safe")) %>% 
    mutate(label = as.factor(label)) 
  
  # copy label to y from newDF
  y <- newDF$label
  
  # scaling of newDF
  preprocess_model <- preProcess(select(newDF, -label), method = "range")
  newDF <- predict(preprocess_model, newdata = select(newDF, -label))
  newDF$label <- y
  
  # omit any NA values
  newDF <-
    newDF %>% na.omit()
  
  return(newDF)
}

preprocessing <- function(newDF, labelDF){
  newDF <- preprocessing_stage_1(newDF)
  newDF <- preprocessing_stage_2(newDF)
  newDF <- preprocessing_stage_3(newDF, labelDF)
  return(newDF)
}

hold_out_test <- function(newDF, labelDF, model){
  # preprocessing of dev set  
  test_data <- preprocessing(newDF, labelDF)
  
  # model prediction with dev set
  predicted_testData <- predict(model, test_data)
  
  # confusion matrix of dev set
  confu_testData <- confusionMatrix(reference = test_data$label, data = predicted_testData)
  
  return(confu_testData)
}






























