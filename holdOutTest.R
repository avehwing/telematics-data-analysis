library(tidyverse)
library(caret)
library(signal)
library(zoo)

source("./preprocessing.R")

# load model
model_rf <- readRDS("./model_rf.rds")

# load test set and label
test_DF <- read_csv("./")
label_DF <- read_csv("./")

# load result
result <- hold_out_test(test_DF, label_DF, model_rf)

