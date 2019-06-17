# Grab AI for S.E.A Challenge (Safety)

## Introduction
This repository is created for participating in the Grab AI for SEA Challenge, specifically the safety category. In this challenge, the participant is required to analyze the telematicc data to classify dangerous driving. A dataset is provided which consists of telematics data for each trip with the label of dangerous driving (0 or 1).

### Dataset
The full dataset can be downloaded at https://s3-ap-southeast-1.amazonaws.com/grab-aiforsea-dataset/safety.zip. The full dataset consists of two separate tables which are the telematic data table and label data table.

The following is the detail of the telematic dataset:

| Field | Description |
| --- | --- |
| bookingID | trip id |
| Accuracy | accuracy inferred by GPS in meters |
| Bearing |GPS bearing in degree |
| acceleration_x | accelerometer reading at x axis (m/s2) |
| acceleration_y | accelerometer reading at y axis (m/s2) |
| acceleration_z | accelerometer reading at z axis (m/s2) |
| gyro_x | gyroscope reading in x axis (rad/s) |
| gyro_y | gyroscope reading in y axis (rad/s) |
| gyro_z | gyroscope reading in z axis (rad/s) |
| second | time of the record by number of seconds |
| Speed | speed measured by GPS in m/s |

The following is the detail of the label dataset:

| Field | Description |
| --- | --- |
| bookingID | trip id |
| label | 0: Safe, 1: Dangerous driving |

However, the dataset may not be available after the deadline of the challenge.

## User Instruction

**This project used R (3.5.3) as the programming language to tackle the problem. It requires the following packages:**

| No. | Package | Version |
| --- | --- | --- |
| 1 | tidyverse | 1.2.1 |
| 2 | zoo | 1.8-4 |
| 3 | signal | 0.7-6 |
| 4 | caret | 6.0-84 |
| 5 | DMwR | 0.4.1 |
| 6 | doParallel | 1.0.14 |
| 7 | ROSE | 0.0-3

This repository contains 5 main R scripts to run the challenge.

1. EDA.R
2. data_preparing.R
3. machine learning.R
4. preprocessing.R
5. holdOutTest.R

The first two scripts are prepared for data pre-processing which including joining tables and features extraction. The third script is dedicated for model training using random forest algorithm. Next, the fourth script is written with helper function to run the hold out test script. Lastly, holdOutTest.R is the script prepared for model validation.

The following is the steps of using holdOutTest.R:
1. Load all of the libraries, helper function and the prediction model.
```
library(tidyverse)
library(caret)
library(signal)
library(zoo)

source("./preprocessing.R")

# load model
model_rf <- readRDS("./model_rf.rds")
```
2. Load the test set and label data
```
test_DF <- read_csv("./grabData_test")
label_DF <- read_csv("./test_label")
```
3. Run the helper function to obtain the result
```
result <- hold_out_test(test_DF, label_DF, model_rf)
print(result)
```









