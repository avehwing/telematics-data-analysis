# Grab AI for S.E.A Challenge (Safety)

## Introduction
This repository is created for participating in the Grab AI for SEA Challenge, specifically the safety category. In this challenge, the participant is required to analyze the telematicc data to classify dangerous driving. A dataset is provided which consists of telematics data for each trip with the label of dangerous driving (0 or 1).

### Dataset
The full dataset can be downloaded at https://s3-ap-southeast-1.amazonaws.com/grab-aiforsea-dataset/safety.zip. The full dataset consists of two separate tables which are the telematic data table and label data table.

The following is the detail of the dataset:
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

However, the dataset may not be available after the deadline of the challenge.
