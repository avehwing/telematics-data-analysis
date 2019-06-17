library(tidyverse)
library(caret)
library(DMwR)
library(ROSE)

# scaling of training DF
# save label data as label
grabData_train_ready <- read_rds("./grabData_train_ready.rds")
load("./preprocessed.RData")

# join preprocessed features with labels
grabData_train_ready <-
  grabData_train_ready %>% left_join(trainDF, by = "bookingID")

# convert labels into factors
grabData_train_ready <-
  grabData_train_ready %>% 
  mutate(label = case_when(label == 1 ~ "Dangerous",
                           label == 0 ~ "Safe")) %>% 
  mutate(label = as.factor(label)) 

y <- grabData_train_ready$label

# apply scalling on the processed data
preprocess_model <- preProcess(select(grabData_train_ready, -label), method = "range")
grabData_train_ready <- predict(preprocess_model, newdata = select(grabData_train_ready, -label))
grabData_train_ready$label <- y

# omit Na values from data
grabData_train_ready <-
  grabData_train_ready %>% na.omit()

set.seed(100)

# to enable CPU parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() -1)
registerDoParallel(cluster)

# configure training parameters
fitcontrol <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
)

# model training using random forest algorithm
model_rf <- train(label~., data = select(grabData_train_ready, -bookingID) , method = "rf", trControl = fitcontrol,
                  tuneLength = 10, metric = "ROC")

# stop parallel processing
stopCluster(cluster)
registerDoSEQ()

saveRDS(model_rf, file = "model_rf.rds")

# Make prediction with model
predicted_trainDF <- predict(model_rf, grabData_train_ready)

# confusion matrix of model prediction
confus_trainDF <- confusionMatrix(
  reference = grabData_train_ready$label,
  data = predicted_trainDF
)

# preprocessing of dev set
test_data <- preprocessing(grabData_dev, devDF)

# model prediction with dev set
predicted_testData <- predict(model_rf, test_data)

# confusion matrix of dev set
confu_testData <- confusionMatrix(reference = test_data$label, data = predicted_testData)













