library(tidyverse)
library(caret)

# scaling of training DF
# save label data as label
grabData_train_ready <- read_rds("./grabData_train_ready.rds")
load("./preprocessed.RData")

grabData_train_ready <-
  grabData_train_ready %>% left_join(trainDF, by = "bookingID")

grabData_train_ready <-
  grabData_train_ready %>% 
  mutate(label = case_when(label == 1 ~ "Dangerous",
                           label == 0 ~ "Safe")) %>% 
  mutate(label = as.factor(label)) 

y <- grabData_train_ready$label
preprocess_model <- preProcess(select(grabData_train_ready, -label), method = "range")
grabData_train_ready <- predict(preprocess_model, newdata = select(grabData_train_ready, -label))

grabData_train_ready$label <- y

grabData_train_ready <-
  grabData_train_ready %>% na.omit()

# create predictors and response variable from training set
xx <- grabData_train_ready %>% select(-bookingID)

yy <- 
  trainDF %>% 
  mutate(label = case_when(label == 1 ~ "Dangerous", 
                           label == 0 ~ "Safe")) %>% 
  mutate(label = as.factor(label)) %>% select(label)


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() -1)
registerDoParallel(cluster)

fitcontrol <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  savePredictions = "final",
  allowParallel = TRUE
)

model_rf <- train(label~., data = select(grabData_train_ready, -bookingID) , method = "rf", trControl = fitcontrol,
                  tuneLength = 10, metric = "ROC")

stopCluster(cluster)
registerDoSEQ()

saveRDS(model_rf, file = "model_rf.rds")

















