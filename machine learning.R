library(tidyverse)
library(caret)

# scaling of training DF
# save label data as label
label <- grabData_train_ready[, "label"]
preprocess_model <- preProcess(grabData_train_ready[,-39], method = "range")
grabData_train_ready <- predict(preprocess_model, newdata = grabData_train_ready[, -39])

# add label data back into training data 
grabData_train_ready$label <- label
