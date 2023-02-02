
train <- read.csv("C:/Users/Molopyane/Downloads/pml-training.csv")
test  <- read.csv("C:/Users/Molopyane/Downloads/pml-testing.csv")


library(caret)
library(dplyr)
library(randomForest)




entries <-train %>% 
  summarise_all(funs(sum(as.numeric(!is.na(.)), na.rm = TRUE))) %>%
  collect()

complete_entries <- grep("19622", entries)
incomplete_entries <- grep("406",entries)
length(sort(c(complete_entries,incomplete_entries)))

train_subset_1 <- train[,complete_entries]

em_cols <- c(12:20,43:48,52:60,74:82)

train_subset_2 <- train_subset_1[,-em_cols]

redundent<- c(1:5)

new_train <- train_subset_2[,-redundent]

new_train$new_window <- as.factor(new_train$new_window)
new_train$classe     <- as.factor(new_train$classe)

set.seed(2242)
inTrain <- createDataPartition(y = new_train$classe, p= 0.85, list = FALSE)
training <- new_train[inTrain,]

model <- randomForest(classe~.,data = training, importance = TRUE)

trainingpredictions <- predict(model,training[,-55])
confusionMatrix(trainingpredictions,training$classe)


testingpredictions <- predict(model,testing[,-55])
confusionMatrix(testingpredictions,testing$classe)

chosen_columns <- names(training[,-55])
validation_set <- test[,chosen_columns]
validation_set$new_window<- as.factor(validation_set$new_window)
validation_set <- rbind(training[1,-55], validation_set)
validation_set <- validation_set[-1,]

validationprediction <- predict(model,validation_set)
validationprediction

importance<-data.frame(model$importance[,7])
importance <- arrange(importance,desc(model$importance[,7]))
sum <- sum(importance )
importance <- (importance/sum)*100
importance[1:5,]

model2 <- randomForest(classe~num_window+roll_belt+yaw_belt+magnet_dumbbell_z+pitch_forearm,
                       data = training, importance = TRUE)

trainingpredictions2 <- predict(model2, training[,-55])
confusionMatrix(trainingpredictions2,training$classe)

model3 <- randomForest(classe~num_window, data = training, importance = TRUE)
testingpredictions2 <- predict(model2,testing[,-55])
confusionMatrix(testingpredictions2,testing$classe)

validationpredictions2 <- predict(model,validation_set)

vals <- data.frame(validationprediction, validationpredictions2)
vals
