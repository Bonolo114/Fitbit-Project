train <- read.csv("C:/Users/Molopyane/Downloads/pml-training.csv")
test  <- read.csv("C:/Users/Molopyane/Downloads/pml-testing.csv")

# load all used libraries
library(caret)
library(dplyr)
library(randomForest)

# Complete entries
entries <-train %>% 
  summarise_all(funs(sum(as.numeric(!is.na(.)), na.rm = TRUE))) %>%
  collect()

complete_entries <- grep("19622", entries)

incomplete_entries <- grep("406",entries)

length(sort(c(complete_entries,incomplete_entries)))

# create the a dataset with no NA cells
train_subset_1 <- train[,complete_entries]

# Mainly empty columns

em_cols <- c(12:20,43:48,52:60,74:82)

train_subset_2 <- train_subset_1[,-em_cols]
# Columns not useful for analysis
redundent<- c(1:5)

new_train <- train_subset_2[,-redundent]



new_train$new_window <- as.factor(new_train$new_window)
new_train$classe     <- as.factor(new_train$classe)

# Partition the data
set.seed(2242)
inTrain <- createDataPartition(y = new_train$classe, p= 0.85, list = FALSE)
training <- new_train[inTrain,]
testing  <- new_train[-inTrain,]

preobj <- preProcess(training, method = c("center","scale"))
trainingpred <- predict(preobj,training)

testingpred  <- predict(preobj,testing)

# Prepare validation set
chosen_columns <- names(training[,-55])

validation_set <- test[,chosen_columns]
validation_set$new_window<- as.factor(validation_set$new_window)

validationpred<- predict(preobj,validation_set)

model <- randomForest(classe~.,data = trainingpred, importance = TRUE)

importancee<-data.frame(model$importance[,7])
importancee

predictiontrain    <- predict(model,trainingpred)
predictiontrain
confusionMatrix(predictiontrain,training$classe)

predictiontest     <- predict(model,testingpred)
predictiontest
confusionMatrix(predictiontest,testing$classe)

predictionvalidate <- predict(model,validationpred)

data <- rbind(trainingpred[1,-55], validationpred)
data <- data[-1,]

predict(model,data)

nrow(training)
length(training)
length(validation_set)

dput(test)
