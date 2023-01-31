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
data<- rbind(train,test)
# Partition the data
set.seed(2242)
inTrain <- createDataPartition(y = new_train$classe, p= 0.85, list = FALSE)
training <- new_train[inTrain,]
testing  <- new_train[-inTrain,]


?train

model<- train(classe~.,data = training, method = "gbm")

model <- randomForest(classe~.,data = training, importance = TRUE)
importancee<-data.frame(model$importance[,7])
importancee
importancee <- data.frame(names(training[1:54]),"Gini"=model$importance[,7])
order(importancee,Gini)
importancee <- arrange(importancee,desc(model$importance[,7]))



trainingpredictions <- predict(model,training[,-55])
confusionMatrix(trainingpredictions,training$classe)


testingpredictions <- predict(model,testing[,-55])
confusionMatrix(testingpredictions,testing$classe)

# Prepare validation set
chosen_columns <- names(training[,-55])

validation_set <- test[,chosen_columns]
validation_set$new_window<- as.factor(validation_set$new_window)

valpredictions <- predict(model,newdata = validation_set, 
                                       type = "response")

validation_set$num_window <- as.numeric(validation_set$num_window)
validation_set$roll_belt <- as.numeric(validation_set$roll_belt)
validation_set$pitch_belt<- as.numeric(validation_set$pitch_belt)
validation_set$yaw_belt<- as.numeric(validation_set$yaw_belt)
validation_set$total_accel_belt<- as.numeric(validation_set$total_accel_belt)
validation_set$gyros_belt_x<- as.numeric(validation_set$gyros_belt_x)
validation_set$gyros_belt_y<- as.numeric(validation_set$gyros_belt_y)
validation_set$gyros_belt_z<- as.numeric(validation_set$gyros_belt_z)
validation_set$accel_belt_x<- as.numeric(validation_set$accel_belt_x)
validation_set$accel_belt_y<- as.numeric(validation_set$accel_belt_y)
validation_set$accel_belt_z<- as.numeric(validation_set$accel_belt_z)
validation_set$magnet_belt_x<- as.numeric(validation_set$magnet_belt_x)
validation_set$magnet_belt_y<- as.numeric(validation_set$magnet_belt_y)
validation_set$magnet_belt_z<- as.numeric(validation_set$magnet_belt_z)
validation_set$roll_arm <- as.numeric(validation_set$roll_arm)
validation_set$pitch_arm <- as.numeric(validation_set$pitch_arm)
validation_set$yaw_arm <- as.numeric(validation_set$yaw_arm)
validation_set$total_accel_arm<-as.numeric(validation_set$total_accel_arm)
validation_set$gyros_arm_x <- as.numeric(validation_set$gyros_arm_x)
validation_set$gyros_arm_y <- as.numeric(validation_set$gyros_arm_y)
validation_set$gyros_arm_z <- as.numeric(validation_set$gyros_arm_z)
validation_set$accel_arm_x <- as.numeric(validation_set$accel_arm_x)
validation_set$accel_arm_y <- as.numeric(validation_set$accel_arm_y)
validation_set$accel_arm_z <- as.numeric(validation_set$accel_arm_z)
validation_set$magnet_arm_x<- as.numeric(validation_set$magnet_arm_x)
validation_set$magnet_arm_y<- as.numeric(validation_set$magnet_arm_y)
validation_set$magnet_arm_z<- as.numeric(validation_set$magnet_arm_z)
validation_set$roll_dumbbell<- as.numeric(validation_set$roll_dumbbell)
validation_set$pitch_dumbbell <- as.numeric(validation_set$pitch_dumbbell)
validation_set$yaw_dumbbell <- as.numeric(validation_set$yaw_dumbbell)
validation_set$total_accel_dumbbell<- as.numeric(validation_set$total_accel_dumbbell)
validation_set$gyros_dumbbell_x<- as.numeric(validation_set$gyros_dumbbell_x)
validation_set$gyros_dumbbell_y<- as.numeric(validation_set$gyros_dumbbell_y)
validation_set$gyros_dumbbell_z<- as.numeric(validation_set$gyros_dumbbell_z)
validation_set$accel_dumbbell_x<- as.numeric(validation_set$accel_arm_x)
validation_set$accel_dumbbell_y<- as.numeric(validation_set$accel_arm_y)
validation_set$accel_dumbbell_z<- as.numeric(validation_set$accel_arm_z)
validation_set$magnet_dumbbell_x<- as.numeric(validation_set$magnet_dumbbell_x)
validation_set$magnet_dumbbell_y<- as.numeric(validation_set$magnet_dumbbell_y)
validation_set$magnet_dumbbell_z<- as.numeric(validation_set$magnet_dumbbell_z)
validation_set$roll_forearm <- as.numeric(validation_set$roll_forearm)
validation_set$pitch_forearm <- as.numeric(validation_set$pitch_forearm)
validation_set$yaw_forearm <- as.numeric(validation_set$yaw_forearm)
validation_set$total_accel_forearm<- as.numeric(validation_set$total_accel_forearm)
validation_set$gyros_forearm_x<- as.numeric(validation_set$gyros_forearm_x)
validation_set$gyros_forearm_y<- as.numeric(validation_set$gyros_forearm_y)
validation_set$gyros_forearm_z<- as.numeric(validation_set$gyros_forearm_z)
validation_set$accel_forearm_x<- as.numeric(validation_set$accel_forearm_x)
validation_set$accel_forearm_y<- as.numeric(validation_set$accel_forearm_y)
validation_set$accel_forearm_z<- as.numeric(validation_set$accel_forearm_z)
validation_set$magnet_forearm_x<- as.numeric(validation_set$magnet_arm_x)
validation_set$magnet_forearm_y<- as.numeric(validation_set$magnet_arm_y)
validation_set$magnet_forearm_z<- as.numeric(validation_set$magnet_arm_z)

k <-names(validation_set)
l <-names(training[,-55])
cc <- data.frame(k,l)
View(cc)
m <- sapply(validation_set,class)
n <- sapply(training[,-55],class)
dd<- data.frame(m,n)
View(dd)
model$type
model$terms

mod <- train(classe~., data= training, method = "rf")
 
mod <- train()
testprediction <- predict(model,validation_set)

?randomForest
?predict

 # Coerced dataset
new_train$new_window <- as.factor(new_train$new_window)
new_train$num_window <- as.numeric(new_train$num_window)
new_train$roll_belt <- as.numeric(new_train$roll_belt)
new_train$pitch_belt<- as.numeric(new_train$pitch_belt)
new_train$yaw_belt<- as.numeric(new_train$yaw_belt)
new_train$total_accel_belt<- as.numeric(new_train$total_accel_belt)
new_train$gyros_belt_x<- as.numeric(new_train$gyros_belt_x)
new_train$gyros_belt_y<- as.numeric(new_train$gyros_belt_y)
new_train$gyros_belt_z<- as.numeric(new_train$gyros_belt_z)
new_train$accel_belt_x<- as.numeric(new_train$accel_belt_x)
new_train$accel_belt_y<- as.numeric(new_train$accel_belt_y)
new_train$accel_belt_z<- as.numeric(new_train$accel_belt_z)
new_train$magnet_belt_x<- as.numeric(new_train$magnet_belt_x)
new_train$magnet_belt_y<- as.numeric(new_train$magnet_belt_y)
new_train$magnet_belt_z<- as.numeric(new_train$magnet_belt_z)
new_train$roll_arm <- as.numeric(new_train$roll_arm)
new_train$pitch_arm <- as.numeric(new_train$pitch_arm)
new_train$yaw_arm <- as.numeric(new_train$yaw_arm)
new_train$total_accel_arm<-as.numeric(new_train$total_accel_arm)
new_train$gyros_arm_x <- as.numeric(new_train$gyros_arm_x)
new_train$gyros_arm_y <- as.numeric(new_train$gyros_arm_y)
new_train$gyros_arm_z <- as.numeric(new_train$gyros_arm_z)
new_train$accel_arm_x <- as.numeric(new_train$accel_arm_x)
new_train$accel_arm_y <- as.numeric(new_train$accel_arm_y)
new_train$accel_arm_z <- as.numeric(new_train$accel_arm_z)
new_train$magnet_arm_x<- as.numeric(new_train$magnet_arm_x)
new_train$magnet_arm_y<- as.numeric(new_train$magnet_arm_y)
new_train$magnet_arm_z<- as.numeric(new_train$magnet_arm_z)
new_train$roll_dumbbell<- as.numeric(new_train$roll_dumbbell)
new_train$pitch_dumbbell <- as.numeric(new_train$pitch_dumbbell)
new_train$yaw_dumbbell <- as.numeric(new_train$yaw_dumbbell)
new_train$total_accel_dumbbell<- as.numeric(new_train$total_accel_dumbbell)
new_train$gyros_dumbbell_x<- as.numeric(new_train$gyros_dumbbell_x)
new_train$gyros_dumbbell_y<- as.numeric(new_train$gyros_dumbbell_y)
new_train$gyros_dumbbell_z<- as.numeric(new_train$gyros_dumbbell_z)
new_train$accel_dumbbell_x<- as.numeric(new_train$accel_arm_x)
new_train$accel_dumbbell_y<- as.numeric(new_train$accel_arm_y)
new_train$accel_dumbbell_z<- as.numeric(new_train$accel_arm_z)
new_train$magnet_dumbbell_x<- as.numeric(new_train$magnet_dumbbell_x)
new_train$magnet_dumbbell_y<- as.numeric(new_train$magnet_dumbbell_y)
new_train$magnet_dumbbell_z<- as.numeric(new_train$magnet_dumbbell_z)
new_train$roll_forearm <- as.numeric(new_train$roll_forearm)
new_train$pitch_forearm <- as.numeric(new_train$pitch_forearm)
new_train$yaw_forearm <- as.numeric(new_train$yaw_forearm)
new_train$total_accel_forearm<- as.numeric(new_train$total_accel_forearm)
new_train$gyros_forearm_x<- as.numeric(new_train$gyros_forearm_x)
new_train$gyros_forearm_y<- as.numeric(new_train$gyros_forearm_y)
new_train$gyros_forearm_z<- as.numeric(new_train$gyros_forearm_z)
new_train$accel_forearm_x<- as.numeric(new_train$accel_forearm_x)
new_train$accel_forearm_y<- as.numeric(new_train$accel_forearm_y)
new_train$accel_forearm_z<- as.numeric(new_train$accel_forearm_z)
new_train$magnet_forearm_x<- as.numeric(new_train$magnet_arm_x)
new_train$magnet_forearm_y<- as.numeric(new_train$magnet_arm_y)
new_train$magnet_forearm_z<- as.numeric(new_train$magnet_arm_z)
new_train$classe <- as.factor(new_train$classe)

# Partition the data
set.seed(2242)
inTrain <- createDataPartition(y = new_train$classe, p= 0.85, list = FALSE)
training <- new_train[inTrain,]
testing  <- new_train[-inTrain,]

model <- randomForest(classe~.,data = training, importance = TRUE)

model <- train(classe~.,data = training, method = "gbm")

 trainingpredictions <- predict(model,training[,-55])
confusionMatrix(trainingpredictions,training$classe)

testingpredictions <- predict(model,testing[,-55])
confusionMatrix(testingpredictions,testing$classe)
