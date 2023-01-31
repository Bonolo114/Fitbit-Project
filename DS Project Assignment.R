library(caret)
library(dplyr)

null_counts <-train %>% 
  summarise_all(funs(sum(as.numeric(is.na(.)), na.rm = TRUE))) %>%
  collect()
null_counts

# Extract Columns with no NA 
a <- grep("0",null_counts)
a

not_null <-train %>% 
  summarise_all(funs(sum(as.numeric(!is.na(.)), na.rm = TRUE))) %>%
  collect()
not_null
cc <- grep("406", not_null)
cc

# create the a dataset with no NA cells
check1 <- train[,a]

check1 


# Mainly empty columns

em_cols <- c(12:20,43:48,52:60,74:82)

# Columns not useful for analysis
redundent<- c(1:5)

dd<- a[-em_cols]
length(em_cols)
ddd<-dd[-redundent]
tet<- train[,ddd]
tit<- test[,ddd]

c <- data.frame(names(new_train), names(tit))
class(names(tit))
class(names(new_train))
View(c)
confusionMatrix(names(new_train), names(tit))

c <- data.frame(names(train),names(test))
c
View(tet)
# Extract dataset with mainly filled cells

check2 <- check1[,-em_cols]
View(check2)

# Data set with analysible variables
new_train <- check2[,-redundent]
View(new_train)

ncol(new_train)

# Change variable from the "character" class to the most appropriate class

unused_var <- c(cc,em_cols,redundent)

160-length(unused_var)

sort(unused_var)
used_dataset<- train[,-unused_var]
View(used_dataset)
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

View(new_train)
library(caret)
library(randomForest)

mod<- randomForest(classe~., data = new_train, importance = TRUE)

prediction <- predict(mod,new_train[,-55])



prediction

confusionMatrix(prediction,new_train$classe)

# Cross Validation

names(test)
test$problem_id

testa<- test[,ddd]



predictions <- predict(mod,testa)

model <- glm(classe~., data = new_train, family = "binomial")

summary(model)

pred1 <- predict(model,newdata = new_train)
pred1

library()
class<- c(quantile(pred1,0.2),quantile(pred1,0.4),quantile(pred1,0.6),
          quantile(pred1,0.8),quantile(pred1,1))
class
predd<- ifelse(pred1<=quantile(pred1,0.2),"A",ifelse(pred1<=quantile(pred1,0.4),"B",
        ifelse(pred1<=quantile(pred1,0.6),"C",ifelse(pred1<=quantile(pred1,0.8),
        "D","E"))))
predictions <- as.factor(predd)
class(new_train$classe)
class(predictions)

unique(predictions)
unique(new_train$classe)

table(predd)
table(new_train$classe)

length(new_train$classe)
length(predictions)

confusionMatrix(new_train$classe,predictions)

###############
sort(unused_var)

View(tit)
tit$new_window <- as.factor(tit$new_window)
tit$num_window <- as.numeric(tit$num_window)
tit$roll_belt <- as.numeric(tit$roll_belt)
tit$pitch_belt<- as.numeric(tit$pitch_belt)
tit$yaw_belt<- as.numeric(tit$yaw_belt)
tit$total_accel_belt<- as.numeric(tit$total_accel_belt)
tit$gyros_belt_x<- as.numeric(tit$gyros_belt_x)
tit$gyros_belt_y<- as.numeric(tit$gyros_belt_y)
tit$gyros_belt_z<- as.numeric(tit$gyros_belt_z)
tit$accel_belt_x<- as.numeric(tit$accel_belt_x)
tit$accel_belt_y<- as.numeric(tit$accel_belt_y)
tit$accel_belt_z<- as.numeric(tit$accel_belt_z)
tit$magnet_belt_x<- as.numeric(tit$magnet_belt_x)
tit$magnet_belt_y<- as.numeric(tit$magnet_belt_y)
tit$magnet_belt_z<- as.numeric(tit$magnet_belt_z)
tit$roll_arm <- as.numeric(tit$roll_arm)
tit$pitch_arm <- as.numeric(tit$pitch_arm)
tit$yaw_arm <- as.numeric(tit$yaw_arm)
tit$total_accel_arm<-as.numeric(tit$total_accel_arm)
tit$gyros_arm_x <- as.numeric(tit$gyros_arm_x)
tit$gyros_arm_y <- as.numeric(tit$gyros_arm_y)
tit$gyros_arm_z <- as.numeric(tit$gyros_arm_z)
tit$accel_arm_x <- as.numeric(tit$accel_arm_x)
tit$accel_arm_y <- as.numeric(tit$accel_arm_y)
tit$accel_arm_z <- as.numeric(tit$accel_arm_z)
tit$magnet_arm_x<- as.numeric(tit$magnet_arm_x)
tit$magnet_arm_y<- as.numeric(tit$magnet_arm_y)
tit$magnet_arm_z<- as.numeric(tit$magnet_arm_z)
tit$roll_dumbbell<- as.numeric(tit$roll_dumbbell)
tit$pitch_dumbbell <- as.numeric(tit$pitch_dumbbell)
tit$yaw_dumbbell <- as.numeric(tit$yaw_dumbbell)
tit$total_accel_dumbbell<- as.numeric(tit$total_accel_dumbbell)
tit$gyros_dumbbell_x<- as.numeric(tit$gyros_dumbbell_x)
tit$gyros_dumbbell_y<- as.numeric(tit$gyros_dumbbell_y)
tit$gyros_dumbbell_z<- as.numeric(tit$gyros_dumbbell_z)
tit$accel_dumbbell_x<- as.numeric(tit$accel_arm_x)
tit$accel_dumbbell_y<- as.numeric(tit$accel_arm_y)
tit$accel_dumbbell_z<- as.numeric(tit$accel_arm_z)
tit$magnet_dumbbell_x<- as.numeric(tit$magnet_dumbbell_x)
tit$magnet_dumbbell_y<- as.numeric(tit$magnet_dumbbell_y)
tit$magnet_dumbbell_z<- as.numeric(tit$magnet_dumbbell_z)
tit$roll_forearm <- as.numeric(tit$roll_forearm)
tit$pitch_forearm <- as.numeric(tit$pitch_forearm)
tit$yaw_forearm <- as.numeric(tit$yaw_forearm)
tit$total_accel_forearm<- as.numeric(tit$total_accel_forearm)
tit$gyros_forearm_x<- as.numeric(tit$gyros_forearm_x)
tit$gyros_forearm_y<- as.numeric(tit$gyros_forearm_y)
tit$gyros_forearm_z<- as.numeric(tit$gyros_forearm_z)
tit$accel_forearm_x<- as.numeric(tit$accel_forearm_x)
tit$accel_forearm_y<- as.numeric(tit$accel_forearm_y)
tit$accel_forearm_z<- as.numeric(tit$accel_forearm_z)
tit$magnet_forearm_x<- as.numeric(tit$magnet_arm_x)
tit$magnet_forearm_y<- as.numeric(tit$magnet_arm_y)
tit$magnet_forearm_z<- as.numeric(tit$magnet_arm_z)
tit$problem_id <- as.factor(tit$problem_id)
test$problem_id<- names(test$classe)
?names
names(test)
cll<- sapply(tit,class)
View(cll)
cll1<- sapply(new_train,class)
dded<- data.frame(cll,cll1,cll2)
cll2 <- sapply(test,class)
View(dded)

testa<- test[,-unused_var]
testpred <- predict(mod,new_train[,-55])
