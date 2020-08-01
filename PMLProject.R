library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(corrplot)

# Download datasets

train_filename <- 'pml-training.csv'
test_filename <- 'pml-testing.csv'
  
if (!file.exists(train_filename)){
    
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",train_filename, method = "curl")
  filedownloadtime <- paste(file.info(train_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights training set was downloaded:", filedownloadtime,sep = " "))
    
} else {
    
  filedownloadtime <- paste(file.info(train_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights training set was downloaded:", filedownloadtime, sep = " "))
    
}

if (!file.exists(test_filename)){
  
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",test_filename, method = "curl")
  filedownloadtime <- paste(file.info(test_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights testing set was downloaded:", filedownloadtime,sep = " "))
  
} else {
  
  filedownloadtime <- paste(file.info(test_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights testing set was downloaded:", filedownloadtime, sep = " "))
  
}

# EDA

testing <- read_csv('pml-testing.csv')
training <- read_csv('pml-training.csv')

str(training)
# Lots of summary statistics with almost all NA
training_small <- training %>%
  select(-matches('var|kurtosis|skewness|max|min|amplitude|avg|stddev')) %>%
  mutate(classe = as.factor(classe)) %>%
  group_by(classe)

#Due to the specifications in the article, we will use the gyroscope as our predictor
training_ga <- training_small %>%
  select(matches('classe|^gyros|^accel'))

set.seed(1579)
trainingset <- createDataPartition(y = training_ga$classe, p = 0.6, list = FALSE)
TrainTrainingSet <- training_ga[trainingset, ]
TestTrainingSet <- training_ga[-trainingset, ]

corrplot(cor(TrainTrainingSet[, -length(names(TrainTrainingSet))]), 
         method = "color", tl.cex = 0.5)

#decision tree
set.seed(8745)

model_dt <- rpart(classe~., data= TrainTrainingSet, method = 'class')
pred_dt <- predict(model_dt, TestTrainingSet, type = 'class')

fancyRpartPlot(model_dt)
confusionMatrix(pred_dt, TestTrainingSet$classe)

#Random Forest
set.seed(7654)

model_rf <- train(classe~.,data=TrainTrainingSet, method='rf')
pred_rf <- predict(model_rf, TestTrainingSet)
confusionMatrix(pred_rf, TestTrainingSet$classe)

#assignment answers
answers <- predict(model_rf, testing)
