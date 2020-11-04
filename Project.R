if (!require("librarian")) install.packages("librarian")
library(librarian)
shelf(caret, randomForest, rpart, ggplot2, dplyr, tidyr, janitor, rpart.plot)

set.seed(69)
setwd('/users/coreyneff/downloads/')
train_file <- download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',
                            destfile = 'train.csv')
test_file <- download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',
                            destfile = 'test.csv')
train <- read.csv('train.csv', na.strings = c('#DIV/0!', 'NA',''))
test <- read.csv('test.csv', na.strings = c('#DIV/0!', 'NA', ''))
train <-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]
train <- train[,-c(1:7)]
test <- test[,-c(1:7)]
train$classe <- as.factor(train$classe)

partition <- createDataPartition(train$classe, p=0.75, list=FALSE)
train_train <- train[partition,] 
train_test <- train[-partition,]

ggplot(train_train) +
      geom_bar(aes(classe), col="black", fill="darkgreen") +
      xlab("Classe Levels")
train_cart <- rpart(classe ~., train_train, method = "class")
cart_predict <- predict(train_cart, train_test, type = "class")
confusion<- confusionMatrix(cart_predict,as.factor(train_test$classe))

train_forest <- randomForest(classe ~., train_train, method = "class")
forest_predict <- predict(train_forest, train_test, method = "class")
confusion2 <- confusionMatrix(forest_predict, as.factor(train_test$classe))

list(CART = confusion$overall["Accuracy"],
        randomForest = confusion2$overall["Accuracy"])


final <- predict(train_forest, test, type = "class")
final