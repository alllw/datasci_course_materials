library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(e1071)
data <- read.csv("seaflow_21min.csv", header = T)
summary(data$pop)
summary(data$fsc_small)
data.train <- data[sample(1:dim(data)[1], size = dim(data)[1]/2, replace = TRUE), ]
data.test  <- data[sample(1:dim(data)[1], size = dim(data)[1]/2, replace = TRUE), ]
mean(data.train$time)
qplot(pe, chl_small, data = data, color = pop)
fol <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
model <- rpart(fol, method = "class", data = data.train)
test.labels <- predict(model, data.test, type = "class")
confusionMatrix(test.labels, data.test$pop)
modelRF <- randomForest(fol, data=data.train)
confusionMatrix(predict(modelRF, data.test, type = "class"), data.test$pop)
importance(modelRF)
modelSVM <- svm(fol, data = data.train)
confusionMatrix(predict(modelSVM, data.test, type = "class"), data.test$pop)
datal <- data[data$file_id != 208, ]
datal.train <- datal[sample(1:dim(datal)[1], size = dim(datal)[1]/2, replace = TRUE), ]
datal.test  <- datal[sample(1:dim(datal)[1], size = dim(datal)[1]/2, replace = TRUE), ]
modelSVM <- svm(fol, data = datal.train)
confusionMatrix(predict(modelSVM, datal.test, type = "class"), datal.test$pop)