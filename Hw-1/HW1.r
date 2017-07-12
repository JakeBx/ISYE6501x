
library("kernlab")
myData=as.matrix(read.table("credit_card_data-headers.txt",header=TRUE))
head(myData,5)

# fit model
model <- ksvm(myData[,1:10],myData[,11],type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)

# calculate each and display the weights
a <- colSums(myData[model@SVindex,1:10]* model@coef[[1]])
a0 <- sum(a*myData[1,1:10]) - model@b

a
a0

pred <- predict(model,myData[,1:10])
pred

# display accuracy
sum(pred == myData[,11]) / nrow(myData)

#reload data
library("kknn")
myData <- read.table("credit_card_data-headers.txt",header=TRUE)
head(myData, 5)

for(i in 1:10) {
  myData[,i] <- as.numeric(as.character(Data[,i]))
}
myData$R1 <- as.factor(Data$R1)

# remove 50 entries for assesment of model
m <- dim(Data)[1]
Sample <- sample(1:m, 50)
testing <- myData[Sample, ]
learning <- myData[-Sample, ]

dim(learning)
dim(testing)

# assess models
model_train <- train.kknn(R1 ~ ., data=learning, scale=TRUE, kmax=9)
model_train

prediction <- predict(model_train, testing[, -11])
CM <- table(testing[, 11], prediction)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

# finalize model model
model <- kknn(R1 ~ ., train=learning, test=testing, scale=TRUE, k=7)
model

summary(model)
fit <- fitted(model)


CM <- table(testing$R1, fit)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

# I am going to use the caret libray for classfiers
# detach("package:kernlab", unload=TRUE)
# detach("package:kknn", unload=TRUE)
library(caret)

# Have a look at the data

# class distribution
cbind(freq=table(myData$R1), percentage=prop.table(table(myData$R1))*100)

# summarize correlations between input variables
complete_cases <- complete.cases(myData)
cor(myData[complete_cases,1:10])

# Load data into df
myData <- read.table("credit_card_data-headers.txt",header=TRUE)
# break-off validation set
set.seed(42)
validation_index <- createDataPartition(myData$R1, p=0.80, list=FALSE)
validation <- myData[-validation_index,]
temp <- myData[validation_index,]
test_index <- createDataPartition(temp$R1, p=0.75, list=FALSE)
testing <- temp[-test_index,]
training <- temp[test_index,]

# manually running c through ksvm, I found insensitivity with the vanilladot kernel and with rbf diminished
# as I moved away from 1.
x <- training[,1:10]
x <- as.matrix(x)
y <- as.numeric(training$R1)
x_test <- testing[,1:10]
x_test <- as.matrix(x_test)
y_test <- as.numeric(testing$R1)
c=seq(1,200, by=20)
for(i in 1:10) {
  model<-ksvm(x,y,type="C-svc",kernel="rbf",C=c[i],scaled=TRUE)
  pred_svm <- predict(model,x_test)
  CM <- table(y_test, pred_svm)
  accuracy <- (sum(diag(CM)))/sum(CM)
  print(c[i])
  print(CM)
  print(accuracy)
}

# Convert DF values to numeric
for(i in 1:10) {
  myData[,i] <- as.numeric(as.character(myData[,i]))
}
myData$R1 <- as.factor(myData$R1)

# manually running k through kknn
k=seq(1,20,by=1)
for(i in 1:20) {
  model_knn <- kknn(R1 ~ ., train=training, test=testing, scale=TRUE, k=k[i])
  fit <- fitted(model_knn)
  CM <- table(testing$R1, fit)
  print(k[i])
  print(CM)
  accuracy <- (sum(diag(CM)))/sum(CM)
  print(accuracy)
}

# Tune SVM
# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 200, by=20))
fit.svm <- train(R1~., data=training, method="svmRadial", metric=metric, tuneGrid=grid, preProc=c("scale"), trControl=control)
print(fit.svm)
plot(fit.svm)


# Tune kNN
# 10-fold cross validation with 3 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.k=seq(1,20,by=1))
fit.knn <- train(R1~., data=training, method="knn", metric=metric, tuneGrid=grid, preProc=c("scale"), trControl=control)
print(fit.knn)
plot(fit.knn)

# build final model svm
x <- training[,1:10]
x <- as.matrix(x)
y <- as.numeric(training$R1)
model_svm <- ksvm(x, y,type="C-svc",kernel="rbf",simga=0.025,C=1)

x_valid <- as.matrix(validation[,1:10])
y_valid <- as.numeric(validation$R1)
pred_svm <- predict(model,x_valid)
CM <- table(y_valid, pred_svm)
CM
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

# Acheived 83.8 accuracy on the validation set

# build final model knn
model_knn <- kknn(R1 ~ ., train=training, test=validation, scale=TRUE, k=19)
fit <- fitted(model_knn)
CM <- table(validation$R1, fit)
CM
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy
