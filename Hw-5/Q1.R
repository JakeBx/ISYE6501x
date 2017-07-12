# Stepwise Regression
library(MASS)
library(caret)
crime <- read.table('../Hw-3/uscrime.txt', header = TRUE)
sapply(crime, class)
crime$So <- as.factor(crime$So)
crime$Crime <- as.numeric(crime$Crime)

preprocessParams <- preProcess(crime[,1:15], method=c("scale","center"))
crime.lm <- crime
crime.lm[,1:15] <- predict(preprocessParams,crime.lm[,1:15])

## Stepwise Regression Model
crime.full <- lm(Crime~., data = crime.lm)
crime.null <- lm(Crime~1, data = crime.lm)

# Forward Selection
step.forw <- stepAIC(crime.null, direction = "forward",trace = FALSE,
                     scope = ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob +Time)
summary(step.forw)

# Backward Selection
step.back <- stepAIC(crime.mod, direction = "backward", trace = FALSE)
summary(step.back)

# Forward and Bakward
step.both <- stepAIC(crime.null, direction = "both",trace = FALSE,
                     scope = ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob +Time)
summary(step.both)

# RIDGE, LASSO, ELASTICNET
library(caret)
library(glmnet)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# GLMNET
set.seed(7)
grid <- expand.grid(.alpha = seq(.0, 1, length = 15),.lambda = c((1:5)/10,(1:10),(2:10)*10))
fit.glmnetlong <- train(Crime~., data=crime, method="glmnet", family="gaussian", tuneGrid=grid, metric=metric, preProc=c("center", "scale"), trControl=control)
plot(glmnetlong)

grid <- expand.grid(.alpha = seq(.0, 1, length = 11),.lambda = seq(0, 30, length = 30))
fit.glmnet <- train(Crime~., data=crime, method="glmnet", family="gaussian", tuneGrid=grid, metric=metric, preProc=c("center", "scale"), trControl=control)
fit.glmnetbc <- train(Crime~., data=crime, method="glmnet", family="gaussian", tuneGrid=grid, metric=metric, preProc=c("center", "scale", "BoxCox"), trControl=control)

plot(fit.glmnet)
plot(fit.glmnetbc)

results <- resamples(list(GLMNET=fit.glmnet, GLMNET.BOXCOX=fit.glmnetbc))
summary(results)
dotplot(results)
# fit.glmnet
# fit.glmnetbc
