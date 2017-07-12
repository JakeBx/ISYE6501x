# initialize the data
cancer <- read.table('breast-cancer-wisconsin.data.txt', sep=',',header = FALSE)
head(cancer,10)
sapply(cancer,class)
summary(cancer)

cancer.copy <- cancer
cancer.copy$V7[cancer.copy$V7==('?')] <- NA
cancer.copy$V7 <- as.numeric(cancer.copy$V7)
summary(cancer.copy)

# Pattern of missing values
library('mice')
library(Hmisc)

# how much data is missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(cancer.copy,2,pMiss) # 2.3% of V7 missing, less than 5%

# impute mean
cancer.mean <- cancer.copy
cancer.mean$V7 <- with(cancer.mean, impute(cancer.mean$V7, mean))
summary(cancer.mean)

# define mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# impute mode
cancer.mode <- cancer.copy
cancer.mode$V7 <- with(cancer.mode, impute(cancer.mode$V7, fun=getmode(cancer.mode$V7)))
summary(cancer.mode)

# imputation via regression
cancer.regr <- cancer.copy
imp <- mice(cancer.regr, method = "norm.predict", m = 1)
cancer.regr <- complete(imp)

# add perturbation to regr
cancer.per <- cancer.regr
idx <- which(cancer.copy$V7 %in% NA)
rando <- runif(length(idx),-1,1)
for (i in 1:length(idx)){
  cancer.per[idx[i],7]<-cancer.per[idx[i],7]+rando[i]
}

# SVM Classifier
library("kernlab")
results = {}
#mean dataset
cancer.mean <- as.matrix(cancer.mean)
model.mean <- ksvm(cancer.mean[,1:10],cancer.mean[,11],type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred <- predict(model.mean,cancer.mean[,1:10])
acc.mean <- sum(pred == cancer.mean[,11]) / nrow(cancer.mean)
results['MEAN'] <- acc.mean

# mode dataset
cancer.mode <- as.matrix(cancer.mode)
model.mode <- ksvm(cancer.mode[,1:10],cancer.mode[,11],type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred <- predict(model.mode,cancer.mode[,1:10])
acc.mode <- sum(pred == cancer.mode[,11]) / nrow(cancer.mode)
results['MODE'] <- acc.mode

#regression dataset
cancer.regr <- as.matrix(cancer.regr)
model.regr <- ksvm(cancer.regr[,1:10],cancer.regr[,11],type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred <- predict(model.regr,cancer.regr[,1:10])
acc.regr <- sum(pred == cancer.regr[,11]) / nrow(cancer.regr)
results['REGRESSION'] <- acc.regr

# use perturbation dataset
cancer.per <- as.matrix(cancer.per)
model.per <- ksvm(cancer.per[,1:10],cancer.per[,11],type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred <- predict(model.per,cancer.per[,1:10])
acc.per <- sum(pred == cancer.per[,11]) / nrow(cancer.per)
results['PERTURBATION'] <- acc.per

# omit NAs
cancer.orig <- as.matrix(cancer.copy)
model.orig <- ksvm(cancer.orig[,1:10],cancer.orig[,11], na.action = na.omit,type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred.orig <- predict(model.orig,cancer.orig[-idx,1:10])
acc.orig <- sum(pred.orig == cancer.orig[-idx,11]) / nrow(cancer.orig-length(idx))
results['REMOVED'] <- acc.orig

# introduce binary value
cancer.bin <- cancer.copy
cancer.bin$m <- 0
for (i in 1:length(idx)){
  cancer.bin[idx[i],12]<-1
  cancer.bin[idx[i],7]<-0
}
cancer.bin <- as.matrix(cancer.bin)
model.bin <- ksvm(cancer.bin[,-11],cancer.bin[,11], na.action=na.fail, type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
pred <- predict(model.bin,cancer.bin[,-11])
acc.bin <- sum(pred == cancer.bin[,11]) / nrow(cancer.bin)
results['BINARY'] <- acc.bin

results
