# load data
crime <- read.table('uscrime.txt', header=TRUE)
head(crime,5)

#bocplot show 2 datapoints someway higher
boxplot(crime$Crime)
pairs(crime)

# check normal distribution
hist(crime$Crime)
hist(log(crime$Crime))
x <- log(crime$Crime)
boxplot(x)
shapiro.test(crime$Crime)
shapiro.test(log(crime$Crime))
qqnorm(crime$Crime)
qqline(crime$Crime)
qqnorm(x)
qqline(x)

library(outliers)

grubbs.test(crime$Crime)
grubbs.test(x, opposite=TRUE)

grubbs.test(crime$Crime, opposite=TRUE)
grubbs.test(x)

grubbs.test(crime$Crime, type=11)

crime <- crime[!(crime$Crime==1993),]

scores_test <- scores(crime$Crime, prob=0.95)
crime$Scores <- scores_test
crime[(crime$Scores==TRUE),16:17]
crime$Crime[order(crime$Crime, decreasing = TRUE)[1:5]]

log_scores_test <- scores(x, prob=0.95)
log_scores_test[(log_scores_test==TRUE)]
crime$log_score <- log_scores_test
crime[(crime$log_score==TRUE),16:18]
