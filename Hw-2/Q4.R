temps <- read.table('temps.txt', header = TRUE)
head(temps,5)
rows <- rowMeans(temps[2:21], na.rm=TRUE, dims = 1)
cols <- colMeans(temps[2:21], na.rm = TRUE, dims = 1)

library('qcc')
change_days <- cusum(rows)
change_days$violations$lower
temps$DAY[81]
summary(change_days)

change_years <- cusum(cols)
