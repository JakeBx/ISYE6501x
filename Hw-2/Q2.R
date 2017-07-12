# load data
iris <- read.table('iris.txt', header=TRUE)
head(iris,5)

# There are 6 variables one is just an id and is useless, the species is the classification

# we can see some clear structure in this data
# I will begin by running a kmeans on all the data then drop the sepal values as there is more overlap here
pairs(Species~., data=iris[,2:6], col=iris$Species)

# attach lattice for the plotting
library('latticeExtra')
# set seed for repeatability
set.seed(42)

# this functions runs the kmeans on the data, and then outputs the classification accuracy
# I will call it as I drop elements from the set I am computing the means on
run <- function(data, iris){
  
  results <- kmeans(data, 3, iter.max = 10, nstart = 1, algorithm = "Hartigan-Wong")
  print(results)
  
  data$Species <- data.frame(unlist(results[1]))[,1]
  
  A <- xyplot(Petal.Width ~ Petal.Length, group = Species, data = data, auto.key = list(space = "right"),
              par.settings = list(superpose.symbol = list(pch = 0, cex = 1, col = c("red", "green", "black"))))
  B <- xyplot(Petal.Width ~ Petal.Length, group = Species, data = iris, auto.key = list(space = "rigth"),
              par.settings = list(superpose.symbol = list(pch = 1, cex = 2, col = c("red", "green", "black"))), xlab 
              = "Sepal Length", ylab = "Sepal Width", main = "KMeans clustering on Iris")
  C <- xyplot(results$centers[,c("Petal.Width")] ~ results$centers[,c("Petal.Length")],pch = "@", cex = 2,
              col = c("red", "black", "green"), auto.key = list(space = "right"))
  D <- B + as.layer(A + C)
  
  
  results <- data.frame(table(iris$Species, results$cluster))
  names(results) <- c("Real.Value", "Assigned.Cluster", "Frequency")
  accuracy <- (max(results$Frequency[1:3])+max(results$Frequency[4:6])+max(results$Frequency[7:9]))/150
  
  print(D)
  print(results)
  print(accuracy)
}

iris_trim <- iris[,2:5]

results <- run(iris_trim, iris)

iris_trim<-subset(iris_trim, select = -Sepal.Length)
results <- run(iris_trim, iris)

iris_trim<-subset(iris_trim, select = -Sepal.Width)
results <- run(iris_trim, iris)
