# loading training data

iris_training <- read.csv("iris_training.csv")
iris_training_x <- iris_training[, 1:4]
iris_training_y <- iris_training[, 5]

# loading testing data

iris_test <- read.csv("iris_test.csv")
iris_test_x <- iris_test[, 1:4]
iris_test_y <- iris_test[, 5]

# hiding warnings

suppressWarnings(suppressMessages(library(kknn)))

# creating k nearest neighbors classifier

model <- train.kknn(Species ~ ., data = iris_training, kmax = 9)

# predicting testing data y values with k nearest neighbors classifier

prediction <- predict(model, iris_test[, -5])

# rounding predictions for integer classifications
# and calculating accuracy of classifications

rounded_predictions <- c()

accuracy <- 0

i <- 1

for (p in prediction){
	rounded_predictions <- c(rounded_predictions, round(p))
	if (round(p) == iris_test_y[[i]]){
		accuracy <- accuracy + (1 / length(prediction))
	}
	i <- i + 1
}

# showing accuracy

accuracy

# showing classifications made by k nearest neighbors classifier

rounded_predictions

# showing actual classifications of testing set

iris_test_y

# generating plot to visualize k nearest neighbors classifier

png("knn-plot-prediction.png")
plot(rounded_predictions)
dev.off()

# generating plot to visualize actual classifications of testing set

png("knn-plot-actual.png")
plot(iris_test_y)
dev.off()