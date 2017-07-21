iris_training <- read.csv("iris_training.csv")
iris_training_x <- iris_training[, 1:4]
iris_training_y <- iris_training[, 5]

iris_test <- read.csv("iris_test.csv")
iris_test_x <- iris_test[, 1:4]
iris_test_y <- iris_test[, 5]

suppressWarnings(suppressMessages(library(kknn)))
model <- train.kknn(Species ~ ., data = iris_training, kmax = 9)

prediction <- predict(model, iris_test[, -5])

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

accuracy

rounded_predictions
iris_test_y

png("knn-plot-prediction.png")
plot(rounded_predictions)
dev.off()

png("knn-plot-actual.png")
plot(iris_test_y)
dev.off()