data <- read.csv("C:/Users/Maya/Desktop/New folder/data.csv")

#gives names to columns
colnames(data) <- c(
  "ID", "Diagnosis",
  "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean",
  "compactness_mean", "concavity_mean", "concave_points_mean", "symmetry_mean", "fractal_dimension_mean",
  "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se",
  "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se",
  "radius_worst", "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst",
  "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst"
)
#to display the first few rows
head(data)

#shows internal structure of the dataset
str(data)

#for statistical properties
summary(data)

#missing values in data set
sum(is.na(data))

#Describe the distribution of the diagnosis variable (how many benign vs. malignant)
table(data$Diagnosis)

#range of values for at least three numeric features
range(data$radius_mean)
range(data$texture_mean)
range(data$area_mean)

# check for distributions or outliers
boxplot(data[,3:32]) #as first 2 are id and diagnose

#Bar plot
barplot(table(data$Diagnosis),
        main="Distribution of Benign and Malignant Diagnosis Cases",
        xlab="Diagnosis Type",
        ylab="Number of Patients",
        col=c("green","purple"),
        names.arg=c("Benign","Malignant"))

#scatter
plot(data$radius_mean, data$area_mean,
     col=as.factor(data$Diagnosis),
     pch=19,
     main="Radius vs Area",
     xlab="Radius Mean",
     ylab="Area Mean")

legend("topright",
       legend=c("Benign","Malignant"),
       col=c("red","black"),
       pch=19)

#train and split
set.seed(123)

train_index <- sample(nrow(data), 0.7*nrow(data))

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

cat("Training set:", nrow(train_data), "instances\n")
cat("Testing set:", nrow(test_data), "instances\n")

#kknn
library(kknn)

model <- kknn(Diagnosis ~ ., train_data, test_data, k=5, scale=TRUE)

summary(model)

# Make predictions using the trained model
predictions <- fitted(model)

# Show first 10 predictions
head(predictions, 10)

#conf_matrix <- table(
Actual = test_data$Diagnosis
Predicted = predictions

conf_matrix


# confusion matrix 
TN <- conf_matrix["Benign","Benign"]
TP <- conf_matrix["Malignant","Malignant"]
FP <- conf_matrix["Benign","Malignant"]
FN <- conf_matrix["Malignant","Benign"]

# metrics
accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display results
cat("Accuracy", round(accuracy,4))
cat("Precision", round(precision,4))
cat("Recall", round(recall,4))
cat("F1 Score", round(f1_score,4))




