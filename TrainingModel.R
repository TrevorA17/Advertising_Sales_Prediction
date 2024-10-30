# Load dataset
AdData <- read.csv("data/Advertising_Budget_and_Sales.csv", colClasses = c(
  TV_Ad_Budget = "numeric",
  Radio_Ad_Budget = "numeric",
  Newspaper_Ad_Budget = "numeric",
  Sales = "numeric"
))

# Display structure to verify data types
str(AdData)

# Display first few rows to ensure data is loaded correctly
head(AdData)

View(AdData)

# Set seed for reproducibility
set.seed(123)

# Define the proportion of data to use for training
train_size <- 0.7  # 70% for training, 30% for testing

# Create a random sample of row indices for the training set
train_indices <- sample(1:nrow(AdData), size = train_size * nrow(AdData))

# Split the data into training and testing sets
train_data <- AdData[train_indices, ]
test_data <- AdData[-train_indices, ]

# Check the dimensions of the training and testing sets
dim(train_data)
dim(test_data)

# Set seed for reproducibility
set.seed(123)

# Number of bootstrap samples
n_bootstrap <- 1000

# Initialize a vector to store bootstrap sample means
bootstrap_means <- numeric(n_bootstrap)

# Perform bootstrapping
for (i in 1:n_bootstrap) {
  # Sample with replacement
  sample_data <- AdData[sample(1:nrow(AdData), replace = TRUE), ]
  
  # Calculate the mean of Sales for the bootstrap sample
  bootstrap_means[i] <- mean(sample_data$Sales)
}

# Display the results
mean(bootstrap_means)  # Average of the bootstrap sample means
sd(bootstrap_means)    # Standard deviation of the bootstrap sample means

# Plot the bootstrap distribution
hist(bootstrap_means, main = "Bootstrap Distribution of Sales Mean",
     xlab = "Mean Sales", col = "lightblue", border = "black")

# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
k <- 5

# Create the training control
train_control <- trainControl(method = "cv", number = k)

# Fit a linear model (you can change this to any model of your choice)
model <- train(Sales ~ TV_Ad_Budget + Radio_Ad_Budget + Newspaper_Ad_Budget, 
               data = AdData, 
               method = "lm", 
               trControl = train_control)

# Display the results
print(model)

