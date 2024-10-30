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

# Load necessary libraries
library(caret)
library(glmnet)      # For Lasso Regression
library(randomForest) # For Random Forest Regression

# Set seed for reproducibility
set.seed(123)

# Create the training control
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# 1. Linear Regression
linear_model <- train(Sales ~ TV_Ad_Budget + Radio_Ad_Budget + Newspaper_Ad_Budget, 
                      data = AdData, 
                      method = "lm", 
                      trControl = train_control)


# 2. Random Forest Regression
rf_model <- train(Sales ~ TV_Ad_Budget + Radio_Ad_Budget + Newspaper_Ad_Budget, 
                  data = AdData, 
                  method = "rf", 
                  trControl = train_control)


# 3. Lasso Regression
lasso_model <- train(Sales ~ TV_Ad_Budget + Radio_Ad_Budget + Newspaper_Ad_Budget,
                     data = AdData,
                     method = "glmnet",
                     trControl = train_control,
                     tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.1, by = 0.01)))

# Display results for all models
print(linear_model)
print(lasso_model)
print(rf_model)

# Combine the results from the models into a resamples object
model_list <- resamples(list(Linear = linear_model, 
                             Lasso = lasso_model, 
                             RandomForest = rf_model))

# Summary of the resamples
summary(model_list)

# Visualization of the performance comparison
bwplot(model_list)  # Boxplot to compare RMSE and other metrics
