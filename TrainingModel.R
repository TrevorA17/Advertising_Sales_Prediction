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
