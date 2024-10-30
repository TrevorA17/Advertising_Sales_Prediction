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

# Check for missing values in each column
missing_values_summary <- sapply(AdData, function(x) sum(is.na(x)))
missing_values_summary

# Total number of missing values in the dataset
total_missing_values <- sum(is.na(AdData))
total_missing_values

# Load necessary library for visualization
library(VIM)

# Visualize missing values
aggr_plot <- aggr(AdData, col = c('navyblue', 'orange'), numbers = TRUE, sortVars = TRUE, 
                  labels = names(AdData), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))

