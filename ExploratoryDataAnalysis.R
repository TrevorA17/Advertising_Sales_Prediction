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

# Frequency for each budget type and sales (bins for better insight)
hist(AdData$TV_Ad_Budget, main = "Frequency of TV Ad Budget", xlab = "TV Ad Budget", col = "lightblue")
hist(AdData$Radio_Ad_Budget, main = "Frequency of Radio Ad Budget", xlab = "Radio Ad Budget", col = "lightgreen")
hist(AdData$Newspaper_Ad_Budget, main = "Frequency of Newspaper Ad Budget", xlab = "Newspaper Ad Budget", col = "lightcoral")
hist(AdData$Sales, main = "Frequency of Sales", xlab = "Sales", col = "lightyellow")

# Mean
mean_tv <- mean(AdData$TV_Ad_Budget)
mean_radio <- mean(AdData$Radio_Ad_Budget)
mean_newspaper <- mean(AdData$Newspaper_Ad_Budget)
mean_sales <- mean(AdData$Sales)

# Median
median_tv <- median(AdData$TV_Ad_Budget)
median_radio <- median(AdData$Radio_Ad_Budget)
median_newspaper <- median(AdData$Newspaper_Ad_Budget)
median_sales <- median(AdData$Sales)

# Mode function (R does not have a built-in mode function)
get_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

mode_tv <- get_mode(AdData$TV_Ad_Budget)
mode_radio <- get_mode(AdData$Radio_Ad_Budget)
mode_newspaper <- get_mode(AdData$Newspaper_Ad_Budget)
mode_sales <- get_mode(AdData$Sales)

# Display central tendency measures
list(
  Mean = c(TV = mean_tv, Radio = mean_radio, Newspaper = mean_newspaper, Sales = mean_sales),
  Median = c(TV = median_tv, Radio = median_radio, Newspaper = median_newspaper, Sales = median_sales),
  Mode = c(TV = mode_tv, Radio = mode_radio, Newspaper = mode_newspaper, Sales = mode_sales)
)

# Standard deviation and variance
sd_tv <- sd(AdData$TV_Ad_Budget)
var_tv <- var(AdData$TV_Ad_Budget)

sd_radio <- sd(AdData$Radio_Ad_Budget)
var_radio <- var(AdData$Radio_Ad_Budget)

sd_newspaper <- sd(AdData$Newspaper_Ad_Budget)
var_newspaper <- var(AdData$Newspaper_Ad_Budget)

sd_sales <- sd(AdData$Sales)
var_sales <- var(AdData$Sales)

# Range and Interquartile Range (IQR)
range_tv <- range(AdData$TV_Ad_Budget)
iqr_tv <- IQR(AdData$TV_Ad_Budget)

range_radio <- range(AdData$Radio_Ad_Budget)
iqr_radio <- IQR(AdData$Radio_Ad_Budget)

range_newspaper <- range(AdData$Newspaper_Ad_Budget)
iqr_newspaper <- IQR(AdData$Newspaper_Ad_Budget)

range_sales <- range(AdData$Sales)
iqr_sales <- IQR(AdData$Sales)

# Display distribution measures
list(
  Standard_Deviation = c(TV = sd_tv, Radio = sd_radio, Newspaper = sd_newspaper, Sales = sd_sales),
  Variance = c(TV = var_tv, Radio = var_radio, Newspaper = var_newspaper, Sales = var_sales),
  Range = list(TV = range_tv, Radio = range_radio, Newspaper = range_newspaper, Sales = range_sales),
  IQR = c(TV = iqr_tv, Radio = iqr_radio, Newspaper = iqr_newspaper, Sales = iqr_sales)
)

# Correlation matrix
cor_matrix <- cor(AdData[, c("TV_Ad_Budget", "Radio_Ad_Budget", "Newspaper_Ad_Budget", "Sales")])

# Scatter plot matrix
pairs(AdData[, c("TV_Ad_Budget", "Radio_Ad_Budget", "Newspaper_Ad_Budget", "Sales")], main = "Scatterplot Matrix")

# Display correlation matrix
cor_matrix

# Load necessary library
library(ggplot2)

# Histogram for each variable
ggplot(AdData, aes(x = TV_Ad_Budget)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "black") +
  labs(title = "Distribution of TV Ad Budget", x = "TV Ad Budget", y = "Frequency")

ggplot(AdData, aes(x = Radio_Ad_Budget)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Radio Ad Budget", x = "Radio Ad Budget", y = "Frequency")

ggplot(AdData, aes(x = Newspaper_Ad_Budget)) +
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Newspaper Ad Budget", x = "Newspaper Ad Budget", y = "Frequency")

ggplot(AdData, aes(x = Sales)) +
  geom_histogram(binwidth = 5, fill = "lightyellow", color = "black") +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency")

# Boxplot for each variable
ggplot(AdData, aes(y = TV_Ad_Budget)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(title = "Boxplot of TV Ad Budget", y = "TV Ad Budget")

ggplot(AdData, aes(y = Radio_Ad_Budget)) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(title = "Boxplot of Radio Ad Budget", y = "Radio Ad Budget")

ggplot(AdData, aes(y = Newspaper_Ad_Budget)) + 
  geom_boxplot(fill = "lightcoral") + 
  labs(title = "Boxplot of Newspaper Ad Budget", y = "Newspaper Ad Budget")

ggplot(AdData, aes(y = Sales)) + 
  geom_boxplot(fill = "lightyellow") + 
  labs(title = "Boxplot of Sales", y = "Sales")

# Density plot for each variable
ggplot(AdData, aes(x = TV_Ad_Budget)) + 
  geom_density(fill = "lightblue") + 
  labs(title = "Density Plot of TV Ad Budget", x = "TV Ad Budget")

ggplot(AdData, aes(x = Radio_Ad_Budget)) + 
  geom_density(fill = "lightgreen") + 
  labs(title = "Density Plot of Radio Ad Budget", x = "Radio Ad Budget")

ggplot(AdData, aes(x = Newspaper_Ad_Budget)) + 
  geom_density(fill = "lightcoral") + 
  labs(title = "Density Plot of Newspaper Ad Budget", x = "Newspaper Ad Budget")

ggplot(AdData, aes(x = Sales)) + 
  geom_density(fill = "lightyellow") + 
  labs(title = "Density Plot of Sales", x = "Sales")

# Scatter plots with regression line
ggplot(AdData, aes(x = TV_Ad_Budget, y = Sales)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "TV Ad Budget vs Sales", x = "TV Ad Budget", y = "Sales")

ggplot(AdData, aes(x = Radio_Ad_Budget, y = Sales)) + 
  geom_point(color = "green") + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Radio Ad Budget vs Sales", x = "Radio Ad Budget", y = "Sales")

ggplot(AdData, aes(x = Newspaper_Ad_Budget, y = Sales)) + 
  geom_point(color = "coral") + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Newspaper Ad Budget vs Sales", x = "Newspaper Ad Budget", y = "Sales")

# Pairwise scatter plot matrix
pairs(AdData[, c("TV_Ad_Budget", "Radio_Ad_Budget", "Newspaper_Ad_Budget", "Sales")], main = "Pairwise Scatter Plot Matrix")

# Correlation heatmap
library(reshape2)
cor_data <- cor(AdData[, c("TV_Ad_Budget", "Radio_Ad_Budget", "Newspaper_Ad_Budget", "Sales")])
melted_cor_data <- melt(cor_data)

ggplot(data = melted_cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)

