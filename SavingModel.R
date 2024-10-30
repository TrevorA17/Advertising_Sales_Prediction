# Saving the model
saveRDS(rf_model, "./models/saved_rf_model.rds")

# Load the saved model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Create new data for predictions
new_data <- data.frame(
  TV_Ad_Budget = 100,  # Example value for TV Ad Budget
  Radio_Ad_Budget = 20,  # Example value for Radio Ad Budget
  Newspaper_Ad_Budget = 30  # Example value for Newspaper Ad Budget
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
