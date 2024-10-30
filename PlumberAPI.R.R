# Plumber API
# Load the saved Random Forest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

#* @apiTitle Advertising Sales Prediction Model API

#* @apiDescription Used to predict sales based on advertising budgets.

#* @param TV_Ad_Budget TV Ad Budget in dollars
#* @param Radio_Ad_Budget Radio Ad Budget in dollars
#* @param Newspaper_Ad_Budget Newspaper Ad Budget in dollars

#* @get /predict

predict_sales <- function(TV_Ad_Budget, Radio_Ad_Budget, Newspaper_Ad_Budget) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    TV_Ad_Budget = as.numeric(TV_Ad_Budget),
    Radio_Ad_Budget = as.numeric(Radio_Ad_Budget),
    Newspaper_Ad_Budget = as.numeric(Newspaper_Ad_Budget)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}
