dw = samples_df %>%
  filter(Gender == "w") %>%
  select(-HB, -PB, -PH, -SR) %>%
  na.omit()

# set.seed(123) # Set seed for reproducibility
# trainIndex <- createDataPartition(d$VT, p = 0.7, 
#                                   list = FALSE, 
#                                   times = 1)
# data_train <- d[trainIndex, ]
# data_test <- d[-trainIndex, ]

# Create the linear regression models for all apparatuses for women
wVT <- lm(VT ~ BB + UB + FX, data = dw)
wBB <- lm(BB ~ VT + UB + FX, data = dw)
wUB <- lm(UB ~ BB + VT + FX, data = dw)
wFX <- lm(FX ~ BB + UB + VT, data = dw)


# Make predictions on the test set
# predictions <- predict(model, newdata = data_test)

# Evaluate the model (optional)
# You can use various metrics to evaluate the model's performance
# For example, you can use Mean Absolute Error (MAE) or Root Mean Squared Error (RMSE)
# mae <- mae(data_test$VT, predictions)
# rmse <- rmse(data_test$VT, predictions)
# 
# # Print the evaluation metrics
# cat("Mean Absolute Error (MAE):", mae, "\n")
# cat("Root Mean Squared Error (RMSE):", rmse, "\n")
# 
# # You can also plot the actual vs. predicted values to visualize the model's performance
# plot(data_test$VT, predictions, main = "Actual vs. Predicted VT Values")
# abline(0, 1, col = "red")


dm = samples_df %>%
  filter(Gender == "m") %>%
  select(-BB, -UB) %>%
  na.omit()

# Create the linear regression models for all apparatuses for men
mVT <- lm(VT ~ FX + HB + PB + PH + SR, data = dm)
mFX <- lm(FX ~ VT + HB + PB + PH + SR, data = dm)
mHB <- lm(HB ~ VT + PB + PH + SR + FX, data = dm)
mPB <- lm(PB ~ VT + HB + PH + SR + FX, data = dm)
mPH <- lm(PH ~ VT + PB + HB + SR + FX, data = dm)
mSR <- lm(SR ~ VT + PB + PH + HB + FX, data = dm)
