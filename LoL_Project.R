league <- read.csv('Challenger_Ranked_Games.csv')

league_subset = league[, c('gameId', 'blueFirstBaron', 'blueTotalGold', 'blueChampionDamageDealt')]

### RESPONSE VARIABLE 'blueChampionDamageDealt':

# Create a histogram to visualize the marginal distribution of response variable
ggplot(league_subset, aes(x = blueChampionDamageDealt)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Blue Team Champion Damage Dealt Distribution",
       x = "Champion Damage Dealt",
       y = "Frequency")

# Calculate univariate statistics for response variable
summary(league_subset$blueChampionDamageDealt)

# Apply a square root transformation to response variable
league_subset$sqrt_blueChampionDamageDealt <- sqrt(league_subset$blueChampionDamageDealt)

# Create a histogram of the transformed data of response variable
ggplot(league_subset, aes(x = sqrt_blueChampionDamageDealt)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Sqrt-Transformed Blue Team Champion Damage Dealt Distribution",
       x = "Square Root(Champion Damage Dealt)",
       y = "Frequency")


### NUMERIC EXPLANATORY VARIABLE 'blueTotalGold':

# Create a histogram to visualize the marginal distribution of 'blueTotalGold'
ggplot(league_subset, aes(x = blueTotalGold)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Blue Team Total Gold Distribution",
       x = "Total Gold",
       y = "Frequency")

# Calculate univariate statistics for 'blueTotalGold'
summary(league_subset$blueTotalGold)

# Create a scatter plot to visualize the relationship between 'blueTotalGold' and 'sqrt_blueChampionDamageDealt'
ggplot(league_subset, aes(x = blueTotalGold, y = sqrt_blueChampionDamageDealt)) +
  geom_point(color = "blue") +
  labs(title = "Bivariate Relationship between Total Gold and Champion Damage Dealt",
       x = "Total Gold",
       y = "Champion Damage Dealt")


### CATEGORICAL EXPLANATORY VARIABLE 'blueFirstBaron':

# Get counts for each category
category_counts <- table(league_subset$blueFirstBaron)
print(category_counts)

# Create a bar plot to visualize the marginal distribution of 'blueFirstBaron'
ggplot(league_subset, aes(x = blueFirstBaron)) +
  geom_bar(fill = "blue") +
  labs(title = "Blue Team First Baron Distribution",
       x = "First Baron",
       y = "Count")

# Create a box plot to visualize the relationship between 'blueFirstBaron' and 'sqrt_blueChampionDamageDealt'
ggplot(league_subset, aes(x = factor(blueFirstBaron), y = sqrt_blueChampionDamageDealt)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Relationship between First Baron and Champion Damage Dealt",
       x = "First Baron",
       y = "Champion Damage Dealt")


### Perform GLM using lm()
# Fit the Linear Model
lm_model <- lm(sqrt_blueChampionDamageDealt ~ blueTotalGold + blueFirstBaron, data = league_subset)

# Confirm linearity of numeric predictors
plot(league_subset$blueTotalGold, league_subset$sqrt_blueChampionDamageDealt, xlab = "Total Gold",
     ylab = "Square Root(Champion Damage Dealt)", main = "Total Gold and Champion Damage Dealt", pch = 20, cex = 0.25)

# Confirm normality of residuals
hist(lm_model$residuals, main = "Model Residuals", xlab = "Residuals",
     col = "light grey", right = FALSE)

# Confirm equal variance
plot(lm_model$fitted.values, lm_model$residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Residual Plot", pch = 20, cex = 0.25)
abline(h = 0, col = "red")

# View results of Linear Model
summary(lm_model)

# Adjusted R-squared
summary(lm_model)$adj.r.squared

# Add predicted values (yhat) into our data frame
league_subset$yhat <- lm_model$fitted.values

# Make multivariate plot
library(ggplot2)
ggplot(league_subset, aes(x = blueTotalGold, y = sqrt_blueChampionDamageDealt, color = factor(blueFirstBaron))) +
  geom_point(size = 0.1, alpha = 0.25) +
  xlab("Total Gold") +
  ylab("Square Root(Champion Damage Dealt)") +
  labs(color = "First Baron (0 = No, 1 = Yes)") +
  ggtitle("Champion Damage Dealt by Total Gold, First Baron") +
  theme_classic() +
  scale_color_manual(values = c("blue", "orange")) +
  geom_line(aes(y = yhat), size = 1)


# Confirm equal variance with a scale-location plot
scale_location_plot <- function(model) {
  # Create a scale-location plot
  scale_location_data <- data.frame(
    Fitted_Values = model$fitted.values,
    SQRT_Residuals = sqrt(abs(model$residuals))
  )
  
  # Plot the scale-location plot
  plot(SQRT_Residuals ~ Fitted_Values, data = scale_location_data,
       xlab = "Fitted Values", ylab = "Square Root of Residuals",
       main = "Scale-Location Plot", pch = 20, cex = 0.75)
  
  # Add a smooth line to check for trends
  smooth_line <- loess(SQRT_Residuals ~ Fitted_Values, data = scale_location_data)
  lines(scale_location_data$Fitted_Values, predict(smooth_line), col = "blue", lwd = 2)
  
  # Add a horizontal line at y = 0
  abline(h = 0, col = "red")
}

# Call the function with your lm_model
scale_location_plot(lm_model)





### Perform GLM

# Fit the GLM

# Add a small constant to the response variable to account for exact-zeros
league_subset$sqrt_blueChampionDamageDealt <- league_subset$sqrt_blueChampionDamageDealt + 0.001

# Ensure the distribution of the response variable is still normal
hist(league_subset$sqrt_blueChampionDamageDealt, breaks = 20, col = "blue", main = "Distribution of Response Variable")

# Apply GLM Model
glm_model <- glm(sqrt_blueChampionDamageDealt ~ blueTotalGold + blueFirstBaron, 
                 data = league_subset, 
                 family = Gamma(link = "log"))

# Check model summary
summary(glm_model)

# Confirm linearity of numeric predictors
plot(league_subset$blueTotalGold, glm_model$residuals, xlab = "Total Gold",
     ylab = "Residuals", main = "Residual Plot - Total Gold", pch = 20)
abline(h = 0, col = "red")

# Confirm normality of residuals
hist(glm_model$residuals, main = "Model Residuals", xlab = "Residuals",
     col = "light grey", border = "black", right = FALSE)

# Confirm equal variance
plot(glm_model$fitted.values, glm_model$residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Residual Plot - Fitted Values", pch = 20)
abline(h = 0, col = "red")

# Plot GLM
plot(league_subset$blueTotalGold, league_subset$sqrt_blueChampionDamageDealt, 
     xlab = "Total Gold", ylab = "Square Root(Champion Damage Dealt)",
     main = "GLM Fit", col = "blue", pch = 20)
lines(league_subset$blueTotalGold, exp(predict(glm_model)), col = "red", lw = 2)

