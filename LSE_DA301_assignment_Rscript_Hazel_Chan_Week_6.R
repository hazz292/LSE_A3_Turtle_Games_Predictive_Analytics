###################################################################################################################################################### 

# Assignment 3 Week 6: Making recommendations to the business

# Objective
  # Apply multi-linear regression to determine the optimal global sales for all the video games based on:
        # Sales of video games in North America and Europe.

######################################################################################################################################################
# Install packages and library
######################################################################################################################################################

install.packages("tidyverse") 
library(tidyverse)

######################################################################################################################################################
# Import games_sales dataset 
######################################################################################################################################################

# Determine the working directory.
getwd()  

# Set working directory
setwd(dir = "/Users/hazel/Documents/LSE Data Analytics/Course 3 - Advanced Analytics/Assignment 3/game_data")

game <- read.csv("games_sales.csv", header = TRUE)

# View head of dataframe
head(game)

# View dataframe
View(game)

# View descriptive summary
summary(game)

# View information of dataframe
as_tibble(game) 

# Check for missing values
sum(is.na(game))

unique(game$Name)

######################################################################################################################################################
# Create subset of games_sales dataset 
    # Variables include NA, EU and Global sales
######################################################################################################################################################

# Create subset
sales <- subset(game, select = c(NA_Sales, EU_Sales, Global_Sales))

# View subset information
as_tibble(sales)
######################################################################################################################################################
######################################################################################################################################################

# Multiple Linear Regression Model
  # Predict global sales for all video games based on NA and EU sales.

######################################################################################################################################################
# Build model
######################################################################################################################################################

# R syntax for MLR:
  # myModel <- lm(y ~ x1 + x2 + x3, data=mydata)
  # y dependent variable = Global Sales
  # x independent variable = EU_Sales, Global_Sales

# View correlation between EU/NA sales and Global sales
cor(sales)
  # Both EU and NA sales are highly correlated with global sales 
  # with 0.9 correlation coefficient, very close to 1.

# Create a new regression model with lm function
model1 = lm(Global_Sales ~ NA_Sales + EU_Sales, data=sales)

# Print the summary statistics.
summary(model1) 
  # In this model, the Multiple R-squared is very strong at 0.96, very close to 1.
  # This means that North America and Europe sales explains 96% of the variability of the Global Sales variable.
  # The three stars next to the variables indicates that they have high significance in the model.

######################################################################################################################################################
# Predict Total Global Sales for all products in next financial year
  # Report: The predicted global sales for all products is 8341 millions of units which is relatively close to
            # the actual global sales for all products at 8920 millions of units as indicated in the final dataframe.
######################################################################################################################################################

# Create new dataframe with sum of all EU, NA and Global sales
total <- sales %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

# Print Total Sales dataframe
total

# Make prediction based on new dataframe
predictTotal = round(predict(model1, newdata = total, interval = 'confidence'),2)

# Print prediction of global sales for next year
predictTotal

# Print the object.
View(predictTotal)

# Convert predicted values to dataframe
total_df <- data.frame(predictTotal)

# Create final dataframe with actual and predicted global sales
final_total <- cbind(total[("Global_Sales")], total_df)

# Rename column name
final_total <- final_total %>% rename(Predicted_Global_Sales = fit,
                                      Actual_Global_Sales = Global_Sales)

# Print final dataframe
View(final_total)


######################################################################################################################################################
# Predict Global Sales for each product genre
  # Report: The predicted global sales for each genre products is relatively close to the actual global sales.
            # For example in the final dataframe, the Action genre predicted global sales is 1719 millions of units and the actual value is 1751 millions of units.
######################################################################################################################################################
# Create new dataframe aggregate by genre
genre_sales <- game %>% 
  group_by(Genre) %>% 
  summarise(Global_Sales = sum(Global_Sales), 
            EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales))

# View information
as_tibble(genre_sales)

# Make prediction with model
predictGenre_Sales = round(predict(model1, newdata = genre_sales, interval = 'confidence'),2)

# Print predicted values
predictGenre_Sales

# View class
class(predictGenre_Sales)

# Convert predicted values to dataframe
genre_df <- data.frame(predictGenre_Sales)

# Create final dataframe with genre name and predicted global sales
final_genre <- cbind(genre_sales[c("Genre", "Global_Sales")], genre_df)

# Rename column name
final_genre <- final_genre %>% rename(Predicted_Global_Sales = fit,
                                      Actual_Global_Sales = Global_Sales)

# Print dataframe
final_genre

######################################################################################################################################################
# Predict Global Sales for each product name
    # Report: The predicted global sales for each name product is relatively close to the actual global sales.
             # For example in the final dataframe, the product " Wii Sports" predicted global sales is 86.96 millions of units and the actual value is 82.74 millions of units.
######################################################################################################################################################
# Make prediction for all products (Name)
predictProduct = round(predict(model1, newdata = sales, interval = 'confidence'),2)

# Print predicted values
View(predictProduct)

as_tibble(predictProduct)

# Convert predicted values to dataframe
product_df <- data.frame(predictProduct)

# Create final dataframe with genre name and predicted global sales
final_prod <- cbind(game[c("Name", "Global_Sales")], product_df)

# Rename column name
final_prod <- final_prod %>% rename(Predicted_Global_Sales = fit,
                                      Actual_Global_Sales = Global_Sales)

# Print dataframe
head(final_prod)
