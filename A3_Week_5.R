###################################################################################################################################################### 

# Assignment 3 Week 5: Clean, manipulate, and visualise the data

    # Objective
        # Prepare the data and ensure that it is ready for analysis to predict global sales based on EU and NA sales by:
            # 1. ensuring there are no missing values
            # 2. modifying the data using string manipulation
            # 3. visualising the data to identify and understand possible trends in the data set.

######################################################################################################################################################

# Install tidyverse
install.packages("tidyverse") 
library(tidyverse)
tidyverse_conflicts()
tidyverse_update()

library('stringr')
install.packages("moments") 
library(moments)

######################################################################################################################################################
# Import games_sales dataset 
######################################################################################################################################################

# Determine the working directory.
getwd()  

# Set working directory
setwd(dir = "/Users/hazel/Documents/LSE Data Analytics/Course 3 - Advanced Analytics/Assignment 3/game_data")

sales <- read.csv("games_sales.csv", header = TRUE)

# View head of dataframe
head(sales)

# View dataframe
View(sales)

# View descriptive summary
summary(sales)

# View information of dataframe
as_tibble(sales) 

# Check for missing values
sum(is.na(sales))

# Convert Genre values to lowercase
  # Identify column names
  colnames(sales)

# Convert Genre values to lowercase
sales$Genre <- str_to_lower(sales$Genre)

head(sales$Genre)

# Convert Platform values to lowercase
sales$Platform <- str_to_lower(sales$Platform)

head(sales$Platform)

# Merge values for variables Genre and Platform
  # Number of unique values in Platform column
  length(unique(sales$Platform))

  # Number of unique values in Genre column
  length(unique(sales$Genre))

# Merge Genre and Platform values and create new column Genre_Platform
sales$Genre_Platform <- str_c(sales$Genre, sales$Platform, sep = " ")

# Check new column
head(sales)

######################################################################################################################################################
# Visualise distribution of data
  # Variables for analysis:
    # NA_Sales
    # EU_Sales
    # Global_Sales
    # Genre
    
######################################################################################################################################################
# Genre distribution 
######################################################################################################################################################
# Histogram
ggplot (sales, aes (x = Genre)) +
  geom_histogram (stat = "count", fill = "blue") +
  coord_flip() + 
  labs(title = "Distribution of Genre")

######################################################################################################################################################
# Platform distribution 
######################################################################################################################################################
# Histogram
ggplot (sales, aes (x = Platform)) +
  geom_histogram (stat = "count", fill = "blue") +
  coord_flip()  + 
  labs(title = "Distribution of Platform")


######################################################################################################################################################
# Global Sales distribution
  # According to the plots, the global sales is extremely positively skewed to the right. 
  # This means most games sold between 0.06M to 1M units. 
  # The distribution is rightly skewed due to outlier of 82M sales units.
######################################################################################################################################################
# Summary Statistics
summary(sales$Global_Sales)

# View initial distribution of global sales
# Histogram
ggplot (sales, aes (x = Global_Sales)) +
  geom_histogram(fill = "green", color = "black") +
  scale_x_continuous(breaks = seq(0, 80, 2)) +
  labs (title = "Distribution of Global Sales Units (M)",
        x = "Global Sales in millions of units",
        y = "Count") +
  theme_bw()

# Boxplot
ggplot(sales,aes(x = Global_Sales)) +
  geom_boxplot(fill = "green") +
  labs (title = "Distribution of Global Sales Units (M)",
        x = "Global Sales in millions of units") +
  theme_bw()

# Skewness
skewness(sales$Global_Sales)
  # skewness is greater than 1, extremely positively skewed to the right

# Kurtosis
kurtosis(sales$Global_Sales) 
  # value higher than 3 indicated leptokurtic distribution (heavy-tailed distribution)

# Remove outliers
new_global <- filter(sales, Global_Sales < 40)

# Boxplot without outliers
ggplot(new_global,aes(x = Global_Sales)) +
  geom_boxplot(fill = "green") +
  labs (title = "Distribution of Global Sales Units (M) without Outliers",
        x = "Global Sales in millions of units") +
  theme_bw()

# Check for normal distribution
  # Draw a qqplot using the Global_Sales data:
  qqnorm(sales$Global_Sales, col="green", xlab="z Value", ylab="Time")

  # Add a reference line to the qqplot:
  qqline(sales$Global_Sales, col="red", lwd=2) 

  
  ## Create a boxplot
  install.packages("plotly")
  library(plotly)  # Call the plotly library.
  
  # Interactive box plot
  global_boxplot <- plot_ly(sales, x = ~Global_Sales, type = "box") %>%
    layout(title = 'Global Sales Distribution',
           xaxis = list(title = list(text ='Global Sales Units (M)')))
  
  # Print the plot.  
  global_boxplot
  
######################################################################################################################################################
# NA Sales distribution
  # According to the plots, the NA sales is extremely positively skewed to the right. 
  # This means most games sold between 0.01M to 0.6M units. 
  # The distribution is rightly skewed due to outlier of 41M sales units.
######################################################################################################################################################
# Summary Statistics
summary(sales$NA_Sales)

# View initial distribution of NA sales
# Histogram
ggplot (sales, aes (x = NA_Sales)) +
  geom_histogram(fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(0, 43, 2)) +
  labs (title = "Distribution of NA Sales Units (M)",
        x = "NA Sales in millions of units",
        y = "Count") +
  theme_bw()

# Boxplot
ggplot(sales, aes(x = NA_Sales)) +
  geom_boxplot(fill = "red") +
  labs (title = "Distribution of NA Sales Units (M)",
        x = "NA Sales in millions of units") +
  theme_bw()

# Skewness
skewness(sales$NA_Sales)
# skewness is greater than 1, extremely positively skewed to the right

# Kurtosis
kurtosis(sales$NA_Sales) 
# value higher than 3 indicated leptokurtic distribution (heavy-tailed distribution)

# Remove outliers
new_na <- filter(sales, NA_Sales < 1)

# Boxplot without outliers
ggplot(new_na,aes(x = NA_Sales)) +
  geom_boxplot(fill = "red") +
  labs (title = "Distribution of NA Sales Units (M) without Outliers",
        x = "NA Sales in millions of units") +
  theme_bw()

# Check for normal distribution
  # Draw a qqplot using the NA_Sales data:
  qqnorm(sales$NA_Sales, col="red", xlab="z Value", ylab="Time")

  # Add a reference line to the qqplot:
  qqline(sales$NA_Sales, col="blue", lwd=2) 


# Interactive box plot
  na_boxplot <- plot_ly(sales, x = ~NA_Sales, type = "box") %>%
    layout(title = 'NA Sales Distribution',
           xaxis = list(title = list(text ='NA Sales Units (M)')))
  
  # Print the plot.  
  na_boxplot
  
######################################################################################################################################################
# EU Sales distribution
  # According to the plots, the EU sales is extremely positively skewed to the right. 
  # This means most games sold between 0.01M to 0.27M units. 
  # The distribution is rightly skewed due to outlier of 29M sales units.
######################################################################################################################################################
# Summary Statistics
summary(sales$EU_Sales)

# View initial distribution of EU sales
# Histogram
ggplot (sales, aes (x = EU_Sales)) +
  geom_histogram(fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  labs (title = "Distribution of EU Sales Units (M)",
        x = "EU Sales in millions of units",
        y = "Count") +
  theme_bw()

# Boxplot
ggplot(sales, aes(x = EU_Sales)) +
  geom_boxplot(fill = "blue") +
  labs (title = "Distribution of EU Sales Units (M)",
        x = "EU Sales in millions of units") +
  theme_bw()

# Skewness
skewness(sales$EU_Sales)
# skewness is greater than 1, extremely positively skewed to the right

# Kurtosis
kurtosis(sales$EU_Sales) 
# value higher than 3 indicated leptokurtic distribution (heavy-tailed distribution)

# Remove outliers
new_eu <- filter(sales, EU_Sales < 15)

# Boxplot without outliers
ggplot(new_eu,aes(x = EU_Sales)) +
  geom_boxplot(fill = "blue") +
  labs (title = "Distribution of EU Sales Units (M) without Outliers",
        x = "EU Sales in millions of units") +
  theme_bw()

# Check for normal distribution
  # Draw a qqplot using the NA_Sales data:
  qqnorm(sales$EU_Sales, col="blue", xlab="z Value", ylab="Time")

  # Add a reference line to the qqplot:
  qqline(sales$EU_Sales, col="red", lwd=2) 

# Interactive box plot
  eu_boxplot <- plot_ly(sales, x = ~EU_Sales, type = "box") %>%
    layout(title = 'EU Sales Distribution',
           xaxis = list(title = list(text ='EU Sales Units (M)')))
  
  # Print the plot.  
  eu_boxplot
  
  
################################################################################################################################################################
# Correlation between EU & NA sales versus Global Sales
  # Global sales = EU + NA + online sales
  # Sales in units (millions)
################################################################################################################################################################

# Scatterplot - relationship between NA Sales vs Global Sales
  # Independent variable (x) = NA sales
  # Dependent variable (y) = Global sales
ggplot(sales, aes(NA_Sales, Global_Sales)) +
  geom_point(color = "red",
             alpha = .5,
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1) +
  labs(title = "Relationship between NA Sales and Global Sales Units (M)",
       x = "NA Sales Units (millions)",
       y = "Global Sales Units (millions") +
  theme_bw()

# Correlation coefficient
cor (sales$NA_Sales, sales$Global_Sales)

################################################################################################################################################################
# Scatterplot - relationship between EU Sales vs Global Sales
  # Independent variable (x) = EU sales
  # Dependent variable (y) = Global sales
ggplot(sales, aes(EU_Sales, Global_Sales)) +
  geom_point(color = "blue",
             alpha = .5,
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1,
              color = "green") +
  labs(title = "Relationship between EU Sales and Global Sales Units (M)",
       x = "EU Sales Units (millions)",
       y = "Global Sales Units (millions") +
  theme_bw()

# Correlation coefficient
cor (sales$EU_Sales, sales$Global_Sales)

################################################################################################################################################################

# Report:
  # Correlation can take a range of values between +1 and -1. 
    # When r = 0, there is no association between variables.
    # When r > 0, there is a positive association between variables.
    # When r < 0, there is a negative association between variables.
  # There is a positive linear correlation between NA sales and Global sales with a correlation coefficient of 0.94 which is very close to 1, signfiying perfect correlation.
  # There is a positive linear correlation between EU sales and Global sales with a correlation coefficient of 0.90 which is very close to 1, signfiying perfect correlation.

################################################################################################################################################################

