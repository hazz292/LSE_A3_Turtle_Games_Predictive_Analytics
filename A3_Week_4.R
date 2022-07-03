###################################################################################################################################################### 

# Assignment 3 Week 4: Visualise data to gather insights

    # Objective
        # 1. Determine the customer group that will most likely leave a review on the products they have purchased: 
            # Which age group submits the most reviews?
        # 2. Determine the most expensive product purchased by a particular group of customers: 
            # What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

######################################################################################################################################################

# Install tidyverse
install.packages("tidyverse") 
library(tidyverse)
tidyverse_conflicts()
tidyverse_update()

######################################################################################################################################################
# Import games_sales dataset 
######################################################################################################################################################

# Determine the working directory.
getwd()  

# Set working directory
setwd(dir = "/Users/hazel/Documents/LSE Data Analytics/Course 3 - Advanced Analytics/Assignment 3/game_data")

lego <- read.csv("lego.csv", header = TRUE)

# View head of dataframe
head(lego)

# View dataframe
View(lego)

# View descriptive summary
summary(lego)

# View information of dataframe
as_tibble(lego) 

# Check for missing values
sum(is.na(lego))


######################################################################################################################################################

# 1. Which age group submits the most reviews?
    # Report: According to the column chart, the age group of 6 to 10 years old has submitted the most number of reviews at 76,120 in this dataset.

######################################################################################################################################################

# View distribution of age
  # Histogram
  qplot(ages, bins = 6, data = lego, geom = "histogram")

  # Boxplot
  qplot(ages, data = lego, geom = "boxplot")

# Relationship between age and number of reviews
  
  # Column chart
  ggplot(lego, aes(x=ages, y=num_reviews)) + 
    geom_col(fill ='blue') +
    scale_x_continuous(breaks = seq(min(lego$ages), max(lego$ages), 1), "Age of the Customer") +
    scale_y_continuous(breaks = seq(0, 60000 , 10000), "Number of reviews") +
    labs(title = "Number of Reviews for each age")

# Age of customers ranges from 0 to 30 years old
  # The customers can be grouped into 6 age groups
  # 0-5 years old
  # 6-10 years old
  # 11-15 years old
  # 16-20 years old
  # 21-25 years old
  # 26-30 years old

# Create subset with 6 age groups in range
age_df <- subset(lego, select = c(ages, num_reviews, list_price))

# View new dataframe
head(age_df)

# Group customers into corresponding age group
age_df <- age_df %>%
    mutate(age_group = case_when(ages <= 5 ~ "G1:0-5",
                                 ages >= 6 & ages <= 10 ~ "G2:6-10",
                                 ages >= 11 & ages <= 15 ~ "G3:11-15",
                                 ages >= 16 & ages <= 20 ~ "G4:16-20",
                                 ages >= 21 & ages <= 25 ~ "G5:21-25",
                                 ages >= 26 & ages <= 30 ~ "G6: 26-30"))

# View dataframe with age group column
head(age_df)

# View info
as_tibble(age_df)

# Aggregate sum of reviews for each age group
age_sum <- age_df %>%
  group_by(age_group) %>%
  summarise(total_reviews = sum(num_reviews))

# View dataframe with age group column
head(age_sum)

# View info
as_tibble(age_sum)

# Column chart of number of reviews in each age group
ggplot(age_sum, aes(x=age_group, y=total_reviews)) + 
  geom_col(fill ='purple', color = 'black') +
  geom_text(aes(label = total_reviews), vjust = -0.4, check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(0, 100000 , 10000), "Number of Reviews") +
  labs(title = "Number of Reviews in each Age Group", x = "Age Group") +
  theme_classic()


######################################################################################################################################################

# 2. Which is the most expensive lego set purchased by customers who are at least 25 years old?
    # Report: According to the column chart, customer of age 29 purchased the most expensive lego set at price of USD 260.

######################################################################################################################################################

# Create new dataframe with customer who are at least 25 years old.
data_age25 <- age_df[age_df$ages>=25,]

# View new dataframe
summary(data_age25)

age_price

# Histogram to view price distribution
qplot(list_price, data = data_age25, geom = "histogram")

# Scatter plot
qplot(ages, list_price, data = data_age25, geom = c("point", "jitter"))

# Aggregate max price for each age
age_price <- data_age25 %>%
  group_by(ages) %>%
  summarise(max_price = round(max(list_price, na.rm=TRUE)))

# View dataframe
head(age_price)

# Plot maximum price of product purchased by each age
# Column chart
ggplot(age_price, aes(x=ages, y=max_price)) + 
  geom_col(fill ='orange', color = 'black') +
  geom_text(aes(label = max_price), vjust = -0.4, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(25, 30 , 1), "Age of Customer") +
  scale_y_continuous(breaks = seq(0, 300 , 50), "Maximum Price") +
  labs(title = "Maximum price of product purchased by each age") +
  theme_classic()

