# Run this cell to load the libraries and data
# Load required libraries: dplyr, gplot2
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)



# Load the data
train <- read.csv("historicsales_fastfooditems_train.csv")
test <- read.csv("historicsales_fastfooditems_test.csv")

# Review the first few rows of your data
head(train)
str(train)

#type of data in date is still character, let's change it into date data type
# Specify the correct format of the date string
train$date <- as.Date(train$date, format="%Y-%m-%d")

# Invalid number spotted in discount_percent column, so we will start by cleaning it first
train$discount_percent <- replace_na(train$discount_percent, 0)
test$discount_percent <- replace_na(test$discount_percent, 0)

#next find the correlation between discount and sales quantity
correlation <- cor(train$discount_percent, train$sales_quantity)
correlation

# Find the same correlation for different subsets of restaurant and food item
r1_burger <- train %>%
  filter(restaurant == 'R1' & item_name == 'Burger')

r1_burger_cor <- cor(r1_burger$discount_percent, r1_burger$sales_quantity)
r1_burger_cor 

r1_salad <- train %>%
  filter(restaurant == 'R1' & item_name == 'Salad')

r1_salad_cor <- cor(r1_salad$discount_percent, r1_salad$sales_quantity)
r1_salad_cor

r2_burger <- train %>%
  filter(restaurant == 'R2' & item_name == 'Burger')

r2_burger_cor <- cor(r2_burger$discount_percent, r2_burger$sales_quantity)
r2_burger_cor

r2_salad <- train %>%
  filter(restaurant == 'R2' & item_name == 'Salad')

r2_salad_cor <- cor(r2_salad$discount_percent, r2_salad$sales_quantity)
r2_salad_cor

# Save the highest correlation pair
highest_cor <- c('R1', 'Salad')
highest_cor

#plot graph to see connection between restaurant-item pair
ggplot(train, aes(x = date, y = sales_quantity)) +
  geom_line() +
  labs(title = "Time Series of Sales Quantity by Restaurant and Item", x = "Date", y = "Sales Quantity (Units)", color = "Item, Restaurant")

#build the model using linear regression
model <- lm(sales_quantity ~ discount_percent, data = r1_burger)

#build another one to see differences 
model_mul <- lm(sales_quantity ~ discount_percent + is_weekend + is_friday + is_holiday , data = r1_burger)

#extract adjusted r squared from the model
adj_rsquared1 <- glance(model)$adj.r.squared
adj_rsquared2 <- glance(model_mul)$adj.r.squared
adj_rsquared <- adj_rsquared2

#make prediction on the test data set
test$pred <- predict(model_mul, test)

#compute the RMSE
rmse <- test %>%
mutate(
residuals = sales_quantity - pred) %>%
summarize(
rmse = sqrt(mean(residuals^2))) %>%
pull(rmse)

rmse
