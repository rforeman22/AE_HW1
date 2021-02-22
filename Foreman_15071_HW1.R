# 15.701 HW 1
# Names: Riley Foreman
# Time Spent: 2:00

# Load tidyverse library (need it for correlation matrix later)
library("tidyverse")

# Set work directory

# Load in dataset
ames = read.csv("AmesSales.csv")

# Make sure variable 'SalePrice' is encoded as a numeric variable (not factor)
ames$SalePrice = as.numeric(ames$SalePrice)

###----------------
### 1a
###----------------
summary(ames$SalePrice)
hist(ames$SalePrice)

###----------------
### 1b
###----------------
# Train/test split
library("caret")
RNGkind(sample.kind = "Rounding")
set.seed(310)
idx = createDataPartition(ames$SalePrice, p = 0.70, list = FALSE)
train = ames[idx,]
test = ames[-idx,]

# Check
mean(train$SalePrice)
# Should be 178635.7

# Train linear regression model to predict SalesPrice using all other variables
mod1 <- lm(SalePrice~., data=train)
summary(mod1)
# The multiple R-squared is 0.7369

###----------------
### 1c
###----------------
# Plot the regression model
par(mfrow=c(2,2)) #create two rows and columns in the plot window plot(model)
plot(mod1)
par(mfrow=c(1,1)) #reset the plot window to default settings

# Identify outliers
# NOTE: Row names are strings
outliers=c("1451", "2114", "2115")
print(train[outliers ,])
# Data summary
summary(train)

###----------------
### 1d
###----------------
# Remove outliers from training set
train2 = ames[setdiff(idx,outliers), ]

# Train a new regression model without the outliers using all variables
mod2 <- lm(SalePrice~., data=train2)
summary(mod2)
# The multiple R-squared is 0.7845

###----------------
### 1e
###----------------
# Check for correlations among the numerical variables in train2 data
# Remove variable 'BldgType' because it is categorical
numeric_vars <- select(train2, -BldgType)
# Round correlation matrix to 2 decimals
coreMatrix <- round(cor(numeric_vars, y=NULL, use="everything"),2)
# Take a look. Assume variables are correlated at 0.75 level
coreMatrix
# 'LivArea' and 'TotalRooms' are correlated at 0.80 (what we expected)

###----------------
### 1g
###----------------
mod3 <- lm(SalePrice~BldgType+YearBuilt+Fireplaces+GarageArea+PoolArea+LivArea,
           data=train2)
summary(mod3)
# The multiple R-squared is 0.7592

# Add 'prediction' column to train2 data with predicted sale price using mod3
pred_train_mod3 <- predict(mod3, newdata=train2)
train2 = mutate(train2, prediction = pred_train_mod3)
view(train2)

# Make predictions using mod3 on the test set
pred_test_mod3 <- predict(mod3, newdata=test)

# Calculate OSR2 for mod3
resid_test <- test$SalePrice - pred_test_mod3
SSR_test = sum((resid_test)^2)
baseline_train = mean(train2$SalePrice)
SST_test = sum((test$SalePrice - baseline_train)^2)
# Note: we use the baseline computed on the train data
R2_test = 1 - SSR_test / SST_test
R2_test
# OSR2 for mod3 is 0.742

# -------------------------------------
# Repeat above OSR2 calculation for mod2
#--------------------------------------
# Add 'prediction_2' column to train2 data with predicted sale price using mod2
pred_train_mod2 <- predict(mod2, newdata=train2)
train2 = mutate(train2, prediction_2 = pred_train_mod2)
view(train2)

# Make predictions using mod2 on the test set
pred_test_mod2 <- predict(mod2, newdata=test)

# Calculate OSR2 for mod2
resid_test_2 <- test$SalePrice - pred_test_mod2
SSR_test_2 = sum((resid_test_2)^2)
SST_test_2 = sum((test$SalePrice - baseline_train)^2)
# Note: we use the baseline computed on the train data
R2_test_2 = 1 - SSR_test_2 / SST_test_2
R2_test_2
# OSR2 for mod2 is 0.774

