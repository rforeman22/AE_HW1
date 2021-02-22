# This script contains code provided in homework assignment 1

# Read data
ames = read.csv("AmesSales.csv")
ames$SalePrice = as.numeric(ames$SalePrice)


# Train/test split
library(caret)
RNGkind(sample.kind = "Rounding")
set.seed(310)
idx = createDataPartition(ames$SalePrice, p = 0.70, list = FALSE)
train = ames[idx,]
test = ames[-idx,]

# Check
mean(train$SalePrice)
# Should be 178635.7

# Diagnostics
par(mfrow=c(2,2)) #create two rows and columns in the plot window
plot(model)
par(mfrow=c(1,1)) #reset the plot window to default settings

# Outliers
outliers=c("1451", "2114", "2115")
# Why are we using strings instead of numbers in the "outliers" vector?
# The outlier labels in the plot refer to the *row names* taken from the original dataset
# Row names are strings, not numbers 
# In the initial dataset, the row names match the row numbers (e.g., the name of the 10th row is "10")
# Since the training set only contains 70% of the original data, the row names are not the same as row numbers
print(train[outliers,])
# Data summary
summary(ames)

# Remove outliers
train2 = ames[setdiff(idx,outliers), ]

# Reduced model variables
# SalePrice ~ BldgType+YearBuilt+Fireplaces+GarageArea+PoolArea+LivArea

# Bigger tree model
tree.model2 = rpart(SalePrice ~., data = train2, control = rpart.control(cp=0.0001))

# Variable importance plot
barplot(tree.model2$variable.importance, cex.names=.7)