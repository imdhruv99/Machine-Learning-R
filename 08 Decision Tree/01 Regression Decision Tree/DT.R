# import Dataset
df <- read.csv('Movie_regression.csv')
View(df)

# Data Pre-processing
summary(df)

# missing value imputation of Time_taken variable
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)
summary(df)


# Train Test Split
library(caTools)
# setting seed = 0, so we can get same data for each time
set.seed(0)
split <- sample.split(df, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)


# Regression Tree
# install.packages('rpart')
# install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

# running regression model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))

# plotting the Decision Tree
rpart.plot(regtree, box.palette = "RdBu", digits = -3)

# predicting value at any point
test$pred <- predict(regtree, test, type = 'vector')

MSE <- mean((test$pred - test$Collection) ^2)


# Tree Pruning
# cp = 0, it will grow as normal without any pruning and we will get max length
fullTree <- rpart(formula = Collection~., data = train, control = rpart.control(cp = 0))
rpart.plot(fullTree, box.palette = "RdBu", digits = -3)
printcp(fullTree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(fullTree, cp = mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits = -3)

test$fullTree <- predict(fullTree, test, type = "vector")
MSE2full <- mean((test$fullTree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)

accuracy_postprun <- mean(test$pred == test$left)

data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)