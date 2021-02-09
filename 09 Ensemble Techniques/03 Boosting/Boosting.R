#import dataset
movie <- read.csv("Movie_regression.csv")
View(movie)

#Data Preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

# Test-Train Split
#install.packages('caTools')

library(caTools)
set.seed(0)
split <- sample.split(movie,SplitRatio = 0.8)
train <- subset(movie,split == TRUE)
test <- subset(movie,split == FALSE)

library(rpart)
library(rpart.plot)

#Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
#press F1 on rpart for help on this function

#Plot the decision Tree
rpart.plot(regtree, box.palette="RdBu", digits = -3)

#Predict value at any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)

### Boosting ###

### Gradient Boosting ###

# install.packages('gbm')
library(gbm)
set.seed (1)
boosting <- gbm(Collection~Budget+Trailer_views, data = train, distribution="gaussian",n.trees =5000 , interaction.depth =10, shrinkage =0.1,verbose =F)
test$boost <- predict (boosting, test,n.trees =1000)
MSE2boost <- mean((test$boost - test$Collection)^2)


### XGBoost ###
# install.packages('xgboost')
library(xgboost)
library(data.table)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
bst <- xgboost(data = train$data, label = train$label, max_depth = 2, eta = 1,nrounds = 2, objective = "binary:logistic")
pred <- predict(bst, test$data)
