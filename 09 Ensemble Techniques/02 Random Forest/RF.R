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

### Random Forest
library(randomForest)

fit <- randomForest(Collection~., data = train,ntree=500)
summary(fit)
#Predict Output 
test$random <- predict(fit, test)
MSE2random <- mean((test$random - test$Collection)^2)
