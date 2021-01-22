# Reading Dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# importing caTools
 library(caTools)

# setting seed
set.seed(0)

# splitting data
split <- sample.split(df, SplitRatio = 0.8)

# training and testing dataset
train_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

# logistic regression model
train.fit <- glm(Sold~., data = train_set, family = binomial)

# prediction
test_pred = predict(train.fit, test_set, type = 'response')

# creating array for no, and change value for greater than 0.5
test.pred = rep('NO', 120)
test.pred[test_pred > 0.5] = 'YES'

# confusion matrix
table(test.pred, test_set$Sold)

#test.pred  0  1
#      NO  42 16
#      YES 26 36

# --- K-Nearest Neighbors --- #

# importing class library
library(class)

trainX <- train_set[, -16]
testX <- train_set[, -16]
trainY <- train_set$Sold
testY <- train_set$Sold

# change the k value and run analysis again
k <- 3

# standardizing data
trainX_s <- scale(trainX)
testX_s <- scale(testX)

# setting seed
set.seed()

knn.pred <- knn(trainX_s, testX_s, trainY, k = k)

# confusion matrix
table(knn.pred, testY)
# knn.pred   0   1
#        0 162  36
#        1  46 142

