# Reading Dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# importing caTools
 library(caTools)

# setting seed
set.seed(0)

# spliting data
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