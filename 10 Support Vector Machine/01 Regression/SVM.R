## Import data from CSV file.
df <- read.csv("Movie_regression.csv", header = TRUE)

## Data Preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)

## Test-Train Split
# install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(df, SplitRatio = 0.8)
train = subset(df,split == TRUE)
test = subset(df,split == FALSE)

## Importing relevant Library e1071
# install.packages('e1071')
library (e1071)

svmfit = svm(Collection∼., data=train , kernel = "linear", cost =0.01, scale = TRUE)
summary (svmfit)

## Predicting on test set
ypred = predict (svmfit ,test)
mse <- mean((ypred-test$Collection)^2)
mse