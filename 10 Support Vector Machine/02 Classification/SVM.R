## Import data from CSV file. Note a forward slash '/' is used in file location.
movie <- read.csv("Movie_classification.csv", header = TRUE)

## Data Preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

## Test-Train Split

# install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(movie,split == TRUE)
testc = subset(movie,split == FALSE)

## For Classification
trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)
testc$Start_Tech_Oscar <- as.factor(testc$Start_Tech_Oscar)

## Importing relevant Library e1071

# install.packages('e1071')
library (e1071)


svmfit = svm (Start_Tech_Oscar∼., data=trainc , kernel = "linear", cost =1 ,scale = TRUE)
summary (svmfit)

## Predicting on test set
ypred=predict (svmfit ,testc)
table(predict =ypred , truth= testc$Start_Tech_Oscar)

69/108
## To check the support vectors
svmfit$index