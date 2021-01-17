# reading dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# importing package
library(MASS)

# --- Linear Discriminant Analysis --- #

# fitting model
lda <- lda(Sold~., data = df)
lda

# finding predictive probability
lda_pred <- predict(lda, data = df)
View(lda_pred)

# posterior probability
lda_pred$posterior

# classes
lda_class <- lda_pred$class
View(lda_class)

# confusion matrix
table(lda_class, df$Sold)

# lda_class   0   1
#         0 192  79
#         1  84 151

# number of classes belongs to 1 using boundary condition
sum(lda_pred$posterior[ ,1] > 0.8)
# 76 classes belongs to 1

# --- Quadratic Discriminant Analysis --- #

# fitting model
qda <- qda(Sold~., data = df)

# finding predictive probability
qda_pred <- predict(qda, data = df)
View(qda_pred)

# posterior probability
qda_pred$posterior

# classes
qda_class <- qda_pred$class
View(qda_class)

# confusion matrix
table(qda_class, df$Sold)

# qda_class   0   1
#         0 167  32
#         1  109 198

# number of classes belongs to 1 using boundary condition
sum(qda_pred$posterior[ ,1] > 0.8)
# 135 classes belongs to 1