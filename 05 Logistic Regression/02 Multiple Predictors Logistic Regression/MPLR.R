# reading dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# --- Multiple Predictor Logistic Regression --- #
# Independent Single Variable = All variables except 'Sold' 
# Dependent Variable = Sold

# creating logistic model
# glm stands for generalized linear model

mplr.fit <- glm(Sold~., data = df, family = binomial)
summary(mplr.fit)

# Intercept = -2.139
# We have Coefficients, Standard Error, z-value nd P-value for all variables

# --- Calculating Probability that house will be sold or not --- #

mplr.prop <- predict(mplr.fit, type = "response")
mplr.prop[1:10]

# --- using boundary condition --- #
mplr.pred <- rep("NO", 506)
# wherever the probability value is larger than 0.5, i will change the value from NO to YES
mplr.pred[mplr.prop > 0.5] = "YES"
View(mplr.pred)

# --- Confusion Matrix --- #
table(mplr.pred, df$Sold)

# result
# mplr.pred   0   1
# -------------------
#        NO  197  81
#       YES  79 149