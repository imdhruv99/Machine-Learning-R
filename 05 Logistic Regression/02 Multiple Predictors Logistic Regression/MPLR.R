# reading dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# --- Multiple Predictor Logistic Regression --- #
# Independent Single Variable = All variables except 'Sold' 
# Dependent Variable = Sold

# creating logistic model
# glm stands for generalized linear model

mplr.fit = glm(Sold~., data = df, family = binomial)
summary(mplr.fit)

# Intercept = -2.139
# We have Coefficients, Standard Error, z-value nd P-value for all variables