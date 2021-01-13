# reading dataset
df <- read.csv('preprocessed_HousePrice.csv', header = TRUE)
View(df)

# --- Simple Logistic Regression --- #
# Independent Single Variable = price
# Dependent Variable = Sold

# creating logistic model
# glm stands for generalized linear model

x <- df$price
y <- df$Sold

slr.fit = glm(y~x, family = binomial)
summary(slr.fit)

# beta 0 = 0.614
# beta 1 = -0.03572
