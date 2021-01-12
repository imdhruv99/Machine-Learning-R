# importing data
df <- read.csv("D:/Programming/Machine Learning/Machine Learning with Python and R/R/03_Linear_Regression/04_Subset_Selection/House_Price.csv",
               header = TRUE)

# view content
View(df)

# get short summary
str(df)

# --- EDD & Univariate Analysis --- #
summary(df)

# here we can see that the mean and median values of crime_rate has bigger difference.
# crime_rate has skeweness or outliers in the data we can see by comparing 3rd QU and max value of the variable.
# same analysis also can be done for n_hot_rooms and rainfall variables.

# Another thing to check is n_hos_beds has one more value "NA's" which show us that it has 8 missing/blank values.

# let's check distribution of crime_rate, n_hot_rooms and rainfall by histogram.

# histogram for crime_rate
hist(df$crime_rate)

# we can see the values are in bulk between 0 to 10, but how many values are going beyond 20, we are not really sure. This may not be outliers, this 
# may be genuine values.

# so lets plot scatter-plot for all the variables 
pairs(~price+crime_rate+n_hot_rooms+rainfall,data=df)

# by checking price vs crime_rate we can see that there are lots of values going beyond 30
# so they have polynomial relationship, we need to convert that linear.

# price vs n_hot_rooms
# we can see that all the values are in short range except 2  extremely right, they are clearly outliers

# price vs rainfall
# we can see that all the values are beyond 20 and within 60, only one value is below 10, which is outlier.


# so, we identified outliers in n_hot_rooms and rainfall variable.

# crime_rate has some different kind of relationship with price.

# Let's check categorical variables using bar-plot

# bar-plot for airport
barplot(table(df$airport))
# we have bar plot of airport, but we can see that there is nothing suspicious about this variables.

# bar-plot for waterbody
barplot(table(df$waterbody))
# we have bar plot of waterbody, but we can see that there is nothing suspicious about this variables.

# bar-plot for bus-terminal
barplot(table(df$bus_ter))
# here, bus_ter have only one value, all the cities have bus terminal, so it will not impact our model in any case.


# ------ List of the observations by EDD ------ #
# 1. n_hot_rooms and rainfall has outliers.
# 2. n_hos_beds has missing values.
# 3. bus_ter is useless variable.
# 4. crime_rate has some other function relationship with price.

# ----- Outlier Treatments ----- #

# finding max percentile value of n_hot_rooms
quantile(df$n_hot_rooms, 0.99)

uv = 3 * quantile(df$n_hot_rooms, 0.99)

# now for all the values in n_hot_rooms, we will compare it to uv and if it is grater than uv we will change it according to UV
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv

# EDD for n_hot_rooms
summary(df$n_hot_rooms)
# now from summary we can see median and mean are much closer than before and max value changed from 101 to 46

# lets do this for rainfall
# rainfall has outliers from lower side
lv = 0.3 * quantile(df$rainfall, 0.01)

df$rainfall[df$rainfall < lv] <- lv

summary(df$rainfall)


# --- Missing value Imputation --- #

mean(df$n_hos_beds)

# it will remove NA's while calculating the mean
mean(df$n_hos_beds, na.rm = TRUE)

# now we need to identify blank values in this variable
# below line identify which of the cells are NA and it will return it
which(is.na(df$n_hos_beds))

df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE)

summary(df$n_hos_beds)


# Final EDD
summary(df)

# --- Variable Transformation --- #

# scatter-plot crime_rate vs price
# command one
pairs(~price+crime_rate, data=df)

# command two
plot(df$price, df$crime_rate)

# from the plot, relation seems to look like logarithmic curve and we need to transform it to have linear relationship between x and y.
# One way is take log of x (crime_rate).
# Since most of the values are near 0 and log(0) is not defined that stats minus infinity. 
# so to remove this we will add value of one to our crime_rate.

# log(0) = minus infinity
# log(1) = zero
df$crime_rate <- log(1 + df$crime_rate)

# plotting after transformation
plot(df$price, df$crime_rate)

# Now relationship looking more linear than before.
# One more thing is that there are no visible outliers in the plot.

# transformation of four dist variable into "avg_dist" variable
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4) / 4

# summary
summary(df)

# Now delete four dist variables and also bus_ter variable
# df[rows, columns]

df2 <- df[,-7:-10]
df <- df2
rm(df2)
# bus_ter
df <- df[,-14]

View(df)

# --- Creating Dummy Variable --- #

# We need to change non-numerical(categorical) values into numerical values.
# we will do that by creating dummy variables.

# we need to install package to create dummy variable
# install.packages("dummies")

library(dummies)
# creating dummy variable
df <- dummy.data.frame(df)

# we will remove one of from airport_NO or airport_yes as they both conveying similar information.
# Also we will remove waterbody_none variable as if out of river and lake, 
# if city does not have anything it will put 1 to none variable so we will also remove that variable also.

# removing airport_NO and waterbody_None
df <- df[,-9]
df <- df[,-14]

View(df)

# --- Correlation Matrix --- #
round(cor(df), 2)

#### from matrix we can identify below things
# --------------------------------------------------
# price vs room_num has very high correlation coefficient and so that it will be very important in our analysis.
# price vs poor_prop highly negatively correlated and so that it will be very important in our analysis.

# price vs n_hot_rooms has very low correlation coefficient and so that it will not be very important.
# price vs rainfall has very low correlation coefficient and so that it will not be very important.
# price vs waterbody_river has very low correlation coefficient and so that it will not be very important.

# The correlation between air quality and parks is zero point nine one.
# This means that these two independent variables are highly correlated, And if we take both of these variables for our analysis, this may lead to multicolinearity.
# So we have to delete one of these two variables.
# There are multiple ways to select which very way to delete.
# First one is to check the correlation of these two variables with our dependent variable.
# EX: parks vs price and air_qual vs price. These has high negative correlationship.
# we will take that variable which has higher correlation than other. In our case that is air_qual.
# so we will remove parks from our dataset.

df <- df[,-16]
View(df)

# --- SubSet Selection --- #

# installing Library
# install.packages("leaps")

library(leaps)

# --- Best Subset Selection --- #
# by default it only runs with 8 variables
# so for run with all variable we need to pass nvmax parameter
ss_best <- regsubsets(price~., data = df, nvmax = 15)
summary(ss_best)

# from Output we can conclude below statements
# for one variable "poor_prop" has significant variable impact
# for two variables "room_num and poor_prop" has significant variable impact
# So, That is how it will create 15 best models
# Among These we will only select model, which has highest Adjusted R-Squared value

# Finding Adjusted R-Squared values
summary(ss_best)$adjr2
# [1] 0.5479428 0.6410518 0.6792769 0.6909566 0.7088919 0.7120175 0.7142743 0.7145585 0.7144634 0.7143695 0.7141053 0.7138216 0.7133427 0.7128582 0.7122727
# Above are Adjusted R-Squared values for 15 models
# we can find which is highest and we can use that model as selected

# finding max values
which.max(summary(ss_best)$adjr2)
# we can see that 8th value is max value among all 15
# which is 0.7145585

# coefficient of model
coef(ss_best, 8)
# (Intercept)     air_qual     room_num     teachers    poor_prop   airportYES   n_hos_beds  n_hot_rooms     avg_dist 
# -8.22796010 -17.12484002   4.00060153   1.03360799  -0.58517689   1.18298230   0.33531318   0.09963185  -1.14187507 

# we can see all the values for 8th model

# --- Forward Subset Selection --- #

# to perform forward selection we only need to add method = "forward"
ss_forward <- regsubsets(price~., data = df, nvmax = 15, method = "forward")

summary(ss_forward)
# It has also run all the 15 models

# Finding Adjusted R-Squared values
summary(ss_forward)$adjr2
# [1] 0.5479428 0.6410518 0.6792769 0.6909566 0.7088919 0.7120175 0.7142743 0.7145585 0.7144634 0.7143695 0.7141053 0.7138216 0.7133427 0.7128582 0.7122727

# we can find best adjusted R squared value
which.max(summary(ss_forward)$adjr2)
# It also gives 8th model has best values
# which is 0.7145585

# coef of 8th model
coef(ss_forward, 8)
#  (Intercept)     air_qual     room_num     teachers    poor_prop   airportYES   n_hos_beds  n_hot_rooms     avg_dist 
# -8.22796010 -17.12484002   4.00060153   1.03360799  -0.58517689   1.18298230   0.33531318   0.09963185  -1.14187507

# --- Backward Subset Selection --- #

# to perform forward selection we only need to add method = "backward"
ss_backward <- regsubsets(price~., data = df, nvmax = 15, method = "backward")

summary(ss_backward)
# It has also run all the 15 models

# Finding Adjusted R-Squared values
summary(ss_backward)$adjr2
# 0.5479428 0.6410518 0.6792769 0.6909566 0.7088919 0.7120175 0.7142743 0.7145585 0.7144634 0.7143695 0.7141053 0.7138216 0.7133427 0.7128582 0.7122727

# we can find best adjusted R squared value
which.max(summary(ss_backward)$adjr2)
# It also gives 8th model has best values
# which is 0.7145585

# coef of 8th model
coef(ss_backward, 8)
#  (Intercept)     air_qual     room_num     teachers    poor_prop   airportYES   n_hos_beds  n_hot_rooms     avg_dist 
# -8.22796010 -17.12484002   4.00060153   1.03360799  -0.58517689   1.18298230   0.33531318   0.09963185  -1.14187507 