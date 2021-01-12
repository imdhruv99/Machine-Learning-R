# importing data set
df <- read.csv("House-Price.csv", header = TRUE)
View(df)

# structure of Data
str(df)

# edd
summary(df)

# boxplot
boxplot(df$n_hot_rooms, data = df)

pairs(~df$Sold+df$rainfall)

barplot(table(df$airport))
barplot(table(df$bus_ter))

# 1. Rainfall and n_hot_rooms have outliers
# 2. n_hos_beds has missing values
# 3. bus_ter is not providing any information

# --- Outliers Treatment --- #

quantile(df$n_hot_rooms, 0.99)
upper_limit <- 3 * quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms > upper_limit] <- upper_limit
summary(df$n_hot_rooms)

lower_limit <- 0.3 * quantile(df$rainfall, 0.1)
df$rainfall[df$rainfall < lower_limit] <- lower_limit
summary(df$rainfall)

# --- Missing Value Imputation --- #
mean(df$n_hos_beds, na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE)
summary(df$n_hos_beds)

# --- Variable Transformation --- #

df$avg_dist <- (df$dist1 + df$dist2 + df$dist3 + df$dist4) / 4
View(df)
df2 <-  df[, -6:-9]
View(df2)
df <- df2
View(df)
rm(df2)
df <- df[,-13]

# --- Dummy Variable Creation --- #

library(dummies)
df <- dummy.data.frame(df)
df <- df[,-8]
df <- df[,-13]
