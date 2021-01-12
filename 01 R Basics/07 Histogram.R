# import Data from CSV
Customer <- read.csv("D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/Customer.csv", header = TRUE)

# structure
str(Customer)

# look at the data
Customer

# look at all the observations
View(Customer)

# Histogram for age variable
hist(Customer$Age)

# create buckets
hist(Customer$Age, breaks = 5)

# width of the category
hist(Customer$Age, breaks = c(0, 40, 60, 100))

# frequency Parameter
hist(Customer$Age, breaks = c(0, 40, 60, 100), freq = TRUE)

# Change the color of plots
# single color
hist(Customer$Age, breaks = c(0, 40, 60, 100), freq = TRUE, col = "red")
# multicolor
hist(Customer$Age, breaks = c(0, 40, 60, 100), freq = TRUE, col = c("red", "green", "blue"))

# change the name of the chart
hist(Customer$Age, breaks = c(0, 40, 60, 100), freq = TRUE, col = c("red", "green", "blue"), main = "Histogram of Age")

# save the plot as image
png(filename = "D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/HistogramOfAge.png", width = 500, height = 500)
hist(Customer$Age, breaks = c(0, 40, 60, 100), freq = TRUE, col = c("red", "green", "blue"), main = "Histogram of Age", border = NA)
dev.off()