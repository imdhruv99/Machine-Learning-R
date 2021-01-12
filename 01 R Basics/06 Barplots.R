# import Data from CSV
Customer <- read.csv("D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/Customer.csv", header = TRUE)

# structure
str(Customer)

# look at the data
Customer

# look at all the observations
View(Customer)

# frequency Distribution
fd <- table(Customer$Region)
fd
View(fd)

# creating Barplot
barplot(fd)

# arrange barplot in the order of the height
# ascending order
barplot(fd[order(fd)])
# descending Order
barplot(fd[order(-fd)])

# change the orientation
barplot(fd[order(fd)], horiz = TRUE)

# change the color of barplot
# single color 
barplot(fd[order(fd)], horiz = TRUE, col = "green")
# multicolor
barplot(fd[order(fd)], horiz = TRUE, col = c("red", "green", "blue", "yellow"))

# list of colors available in R
colors()

# remove black boundaries
barplot(fd[order(fd)], horiz = TRUE, col = c("red", "green", "blue", "yellow"), border = NA)

# add title
barplot(fd[order(fd)], col = c("red", "green", "blue", "yellow"), border = NA, main = "Frequencies of Regions")

# label the axis
barplot(fd[order(fd)], col = c("red", "green", "blue", "yellow"), border = NA, main = "Frequencies of Regions", ylab = "Number of Customers", xlab = "Regions")

# save the plot as image
png(filename = "D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/FreqOfRegion.png", width = 500, height = 500)
barplot(fd[order(fd)], col = c("red", "green", "blue", "yellow"), border = NA, main = "Frequencies of Regions", ylab = "Number of Customers", xlab = "Regions")
dev.off()