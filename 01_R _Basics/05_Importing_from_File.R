# import data from text file
Product <- read.table("D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/Product.txt", header = TRUE, sep = "\t")

# structure
str(Product)

# look at the data
Product

# Look at all the observations
View(Product)

# import Data from CSV
Customer <- read.csv("D:/Programming/Machine Learning/Machine Learning with Python and R/R/01_R _Basics/Customer.csv", header = TRUE)

# structure
str(Customer)

# look at the data
Customer

# look at all the observations
View(Customer)