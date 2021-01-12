# browse the URL
browseURL("http://cran.r-project.org/web/views/")

# Installing Packages
install.packages("LiblineaR")
install.packages("ggplot2")

# list of installed packages
library()

# list of loaded packages
search()

# load package
require("LiblineaR")

# unload package
detach("package:ggplot2")

# Uninstall the package
remove.packages("LiblineaR")

# Know more about packages
? ggplot2