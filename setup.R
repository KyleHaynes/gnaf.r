install.packages("devtools")

setwd("C:\\Users\\kyleh\\OneDrive\\Desktop\\gnaf_r")

# Create package
devtools::create('gnaf.r')

devtools::document()


# Usage
library(gnaf.r)