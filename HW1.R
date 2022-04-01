install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

A <- read.delim("table.tsv")
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
# Remove duplicate rows of the dataframe
A<-distinct(A)
