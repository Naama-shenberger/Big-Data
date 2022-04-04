
#Download packages
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)


#Reading Data
A <- read.delim("table.tsv")
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
# Remove duplicate rows in the dataframe
A<-distinct(A)
 # PJM9, NYIS7, ISNE5, FPL4, CPLE2
# The dimensions of the cube are the districts (each net.number is a district) and the dimension of date time.
Net_generation_ <- A[,c( "Net.generation.2", "Net.generation.4", "Net.generation.5",
                        "Net.generation.7" , "Net.generation.9" )]

#Create a data frame of the dimensions,the measurable variable ןis the Net.generation
tabel<-data.frame(net_g=Net_generation_,Date=A[,'DateTime'])