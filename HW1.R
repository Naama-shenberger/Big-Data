
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
# The dimensions of the cube are the districts (each net.number is a district) and the dimension of date time.
Net_generation_ <- A[,c('Net.generation',"Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6",
                        "Net.generation.7" , "Net.generation.8" , "Net.generation.9" , "Net.generation.10")]
A$DateTime<-as.Date(A$DateTime,"%m%d%Y")
#Create a data frame of the dimensions,the measurable variable ןis the Net.generation
tabel<-data.frame(net_g=Net_generation_,Date=A[,'DateTime'])
#Performing roll up - from a days and hours to days only.
data_cube = tabel %>% group_by(Date) %>% summarise_each(funs(sum))

#Performing Silceing on Data cube - from all days to 07-14 Feb only.
Silce_cube<-data_cube[c(31:38),]

days<-c(7:14)#07-14 Feb
p_generation_mean<-rowMeans(Silce_cube[,2:12])#Mean power generation
# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(7,14), ylim=c(1605748 ,1900000 ))
# rearrange in a new, temporary dataframe
DF <- data.frame ( Time = days, Demand = p_generation_mean)
# plot
lines( DF, col = 'darkorchid1', type = 'b' )
title(main = "Mean daily power generation in the US",
      xlab = "Days (07-14 Feb)", ylab = "Net generation",
      cex.main = 2,   font.main = 2, col.main = "darkorchid1")
























