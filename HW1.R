install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
A <- read.delim("table.tsv")
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
# Remove duplicate rows of the dataframe
A<-distinct(A)
#dc<-which( (as.character(A[,'DateTime'])==as.POSIXct("2021-02-06 00:00:00 EST")) |((as.character(A[,'DateTime'])>as.POSIXct("2021-02-07 00:00:00 EST")) & (as.character(A[,'DateTime'])<as.POSIXct("2021-02-15"))) )
#Demand_  <- c('BPAT','CISO','CPLE','ERCO', 'FPL', 'ISNE', 'MISO','NYIS', 'PACW', 'PJM' ,'United.States.Lower.48..region.' )
#day_ <-c(seq(as.Date("2021-02-04"),to=as.Date("2021-02-14"),by="days"))#A[,'DateTime']

# Generate the data
Net_generation_ <- A[,c('Net.generation',"Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6",
                        "Net.generation.7" , "Net.generation.8" , "Net.generation.9" , "Net.generation.10")]

A$DateTime<-as.Date(A$DateTime,"%m%d%Y")
tabel<-data.frame(net_g=Net_generation_,Date=A[,'DateTime'])

data_cube = tabel %>% group_by(Date) %>% summarise_each(funs(sum))

Silce_cube<-data_cube[c(31:38),]



min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization 
data_norm <- as.data.frame(lapply(Silce_cube[,c(2:12)], min_max_norm))
data_norm$Date<-Silce_cube$Date

# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(0,12), ylim=c(-2, 2))

# linear fit
LM <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA,
            Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )


demands <- seq(11)


df$dia <- format(df$x, "%d")

days<-format(data_norm$Date, "%d")
print(days)
for ( i in demands ) {
  # rearrange in a new, temporary dataframe
  DF <- data.frame ( Time = days, Demand = data_norm[,i] )
  # plot
  lines( DF, col = i, type = 'b' )
  LM[[ i ]] <- lm( Demand ~ Time, data =DF)
  a <- coef(LM[[ i ]])[1]
  b <- coef(LM[[ i ]])[2]
  abline(a, b, col = i, lw =2)
}
print(DF)

legend( 'bottomleft', col = demands, pch = 19,
        legend = sapply(demands, function(x) paste0('Demand.',x) ))
























