
#Download packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("matrixcalc")
install.packages("zoo")
library(dplyr)
library(tidyr)
library(matrixcalc)
library(zoo)

#Reading Data
A <- read.delim("table.tsv")
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
# Remove duplicate rows in the dataframe
A<-distinct(A)
# The dimensions of the cube are the districts (each net.number is a district) and the dimension of date time.
Net_generation_ <- A[,c('Net.generation',"Net.generation.1", "Net.generation.2", "Net.generation.3", "Net.generation.4", "Net.generation.5", "Net.generation.6",
                        "Net.generation.7" , "Net.generation.8" , "Net.generation.9" , "Net.generation.10")]
#Create a data_cube from data frame of the dimensions,the measurable variable is the Net.generation
data_cube<-data.frame(net_g=Net_generation_,Date=as.Date(A$DateTime,"%m%d%Y"),Time=format(A$DateTime, format="%H:%M:%S"))

#Performing Slicing on Data cube - from all days to 07-14 Feb only.
index<-which(data_cube[,'Date']>as.character.POSIXt("2021-02-06") & data_cube[,'Date']<as.character.POSIXt("2021-02-15") )
Silce_cube<-data_cube[index,]
#Performing roll up - from a days and hours to days only.
roll_up_dc = Silce_cube[,1:12] %>% group_by(Date) %>% summarise_each(funs(sum))


days<-c(7:14)#07-14 Feb
p_generation_mean<-rowMeans(roll_up_dc[,2:12])#Mean power generation
# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(7,14), ylim=c(1605748 ,1900000 ))
# rearrange in a new, temporary dataframe
DF <- data.frame ( Time = days, Demand = p_generation_mean)
# plot
lines( DF, col = 'darkorchid1', type = 'b' )
title(main = "Mean daily power generation in the US",
      xlab = "Days (07-14 Feb)", ylab = "Net generation",
      cex.main = 2,   font.main = 2, col.main = "darkorchid1")

# PJM9, NYIS7, ISNE5, FPL4, CPLE2
#a[which(is.na(a)==TRUE)] = mean(a,na.rm = T)
dc<-A[,c("Demand.2","Demand.4","Demand.5","Demand.7","Demand.9")]
dc<-na.locf(na.locf(dc),fromLast=TRUE)
dc$Time<-Silce_cube[,13]
roll_up_dc = dc %>% group_by(Time) %>% summarise_each(funs(sum))
#Performing Slicing on Data cube - from all days to 07-14 Feb only.
index_1<-which(roll_up_dc[,'Time']>as.character.POSIXt("09:00:00") & roll_up_dc[,'Time']<as.character.POSIXt("19:00:00") )
Silce_cube_1<-roll_up_dc[index_1,]

Time_list<- vector( mode = "numeric", length=9)
for (index in c(1:9))
{
  Time_list[index]<- as.integer(stringr::str_extract(Silce_cube_1[index,1], "^.{2}"))
  
}


mat1_data <- c(rep(c(1),9),Time_list)
mat1 <- matrix(mat1_data,nrow=2,ncol=9,byrow=TRUE)
n_vec<- matrix(c(1,1),nrow=2,ncol=1,byrow=TRUE)

# linear fit
LM <- list( CPLE= NA,FPL = NA, ISNE= NA,NYIS = NA,PJM= NA)
demand<- c(2) #seq(2,6)
"%^%" <- function(mat, power) 
  with(eigen(mat), vectors %*% (values^power * t(vectors))) 
print((mat1  %*%  t(mat1) )%^% (-1)%*% mat1)
# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(10,18), ylim=c(363019 ,473561  ))
for (i in demand)
{
  print(min(Silce_cube_1[ ,i]))
  print(max(Silce_cube_1[ ,i]))
  DF <- data.frame ( Time = Time_list, Demand = Silce_cube_1[ , i ] )
  lines( DF, col = i, type = 'b' )
  # linear fit
  LM[[i]]<-((mat1 %*% t(mat1))%^% (-1)) %*%(mat1)*(DF[2])*n_vec
  #LM[[ i ]]
  #LM[[ i ]] <- lm(Demand ~ Time, data = DF)
  a <- coef(LM[[ i ]])[1]
  b <- coef(LM[[ i ]])[2]
  abline(a, b, col = i, lw =2)
}
print(DF)
print(max(Silce_cube_1[ ,]))
max(df1$Mathematics1_score)
print(LM)

for ( i in demands ) {
  # rearrange in a new, temporary dataframe
  DF <- data.frame ( Time = rng - min(rng), Demand = norm.C[ , i ] )
  # plot
  lines( DF, col = i, type = 'b' )
  # linear fit
  LM[[ i ]] <- lm( Demand ~ Time, data = DF)
  a <- coef(LM[[ i ]])[1]
  b <- coef(LM[[ i ]])[2]
  #abline(a, b, col = i, lw =2)
}






index_2<-which( roll_up_dc[,'Time']<as.character.POSIXt("04:00:00")|roll_up_dc[,'Time']>as.character.POSIXt("19:00:00") )
Silce_cube_2<-roll_up_dc[index_2,]

mat2_data <- c(rep(c(1),9),Silce_cube_2[1,1],Silce_cube_2[2,1],Silce_cube_2[3,1],Silce_cube_2[4,1],Silce_cube_2[5,1],Silce_cube_2[6,1],Silce_cube_2[7,1],Silce_cube_2[8,1],Silce_cube_2[9,1])
mat2 <- matrix(mat2_data,nrow=2,ncol=9,byrow=TRUE)

















