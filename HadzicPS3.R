#R Programming
#Problem Set 3
#Dino Hadzic

rm(list=ls()) #Clears workspace.

setwd("~/Desktop/ProblemSet3") #Sets appropriate working directory.

install.packages(c("doMC"," plyr", "multicore", "foreach")) #Installs necessary packages.

#The code below loads the necessary packages.
library(doMC)
library(plyr)
library(multicore)
library(foreach)


#Question 1
#Make a three dimensional array with dim=c(20,5,1000) and fill it with random data. 
#Think of this as 1000 random datasets with 20 observations and 5 covariates.

my.array <- array(data = rnorm(n=20*5*1000), dim=c(20,5,1000)) #Creates the array and fills it with random data.

#Question 2
#Here is the vector of covariates

Beta <- matrix(c(1,2,0,4,0), ncol=1)

#Mae a function to create Y values (for a linear model). The Y-values should be a linear 
#combination of the X's plus some normally distributed error. (HINT: rnorm). The output
#should be a 20 by 1000 array.

#The function Y.fun below creates Y values for a linear model, each of which is a linear
#combination of the X's and some normally distributed error.

Y.fun <- function(X, Beta){
  Y = (X %*% Beta) + rnorm(length(X %*% Beta))
}

Y.res <- aaply(.data=my.array, .margins=3, .fun=Y.fun, Beta=Beta) #Including third dimension.
Y.res2 <- t(Y.res) #Transposes Y values. Y.res2 now has appropriate dimensions.

#Question 3
#Run 1,000 regressions across all of this simulated data. Have as the output a 1000 by 6
#matrix of estimated regression coefficients.

