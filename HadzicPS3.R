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

my.array <- array(rnorm(20*5*1000), dim=c(20,5,1000)) #Creates the array and fills it with random data.

#Question 2
#Here is the vector of covariates

Beta <- matrix(c(1,2,0,4,0), ncol=1)

#Mae a function to create Y values (for a linear model). The Y-values should be a linear 
#combination of the X's plus some normally distributed error. (HINT: rnorm). The output
#should be a 20 by 1000 array.

Y.fun <- function(X, Beta){                     #The Y.fun function generates appropriate Y values.
  Y = (X %*% Beta) + rnorm(length(X %*% Beta))
}

Y.res <- aaply(.data = my.array, .margins = 3, .fun=Y.fun, Beta=Beta) #Applies Y.fun over the third dimension.
Y.res2 <- t(Y.res) #Transposes Y values. Y.res2 now has appropriate dimensions.

#Question 3
#Run 1,000 regressions across all of this simulated data. Have as the output a 1000 by 6
#matrix of estimated regression coefficients.

regression.1000 <- function(i, Y, X){   #regression.1000 runs the necessary regressions and obtains the coefficients.
  lm(Y[,i] ~ X[,,i])$coefficients
}

coefficients <- laply(.data=1:1000, .fun=regression.1000, Y.res2, my.array) #applies regression.1000 and stores the 
#coefficients as a 1000 by 6 matrix.

#Question 4
#Create a density plot for each of the 6 coefficients (each of which should have been estimated 1,000)
#times.) What does this distribution represent?




