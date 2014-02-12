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

par(mfrow=c(2,3))  #Sets the graph display to 3 by 2, so we may view all graphs in one window.

plot.fun <- function(a,b,c,d,e,f){                    #plot.fun takes six inputs, and produces density plots for each.
  plot(density(a), main="Alpha", xlab="Coefficient Value") 
  plot(density(b), main="Beta 1", xlab="Coefficient Value")
  plot(density(c), main="Beta 2", xlab="Coefficient Value")
  plot(density(d), main="Beta 3", xlab="Coefficient Value")
  plot(density(e), main="Beta 4", xlab="Coefficient Value")
  plot(density(f), main="Beta 5", xlab="Coefficient Value")
}

plot.fun(coefficients[,1], coefficients[,2], coefficients[,3], coefficients[,4], coefficients[,5],
         coefficients[,6])                          #Plots the density of the six coefficients.


#Question 5
#Alter your code so that you now collect t-statistics for all 1,000 regressions for all six coefficients.

t.fun <- function(i, Y, X){                       #t.fun extracts t-statistics from the summary table.
  coefficients(summary(lm(Y[,i] ~ X[,,i])))[,3]
}

t.statistics <- laply(1:1000, t.fun, Y.res2, my.array)  #Applying the function over the data and storing
#extracted t-statistics as object "t.statistics."

#Question 6
#For the 1,000 regressions, calculate how many t.statistics are statistically "significant" (p <= .05)
#for each variable. (Make sure you use the right degrees of freedom). Discuss.

sig.t.fun <- function(X){                  
  length(which(X >= qt(0.975, 14) | X <= -qt(0.975, 14)))
}  #sig.t.fun calculates the number of statistically significant t-statistics for each variable
#using the apprpriate significance level (0.05) and the correct number of  degrees of freedom (14).

sig.ts <- apply(t.statistics, 2, sig.t.fun) #Applying function over t.statistics and storing results
#as sig.ts, which provides the number of significant t-statistics for all variables involved.


#Question 7
#Re-run the code in parallel. Using the system.time command, estimate how much time is saved (or not) 
#using the parallel code.
