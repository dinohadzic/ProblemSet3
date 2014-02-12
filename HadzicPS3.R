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

system.time(coefficients <- laply(.data=1:1000, .fun=regression.1000, Y.res2, my.array)) #Re-running some
#code from Question 3, not in parallel.  Although the measures vary somewhat between different runs,
#user typically registers at between 1.020 and 1.050, system is always very low, and elapsed is also
#between 1.020 and 1.050 usually.

registerDoMC(cores=4) #Enables parallel run over 4 cores.

system.time(coefficients <- laply(.data=1:1000, .fun=regression.1000, Y.res2, my.array, .parallel=TRUE))
#Interestingly, when run in parallel, the same code registers considerably higher measures for user and
#system, but a consistenly lower one for elapsed.  User varies greatly, as does system to a lesser extent.
#Elapsed generally regsters at approximately 0.8, which is noticeably lower than it was when the code
#was not run in parallel.


#Part 2: Calculating Fit Statistics

#Question 1
#Using the Out of step dataset, randomly subset the data into two partitions. Use one partition 
#(your "training set") to build at least three statistical mdoels where incumbent vote share is 
#the dependent variable.
#Using these models, make "predictions" for the partition of the data you did NOT use to fit the
#model.  This is your "test" set.

incumbents <- read.table("http://pages.wustl.edu/montgomery/incumbents.txt", header = T) #Imports the 
#incumbents.txt dataset directly from website and stores it as object incumbents.

nrow(incumbents) #Incumbents has 6687 rows. 

sample.rows <- sample(1:6687, 3344) #Samples randomly 3344 of the rows in the incumbents dataset.

training.set <- incumbents[sample.rows,] #Creates "training set" which contains the randomly sampled rows
#from the incumbents data.  This new dataset has 3344 rows.

test.set <- incumbents[-sample.rows,] #Creates "test set" which contains the remaining rows.  This dataset
#has 3343 rows.

mod1 <- lm(voteshare ~ incspend + chalspend, data = training.set) #mod1 is the first model with the incumbents
#voteshare as the response variable, incumbent and challenger spending as explanatory variables.

pred.1 <- predict(mod1, newdata = test.set) #pred.1 stores the predicted incumbent voteshares using mod1 and
#the test.set data.

mod2 <- lm(voteshare ~ inparty + presvote + unemployed, data = training.set) #mod2 is the second model with the
#incumbent's voteshare as the response variable.  The explanatory variables include whether the incumbent and 
#the presidents are of the same party (inparty), the voteshare of the presidential candidate from the incumbent's 
#party in the previous two elections in the district (presvote), and the number of people unemployed at the district
#level (logged).

pred.2 <- predict(mod2, newdata = test.set) #pred.2 stores the predicted incumbent voteshares using mod2 and 
#the test.set data.

mod3 <- lm(voteshare ~ chalquality + seniority + midterm, data = training.set) #mod3 is the third model with the
#incumbent's voteshare as the response variable.  The explanatory variables include whether the challenger has 
#previously won an elected position (chalquality), the number of terms the incumbents has served in congress (seniority),
#and whether the election is a midterm election (midterm).

pred.3 <- predict(mod3, newdata = test.set) #pred.3 stores the predicted incumbent voteshares using mod3 and the
#test.set data.


