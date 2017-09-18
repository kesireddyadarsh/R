# Problem 1: Central Limit Theorm

n = 5  #number of random variables
#n = 20 #number of random variables

min_interval_value = 0  #Lowest value for interval
max_interval_value = 1  #highest value for interval
number_of_simulations = 1000 #NUmber of simulations

#generate 1000 values This are random numbers generated using in built runif function
uniform_random_numbers = runif(number_of_simulations,min_interval_value,max_interval_value)
#exponential_random_numbers = rexp(number_of_simulations,lambda)
#hist(uniform_random_numbers) #plot 
hist(exponential_random_numbers)

#Setting values
lambda = 0.2 
Averages = NULL
#for (i in 1 : s) Averages = c(Averages, mean(rexp(n,lambda)))
for (i in 1 : s) Averages = c(Averages, mean(runif(n,min_interval_value,max_interval_value)))
hist(Averages)

# Compare the sample and theoretical mean and variance for this distribution 
#Theorectical Mean
1/lambda
#Sample Mean
mean(Averages)
#Theorectical Variance
((1/lambda)^2)/n
#Sample Variance
var(Averages)

############################################################################
# Normal Distribution and Simulation Results together on one plot

hist(Averages, freq=FALSE, breaks=40, main = "Uniform Distribution and Simulation Results", 
     xlab = "Averages of Exponential Samples", ylab = "Density of Averages", col = "light blue")

# Density of the Simulated sample means
lines(density(Averages),col="blue", lty=2, lwd =2)

# Theoretical Mean - Red Line
abline(v=1/lambda,col='red',lwd=2)

# Sample Mean - Blue Line
abline(v=mean(Averages), col='blue', lwd=2)

# Theoretical density of the exponential distribution (normal density)
xfit <- seq(min(Averages), max(Averages), length=100)
yfit <- dnorm(xfit, mean=1/max_interval_value, sd=(1/(12*n)))
#xfit <- seq(min(Averages), max(Averages), length=100)
#yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(n)))
lines(xfit, yfit, pch=22, col="red", lty=2, lwd=2)

# Legend
legend('topright', c("Simulation", "Theoretical"), 
       col=c("blue", "red"), lty=c(1,1))


#Problem 3:
n = 5  #number of random variables
#n = 20 #number of random variables

number_of_simulations = 1000 #NUmber of simulations

#generate 1000 values This are random numbers generated using in built runif function
binomial_random_numbers = dbinom(number_of_simulations,n,prob = 0.5);

#plot 
hist(binomial_random_numbers)
