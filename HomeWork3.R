# Install the markovchain package in R
library(markovchain)

###################### Problem 2 ########################
number_of_simulations = 1000;
#number_of_simulations = 10000;
U = runif(number_of_simulations);
a = 2;
b = 4;
Y = a*((-log(1-U))^b);
hist(Y);


###################### Problem 3 ########################
# Dimension (number of states)
N=101;
#p=0.5;
p=0.8;

# P is the Transition matrix - randomly generated
P = matrix(0,N,N)
P = zeros(N,N);
for(i in 1:N) {
  P[i,i+1] = p;
  P[i+1,i] = p;
}
P;

# Double check that rows of P sum to 1
rowSums(P)

# p is the initial distribution - randomly generated
p = runif(N); p = p/sum(p)
p

# Initial condition for M particles transitioning among the N states
M=1
x = rmultinom(1,size=M,prob=p) # random initial condition

# Do many (100, 1000, more?) iterations using P
iters = 100
for(i in 1:iters) {
  xtmp=matrix(0,N,N)
  for(j in 1:N) {
    xtmp[,j] = rmultinom(1,x[j],P[j,]) 
  }
  x<-rowSums(xtmp)
  barplot(x,main="Current Iteration", xlab="States", col="seagreen")
  Sys.sleep(0.1)
}


# Calculate the stationary distribution
v=eigen(P)$vectors[,1]
v = v/sum(v)
v