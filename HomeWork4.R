#problem 14
# 1. all rec 2. all rec 3. first two rec third tran 4. first two rec last two trans
# Install the markovchain package in R
library(markovchain)

# First define the transition matrix
tmA <- matrix(c(0,0.5,0.5,0.5,0,0.5,0.5,0.5,0), nrow=3, byrow=TRUE);
tmA

# Create a discrete time Markov chain (DTMC)
dtmcA <-new("markovchain", transitionMatrix=tmA, states=c("a","b","c"), name="Markov Chain A");
dtmcA

# Display the graph (uses the igraph package)
plot(dtmcA)

# First define the transition matrix
tmA <- matrix(c(0,0,0,1,0,0,0,1,0.5,0.5,0,0,0,0,1,0), nrow=4, byrow=TRUE);
tmA

# Create a discrete time Markov chain (DTMC)
dtmcA <-new("markovchain", transitionMatrix=tmA, states=c("a","b","c","d"), name="Markov Chain A");
dtmcA

# Display the graph (uses the igraph package)
plot(dtmcA)

# First define the transition matrix
tmA <- matrix(c(0.5,0,0.5,0,0,0.25,0.5,0.25,0,0,0.5,0,0.5,0,0,0,0,0,0.5,0.5,0,0,0,0.5,0.5), nrow=5, byrow=TRUE);
tmA

# Create a discrete time Markov chain (DTMC)
dtmcA <-new("markovchain", transitionMatrix=tmA, states=c("a","b","c","d","e"), name="Markov Chain A");
dtmcA

# Display the graph (uses the igraph package)
plot(dtmcA)

# First define the transition matrix
tmA <- matrix(c(0.25,0.75,0,0,0,0.5,0.5,0,0,0,0,0,1,0,0,0,0,0.3,0.6,0,1,0,0,0,0), nrow=5, byrow=TRUE);
tmA

# Create a discrete time Markov chain (DTMC)
dtmcA <-new("markovchain", transitionMatrix=tmA, states=c("a","b","c","d","e"), name="Markov Chain A");
dtmcA

# Display the graph (uses the igraph package)
plot(dtmcA)

#Algorithm
Gam.sim <- function(p,N){
  q = 1-p;
  for(i in 1:N){
    for(j in 1:N){
      data[i,j]=0;
    }
  }
}

Gam.sim(0.6,10)



