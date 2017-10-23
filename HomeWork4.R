#problem 14
# 1. all rec 2. all rec 3. first two rec third tran 4. first two rec last two trans
# Install the markovchain package in R
library(markovchain)
library(Matrix)

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
gim<-function(p,N){
  q = 1-p; #Find other half
  data_values <-matrix(0,N,N); #Zero Matrix
  #Required Matrix
  for(i in 1:N){
    sub_i = i - 1;
    add_i = i+1;
    if(i == 1){
      data_values[i,add_i] = q;
    }else if(i == N){
      data_values[i,sub_i] = p;
    }else{
      data_values[i,add_i]=q;
      data_values[i,sub_i]=p;
    }
  }
  print(data_values);
  
  #Transpose Matrix
  transpose_data_values <- t(data_values);
  print(transpose_data_values);
  
  #Create a idenitiy matix
  Identity_matrix = diag(N);
  #I-data_values;
  new_matrix = Identity_matrix - data_values;
  #find inverse of above
  inverse_matrix = solve(new_matrix);
  return(inverse_matrix);
}

matrix <- gim(0.4,5)

