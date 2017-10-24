#Midterm  -- Adarsh Kesireddy

#Problem 2 
branchingProcess <- function(number_of_generations, n, p){
  # negativie chances
  q = 1-p;
  
  #Create matrix
  X = c(1, rep(0,number_of_generations-1));
  
  #
  for(i in 2:number_of_generations){
    X[i]=sum(rbinom(X[i-1],n,p));
  }
  
  return(X);
}

branchingProcess(40,5,0.25)
graph <- replicate(100, branchingProcess(40,5,0.25))
matplot(graph)
colSums(graph)
mean(colSums(graph))
summary(graph)
sapply(graph,mean)
mean(graph)

#Problem 3 
