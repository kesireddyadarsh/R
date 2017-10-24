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
hyperexpo <- function(number_of_samples,p_1,p_2,lambda_1,lambda_2){
  #X_1 with lambda_1
  X_1 = rexp(number_of_samples,lambda_1);
  hist(X_1);
  #X_2 with lambda_2
  X_2 = rexp(number_of_samples,lambda_2);
  hist(X_2);
  #equation
  Y = p_1*X_1 + p_2*X_2;
  hist(Y)
  #pdf of exponential distribution lambda*exp(-lambda*x)
  pdf_X_1 = lambda_1*exp((-lambda_1)*X_1);
  pdf_X_2 = lambda_2*exp((-lambda_2)*X_2);
  #hist(pdf_X_1);
  #hist(pdf_X_2);
  Y_pdf = p_1*pdf_X_1+p_2*pdf_X_2;
  hist(Y_pdf);
  return(Y);
}

value <-hyperexpo(5000,0.3,0.7,1,0.1)
hist(value)
