simHIMA2.0<-function(n,p,q,alpha,beta,binaryOutcome=FALSE,seed=123) 
{
  set.seed(seed)
  
  X <- matrix(rnorm(n, mean = 0, sd = 2),n,1) # expoure
  Z <- matrix(rnorm(n*q, mean = 0, sd = 2),n,q) # covariates
  
  mu <- matrix(0,p,1)
  e <- mvrnorm(n, mu, sigma_e)  # the error terms
  
  M <- matrix(0,n,p)
  M <- X%*%(alpha) + Z%*%t(eta) + e  # mediators
  MZX <- cbind(M,Z,X)
  
  beta_gamma <- cbind(beta,delta,gamma)
  E_error <- rnorm(n, mean = 0, sd = 1)
  Y <- MZX%*%t(beta_gamma) + E_error # continuous outcome
  if (binaryOutcome) 
    Y <- matrix(rbinom(n, 1, 1/(1 + exp(-Y))), nrow = n)  # binary outcome
  return(list(Y = Y, M = M, X = X, Z = Z, n = n, p = p))
}