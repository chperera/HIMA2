library(MASS)
library(HDMT)
library(hdi)

n <- 400 # sample size
p <- 1000 # the dimension of mediators
q<-2 #the number of adjusted covariates

# the regression coefficients alpha (exposure --> mediators)
alpha <- matrix(0,1,p)

# the regression coefficients beta (mediators --> outcome)
beta <- matrix(0,1,p)

# the first five markers are true mediators.
alpha[1:5] <- c(0.20,0.25,0.15,0.30,0.35)
beta[1:5] <- c(0.20,0.25,0.15,0.30,0.35)

alpha[6] <- 0.1
beta[7] <- 0.1

##Regression coefficients eta (covariates --> outcome)
eta <- matrix(0.3,p,q)
## the regression coefficients gamma (exposure --> outcome) 
gamma <- matrix(0.5,1,1)
## the regression coefficients delta (covariates --> mediator)
delta <- matrix(0.5,1,q)

##Correlation matrix
sigma_e <- matrix(0,p,p)
rou <- 0.25  # the correlation of X
for (i in 1:p) {
  for (j in 1:p) {
    sigma_e[i,j]=(rou^(abs(i-j)));
  }
}

# Generate simulation data
simdat = simHIMA2(n, p,q,alpha, beta, seed=1234)

# HIMA2 output
hima2.0.fit <- HIMA2.0(X=simdat$X, Y=simdat$Y, M=simdat$M, Z=simdat$Z)
hima2.0.fit
