## Problem Set 4, Question 2 Part a
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu


# Libraries:
library(parallel)

# Source the functions:
source('./ps4_q2_funcs.R')

# Parameters:
n = 1e3; p = 1e2; r = .1
beta = c( rep(0.1, floor(r*p)), rep(0, p - floor(r*p)) )
dim(beta)=c(p,1)

# Write function:
myFun=function(rho,sigma=1){
  
  # X ~ N(0, Sigma): 
  Sigma = beta %*% t(beta) *rho
  
  # Make the diagonals of Sigma be 1:
  diag(Sigma)=1
  R = chol(Sigma)
  
  # Here is an X for testing: 
  X = matrix( rnorm(n*p), n, p) %*%  R
  
  # Get the monte carlo p-value matrix:
  P=pval_beta(X, beta,sigma=sigma, mc_rep=1e4)
  
  # Compute Monte Carlo estimate and se of metrics of 
  # different multiple comparison methods:
  result =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  
  # Return the result:
  return(result)
}

# Check the random number stream： #
RNGkind()
RNGkind("L'Ecuyer-CMRG")
RNGkind()

# Set seed:
set.seed(1022)

# Parallel computing using mclappy:
results=mclapply(0.25*c(-3:3),myFun)

# Reorganize the results:
est=NULL; se=NULL
for (i in 1:7){
  for(j in 1:4) {
    est=c(est,results[[i]][[j]][c(1,3,5,7)])
    se=c(se,results[[i]][[j]][c(2,4,6,8)])
    
  }
}

# Create values of other columns:
rho=rep(0.25*c(-3:3),each=16)
sigma=rep(1,112)
method=rep(rep(c('holm', 'bonferroni', 'BH', 'BY'),each=4),7)
metric=rep(c('FWER', 'FDR', 'Sensitivity', 'Specificity'),28)

# Store the result into a data.frame: 
results_q4a = data.frame('rho'= rho, 'sigma'=sigma, 
                         'metric'=metric, 'method'=method, 
                         'est'=unlist(est),  'se'=unlist(se))






