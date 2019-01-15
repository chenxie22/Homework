## Problem Set 4, Question 2 Part c
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu


# Libraries:
library(future)
library(doParallel)

# Source: 
source('./ps4_q2_funcs.R')

# Get command line arguments and assign as global variables
# Use to assign "n_cores" ,"mc_rep", and "sigma"
ca = commandArgs(trailingOnly = TRUE)
ind = grep('=', ca)  
args_list = strsplit(ca[ind], '=')
lapply(args_list,function(x) assign(x[1],as.numeric(x[2]),
        envir=.GlobalEnv))

# Parameters:
n = 1e3; p = 1e2; r = .1
beta = c( rep(0.1, floor(r*p)), rep(0, p - floor(r*p)) )
dim(beta)=c(p,1)

# Write function:
myFun=function(rho,sigma=1,mc_rep=1e4){
  
  # X ~ N(0, Sigma): 
  Sigma = beta %*% t(beta) *rho
  
  # Make the diagonals of Sigma be 1:
  diag(Sigma)=1
  R = chol(Sigma)
  
  # Here is an X for testing: 
  X = matrix( rnorm(n*p), n, p) %*%  R
  
  # Get the monte carlo p-value matrix:
  P=pval_beta(X, beta,sigma=sigma, mc_rep=mc_rep)
  
  # Compute Monte Carlo estimate and se of metrics of 
  # different multiple comparison methods:
  result =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  
  # Return the result:
  return(result)
}

# Cluster setup:
cl = makeCluster(n_cores)
registerDoParallel(cl)

# Parallel computations using `plan(multicore)`:
plan(multicore)

# Set seed:
set.seed(1022)

# Parallel computations using future:
results = list()
for( i in 1:7 ){
  results [[i]]= future({myFun(rho=0.25*i-1,sigma=sigma,mc_rep=mc_rep)})
}

# Cluster close:
stopCluster(cl)

# Get the values:
results=matrix(unlist( lapply(results, value) ),nrow = 8,ncol=28)
est=c(results[c(1,3,5,7),]) ; se=c(results[c(2,4,6,8),])

# Create values of other columns:
rho=rep(0.25*c(-3:3),each=16)
sigmalist=rep(sigma,112)
method=rep(rep(c('holm', 'bonferroni', 'BH', 'BY'),each=4),7)
metric=rep(c('FWER', 'FDR', 'Sensitivity', 'Specificity'),28)

# Store the result into a data.frame: 
results_q4c = data.frame('rho'= rho, 'sigma'=sigmalist, 
                         'metric'=metric, 'method'=method, 
                         'est'=est,  'se'=se)
results_q4c