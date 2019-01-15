## Problem Set 4, Question 2 Part b
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu


# Libraries:
library(doParallel)
library(iterators)

# Source: 
source('./ps4_q2_funcs.R')

# Set up a cluster:
ncores= 4
cl = makeCluster(ncores)

# Register the cluster:
registerDoParallel(cl)

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

# Set seed:
set.seed(1022)

# Parallel computing using foreach:
results=foreach(i=0.25*c(-3:3), .combine='rbind') %:% 
  foreach(j=c(0.25,0.5,1), .combine='rbind') %do% {
    myFun(rho=i,sigma=j)
  }

# Close cluser
stopCluster(cl)

# Reorganize the results:
est=NULL; se=NULL
for (i in 1:84){
    est=c(est,results[[i]][c(1,3,5,7)])
    se=c(se,results[[i]][c(2,4,6,8)])
    }


# Create values of other columns:
rho=rep(0.25*c(-3:3),each=48)
sigma=rep(rep(c(0.25,0.5,1),each=16),7)
method=rep(rep(c('holm', 'bonferroni', 'BH', 'BY'),each=4),21)
metric=rep(c('FWER', 'FDR', 'Sensitivity', 'Specificity'),84)

# Store the result into a data.frame: 
results_q4b = data.frame('rho'= rho, 'sigma'=sigma, 
                         'metric'=metric, 'method'=method, 
                         'est'=unlist(est),  'se'=unlist(se))

# Save to RData:
save(results_q4b, file = "./results_q4b.RData")
