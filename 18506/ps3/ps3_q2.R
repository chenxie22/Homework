## Problem Set 3, Question 2
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu


############
## Part a ##
############
# Function: accepts matrixes x and beta,
#           monte carlo replication mc_rep,
#           sd of errors sigma,
#           return p-values:
mc_pval=function(x,beta,sigma,mc_rep=1000){
  
  ## i. Find the estimated beta:
  
  ## Simulate errors, and the response y:
  n=dim(x)[1] ; p=dim(x)[2]
  set.seed(1022)
  error=matrix(rnorm(n*mc_rep,0,sigma),
               nrow=n, ncol=mc_rep)
  y=matrix(x %*% beta,nrow=n,ncol=mc_rep)+error
  
  ## QR decomposition to find estimated beta:
  QR = qr(t(x) %*% x)
  beta_hat= solve(qr.R(QR), t(qr.Q(QR)) %*% t(x) %*% y)
  
  ## ii. Compute estimated y and estimated sigma:
  y_hat=x %*% beta_hat
  sigma_hat= matrix(colSums( {y-y_hat}^2 ) / {n-p},nrow=1)
  
  ## iii.Compute the variance of estimated beta:
  var_beta=matrix(diag(solve(qr.R(QR),t(qr.Q(QR)))),ncol=1)
  var=var_beta %*% sigma_hat
  
  ## iv. Return p-values:
  z=beta_hat/sqrt(var)
  2*(1-pnorm(abs(z)))
}


############
## Part b ##
############
# Create the true beta and choose a sigma:
beta=matrix(c(rep(1,10),rep(0,90)),nrow=100)
sigma=10

# Create the matrix X:
n=1000 ; p=100
sigma_X= diag(1,p,p)
R = chol(sigma_X)
X = rnorm(n*p)
dim(X) = c(n, p)
X = X %*% R

# Apply the function to get p-values:
pval_mc=mc_pval(X,beta,sigma,mc_rep=1)

# Simulate the response Y
# set the same seed in the function in part a:
set.seed(1022)
error=matrix(rnorm(n,0,sigma),
             nrow=n, ncol=1)
Y=matrix(X%*% beta,nrow=n,ncol=1)+error

# Get the p-values from lm:
fit=lm(Y~0+X)
pval_lm=summary(fit)$coefficients[,4]

# Compare the p-values with tolernace 1e-3:
all.equal(as.numeric(pval_mc),unname(pval_lm),tolerance=1e-3)
## [1] TRUE


############
## Part c ##
############
# Indices when beta != 0:
ind=which(beta!=0)

# Function: Compute the following quantiles:
#           family wise error rate,  false discovery rate,
#           sensitivity, and the specificity.
evaluate=function(ind,pval,alpha=0.05){
  CP =length(ind) ; CN =nrow(pval)-length(ind)
  PN=pval<=alpha
  P= colSums(PN) ; N=nrow(pval)-P
  
  # Compute true positive, and false positives
  TP = colSums(PN[ind,]) ; FP=P-TP
  
  # Compute the quantiles
  sensitivity= mean(TP/CP) 
  specificity = mean(1-FP/CN)
  fdr = mean(FP/P,na.rm = T)
  fwer= mean(FP>0)
  return(
    c(fwer,fdr,sensitivity,specificity))
}


############
## Part d ##
############
# Computing adjusted p-values:
pval=mc_pval(X,beta,sigma)
pval_bonf=matrix(p.adjust(pval,method='bonferroni'),nrow=p)
pval_holm=matrix(p.adjust(pval,method='holm'),nrow=p)
pval_bh=matrix(p.adjust(pval,method='BH'),nrow=p)
pval_by=matrix(p.adjust(pval,method='BY'),nrow=p)

# Put all estimates in a table:
table=rbind(evaluate(ind,pval),
            evaluate(ind,pval_bonf),
            evaluate(ind,pval_holm),
            evaluate(ind,pval_bh),
            evaluate(ind,pval_by))
