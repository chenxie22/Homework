## Problem Set 4, Question 2 Functions
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu
##
## Functions copied from PS3 Q2


# Function to generate p*mc_rep pvalue Matrix
pval_beta = function(X, beta, sigma = 1, mc_rep = 1e3){
  # Arguments:
  #   X : an n by p numeric matrix
  #   beta: a p by 1 numeric matrix
  #   sigma: std deviation for Y|X,  Y|X ~ N(XB, sigma^2 I)
  #   mc_rep: The number of Monte Carlo replications to use
  #
  # Output: A p by mc_rep matrix of p-values
  
  QR = qr( crossprod(X) )
  QX = X %*% qr.Q(QR) 
  XtXinv = solve( qr.R(QR), t( qr.Q(QR) ))
  
  n = nrow(X)
  p = ncol(X)
  
  # generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)
  
  # estimate betas 
  b = solve(qr.R(QR), crossprod( QX, Y ) )
  
  # compute residual stanard errors
  s_sq = colSums( {Y - as.numeric(X %*% b)}^2 / {n - p})
  
  # standard error of b
  v = sqrt( diag(XtXinv) * rep(s_sq, each = p) )
  
  # return a matirx of p-values
  # Use pt to replicate lm, but the normal approximation is fine here. 
  matrix( 2*pt( abs( b / v ), df = {n-p}, lower.tail = FALSE ), p, mc_rep )  
}


# Function to evaluate
evaluate = function(P, tp_ind, alpha = .05){
  P = P < alpha
  
  p = nrow(P)
  n = ncol(P)
  
  # Compute TP, FP, TN, FN for each replcation
  TP = colSums(P[tp_ind, ])
  FP = colSums(P[-tp_ind,])
  TN = colSums(!P[-tp_ind,])
  FN = colSums(!P[tp_ind,])
  
  # Call FDR 0 when no discoveries. 
  P = FP + TP
  fdr = ifelse(P > 0, FP  / {FP + TP}, 0)
  fwer = mean( FP > 0 )
  sens = TP / {TP + FN}
  spec = TN / {FP + TN}
  
  list( fwer = fwer, fwer_se = sqrt(fwer*{1-fwer} / n), 
        fdr =  mean(fdr), fdr_se = sd(fdr) / sqrt(n),
        sens = mean(sens), sens_se = sd(sens) / sqrt(n),
        spec = mean(spec), spec_se = sd(spec) / sqrt(n)
  )
}
