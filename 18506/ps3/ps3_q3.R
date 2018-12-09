## Problem Set 3, Question 3
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu
##
## 'mtcars' data is in the R package "datasets".

############
## Part a ##
############
# Translate using data.table

# Libraries:
library(data.table)
library(magrittr)

# Change to data.table and select interested variable:
mtcars_dt=
  data.table(mtcars)[,.(mpg,cyl,disp,hp,wt)] 

# Compute 'beta-hats' os disp,hp,and wt within cyl:
beta_dt= mtcars_dt[,`:=`(disp_gc = disp - mean(disp),
                          hp_gc = hp - mean(hp),
                          wt_gc = wt - mean(wt)),by=cyl] %>%
  .[,.(dispXmpg = sum(mpg*disp_gc), vdisp = var(disp_gc),
       hpXmpg = sum(mpg*hp_gc), vhp = var(hp_gc),
       wtXmpg = sum(mpg*wt_gc), vwt = var(wt_gc),
       n =.N ), by=cyl] %>%
  .[,`:=`(beta_cyl_disp = dispXmpg / {vdisp*{n-1}},
          beta_cyl_hp = hpXmpg / {vhp*{n-1}},
          beta_cyl_wt = wtXmpg / {vwt*{n-1}})] %>%
  .[order(cyl)]

# Export values
fwrite(beta_dt,file='mpg_betas_by_cyl_dt.csv')

############
## Part b ##
############
# Function: compute univariate coefficient,
#           dt is the dataset
#           x denotes the independent variable, 
#           y denotes the dependent variable,
#           grp denotes the group variable:
beta_dt_fn=function(dt,x,y,grp){
  
  # Check if the data set is.data.table,
  # if not, Change to data.table:
  if (!is.data.table(dt)) dt=data.table(dt)
  
  # Select interested variables:
  beta=
    dt[,c(x,y,grp),with=F] 
  
  setnames(beta, c("x","y","grp") )
  
  beta = beta[,`:=`(x_gc = x - mean(x)),by=grp] %>%
    .[,.(xXy = sum(y*x_gc), vx = var(x_gc),
         n =.N ), by=grp] %>%
    .[,`:=`(beta_grp_x = xXy / {vx*{n-1}}),] %>%
    .[order(grp)]
  
  setnames(beta, 
           c(grp,paste(x,y,sep='X'),
             paste0('v',x),'n',
             paste('beta',grp,x,sep='_'))) 
  
  return(beta)
}

############
## Part c ##
############
# Use the dplyr verb summarize_at

# Library:
library(dplyr)

# Select the indepent variables:
vars = c('disp','hp','wt')

# Compute 'beta-hats' os disp,hp,and wt within cyl:
beta_dplyr=mtcars %>%
  select(mpg, cyl, disp, hp, wt) %>%
  group_by(cyl) %>%
  mutate_at(vars , funs(.-mean(.))) %>%
  
  # Compute by summarize_at:
  summarize_at(vars,
               funs(sum(mpg*.),var,n())) %>%
  mutate(
    beta_cyl_disp = disp_sum / {disp_var*{disp_n-1}},
    beta_cyl_hp = hp_sum/ {hp_var*{hp_n-1}},
    beta_cyl_wt = wt_sum / {wt_var*{wt_n-1}},
    n=disp_n
  ) %>%
  select(-disp_n,-hp_n,-wt_n)

# Export values
readr::write_csv(beta_dplyr, path = 'mpg_betas_by_cyl_dplyr.csv')

############
## Part d ##
############
# Function: compute univariate coefficient,
#           dt is the dataset
#           x denotes the independent variable, 
#           y denotes the dependent variable,
#           grp denotes the group variable:
beta_dplyr_fn=function(df,x,y,grp){
  
  # Quote the inputs:
  grp = enquo(grp)
  x=enquo(x)
  y=enquo(y)
  
  # Compute:
  beta=df %>%
    select(!!x,!!y,!!grp) %>%
    group_by(!!grp) %>%
    mutate(x_gc=!!x-mean(!!x)) %>%
    summarize(xXy=sum(!!y*x_gc),vx=var(x_gc),n=n()) %>%
    mutate(beta=xXy/{vx*(n-1)}) %>%
    select(!!grp,beta)
  return(beta)
}



