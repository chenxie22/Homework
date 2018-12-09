## Problem Set 3, Question 1
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu
##
## RECS consumption data is available at:
## https://www.eia.gov/consumption/residential/data/2015/

# Libraries:
library(data.table)
library(magrittr)

# Obtain the data:
recs = fread(
  'https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv'
  )

# Multiplier for confidence level:
m = qnorm(.975)

# Replicate weights:
col=paste('BRRWT',1:96,sep='')
weights=recs[,c('DOEID',col),with=F] %>%
  melt(.,measure = patterns("BRRWT"),
       value.name = 'wt',variable.name='brr')

# Division map:
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)


############
## Part a ##
############
## Stucco: WALLTYPE == 4

# Data manipulation:
stucco= recs[,.(DOEID, 
                division = factor(DIVISION, 1:10, divisions),
                NWEIGHT, 
                WALLTYPE)] 

# Point estimate:
p_stucco= stucco [,
    .(p_stucco=sum( NWEIGHT*{WALLTYPE == 4} ) / sum(NWEIGHT) ),
    by=division]

# Compute standard errors with replicate weight:
se_stucco= merge( stucco, weights, by='DOEID', all = TRUE) %>%
  .[,.(r_stucco=sum(wt*{WALLTYPE == 4} ) / sum(wt) ),
       by=.(division,brr)] %>%
  merge(.,p_stucco, by='division',all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_stucco - p_stucco}^2))),by=division]

# Compute confidence interval:
p_ci_stucco=merge(p_stucco,se_stucco,by='division',all=TRUE) %>%
  .[,`:=`(lwr=pmax(p_stucco - m*se, 0),upr=p_stucco+m*se)] %>%
  .[order(-p_stucco)]


############
## Part b ##
############
# Decoding urbans:
decode_urban=function(x){
  switch (x,
          'U'='Urban','C'='Urban','R'='Rural'
  )}

decode_all_urban=function(x){
  sapply (x,decode_urban)
}

# Data manipulation:
kwh= recs[,.(DOEID, 
             division = factor(DIVISION, 1:10, divisions),
             NWEIGHT, kwh=KWH,
             urban=decode_all_urban(UATYP10)
             )] 

# Point estimates by division:
avg_kwh= kwh [,.(avg_kwh=sum( NWEIGHT*kwh ) / sum(NWEIGHT) ),
                  by=division]

# Compute standard errors with replicate weight:
se_kwh= merge( kwh, weights, by='DOEID', all = TRUE) %>%
  .[,.(r_kwh=sum(wt*kwh ) / sum(wt) ),
    by=.(division,brr)] %>%
  merge(.,avg_kwh, by='division',all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_kwh - avg_kwh}^2))),by=division]

# Compute confidence interval:
avg_ci_kwh=merge(avg_kwh,se_kwh,by='division',all=TRUE) %>%
  .[,`:=`(lwr=avg_kwh - m*se, upr=avg_kwh+m*se)] %>%
  .[order(-avg_kwh)]

# Point estimates by division and urban:
avg_div_urban = kwh [,.(avg_kwh=sum( NWEIGHT*kwh ) / sum(NWEIGHT) ),
              by=.(division,urban)]

# Compute standard errors with replicate weight:
se_div_urban= merge( kwh, weights, by='DOEID', all = TRUE) %>%
  .[,.(r_kwh=sum(wt*kwh ) / sum(wt) ),
    by=.(division,urban,brr)] %>%
  merge(.,avg_div_urban, by=c('division','urban'),all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_kwh - avg_kwh}^2))),by=.(division,urban)]

# Compute confidence interval:
avg_ci_urban=merge(avg_div_urban,se_div_urban,
                   by=c('division','urban'),all=TRUE) %>%
  .[,`:=`(lwr=avg_kwh - m*se,upr=avg_kwh+m*se)] 


############
## Part c ##
############
# Data manipulation:
internet= recs[,.(DOEID, 
                  division = factor(DIVISION, 1:10, divisions),
                  NWEIGHT, internet=INTERNET,
                  urban=decode_all_urban(UATYP10)
                  )] 

# Point estimate of proportions:
p_internet= internet [,.(p_it=sum( NWEIGHT*internet ) / sum(NWEIGHT) ),
                      by=.(division,urban)]

# Compute estiamtes with replicate weight:
r_internet=merge( internet, weights, by='DOEID', all = TRUE) %>%
  .[,.(r_it=sum(wt*internet ) / sum(wt) ), by=.(division,urban,brr)]

# Compute standard errors with replicate weight:
se_internet= merge(r_internet,p_internet, 
                   by=c('division','urban'),all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_it - p_it}^2))),by=.(division,urban)]

# Compute confidence interval of proportions:
p_ci_int=merge(p_internet,se_internet,by=c('division','urban'),all=TRUE) %>%
  .[,`:=`(lwr=pmax(p_it - m*se, 0), upr=p_it+m*se)] 

# Point estimate of difference:
p_diff= dcast(p_internet, division ~ urban,value.var = 'p_it') %>%
  .[,.(p_diff=Urban-Rural),by=division]

# Compute se of difference with replicate weight:
se_diff=dcast(r_internet, division +brr~ urban,value.var = 'r_it') %>%
  .[,.(r_diff=Urban-Rural),by=division] %>%
  merge(.,p_diff, by='division',all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_diff - p_diff}^2))),by=division]

# Compute confidence interval of difference:
p_ci_diff=merge(p_diff,se_diff,by='division',all=TRUE) %>%
  .[,`:=`(lwr=p_diff - m*se,upr=p_diff + m*se)] %>%
  .[order(-p_diff)]


############
## Part d ##
############
# Note: What is the average bedrooms for differenc type of housing units

# Types: 
types = c('Mobile home',
          'Single-family detached house', 
          'Single-family attached house',
          'Apartment in a building with 2 to 4 units',
          'Apartment in a building with 5 or more units')

# Data manipulation:
bedrooms= recs[,.(DOEID, 
             type = TYPEHUQ,
             NWEIGHT, bedroom=BEDROOMS
)] 

# Point estimates by type:
avg_bed= bedrooms [,.(avg_bed=sum( NWEIGHT*bedroom ) / sum(NWEIGHT) ),
              by=type]

# Compute standard errors with replicate weight:
se_bed= merge( bedrooms, weights, by='DOEID', all = TRUE) %>%
  .[,.(r_bed=sum(wt*bedroom ) / sum(wt) ),
    by=type] %>%
  merge(.,avg_bed, by='type',all=TRUE) %>%
  .[,.(se=2 *sqrt( mean( {r_bed - avg_bed}^2))),by=type]

# Compute confidence interval:
avg_ci_bed=merge(avg_bed,se_bed,by='type',all=TRUE) %>%
  .[,`:=`(lwr=avg_bed - m*se, upr=avg_bed+m*se)] %>%
  .[order(type)] %>%
  .[,`:=`(type=factor(type, 1:5, types))]


