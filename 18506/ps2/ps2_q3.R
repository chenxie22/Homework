## Problem Set 2, Question 3
## Stats 506, Fall 2018
## Author: Chen Xie, chenxie@umich.edu
##
## 2005-2006 NHANES oral health data is available at:
## https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT
## demographic data is available at:
## https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT
##

# Libraries:
library(tidyverse)
library(Hmisc)

# Data: 
df_oral=sasxport.get(
  "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT")

df_demo=sasxport.get(
  "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT")


############
## Part a ##
############
# Merge the datasets by seqn:
df_merdt=merge(df_demo,df_oral,by='seqn')

# Decode functions:
## also callapse levels
decode_tooth=function(x){
  switch (x,
          "Primary","Permanent","",
          "Missing", "Permanent")}

decode_all_tooth=function(x){
  sapply (x,decode_tooth)}

decode_gender=function(x){
  switch (x,
          "Male","Female")}

decode_all_gender=function(x){
  sapply (x,decode_gender)}


decode_race=function(x){
  switch (x,
          "Mexican","Other",
          "White",
          "Black", "Other"
          )}

decode_all_race=function(x){
  sapply (x,decode_race)}


############
## Part b ##
############
# Manipulate data for regression:
df_glm=df_merdt %>%
  select(tooth=ohx04htc,age=ridagemn,
         gender=riagendr, race=ridreth1,
         pir=indfmpir) %>%
  mutate(tooth = ifelse(tooth==9, NA, tooth)) %>%
  filter(!is.na(tooth)) %>%
  mutate(response=as.numeric(!tooth==1)) %>%
  mutate(tooth=decode_all_tooth(tooth))

# Logistic regression between age and tooth:
## the original regression
## exclude missing values
df_age=df_glm %>%
  filter(!is.na(age))

## BIC = 1533.407
fit_age = glm(response~age,data=df_age,
            family=binomial(link='logit'))

# Estimate the ages at p=c(0.25,0.5,0.75):
## in the nearest month
coef_age=coef(fit_age)

age_est=function(x){
  (log(x/(1-x))-coef_age[1])/coef_age[2]
}
age_et =round(age_est(c(0.25,0.5,0.75)))

# Choose a range of representative age values:
## in year
age_rg=seq(floor(age_et[1]/12),
           ceiling((age_et[3]/12)),by=1)


############
## Part c ##
############
# Add "gender" to the model:
## result: BIC = 1542.055, drop "gender"
df_gd=df_age %>%
  mutate(gender=(decode_all_gender(gender)),
         gd=as.numeric(gender=="Male"))

fit_gd=glm(response~age+gd,data=df_gd,
          family=binomial(link='logit'))

# Add race to the model:
## using the "White" as the reference 
## create indicators: Mexican, Black, Other
df_race=df_age %>% 
  mutate(race=decode_all_race(race)) %>%
  mutate(Mexican=as.numeric(race=="Mexican"),
         Black=as.numeric(race== "Black"),
         Other=as.numeric(race=="Other"))

## Add "Black" to the model:
### result: BIC = 1529.281, retain "Black"
fit_rc_1=glm(response~age+Black,data=df_race,
             family=binomial(link='logit'))
## Add "Mexican" to the model:
### result: BIC = 1533.103, drop "Mexican"
fit_rc_2=glm(response~age+Black+Mexican,
             data=df_race,
             family=binomial(link='logit'))
## Add "Other" to the model:
### result: BIC = 1536.103, drop "Other"
fit_rc_3=glm(response~age+Black+Other,
             data=df_race,
             family=binomial(link='logit'))
## End of adding race to the model 
fit_rc=fit_rc_1

# Add "pir" to the model:
## result: BIC = 1462.895, retain "pir"
df_pir=df_race %>%
  filter(!is.na(pir))

fit_pir=glm(response~age+Black+pir,
           data=df_pir,
           family=binomial(link='logit'))

## The final regression model:
## logit tooth ~ age + Black +pir
fit_fl=fit_pir
df_fl=df_pir %>%
  select(response,age,Black,pir)


############
## Part d ##
############

#########
## (1) ##
#########
# Compute adjusted predictions at the mean 
## at each of the representative ages
## using the final regression model in Part c
adj_at_mean= 
  tibble(age=age_rg*12) %>%
  mutate(pir=mean(df_fl$pir),
         Black=mean(df_fl$Black))
m1 = predict(fit_fl, adj_at_mean,type='response', se=TRUE)
adj_at_mean=adj_at_mean %>%
  mutate(age=age/12,fit=m1$fit, 
         se=m1$se.fit,
         lwr=fit - 2*se, upr=fit + 2*se)

#########
## (2) ##
#########
# Compute the marginal effects at mean:
## of the retained "Black",
## at each of the representative ages 
mg_at_mean=
  tibble(age=rep(age_rg*12, each=2),
         pir = rep(mean(df_fl$pir), 10),
         Black=rep(c(0,1),5)) 
mg_at_mean=mg_at_mean %>%
  mutate(fitted = predict(fit_fl,mg_at_mean,
                          type='response')) %>%
  spread(Black,fitted) %>%
  select(age,non_Blk=`0`,Blk=`1`) %>%
  mutate(diff=Blk-non_Blk,age=age/12)

#########
## (3) ##
#########
# Compute the average marginal effects:
## of the retained "Black",
## at each of the representative ages
n=nrow(df_fl)
mg_avg= 
  tibble(age = rep(age_rg*12, each=2*n),
         Black = rep(rep(c(0,1),each=n),5),
         pir = rep(df_fl$pir, 10))
mg_avg = mg_avg %>% 
  mutate(fitted = predict(fit_fl, mg_avg,
                          type='response')) %>%
  group_by(age,Black) %>% 
  summarise(avg =mean(fitted)) %>%
  tidyr::spread(Black,avg) %>%
  select(age,non_Blk=`0`,Blk=`1`) %>%
  mutate(diff=Blk-non_Blk) %>%
  ungroup %>%
  mutate(age=age/12)


