## Title: Stats 506, F18, Problem Set 1, ps1_q3.R
## Author: Chen Xie (chenxie@umich.edu)
## Due date: Oct 1st, 2018
##
## Data: RECS 2015 data

# Libraries:
library(tidyverse)
library(dplyr)

# Obtain or restore data:
recs_15 = readr::read_delim (
  "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv"
  ,delim=',')

#########################################################################
## Functions and Variables will be used                                ##
#########################################################################

# functions of decode divisions:
decode_division=function(x){
  switch (x,
          "New England","Middle Atlantic","East North Central",
          "West North Central", "South Atlantic","East South Central",
          "West South Central","Mountain North","Mountain South","Pacific"
          )}

decode_all_division=function(x){
  sapply (x,decode_division)}


# functions of walltypes:
decode_wall_type=function(x){
  switch (x,
          'Brick','Wood','Siding','Stucco','Shingle (composition)',
          'Stone','Concrete or concrete block','','Other'
          )}

decode_all_wall_type=function(x){
  sapply(x,decode_wall_type)}


# functions of decode urbantypes,
# combine "Urban Area" and "Urban Cluster" into "Urban"
decode_urban=function(x){
  switch (x,
          'U'='Urban','C'='Urban','R'='Rural'
  )}

decode_all_urban=function(x){
  sapply (x,decode_urban)
}


# variable vector of replicate weights, and the fianl weight
vars_wt=names(select(recs_15,Weight=NWEIGHT,BRRWT1:BRRWT96))


# funtion of computing BRR se
BRR_se=function(x){
  sqrt(4*sum((x-x[1])^2)/96)
}

#########################################################################
## Part "a"                                                            ##
## Aim: compute percent of homes have stucco as the major outside      ##
##      material within each division, and figure out the highest and  ##
##      lowest proportion. Also compute se for the estimates.          ##
#########################################################################

# compute estimate of percent of walltypes for each division, 
# filter the "Stucco" percent,
# using final weight NWEIGHT:
wall_div_prop=recs_15 %>%
  transmute(Division=DIVISION,Walltype=WALLTYPE,Weight=NWEIGHT) %>%
  mutate(
    Division=decode_all_division(Division),
    Walltype=decode_all_wall_type(Walltype)
    ) %>%
  group_by(Division,Walltype) %>%
  summarise(Walls=sum(Weight)) %>%
  mutate(Wall_prop=Walls/sum(Walls)*100) %>%
  filter(Walltype=='Stucco')

# compute se of estimated percents for each divison:
# using replicate weights BRRWT1-BRRWT96
wall_div_prop_se=recs_15 %>%
  select(Division=DIVISION,Walltype=WALLTYPE,
         Weight=NWEIGHT,BRRWT1:BRRWT96) %>%
  mutate(
    Division=decode_all_division(Division),
    Walltype=decode_all_wall_type(Walltype)
  ) %>%
  group_by(Division,Walltype) %>%
  summarise_at(vars_wt,sum) %>%
  mutate_at(vars_wt,.funs=funs(./sum(.)*100)) %>%
  filter(Walltype=='Stucco')

# attach the BRR se
wall_div_prop$BRR_se=apply(wall_div_prop_se[,3:99],1,BRR_se)

# finally combine the estimates and se for
# percentage of homes of stucco for each division
wall_div_prop=wall_div_prop %>%
  arrange(desc(Wall_prop))

#########################################################################
## Part "b"                                                            ##
## Aim: compute average total electricity usage in kilwatt hors for    ##
##      each division. And also for each urban type. Also compute se   ##
##      for the estimates.                                             ##
#########################################################################

# compute estimate of average of electricity usage for each division, 
# using final weight NWEIGHT:
elec_div_avg=recs_15 %>%
  transmute(Division=DIVISION,elec_use=KWH,Weight=NWEIGHT) %>%
  mutate(
    Division=decode_all_division(Division),
    elec_weight=elec_use*Weight
    ) %>%
  group_by(Division) %>%
  summarise(elec_div_avg=sum(elec_weight)/sum(Weight))

# compute estimate of average of electricity usage for each urban type, 
# using final weight NWEIGHT:
elec_urban_avg=recs_15 %>%
  transmute(Urbantype=UATYP10,elec_use=KWH,Weight=NWEIGHT) %>%
  mutate(
    Urbantype=decode_all_urban(Urbantype),
    elec_weight=elec_use*Weight
  ) %>%
  group_by(Urbantype) %>%
  summarise(elec_urban_avg=sum(elec_weight)/sum(Weight))

# compute se of estimated average for each divison:
# using replicate weights BRRWT1-BRRWT96
elec_div_avg_se=recs_15 %>%
  select(Division=DIVISION,elec_use=KWH,Weight=NWEIGHT,BRRWT1:BRRWT96) %>%
  mutate(Division=decode_all_division(Division)) %>%
  group_by(Division) %>%
  summarise_at(vars_wt,.funs = funs(sum(elec_use*.)/sum(.)))

# attach the BRR se
elec_div_avg$BRR_se=apply(elec_div_avg_se[,2:98],1,BRR_se)

# compute se of estimated average for each urban type:
# using replicate weights BRRWT1-BRRWT96
elec_urban_avg_se=recs_15 %>%
  select(Urbantype=UATYP10,elec_use=KWH,Weight=NWEIGHT,BRRWT1:BRRWT96) %>%
  mutate( Urbantype=decode_all_urban(Urbantype)) %>%
  group_by(Urbantype) %>%
  summarise_at(vars_wt,.funs = funs(sum(elec_use*.)/sum(.)))

# attach the BRR se
elec_urban_avg$BRR_se=apply(elec_urban_avg_se[,2:98],1,BRR_se)


#########################################################################
## Part "c"                                                            ##
## Aim: compute disparity between urban and rural areas in terms of    ##
##      the proportion of homes with internet access for each          ##
##      division. Also compute se  for the estimates.                  ##
#########################################################################

# compute estimates of disparity of proportion 
# between urban and rural area for each division,
# using final weight NWEIGHT:
int_div_prop_diff=recs_15 %>%
  transmute(Division=DIVISION,Urbantype=UATYP10,
            Internet=INTERNET,Weight=NWEIGHT) %>%
  mutate(
    Division=decode_all_division(Division),
    Urbantype=decode_all_urban(Urbantype),
    int_weight=Internet*Weight
    ) %>%
  group_by(Division,Urbantype) %>%
  summarise(int_prop=sum(int_weight)/sum(Weight)*100) %>%
  tidyr::spread(Urbantype,int_prop)%>%
  mutate(int_prop_diff=Urban-Rural) 

# compute se of estimated proportion difference for each divison:
# using replicate weights BRRWT1-BRRWT96
int_div_prop_diff_se=recs_15 %>%
  select(Division=DIVISION,Urbantype=UATYP10,
         Internet=INTERNET,Weight=NWEIGHT,BRRWT1:BRRWT96) %>%
  mutate(
    Division=decode_all_division(Division),
    Urbantype=decode_all_urban(Urbantype)
  ) %>%
  group_by(Division,Urbantype) %>%
  summarise_at(vars_wt,.funs=funs(sum(Internet*.)/sum(.)*100)) %>%
  summarise_at(vars_wt,diff) %>%
  mutate_at(vars_wt,abs)

# attach the BRR se
int_div_prop_diff$BRR_se=apply(int_div_prop_diff_se[,2:98],1,BRR_se)

# arrange in the descending order
int_div_prop_diff=int_div_prop_diff %>%
  arrange(desc(int_prop_diff))



