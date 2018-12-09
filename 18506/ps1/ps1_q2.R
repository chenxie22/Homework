## Title: Stats 506, F18, Problem Set 1, ps1_q2.R
## Author: Chen Xie (chenxie@umich.edu)
## Due date: Oct 1st, 2018
##
## Data: flights originating in New York City, NY(NYC) in 2013 and 2014

# Libraries:
library(nycflights13)
library(tidyverse)
library(dplyr)

# Obtain or restore data:
flights_13 = flights
flights_14 = readr::read_delim (
  "https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv"
  ,delim=',')

#########################################################################
## Part "a"                                                            ##
## Aim: filter the airlines responsible for at least 1% of the flights ##
##      departing any of the three NYC airports between January 1 and  ##
##      October 31, 2013.                                              ##
#########################################################################

# Data in 2013
# compute the sum and percents of every airline,
# filter the airlines for the aim:
airlines_13=flights_13 %>%
  select(month, day,carrier) %>%
  filter(month <= 10) %>%
  group_by(carrier) %>%
  summarise(count_13=n()) %>%
  mutate(total_13=sum(count_13),
         percent_13=count_13/total_13*100) %>%
  filter(percent_13>=1)

# filter and match the airline names: 
airlines_name=airlines%>%
  filter(carrier %in% airlines_13$carrier)

# match the airports names: 
airports_name=airports%>%
  filter(faa %in% c('LGA','EWR','JFK')) %>%
  select(airports_name=name,origin=faa)

# return final result for part "a"
# only include the airline names
airlines_a=airlines_13 %>%
  left_join(airlines_name) %>%
  transmute(airlines=name,percent=percent_13) %>%
  arrange(desc(percent))

#########################################################################
## Part "b"                                                            ##
## Aim: compare the number and percent of annual flights in the first  ##
##      10 months of 2013 and the first 10 months of 2014, also        ##
##      include percents for each year with 95% CI, and change in      ##
##      percent with 95% CI.                                           ##                         
#########################################################################

# Data in 2014
# compute the sum and percents of every airline (same in part "a"),
# filter the airlines from part "a":
airlines_14=flights_14 %>%
  select(month,day, carrier) %>%
  filter(month<=10) %>%
  group_by(carrier) %>%
  summarise(count_14=n()) %>%
  mutate(total_14=sum(count_14),
         percent_14=count_14/total_14*100) %>%
  filter(carrier %in% airlines_13$carrier) 

# combine the datasets of 2013 and 2014,
# compute CI's:
airlines_cb=left_join(airlines_name,airlines_13) %>%
  left_join(airlines_14) %>%
  select(-carrier) %>%
  mutate(se_13=percent_13*(100-percent_13)/total_13,
         lower_ci_13=percent_13-qnorm(0.975)*sqrt(se_13),
         upper_ci_13=percent_13+qnorm(0.975)*sqrt(se_13),
         se_14=percent_14*(100-percent_14)/total_14,
         lower_ci_14=percent_14-qnorm(0.975)*sqrt(se_14),
         upper_ci_14=percent_14+qnorm(0.975)*sqrt(se_14),
         per_dif=percent_14-percent_13,
         se_df=se_13+se_14,
         lower_ci_df=per_dif-qnorm(0.975)*sqrt(se_13+se_14),
         upper_ci_df=per_dif+qnorm(0.975)*sqrt(se_13+se_14),
         num_df=count_14-count_13
         ) 

# return final table for part "b"
# only include airline names, number of flights, 
# and percents for each year with 95% CI,
# and change in percent with 95% CI
airlines_b=airlines_cb %>%
  select(airlines=name,count_13,percent_13,
         lower_ci_13,upper_ci_13,
         count_14,percent_14,lower_ci_14,
         upper_ci_14,num_df,
         per_dif,lower_ci_df,upper_ci_df) %>%
  arrange(desc(num_df))

#########################################################################
## Part "c"                                                            ##
## Aim: Among of the three NYC airports, compute the percent of        ##
##      flights each airline is responsible for in 2013. And among the ##
##      airlines from part "a", also include the confidence intervals  ##
##      for the estimated percents. Also figure out the largest        ##
##      carrier at each airports.                                      ##
#########################################################################

# compute the percent of flights of each airline in 2013, 
# also include the 95% CI for the estimated percent,
# group by the airports:
airports_prop_13=flights_13 %>%
  select(month,day,origin,carrier) %>%
  group_by(origin,carrier) %>%
  summarise(counts=n()) %>%
  mutate(percent=counts/sum(counts)*100,
         se=percent*(100-percent)/sum(counts),
         lower_ci=percent-qnorm(0.975)*sqrt(se),
         upper_ci=percent+qnorm(0.975)*sqrt(se)) 

# spread to a better format for 2013
airports_sp_13=airports_prop_13 %>% 
  select(origin,carrier,percent) %>%
  spread(carrier,percent)

# limit to the airlines in part "a" for 2013,
# also match the names of airlines and airports:
airports_lmt_13=airports_prop_13 %>%
  filter(carrier %in% airlines_13$carrier) %>%
  left_join(airlines_name) %>%
  left_join(airports_name) %>%
  select(origin,airports=airports_name,airlines=name,counts:upper_ci) 

# filter the largest carrier at each airport for 2013:
airports_max_13= airports_lmt_13 %>%
  filter(percent==max(percent))
