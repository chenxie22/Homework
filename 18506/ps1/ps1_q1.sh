#!/bin/bash  

## Title: Stats 506, F18, Problem Set 1, ps1_q1.sh
## Author: Chen Xie (chenxie@umich.edu)
## Due date: Oct 1st, 2018
##
## Data: RECS 2015 data

#############################################
## Part A                                  ##
#############################################

### i. ###
# return number of  rows for regions in RECS 2015 data set
cat recs2015_public_v3.csv |
  awk -F, '{if($2~/3/) {print $2}}' |wc -l  
  
### ii. ###
# return a subdataset of RECS 2015 data set
# containing only the variables:DOEID, NWEIGHT, and 
# BRRWT1-BRRWT96
# write the compressed data set into the file
# recs_15_sub.csv

cat recs2015_public_v3.csv |
  cut -d, -f 1,475-571>recs_15_sub.csv

#############################################
## Part B                                 ##
#############################################

### i. ###
# a for loop to count and print
# the number of observations with each region

for i in 1 2 3 4
do
cat recs2015_public_v3.csv |
  awk -F, '{if($2~/'$i'/) {print $2}}' |wc -l
done

### ii. ###
# produce a file region_division.txt
# containing unique combinations of values 
# from REGIONC and DIVISION

cat recs2015_public_v3.csv |
  awk -F, 'NR==1{print$2 $3};NR > 1{print $2 $3|"sort -V"}' |
  uniq>region_division.txt