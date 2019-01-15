## Problem Set 4, Question 1
## Stats 506, Fall 2018
##
## Author: Chen Xie chenxie@umich.edu
##
## 'lahman' data is in the R package "Lahman".


# Libraries:
library(tidyverse)
library(dbplyr)
library(Lahman)

# Create a local SQLlite database:
lahman = lahman_sqlite()

# Join the tables
# Sum the hits across stints and select interested columns
# Find the all-time leader for each birth country and order
q1=lahman %>% tbl(sql(
  '
SELECT *
  FROM 
    (SELECT b.playerID playerID,nameFirst First, nameLast Last, 
            debut Debut, birthCountry, sum(H) Hits
      FROM BATTING b
      LEFT JOIN Master m ON m.playerID = b.playerID 
      GROUP BY b.playerID)
  GROUP BY birthCountry
  Having Hits==max(Hits) AND Hits >=200
  ORDER BY -Hits
  ')) %>% collect()



