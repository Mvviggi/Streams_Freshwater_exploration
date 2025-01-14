#Fish case study including provisional data
##shrimp bycatch analysis in CUPE
setwd("C:/Users/mvvb8/Documents/GitHub/Fisheries")
library(lubridate)
library(ggplot2)
library(neonUtilities) 
library(neonOS)
library(tidyr)
library(dplyr)
library(devtools)
library(tidyverse)
library(dplyr)
library(stringr)
library(stats)
#bring the data product into your directory
fish <-loadByProduct(dp="DP1.20107.001", site=c("CUPE", "GUIL"), 
                     startdate="2017-01", enddate="2024-03",
                     package= 'basic', check.size=F, include.provisional = TRUE)

#pull data table to Environment
#add tables in the Global Environment
list2env(fish, .GlobalEnv)

cupeshrimp<-fsh_invertBycatch%>%
  filter(siteID =="CUPE")

write.csv(cupeshrimp, "C:/Users/mvvb8/Documents/GitHub/Fisheries/cupeshrimp.csv")
cupeshrimp_cleaned<-read.csv("C:/Users/mvvb8/Documents/GitHub/Fisheries/cupeshrimp_cleaned.csv")
