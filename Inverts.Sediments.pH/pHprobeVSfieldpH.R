#compare pH sonde values vs pH field meter
library(neonUtilities)
library(ggplot2)
library(tidyverse)
#upload  SWC files from Data portal 
dpid<-"DP1.20093.001" #Water chemistry data product code
data.swc<-loadByProduct(dpID=dpid,
                        site=c("CUPE", "GUIL"),
                        startdate="2022-01", enddate="2023-06",
                        package= 'basic', check.size=T)
getwd()
setwd("C:/Users/mviggiano/OneDrive - National Ecological Observatory Network/Documents/R/r scripts")
