#data Challenge
#find changes in chemical elements 

#set working directory
getwd()
setwd("C:/Users/mviggiano/Documents/GitHub/Mvviggi")
library(neonUtilities)
library(devtools)
library(tidyverse)
library(dplyr)
library(ggplot2)

#download data
dpid<- 'DP1.20194.001'
sed<- loadByProduct(dpid,site = c("CUPE", "GUIL"),startdate= '2017-01', enddate= '2018-12',
                    package = "basic",check.size= FALSE,include.provisional= FALSE)
#extract tables from Dpid- external, field data, and dfield data zone
list2env(sed, .GlobalEnv)
#join tables
stat.zone<- merge.data.frame(asc_fieldDataStation,asc_fieldDataZone, by = "startDate")
point.zone<- merge.data.frame(asc_fieldDataPoint, asc_fieldDataZone, by ="startDate")

#create an external data df with desired columns

element<-asc_externalLabData 
element <- element%>%
  select(siteID, namedLocation, startDate, sampleID, analyte, analyteConcentration, analyteUnits) 
element<- na.omit(element)

#new df with selected elements from analyte column
filter.elem<- filter(element, analyte %in% c("Mg","Hg","Pb","Cd","Al", "Cu","Ca","Fe", "Mg","K", "Mn", "Ni"))  
str(filter.elem)
#Rename namedLocation to simple station 1 and station 2
filter.elem$namedLocation<- as.factor(filter.elem$namedLocation)

                                      
#visualization for chemical sediments for years before and after Hurricane Maria
Mg <- filter.elem %>%
  filter(analyte == c("Mg"))%>%
ggplot(aes(x=startDate, y=analyteConcentration))
Mg + geom_point(aes(color = stationID))+
  facet_wrap(~siteID)+
  labs(x= "collection Date", 
          y = " Mg Concentration in mg/kg")
Mg <- filter.elem %>%
  filter(analyte == c("Mg"))%>%
  ggplot(aes(x=startDate,
             geom_bar(aes(y= analyteConcentration, fill = namedLocation))))
Mg + geom_bar(identity = namedLocation)+
  facet_wrap(~siteID)+
  labs(x= "collection Date", 
       y = " Mg Concentration in mg/kg")
#boxplot by site ID
Fe <- filter.elem %>%
  filter(analyte == c("Fe"))%>%
  ggplot(aes(x= startDate, y = analyteConcentration, color= namedLocation)) +
  geom_point(aes(shape=siteID), size = 3)+
  geom_line()+
  labs(title= "Iron", 
       x= "collectDate",
       y = "Fe concentration in mg/kg")
Fe
Al <- boxplot(analyteConcentration~siteID,
              data=filter.elem,
              subset=analyte=="Al",
              xlab="siteID", ylab="Al mg/kg ")

Al <- filter.elem %>%
  filter(analyte == c("Al"))%>%
  ggplot(aes(x= startDate, y = analyteConcentration, color= namedLocation)) +
  geom_point(aes(shape=siteID), size = 3)+
  geom_line()+
  labs(title= "Aluminum", 
       x= "collectDate",
       y = "Al concentration in mg/kg")
Al

Hg<-filter.elem %>%
  filter(analyte == c("Hg"))%>%
  ggplot(aes(x= startDate, y = analyteConcentration, color= namedLocation)) +
  geom_point(aes(shape=siteID), size = 3)+
  geom_line()+
  labs(title= "Mercuy", 
       x= "collectDate",
       y = "Hg concentration in ng/kg")
Hg

many<- filter.elem %>%
  filter(analyte == c("Al", "Fe", "Ca", "K", "Mg", "Mn", "Pb"))%>%
  ggplot(aes(x= analyte, y = analyteConcentration, color= siteID, fill=siteID)) +
  geom_point(aes(shape=namedLocation), size = 3)+
  geom_line()+
  facet_grid(analyte + year)+
  labs(title= "Multiple elements", 
       x= "collectDate",
       y = "analyte concentration in  mg/kg")
many


##################
#examine the field physical parameters collected for QC
str(point)
point$pointNumber<-as.factor(point$pointNumber)
point$zoneNumber<-as.character(point$zoneNumber)
physico<- group_by(point, sedimentSampleID, zoneNumber)
meanDO<- point %>%
  group_by(siteID, startDate, sedimentSampleID, zoneNumber)%>%
  summarise(avg= mean(dissolvedOxygen))
meanSPC<- point %>%
  group_by(startDate, sedimentSampleID, zoneNumber)%>%
  summarise(avg= mean(specificConductance))
ggplot(meanDO, aes(x=startDate, y=avg, fill = zoneNumber)) +
  geom_point()+
  facet_wrap(~siteID)


meandepth<- point %>%
  group_by(zoneNumber, sedimentSampleID)%>%
  summarise(avg= mean(waterDepth))

