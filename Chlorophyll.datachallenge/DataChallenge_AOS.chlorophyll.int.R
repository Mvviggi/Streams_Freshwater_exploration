#periphyton and seston chlorophyll data
#@Maria Viggiano
setwd("C:/Users/mviggiano/Documents/Github/Mvviggi")
#library packages used
library(tidyverse)
library(dplyr)
library(neonUtilities)
library(neonOS)
library(ggplot2)
library(skimr)
#periphyton data product and select sites of interest
dpid ="DP1.20163.001"
site.list = c("CUPE", "GUIL", "LEWI", "POSE", "WALK")
#use neonUtilities to extract data product tables
chlorophyll<- loadByProduct(dpid, site = site.list,
                            startdate="2022-01", enddate="2023-05",
                            package= 'expanded', check.size=F, include.provisional= TRUE)

#add tables in the Global Environment
list2env(chlorophyll, .GlobalEnv)
#check for duplicates
chla.ext<-removeDups(data=alg_algaeExternalLabDataPerSample,
                     variables=variables_20163) #for this to work, package needs to be 'expanded'

#check for duplicates in field and domain tables
alg.dom<-removeDups(data=alg_domainLabChemistry,
                    variables= variables_20163)
alg.field<-removeDups(data=alg_fieldData,
                      variable=variables_20163)

#join tables for domain and field data- need them to use, benthic area, field volume, lab volume.
fielddom<- joinTableNEON(alg_domainLabChemistry,
                         alg_fieldData)

#Data QC for fielddom and external df
str(fielddom)
summary(fielddom)
skim(fielddom)

summary(chla.ext)
skim(chla.ext)

#join tables external lab and fielddom
f.d.e<- merge.data.frame(fielddom, chla.ext, by = "sampleID", all= FALSE)
str(f.d.e)
skim(f.d.e)
skim(f.d.e) %>% summary()


#join tables external lab and fielddom
f.d.e<- merge.data.frame(fielddom, chla.ext, by = "sampleID", all= FALSE)
str(f.d.e)
skim(f.d.e)
skim(f.d.e) %>% summary()

#df for seston samples only
seston<-f.d.e %>%
  filter(algalSampleType == "seston")
##reduce seston table to show necessary columns for seston samples
seston.sm<- seston %>%
  select(siteID.x, collectDate.x, parentSampleID, boutNumber, 
         habitatType, analysisType, fieldSampleVolume, domainFilterVolume, analyteConcentration)
#check seston.sm dataframe
View(seston.sm)
str(seston.sm)

#Seston table: before plotting- change the collectDate and boutNumber to character
seston.sm$year<- as.character(seston.sm$collectDate.x, "%Y")
seston.sm$boutNumber<- as.factor(seston.sm$boutNumber)
seston.median$boutNumber<- as.factor(seston.median$boutNumber)
seston.median$year<- as.character(seston.median$year)

#######Visualizations for seston#########

##plotting for seston
seston.plot<- seston.sm %>%
  ggplot(aes(x= boutNumber, y = analyteConcentration, color = year, group = year))+
  geom_point() +
  facet_wrap(~siteID.x, scale = "free") 
seston.plot + ggtitle(" Seston chlorophyll concentration by bouts and per site")

#realized seston has two replicates per sampling bout as seen in the seston.plot - clean up to get median values.
seston.median<- seston.sm %>%
  group_by(year, siteID.x, boutNumber,parentSampleID ) %>%
  summarize(medianchla= median(analyteConcentration))

#graph including both years 2022 and bout 1 of 2023
seston.Allmed<- seston.median %>%
  ggplot(aes(x= boutNumber, y = medianchla, color= year, fill= year))+
  geom_point(size=1.5,
             shape='square')+
  facet_wrap(~siteID.x, scale = "free")
seston.Allmed + ggtitle("Stream seston chlorophyll-a by boutNumber and year per site")

#plotting by median seston chlorophyll 2022
seston.plotmed22<- seston.median %>% filter(year == 2022) %>%
  ggplot(aes(x= boutNumber, y = medianchla))+
  geom_point(color = "blue")+
  facet_wrap(~siteID.x, scale = "free")
seston.plotmed22


#create separate tables for benthic periphyton and seston
#exclude seston samples
periphyton.chem<- f.d.e[!grepl("SESTON", f.d.e$parentSampleID),] 
#filter only for chlorophyll/pheophytin analysis Type
periphyton.chla<- periphyton.chem %>%
  filter(analysisType == "chlorophyll/pheophytin") 
#select necessary columns
periphyton.chla<- periphyton.chla%>%
  select(domainID.x, siteID.x, collectDate.x, parentSampleID, boutNumber, habitatType, algalSampleType, analyteConcentration, fieldSampleVolume, benthicArea)
#Add new column with calculated periphyton chla biomass

periphyton.chla<- periphyton.chla%>%
  mutate(peri.biomass = ((analyteConcentration * fieldSampleVolume) / benthicArea)/1000)
periphyton.chla
#check variables to be as factor or character   
periphyton.chla$Date<- as.character(periphyton.chla$collectDate.x, "%Y")
periphyton.chla$boutNumber<- as.factor(periphyton.chla$boutNumber)
str(periphyton.chla)
str(periphyton.chla$boutNumber)

####Visualization for periphyton samples only 2022

median.rep.chla<- periphyton.chla %>%
  group_by(domainID.x, siteID.x, Date, parentSampleID, boutNumber, habitatType, algalSampleType) %>%
  summarise(mediansampleID= median(peri.biomass), .groups = "drop")
median.rep.chla$boutNumber<- as.factor(median.rep.chla$boutNumber)
#create boxplot by habitatType, sampletype, site and bout
#Boxplot for 2022 algalSampleType, habitatType by site
benbiomass.sampleType<- median.rep.chla %>%
  filter(Date == 2022) %>%
  ggplot(aes(boutNumber, mediansampleID, color = habitatType))+
  geom_boxplot()+
  facet_wrap(algalSampleType ~siteID.x, scale = "free")
benbiomass.sampleType + labs(title = "Benthic Algal Chlorophyll Biomass for 2022 in five NEON sites",
                                y = 'median chla (mg/m2)',
                                x = 'Sampling bout')
  
#exclude algalsample type- only D04 sites
benbiomass.sampleType<- median.rep.chla %>%
  filter(siteID.x %in% c('CUPE', 'GUIL')) %>%
  ggplot(aes(boutNumber, mediansampleID, color = habitatType))+
  geom_boxplot()+
  facet_wrap(Date~siteID.x , ncol= 4,  scale = "free")
benbiomass.sampleType + labs(title = "Benthic Algal Chlorophyll Biomass for 2022 in five NEON sites",
                             y = 'median chla (mg/m2)',
                             x = 'Sampling bout')

#Include bout 1 2023- not good 
benbiomass.all<- median.rep.chla %>%
  ggplot(aes(boutNumber, mediansampleID, color = algalSampleType))+
  geom_point()+
  facet_grid(habitatType ~siteID.x +Date, scale = "free")

benbiomass.all + labs(title= "Benthic Algal Chlorophyll Biomass for 2022-2023 in five NEON sites",
                         y = 'median chla (mg/m2)',
                         x = 'Sampling bout')
#following 2 more hours would work on the following:
#change x axis to use season-year factor variable
#rename boutNumbers to season : spring, summer, fall
#summarize by sd
#relationship chla with surface water chem
#colnames rename



