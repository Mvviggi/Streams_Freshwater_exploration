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
#check for duplicates for each table needed: external, field, and domain tables
chla.external<-removeDups(data=alg_algaeExternalLabDataPerSample,
                     variables=variables_20163) #for this to work, package needs to be 'expanded'

alg.dom<-removeDups(data=alg_domainLabChemistry,
                    variables= variables_20163)
alg.field<-removeDups(data=alg_fieldData,
                      variable=variables_20163)

#Firstly, join tables for domain and field data
###need these joined to use for calculating chla in mg/m2: benthic area, field volume, lab volume.
field.dom<- joinTableNEON(alg_domainLabChemistry,
                         alg_fieldData)

#Data QC for field.dom and external df
str(field.dom) #check variable types for each column
summary(field.dom)
skim(field.dom) #check for missing data

str(chla.external)
skim(chla.external)

#join tables external lab and field.dom tables
field.dom.external<- merge.data.frame(field.dom, chla.external, by = "sampleID", all= FALSE)
str(field.dom.external)
skim(field.dom.external)
skim(field.dom.external) %>% summary()

#select columns necessary 
short.field.dom.ext<- field.dom.external %>%
  select(sampleID, parentSampleID, siteID.x, domainID.x,collectDate.x,boutNumber, 
         habitatType,algalSampleType,benthicArea,fieldSampleVolume,domainFilterVolume, analyte, analyteConcentration)

#create one df for only seston samples, and one for periphyton 
seston<-short.field.dom.ext %>%
  filter(algalSampleType == "seston")

periphyton<-short.field.dom.ext[!grepl("SESTON", short.field.dom.ext$parentSampleID),]  #exclude seston samples from algalSampleType col

#check seston and periphyton dataframe
head(seston)
head(periphyton)
View(seston)
str(seston)

#seston table: before plotting- focus on season by year instead of collection dates
#change the collectDate and boutNumber to character- for discrete color visualization grouping
seston$year<- as.character(seston$collectDate.x, "%Y")
seston$boutNumber<- as.factor(seston$boutNumber)

#create new column to distinct bout numbers to season
seston<- seston %>% mutate(season= case_when(
  boutNumber == 1 ~ "spring",
  boutNumber == 2 ~ "summer",
  boutNumber == 3 ~ "fall"))



#######Visualizations for seston#########

##plotting chlorophyll a for seston
seston.plot<- seston %>%
  filter(analyte == "chlorophyll a") %>%
  ggplot(aes(x= season, y = analyteConcentration, color = year, group = year))+
  geom_point(size=3)+
  facet_wrap(~siteID.x, scale = "free") +
  scale_x_discrete(limits = c("spring", "summer", "fall"))
seston.plot + ggtitle("Seston chlorophyll a concentration by season per site")


###Periphyton table


#filter only for chlorophyll/pheophytin analysis Type
periphyton.chla<- periphyton %>%
  filter(analyte == "chlorophyll a") 

#Add new column with calculated periphyton chla biomass

periphyton.chla<- periphyton.chla%>%
  mutate(peri.biomass = ((analyteConcentration * fieldSampleVolume) / benthicArea)/1000)

#check variables to be as factor or character   
periphyton.chla$Date<- as.character(periphyton.chla$collectDate.x, "%Y")
periphyton.chla$boutNumber<- as.character(periphyton.chla$boutNumber)

#Adding column for season
periphyton.chla<- periphyton.chla %>% mutate(season= case_when(
  boutNumber == 1 ~ "spring",
  boutNumber == 2 ~ "summer",
  boutNumber == 3 ~ "fall"))  



##dataframe to summarize by Sampling Date
median.periphyton.chl <- periphyton.chla %>%
  group_by(Date, boutNumber, siteID.x, collectDate.x, algalSampleType, habitatType) %>%
  summarize( median.chlaBiomass = median(peri.biomass))%>%
  arrange(desc(median.chlaBiomass))

str(median.periphyton.chl)


###Visualization for periphyton for all samples

#Including all samples per sampling date-  by algalsampling type
benthic.biomass.plot<- periphyton.chla %>%
  ggplot(aes(season, peri.biomass, color = algalSampleType))+
  geom_point()+
  geom_boxplot()+
  scale_x_discrete(limits = c("spring", "summer", "fall"))+
  facet_grid(Date~siteID.x, scale = "free")

benthic.biomass.plot + labs(title= "Benthic Algal Chlorophyll Biomass for 2022-2023 in five NEON sites",
                      y = 'periphyton chla biomass(mg/m2)',
                      x = 'Sampling bout')


#Visualization for periphyton samples only 2022
#create boxplot by habitatType, sampletype, site and bout
#Boxplot for 2022 algalSampleType, habitatType by site
periphyton.chla.22<- periphyton.chla %>%
  filter(Date == 2022) %>%
  ggplot(aes(season, analyteConcentration, color = habitatType))+
  geom_boxplot()+
  scale_x_discrete(limits = c("spring", "summer", "fall"))+
  facet_grid(algalSampleType ~siteID.x, scale = "free")
periphyton.chla.22+ labs(title = "Benthic Algal Chlorophyll Biomass for 2022 in five NEON sites",
                                y = 'chlorophyll a (mg/m2)',
                                x = 'Sampling bout')
  



#following 2 more hours would work on the following:
#Visualization for median values
#explore variance by sampletype, and habitat types, changes of habitat (dominant/subdominant) over the years.
#would explore the periphyton variability by habitat type.
#change x axis to use season-year factor variable in periphyton
#relationship chla with surface water chem





