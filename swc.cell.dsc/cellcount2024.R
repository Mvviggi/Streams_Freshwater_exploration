#cell counts over a year of sampling
#data visualization with ggplot##
library(tidyverse)
library(ggplot2)
library(neonUtilities)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(stats)
library(neonOS)
library(plotly)

options(stringsAsFactors=F)
dpid= "DP1.20138.001"
cc<-loadByProduct(dpid,site=c("CUPE", "GUIL"), 
                  startdate="2017-01", enddate="2024-01",
                  package= 'basic', check.size=F, release = 'current')
list2env(cc, .GlobalEnv)
cellcount<-removeDups(data=amc_cellCounts,
                        variables=variables_20138)
fieldcell<-removeDups(data=amc_fieldCellCounts,
                          variables= variables_20138)
joincc<-joinTableNEON(amc_cellCounts,
                      amc_fieldCellCounts)
str(joincc)
parentfield<- joinTableNEON(amc_fieldCellCounts,
                            amc_fieldSuperParent)


datacc<- select(joincc, siteID, collectDate.x, cellCountSampleID, totalCellCount, 
                rawMicrobialAbundance, cellCountSampleVolume,cellCountPreservantVolume)

datacc$date<-as.Date((datacc$collectDate.x), format ="%d-%b-%Y") #add column from m-year


#filter only for CC samples
onlyCC<- datacc%>% drop_na(rawMicrobialAbundance)
str(onlyCC) 

#Columns with calulations for MicrobialAbundance/mL and log10(MicrobialAbundance/mL)
MicroAbundance<- onlyCC %>%
  mutate(microAbMl=(rawMicrobialAbundance *(cellCountSampleVolume +cellCountPreservantVolume)/cellCountSampleVolume))

MicroAbundance<- MicroAbundance %>%
  mutate(log10Abundance= log10(microAbMl))

#convert Year column to factor
MicroAbundance$Year <-format(MicroAbundance$date, "%Y")
MicroAbundance$Month <-format(MicroAbundance$date, "%b")
MicroAbundance$Year<- as.factor(MicroAbundance$Year)
MicroAbundance$Month<- as.factor(MicroAbundance$Month)
MicroAbundance <- MicroAbundance %>%
  mutate(Month = factor(Month, levels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec")))
#reduce table with selected columns
ShortAbundanceDF<- MicroAbundance %>%
  select(siteID, date, microAbMl, Year, Month, log10Abundance)
str(ShortAbundanceDF)
str(MicroAbundance)
#MicroAbundance <- subset(MicroAbundance, select = -c(MnY)) #remove column

#plotting all years sites together, facet wrap:
{
MicAbund.years <-ggplot(data = ShortAbundanceDF, aes(x=Month, y= microAbMl, color= Year, group = Year))+
  geom_point(size =3)+
  geom_line(size=1) +
  theme(axis.text.x = element_text(hjust = 1))+
  facet_wrap(~siteID, nrows = 2, scale = "free")
  
MicAbund.years +
  labs(title = "Microbial Abundance per mL per month from 2017 to 2022")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(hjust = 1, size= 14),
        axis.text.y = element_text(size= 14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))


MicAb.CUPEbar + theme(axis.text=element_text(size = 12),
                      axis.title=element_text(size=16))

#using dataframe with outliers excluded. 
rmOutlier.DMS<-dsc.combo.all[dsc.combo.all$microAbMl<400000,]
MicAbund.years_outliers <-ggplot(data = rmOutlier.DMS, aes(x=Month, y= microAbMl, color= Year, group = Year))+
  geom_point(size =3)+
  geom_line(size=1) +
  theme(axis.text.x = element_text(hjust = 1))+
  facet_wrap(~siteID, nrows = 2, scale = "free")

MicAbund.years_outliers  +
  labs(title = "Microbial Abundance per mL per month from 2017 to 2022")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(hjust = 1, size= 14),
        axis.text.y = element_text(size= 14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))



}

#subset by siteID
{
CUPEmicrobe<- ShortAbundanceDF[ShortAbundanceDF$siteID == "CUPE",]
GUILmicrobe<- ShortAbundanceDF[ShortAbundanceDF$siteID == "GUIL",]
str(CUPEmicrobe)

summary(short)

#check for NA values
sum(is.na(CUPEmicrobe))

#plotting 
MicAb.CUPE<- CUPEmicrobe %>%
  ggplot(aes(x=Month, y= log10Abundance))+
  geom_point(size =4)+
  geom_smooth(size=1) +
  facet_wrap(~Year, nrow = 2, scale = "free")
  # scale_x_date(date_breaks = "1 month",
  #              date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "CUPE Microbial abundance over time")+
  theme(plot.title = element_text(hjust =0.5))
MicAb.CUPE + theme(axis.text=element_text(size = 12),
              axis.title=element_text(size=16))
  
  

MicAb.GUIL<- GUILmicrobe %>%
  filter(siteID == c("GUIL"))%>%
  ggplot(aes(x=date, y= log10Abundance, color= Year, group = Year))+
  geom_point(size =4)+
  geom_line(size=1) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "GUIL Microbial abundance over time")+
  theme(plot.title = element_text(hjust =0.5))
MicAb.GUIL + theme(axis.text=element_text(size = 12),
                   axis.title=element_text(size=16))
 

#different visualization
#plotting bar plos
MicAb.CUPEbar<- ggplot(data = CUPEmicrobe, aes(x=Month, y= microAbMl))+
              geom_bar(aes(fill = Year), stat= "identity", position = "dodge2")
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              labs(title = "CUPE Microbial abundance by month from 2017 to 2022")+
              theme(plot.title = element_text(hjust =0.5))
MicAb.CUPEbar + theme(axis.text=element_text(size = 12),
                      axis.title=element_text(size=16))

MicAb.GUILbar<- ggplot(data = GUILmicrobe, aes(x=Month, y= microAbMl))+
    geom_bar(aes(fill = Year), stat= "identity", position = "dodge2")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "GUIL Microbial abundance by month from 2017 to 2022")+
    theme(plot.title = element_text(hjust =0.5))
MicAb.CUPE + theme(axis.text=element_text(size = 12),
                   axis.title=element_text(size=16))

}



#plotting by year only monthly microbial Abundance per mL
{MicroAbunplot17 <- ShortAbundanceDF%>%
    filter(Year == c("2017")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot17  +
    theme(axis.text.x = element_text(hjust = 1, size= 12),
          axis.text.y = element_text(size= 12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2021 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
  
  MicroAbunplot18 <- ShortAbundanceDF%>%
    filter(Year == c("2018")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot18 +
    theme(axis.text.x = element_text(hjust = 1, size= 12),
          axis.text.y = element_text(size= 12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2018 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
  
  MicroAbunplot19 <- MicroAbundance%>%
    filter(Year == c("2019")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot19 +
    theme(axis.text.x = element_text(hjust = 1, size= 12),
          axis.text.y = element_text(size= 12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2019 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
  
  MicroAbunplot20 <- ShortAbundanceDF%>%
    filter(Year == c("2020")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot20 +
    theme(axis.text.x = element_text(hjust = 1, size= 12),
          axis.text.y = element_text(size= 12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2020 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
  
  MicroAbunplot21 <- ShortAbundanceDF%>%
    filter(Year == c("2021")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot21 +
    theme(axis.text.x = element_text(hjust = 1, size= 12),
          axis.text.y = element_text(size= 12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2021 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
  
  MicroAbunplot22 <- ShortAbundanceDF%>%
    filter(Year == c("2022")) %>%
    ggplot(aes(Month, microAbMl, color = siteID, group = siteID))+
    geom_point()+
    geom_line()
  MicroAbunplot22 + theme(axis.text.x = element_text(hjust = 1, size= 12),
                          axis.text.y = element_text(size= 12),
                          axis.title.x = element_text(size=12),
                          axis.title.y = element_text(size=12))+
    labs(title = "Microbial abundance for 2022 at both sites")+
    theme(plot.title = element_text(hjust =0.5))
}



rm(g)
rm(gg)
rm(icroAbunplot)
rm(ingestTableShipQuery)
rm(listOfSampleClasses)
rm(appReactives)
