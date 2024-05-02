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
install.packages("ecocomDP")
library(ecocomDP)

options(stringsAsFactors=F)
#data product Id for macroinvertebrates
dpid= "DP1.20120.001"
#load by product for year 2017-early 2018
inverts <-loadByProduct(dpid, site=c("CUPE", "GUIL"), 
                        startdate="2016-01", enddate="2024-04",
                        package= 'basic', check.size=F, release = 'current')
list2env(inverts, .GlobalEnv)
inverts.tax<-removeDups(data=inv_taxonomyProcessed,
                        variables=variables_20120)
inverts.field<-removeDups(data=inv_fieldData,
                          variables= variables_20120)

fieldtax<- joinTableNEON(inv_fieldData,
                         inv_taxonomyProcessed)

str(fieldtax.short)
fieldtax.short<-fieldtax %>%
  select(siteID, collectDate.x, eventID, boutNumber, habitatType , samplerType, benthicArea, scientificName, order)
fieldtax$boutNumber<- as.factor(fieldtax$boutNumber)

#create new column to distinct bout numbers to season
fieldtax.short<- fieldtax.short %>% mutate(season= case_when(
  boutNumber == 1 ~ "spring",
  boutNumber == 2 ~ "summer",
  boutNumber == 3 ~ "fall"))
fieldtax.short<- fieldtax.short %>%
  mutate(season= factor(season, levels= c("spring", "summer", "fall")))


fieldtax.short$Date<- as.Date(fieldtax.short$collectDate.x, format ="%d-%b-%Y")
fieldtax.short$Year<- format(fieldtax.short$Date, "%Y")
fieldtax.short$Year<- as.factor(fieldtax.short$Year)





# Add column to Order_count Df to identify collectDate by year only
########################################



##table with count total of organisms per site per date
count <- fieldtax.short %>%
  group_by(siteID, Bout, habitatType, Year, season)%>%
  count()
str(Order_count)
summary(Order_count)
skim(Order_count)
# #create new column for Year
# Order_count$Year<- as.character(Order_count$collectDate.y, "%Y")
# #make season and Year as factor levels
# season_levels<- c("spring", "sumer", "fall")
# Order_count$season<-factor(Order_count$season, levels = season_levels)
# year_levels<- c("2016", "2017", "2018","2019", "2020", "2021")
# Order_count$Year<- as.factor(Order_count$Year, levels = year_levels)
# Order_count$Year<- as.factor(Order_count$Year)

# #new column joining season and year
# Order_count2 <- unite(Order_count, col="Bout", c('season', 'Year'), sep ='-')
Order_count.rmNA<- Order_count %>% drop_na(order)

#total tally of organisms by Order level  by habitat Type, year, season, site
abundance.bout<- Order_count %>%
  group_by(siteID, Bout, habitatType, Year, season)%>%
  tally(n)  
#remove NA from Year column
Order_count.dropNA<- Order_count %>% drop_na(order)
#new column to join season-year
Totalinverts<- totalinverts%>%
  mutate(unite, col = "Bout", c('season', 'Year'), sep ='-')
str(Totalinverts$Bout)
#make the column Bout to keep unique order. as factor
Totalinverts$Bout<-factor(Totalinverts$Bout, levels= unique(Totalinverts$Bout))
#visualization of total invertebrates by collectionDate and habitat type per site                                
totalserie<- ggplot(data = abundance.bout, aes(x = Year, y= n, color= season)) +
  geom_point()+
  geom_line()+
  scale_x_discrete()+
 facet_wrap(~siteID, nrow= 2, scale = "free")
totalserie

totallines<- ggplot(data = abundance.bout, aes(x = Year, y = n, color=habitatType , group= season)) +
geom_point()+
  geom_line()+
  scale_x_discrete()+
  facet_wrap(~siteID, nrow= 2, scale = "free")
totallines

str(tax_SiteOrder)
#filter only for EPT order - no Plecotera found, using Diptera instead
EPTcount<- Order_count %>%
  filter(order %in% c("Trichoptera", "Ephemeroptera", "Diptera"))

#visualization of order count by site throughout a year of sampling before and after hurricane
#ordering seasons in spring, summer fall

plot1<- ggplot(data = EPTcount,
               aes(x= season, y=n)) +
  scale_x_discrete(limits = c("spring", "summer", "fall"))+
  geom_bar(aes(fill = Year), stat= "identity", position = "dodge2")+
  facet_wrap(~siteID)
plot1
#######################
#separate collectDate  into month and year
# Totalinverts$date<- as.character(Totalinverts$collectDate.y, format = '%M-%Y')
# Order_count$date<- as.character(Order_count$collectDate.y, format = '%M-%Y')
#convert factor variable into numerit
tax_SiteOrder <- tax_SiteOrder %>% mutate (month= as.numeric(as.character(month)))


