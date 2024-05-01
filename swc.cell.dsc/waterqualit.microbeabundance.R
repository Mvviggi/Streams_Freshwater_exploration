##Nutrients and microbes

getwd() #this gives you where the working directory is currently
setwd("C:/mviggiano/Documents/GitHub/Mvviggi")
#Script to calculate median nutrient by month/year for Aquatic sites. Plot sites monthly by year.
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(neonUtilities)
library(tidyr)
library(dplyr)
install.packages("lubridate",repos="http://cran.us.r-project.org")
library(lubridate)

options(stringsAsFactors=F)
data.swc<-loadByProduct(dpID="DP1.20093.001",
                        site=c("CUPE", "GUIL"),
                        startdate="2017-01", enddate="2023-06",
                        package= 'basic', check.size=F, release = 'current')
list2env(data.swc, .GlobalEnv)

joinFieldswc<- joinTableNEON(swc_fieldSuperParent,
                        swc_fieldData)

str(swc_externalLabDataByAnalyte)
head(joinFieldswc)
#no need to join external lab and field tables

swc.lab<-data.swc$swc_externalLabDataByAnalyte

#reduce dataframe for swc.lab with needed columns only
short.swc.lab<- swc.lab %>%
  select(siteID, collectDate, analyte, analyteConcentration,sampleCondition)
#checks
str(short.swc.lab)

#create colunm for date in as.Date
short.swc.lab$date<-as.Date((short.swc.lab$collectDate), format ="%d-%b-%Y")

#remove rows with analyte concentrtion as NA
short.swc.lab<- short.swc.lab%>% drop_na(analyteConcentration)

#convert Year column to factor
short.swc.lab$Year <-format(short.swc.lab$date, "%Y")
short.swc.lab$Month <-format(short.swc.lab$date, "%b")
short.swc.lab$Year<- as.factor(short.swc.lab$Year)
short.swc.lab$Month<- as.factor(short.swc.lab$Month)
short.swc.lab <- short.swc.lab %>%
  mutate(Month = factor(Month, levels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec")))

#check for na values
sum(is.na(short.swc.lab))
summary(short.swc.lab)
which(is.na(short.swc.lab$analyteConcentration))

sum(is.na(ShortAbundanceDF))

#joining water lab analytes and microbes into one single dataframe
swc.microbes<- merge(ShortAbundanceDF,short.swc.lab,  by= "date", all.x = TRUE)


##renaming the date to Date to merge by this column with discharge data
swc.microbes <-rename(swc.microbes,  Date = date )

##create column from analyteconcentration as log10
swc.microbes<- swc.microbes %>%
  mutate(log10analyte= log10(analyteConcentration))
str(swc.microbes)
##reduce columns and renamce columns
swc.microbes<- swc.microbes %>%
  select(Date, siteID.x, microAbMl, Year.x, Month.x, log10Abundance, collectDate, analyte, sampleCondition, log10analyte)
colnames(swc.microbes) <- c("Date", "siteID", "microAbMl", "Year", "Month", 
                            "log10Abundance", "collectDate", "analyte", "sampleCondition", "log10analyte")
sum(is.na(swc.microbes))
#divide by siteID-- not yet

tp.rows

##dataframes by analyte
swc.tn.<-short.swc.lab[short.swc.lab$analyte=='TN',]
swc.toc<-short.swc.lab[short.swc.lab$analyte=='TOC',]
swc.tp<-short.swc.lab[short.swc.lab$analyte=='TP',]
swc.spc<-short.swc.lab[short.swc.lab$analyte=='specificConductance',]
swc.plot<-short.swc.lab[short.swc.lab$analyte %in% c('TN','TP', 'specificConductance', 'TSS'),]
swc.tss<-short.swc.lab[short.swc.lab$analyte=='TSS',]

#tp.rows<-swc.plot$analyte=='TP' #rows with TP data
#spc.rows<- swc.plot$analyte=='specificConductance'
##dataframes by analyte and microbes
swc.tn.mic<-swc.microbes[swc.microbes$analyte=='TN',]
swc.toc.mic<-swc.microbes[swc.microbes$analyte=='TOC',]
swc.tp.mic<-swc.microbes[swc.microbes$analyte=='TP',]
swc.spc.mic<-swc.microbes[swc.microbes$analyte=='specificConductance',]
swc.plot.mic<-swc.microbes[swc.microbes$analyte %in% c('TN','TP', 'specificConductance', 'TSS'),]
swc.tss.mic<-swc.microbes[swc.microbes$analyte=='TSS',]

tp.rows<-swc.plot$analyte=='TP' #rows with TP data
spc.rows<- swc.plot$analyte=='specificConductance'

summary(swc.tn)

###
##swc.plot$analyteConcentration[tp.rows]<-swc.plot$analyteConcentration[tp.rows]*10 #multiply TP by 10
#swc.plot<-swc.lab[swc.lab$analyte %in% c('TN','TOC','TP'),]
### ggplot nutrients only
{
  ggplot(data=swc.plot,aes(x=collectDate,y=analyteConcentration,color=analyte,group=analyte))+
  geom_point()+
  geom_line()+
  facet_wrap(~siteID.x, nrow=2, scale= "free")
str(swc.plot)
ggplot(data=swc.plot,aes(x=collectDate,y=analyteConcentration,color=analyte,group=analyte))+
  geom_point()+
  geom_line()+
  facet_wrap(~siteID, nrow=2, scale= "free")+
  labs(y='TN and TOC (mg/L)',x='Collection Date',title='CUPE and GUIL Nutrients')+
  scale_y_continuous(sec.axis = sec_axis(~.*5,name='TP (mg/L)'))+ #create a second y axis that is 10x
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year", date_minor_breaks = "1 month") 
swc.plot$collectDate<- as.Date(swc.plot$collectDate)
swc.plot<- na.omit(swc.plot)
}

sum(is.na(swc.tn.mic))
#plotting microbes and TN

{
#remove NA values 
LogTNmicrobePlot<-swc.tn.mic %>%
  ggplot(aes(x= log10analyte, y=log10Abundance))+
  geom_line(color="blue")+
  scale_x_date(date_breaks= "1 month",
               date_labels="%b-%Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size= 12),
        axis.text.y = element_text(size= 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))+
  labs(title = "Microbial abundance and TN  over time")+
  theme(plot.title = element_text(hjust =0.5))+
  facet_wrap(~siteID.x, ncol= 1, scale="free")
  
LogTNmicrobePlot +
  geom_line(aes(y=analyteConcentration), color= "red") +
  scale_y_continuous(breaks = seq(0,8,1),
                     name = "log10 of Microbial Abundance per mL",
                     sec.axis = sec_axis(trans = ~. *0.2 ,
                                         breaks= seq(0,3,0.2),
                                         name= "Total Nitrogen mg/L"
                                         )
                     )+
  theme(axis.title.y.left = element_text(color= "blue"),
        axis.title.y.right = element_text(color= "red"))

}

#plotting microbes and Tss dry mass in log10
TssmicrobePlot<-swc.tss %>%
  ggplot(aes(x= Date, y=log10Abundance))+
  geom_line(color="blue")+
  scale_x_date(date_breaks= "1 month",
               date_labels="%b-%Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size= 12),
        axis.text.y = element_text(size= 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))+
  labs(title = "Microbial abundance and TSS  over time")+
  theme(plot.title = element_text(hjust =0.5))+
  facet_wrap(~siteID.x, ncol= 1, scale="free")

TssmicrobePlot +
  geom_line(aes(y=log10analyte), color= "red") +
  scale_y_continuous(#breaks = seq(0,8,1),
                     name = "log10 of Microbial Abundance per mL",
                     sec.axis = sec_axis(trans = ~./2,
                                         #breaks= seq(0,9,1),
                                         name= "Total Suspended Sediments mg/L"
                     )
  )+
  theme(axis.title.y.left = element_text(color= "blue"),
        axis.title.y.right = element_text(color= "red"))


#linear regressions
TNlinerPlot<- swc.tn.mic %>%
  ggplot(aes(x=log10analyte, y= log10Abundance, color= Year, group = Year))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x)+
  facet_wrap(~siteID, scale = "free")
TNlinerPlot
  

TsslinerPlot<- swc.tss %>%
  filter(analyteConcentration < c(20)) %>%
  ggplot(aes(x= analyteConcentration, y=log10Abundance))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x)+
  facet_wrap(~siteID, scale = "free")
TsslinerPlot

TNlinerPlot<- swc.tn.mic %>%
  ggplot(aes(x=log10analyte, y= log10Abundance))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x)+
  facet_wrap(~siteID.x, scale = "free")
TNlinerPlot

print(swc.toc.mic)

