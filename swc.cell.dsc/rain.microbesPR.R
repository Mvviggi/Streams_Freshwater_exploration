##NEONScience: Time Series 03: Cleaning & Subsetting Time Series Data in R - NoData Values & Subset by Date
#setup your working directory
getwd() #this gives you where the working directory is currently
setwd("C:/mviggiano/Documents/GitHub/Mvviggi")

#Download packages
install.packages("lubridate")
install.packages("ggplot2")

library(lubridate)
library(ggplot2)

#name the variables
dpID<- "DP1.00006.001"

#Use neonUtilities to pull data from Portal
precip<- loadByProduct(dpID,site=c("CUPE", "GUIL"), 
              startdate="2018-01", enddate="2023-01",
              package= 'basic', check.size=F, release = 'current')
list2env(precip, .GlobalEnv)

str(SECPRE_30min)
# Create new column with start time convertto to POSIX date time class - ATL time zone
SECPRE_30min$datetime <- as.POSIXct(SECPRE_30min$startDateTime,
                                    format = "%Y-%m-%dT%H:%M",
                                    tz = "Atlantic Standard Time")
#after this expression in line 24, realized the date/time is in local time zone.- ignore

#convert startDateTime to Date variable
SECPRE_30min$startDateTime<- as.Date(SECPRE_30min$startDateTime)
class(CUPEprecip$startDateTime)
str(SECPRE_30min)

#renaming the stratDateTime to merge by this column with microbes data
SECPRE_30min <-rename(SECPRE_30min,  date = startDateTime )

##Calculate Daily Total Precipitation
Dailyprecip<- SECPRE_30min %>%
  group_by(siteID, date) %>%
  summarise(dailyRain=sum(secPrecipBulk))

#view Rows with NA values
Dailyprecip[is.na(Dailyprecip$dailyRain),]

#check for NA values
sum(is.na(Dailyprecip))


#remove rows with NA values from Dailyprecip df
Dailyprecip<- na.omit(Dailyprecip, cols =c(dailyRain))

#subset by siteID
CUPEdaily<- Dailyprecip[Dailyprecip$siteID == "CUPE",]
GUILdaily<-Dailyprecip[Dailyprecip$siteID == "GUIL",]
str(CUPEprecip)


# plot precip data for date range available
CUPEplot<- qplot (date, dailyRain,
       data= CUPEdaily,
       main= "Bulk Precipitation at CUPE from 2018-2022",
       xlab= "Date", ylab= "Daily Total Precipitation (mm)")+
        scale_x_date(date_breaks = "1 month",
                    date_labels = "%b-%Y")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
CUPEplot

# Define Start and end times for the subset as R objects that are the time class
startTime <- as.Date("2018-05-01")
endTime <- as.Date("2023-01-01")

# create a start and end time R object
start.end <- c(startTime,endTime)
start.end

#joining tables into one single dataframe
CUPERainMicrobesDF <- merge(CUPEdaily, CUPEmicrobe, by = "date", all.x = TRUE)
GUILRainMicrobesDF <- merge(GUILdaily, GUILmicrobe, by = "date", all.x = TRUE)
summary(RainMicrobesDF)

#Joines dataframe with only available data
filteredCUPERainMicrobe<- na.omit(CUPERainMicrobesDF)
filteredGUILRainMicrobe<- na.omit(GUILRainMicrobesDF)


#remove outliers
GUILnoOut<- filteredGUILRainMicrobe[filteredGUILRainMicrobe<10,]
#add column for log fit daily rain
#run linear regression - log10 transform x and y
fit<-lm(log10(filteredGUILRainMicrobe$dailyRain)~log10(filteredGUILRainMicrobe$microAbMl))
Ga<-fit$coefficients[1]
Gb<-fit$coefficients[2]

###plotting multiple visualizations

CUPEmonthyears <-ggplot(data= CUPEdaily, aes(x=date, y= dailyRain))+
  geom_line()+
  scale_x_date(limits = start.end,
               date_breaks= "1 month",
               date_labels="%b-%Y"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "CUPE Precipitation")+
  theme(plot.title = element_text(hjust =0.5))
CUPEmonthyears

####
CUPEMxPplot<- filteredCUPERainMicrobe %>%
  ggplot(aes(x=date, y = log10Abundance)) +
  geom_line(color ="blue")+
  geom_point(color ="blue")

CUPEMxPplot + 
  geom_line(aes(y= dailyRain), color = "red")+
  geom_point()+
  scale_y_continuous(breaks = seq(0,8,1),
                     name="Microbial Abundance per mL",
                     sec.axis = sec_axis( trans = ~. /2,
                                         breaks = seq(0,60,10),
                                         name = "Precipitation (mm)"
                                         )
                      ) +
  theme(axis.title.y.left = element_text(color= "blue"),
        axis.title.y.right = element_text(color= "red"))

###correlation between daily precipitation and log10Abundance

CUPEDailylogAb_corr<- filteredCUPERainMicrobe %>%
  ggplot(aes(x= dailyRain, y= log10Abundance))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = 'y~x')
  #facet_wrap(~Year, scale = "free")



# microbesPrecip<- ggplot() +
#   geom_line(data= CUPEprecip, aes(x=startDateTime, y= secPrecipBulk), color = "blue") +
#   geom_line(data=CUPEmicrobe, aes(x=date, y= microAbMl/10), color = "green")+
#   scale_x_date(date_breaks = "1 month",
#                date_labels = "%b-%Y") +
#   scale_y_continuous(name = "MicrobialAbundance per mL",
#                      sec.axis = sec_axis(~./1000, name = "Precipitation (mm)"))+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# microbesPrecip

