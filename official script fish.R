#official script to fish abundance in time series
#uplaod all packages you will need
getwd()
setwd("C:/Users/mvvb8/Documents/GitHub/Fisheries")
library(lubridate)
library(ggplot2)
library(neonUtilities)
library(tidyr)
library(dplyr)
library(devtools)
library(tidyverse)
library(dplyr)
library(stringr)
library(stats)
#bring the data product into your directory
fish <-loadByProduct(dp="DP1.20107.001", site=c("CUPE", "GUIL"), 
                     startdate="2017-01", enddate="2023-03",
                     package= 'basic', check.size=T)

#create the dataframes - only need to do once the write.csv, then only need for read.csv
#dataframe PER FISH
write.csv(fish$fsh_perFish, "C:/Users/mvvb8/Documents/GitHub/Fisheries/fish_length.csv", row.names=F) 
lenweightMain <- read.csv("fish_length.csv")%>%
    select(siteID, namedLocation, passStartTime, eventID, passNumber, taxonID, fishTotalLength, fishWeight) 
colnames(lenweightMain) <- c("site", "location", "start", "eventID", "pass", "taxon", "length", "weight")

#dataframe PER PASS
write.csv(fish$fsh_perPass,"C:/Users/mvvb8/Documents/GitHub/Fisheries/fish_pass.csv", row.names=F) 
pass <- read.csv("fish_pass.csv") %>%
    select(pass, siteID, reachID, passNumber, passStartTime, eventID, efTime)
colnames(pass) <- c("site", "reach", "pass", "start", "eventID", "EF")
pass<- pass[!duplicated(pass), ] #Pass df has duplicates, eliminate them

#dataframe PER BULK
write.csv(fish$fsh_perBulk, "C:/Users/mvvb8/Documents/GitHub/Fisheries/fish_bulkCount.csv",row.names=F) 
bulk <-read.csv("fish_bulk.csv") %>%
    select(bulk, siteID, namedLocation, passStartTime,passNumber, eventID, taxonID, bulkFishCount)
colnames(bulk)<-c("site", "location", "start", "pass","eventID", "taxon", "bulkcount")

#Convert and add columns for season, dates, bouts
###create column for Year-Month for each dataframe
lenweight1$start <-as.Date(lenweight1$start)
pass$start <-as.Date(pass$start)
bulk$start <-as.Date(bulk$start)
lenweight1$collectDate <- format(as.Date(lenweight1$start, format = "%y/%m/%d"), "%Y-%m")
pass$collectDate <- format(as.Date(pass$start, format = "%y/%m/%d"), "%Y-%m")
bulk$collectDate <- format(as.Date(bulk$start, format = "%y/%m/%d"), "%Y-%m")
    
###separate collectDate  into month and year- will come back here if want to keep this column
lenweight1<- separate(lenweight1, col = collectDate, into = c("year", "month"), sep = "-") 
pass<- separate(pass, col = collectDate, into = c("year", "month"), sep = "-")                      
bulk<- separate(bulk, col = collectDate, into = c("year", "month"), sep = "-")

###convert factor variable into numerit
bulk <- bulk %>% mutate (month= as.numeric(as.character(month)))
pass <- pass %>% mutate (month= as.numeric(as.character(month)))
lenweight1 <- lenweight1 %>% mutate (month= as.numeric(as.character(month)))
str(lenweight1)
###generate a new season column based on month
bulk <-bulk %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))
pass <-pass %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))    
lenweight1 <-lenweight1 %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))


###if desired  join Year and month again 
    lenweight1 <-unite(lenweight1, col="date", c('year', 'month'), sep='-')
    bulk <- unite(bulk, col ="bout", c('year', 'season'), sep= '-')
    pass <- unite(pass, col ="bout", c('year', 'season'), sep= '-')

    
###if desired - join Year and season - 
    lenweight <-unite(lenweight, col="bout", c('year', 'season'), sep='-')
    bulk <- unite(bulk, col ="bout", c('year', 'season'), sep= '-')
    pass <- unite(pass, col ="bout", c('year', 'season'), sep= '-')

##create column for reach
lenweight1<- separate(lenweight1, col = location, 
         into = c("one", "two","three", "four", "reach"))
pass1<- separate(pass, col = reach, 
                      into = c("one", "two", "reach"))
bulk1 <- separate(bulk, col = location, 
                           into = c("one", "two","three", "four", "reach"))
#remove unnecessary columns created from above lines 
lenweight1<- lenweight1 %>% select(-c(one, two, three, four))
pass1 <- pass1 %>% select(-c(one, two))
bulk1<- bulk1 %>% select(-c(one, two, three, four))
view(lenweight1)
#Count individuals by event, by season, by species
sumfishevent <- lenweight1 %>%
  group_by(site, reach, eventID, pass, taxon, year, season, collectDate) %>%
count()

#Sum total fish per eventID, no species, missing bulk data
sum_event <- Count_Species_event %>%
  group_by( site, eventID, pass, reach, collectDate, season) %>%
  tally(n)
write.csv(sum_event,"C:/Users/mvvb8/Documents/GitHub/Fisheries/sum_event.csv" )
###Section working with Effort time : convert EF from seconds to hours
pass_ef <- pass1 %>%
  mutate(EFhr= EF/3600)

##adding and counting bulk date
total_eventID_bulk <- bulk1 %>%
  group_by(site, reach, pass, eventID, taxon, year, season, collectDate)%>%
count(bulkcount)
str(bulk1)

#create new unique id
#merging per fish and bulk
counteventtaxaID<- sumfishevent %>%
  mutate(eventaxa = paste(eventID, taxon, sep = "."))
bulkeventtaxaID <-bulk1 %>%
  mutate(eventaxa = paste(eventID, taxon, sep = "."))

#merging per fish and bulk using above unique id
allspeciescount <- merge.data.frame(counteventtaxaID, bulkeventtaxaID, by = "eventaxa", all = TRUE)

#replacing NA in bulkcount and n columns with 0
allspeciescount["n"][is.na(allspeciescount["n"])] <- 0
allspeciescount["bulkcount"][is.na(allspeciescount["bulkcount"])] <- 0

#replacing NA with values from other columns
allspeciescount$site.x[is.na(allspeciescount$site.x)] <- allspeciescount$site.y[is.na(allspeciescount$site.x)]
allspeciescount$season.x[is.na(allspeciescount$season.x)] <- allspeciescount$season.y[is.na(allspeciescount$season.x)]
allspeciescount$eventID.x[is.na(allspeciescount$eventID.x)] <- allspeciescount$eventID.y[is.na(allspeciescount$eventID.x)]
allspeciescount$taxon.x[is.na(allspeciescount$taxon.x)] <- allspeciescount$taxon.y[is.na(allspeciescount$taxon.x)]
allspeciescount$pass.x[is.na(allspeciescount$pass.x)] <- allspeciescount$pass.y[is.na(allspeciescount$pass.x)]
allspeciescount$reach.x[is.na(allspeciescount$reach.x)] <- allspeciescount$reach.y[is.na(allspeciescount$reach.x)]
allspeciescount$collectDate.x[is.na(allspeciescount$collectDate.x)] <- allspeciescount$collectDate.y[is.na(allspeciescount$collectDate.x)]
allspeciescount$year.x[is.na(allspeciescount$year.x)] <- allspeciescount$year.y[is.na(allspeciescount$year.x)]

#remove all duplicate and unnecessary columns
allspeciescount<- allspeciescount %>% 
  select(-c(month, year.y, reach.y, pass.y, site.y, taxon.y, collectDate.y, season.y, eventID.y))
##if only want a table by site, or by species
#####
#sum the bulk by taxon with all per fish by taxon
totalspeciescount <- allspeciescount %>%
  mutate(total = n + bulkcount)
write.csv(totalspeciescount, "C:/Users/mvvb8/Documents/GitHub/Fisheries/totalspeciestally.csv")
totspecies_bout <- totalspeciescount %>%
  mutate(bout = paste(year.x, season.x, sep = "-"))
##create a df for total fish per pass
totalfish <- totalspeciescount %>%
  group_by(site.x, reach.x, eventID.x, season.x,year.x, pass.x, collectDate.x) %>%
  tally(total)
write.csv(totalfish, "C:/Users/mvvb8/Documents/GitHub/Fisheries/totalfish.csv")
#change column names for unique id match the pass_ef df
colnames(totalfish)<-c("site", "reach", "eventID", "season", "year", "pass", "collectDate", "total")
EF<- pass_ef %>% select(c(eventID, EFhr))
#merging tables PER FISH and PER PASS
fish_EF <- merge(totalfish,pass_ef, by = "eventID", all = TRUE) 
#replacing NA to 0 for total in fish_EF
fish_EF["total"][is.na(fish_EF["total"])] <- 0
fish_EF$site.x[is.na(fish_EF$site.x)] <- fish_EF$site.y[is.na(fish_EF$site.x)]
fish_EF$season.x[is.na(fish_EF$season.x)] <- fish_EF$season.y[is.na(fish_EF$season.x)]
fish_EF$eventID.x[is.na(fish_EF$eventID.x)] <- fish_EF$eventID.y[is.na(fish_EF$eventID.x)]
fish_EF$taxon.x[is.na(fish_EF$taxon.x)] <- fish_EF$taxon.y[is.na(fish_EF$taxon.x)]
fish_EF$pass.x[is.na(fish_EF$pass.x)] <- fish_EF$pass.y[is.na(fish_EF$pass.x)]
fish_EF$reach.x[is.na(fish_EF$reach.x)] <- fish_EF$reach.y[is.na(fish_EF$reach.x)]
fish_EF$collectDate.x[is.na(fish_EF$collectDate.x)] <- fish_EF$collectDate.y[is.na(fish_EF$collectDate.x)]
fish_EF$year.x[is.na(fish_EF$year.x)] <- fish_EF$year.y[is.na(fish_EF$year.x)]

#remove duplicate columns
fish_EF<- fish_EF %>% 
  select(-c(year.y, reach.y, pass.y, site.y, collectDate.y, season.y))
#convert passs column into categorical variable
totalfish$pass <- as.factor(totalfish$pass)
fish_EF$pass <- as.factor(fish_EF$pass)
summary(fish_EF)

##CPUE
CPUE <- fish_EF %>%
  mutate(CPUE = total/EFhr)
write.csv(CPUE, "C:/Users/mvvb8/Documents/GitHub/Fisheries/CPUE2.csv")

## median total fish per site
medianfishCUPE <- totalfish %>%
  group_by(site, reach, collectDate)%>%
  summarize(median(total))
#total of fish per reach 
medianCPUE_reach <- CPUE %>%
    filter(pass == c("1")) %>%
  ggplot(aes(x = collectDate, y= total, fill = season)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar")+
  facet_wrap(~site, nrow= 2, scale = "free") +
  labs(x= NULL,
       y = "Mean total for single pass per sampling date")
medianCPUE_reach +
totspecies_bout$bout<-factor(totspecies_bout$bout, levels= unique(totspecies_bout$bout))
#plotting for specific species
AGOMON <- totspecies_bout %>%
  filter(taxon.x == c("AGOMON")) %>%
  ggplot(aes(x= bout, y= total, fill = season.x)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar")
AGOMON +ggtitle("Agonostomus monticola  abundance by bout")
XIPHEL <- totspecies_bout %>%
  filter(taxon.x == c("XIPHEL")) %>%
  ggplot(aes(x= bout, y= total, fill = season.x)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar") +
  labs
XIPHEL + ggtitle("Xiphophorus eleri abundance by bout")

# geom point plotting fro total fish by bout and pass
ggplot(totalfish1, aes(bout, total)) +
  geom_point(aes(color = pass, shape = pass))+
  facet_wrap(~site, nrow =2, scales= "free")
str(totalfish1)
# ggplot stacked bar chart
ggplot(totalfish1, aes(x = eventID, y =total, color = pass)) +
  geom_point()+
  facet_wrap(~site)
ggplot(Totalfish_bout, aes(x= collectDate, y =n, fill = season)) +
  geom_col()+
  facet_wrap(~site, nrow =2)
ggplot(Totalfish_bout2, aes(x= collectDate, y =n, col = season)) +
  geom_col()+
  facet_wrap(~site)

##plotting CPUE 
CPUE1<- ggplot(CPUE, aes(x = collectDate.x, y= CPUE, fill = season.x)) +
  geom_boxplot() +
  facet_wrap(~site.x, nrow=2, scale= "free")
CPUE1

