##############################################################################################
#' @title PLOTTING PARTICLE SIZE DISTRIBUTIONS FOR D04 SITES USING PEBBLE COUNT DATA

#' @author
#' Zachary Nickerson \email{nickerson@battelleecology.org} \cr

#' @return plots using ggplot

# changelog and author contributions / copyrights
#   Zachary Nickerson (2024-04-30)
#     original creation
##############################################################################################

# Load libraries
library(restR2)
library(tidyverse)

# Pull particle size data from D04 sites from L0
pebbleCount_D04 <- restR2::par.get.os.l0.data(
  dpID = "DP0.00131.001",
  ingestTable = "geo_pebbleCount_in",
  namedLocationName = "D04",
  inclDescendants = T,
  startDate = "2017-01-01",
  endDate = "2024-05-01"
)
pebbleFieldData_D04 <- restR2::par.get.os.l0.data(
  dpID = "DP0.00131.001",
  ingestTable = "geo_pebbleFieldData_in",
  namedLocationName = "D04",
  inclDescendants = T,
  startDate = "2017-01-01",
  endDate = "2024-05-01"
)

# Convert pebbleFieldData from wide to long format
pebbleFieldData_D04$eventID <- paste(pebbleFieldData_D04$locationID,format(as.POSIXct(pebbleFieldData_D04$endDate,format="%Y-%m-%dT%H:%M:%S.000Z"),"%Y%m%d"),sep=".")
pebbleFieldData_D04_sum <- pebbleFieldData_D04%>%
  pivot_longer(c(pebbleCountD5,
                 pebbleCountD16,
                 pebbleCountD50,
                 pebbleCountD84),
               names_to = "distribution",
               values_to = "value")%>%
  select(eventID,distribution,value)%>%
  filter(!is.na(value))

# Add standard eventID field to pebbleCount
pebbleCount_D04$eventID <- paste(pebbleCount_D04$locationID,format(as.POSIXct(pebbleCount_D04$endDate,format="%Y-%m-%dT%H:%M:%S.000Z"),"%Y%m%d"),sep=".")
events <- unique(pebbleCount_D04$eventID)
unique(pebbleCount_D04$pebbleSize)

# Create factor levels to clean up categorical variables
pebbleCount_D04 <- pebbleCount_D04%>%
  filter(!is.na(pebbleSize))
factorLevels <- list(c("< 2 mm: silt/clay","<2","<2"),             
                     c("< 2 mm: sand","<2","<2"),
                     c("2 - 2.8 mm: very coarse sand","2-2.8","2.8"),  
                     c("2.8 - 4 mm: very fine gravel","2.8-4","4"), 
                     c("4 - 5.6 mm: fine gravel","4-5.6","5.6"),       
                     c("5.6 - 8 mm: fine gravel","5.6-8","8"),
                     c("8 - 11 mm: medium gravel","8-11","11"),      
                     c("11 - 16 mm: medium gravel","11-16","16"),     
                     c("16 - 22.6 mm: coarse gravel","16-22.6","22.6"),
                     c("22.6 - 32 mm: coarse gravel","22.6-32","32"),
                     c("32 - 45 mm: very coarse gravel","32-45","45"),
                     c("45 - 64 mm: very coarse gravel","45-64","64"),
                     c("64 - 90 mm: small cobble","64-90","90"),      
                     c("90 - 128 mm: medium cobble","90-128","128"),    
                     c("128 - 180 mm: large cobble","128-180","180"),
                     c("180 - 256 mm: large cobble","180-256","256"),
                     c("> 256 mm: boulder",">256",">256"),
                     c("> 256 mm: bedrock",">256",">256"))

# Add cleaned categorical variables to data
pebbleCount_D04$pebbleSize_new <- NA
pebbleCount_D04$pebbleSize_newNUM <- NA
for(i in 1:nrow(pebbleCount_D04)){
  for(j in 1:length(factorLevels)){
    if(factorLevels[[j]][[1]]==pebbleCount_D04$pebbleSize[i]){
      pebbleCount_D04$pebbleSize_new[i] <- factorLevels[[j]][[2]]
      pebbleCount_D04$pebbleSize_newNUM[i] <- factorLevels[[j]][[3]]
    }  
  }
}
unique(pebbleCount_D04$pebbleSize_new)
unique(pebbleCount_D04$pebbleSize_newNUM)

# Create cleaned labels for plotting the x-axis
factorLevels2 <- c("<2",
                   "2.8",
                   "4",
                   "5.6",
                   "8", 
                   "11",
                   "16",
                   "22.6",
                   "32",
                   "45",
                   "64",
                   "90",
                   "128",
                   "180",
                   "256",
                   ">256")


# Add bins to pebble field data so they can be plotted as well
pebbleFieldData_D04_sum$sizeBin <- NA
for(i in 1:nrow(pebbleFieldData_D04_sum)){
  if(grepl(">|<",pebbleFieldData_D04_sum$value[i])){
    pebbleFieldData_D04_sum$sizeBin[i] <- pebbleFieldData_D04_sum$value[i]
  }else{
    for(j in 1:length(factorLevels2)){
      if(as.numeric(pebbleFieldData_D04_sum$value[i])>=as.numeric(gsub("<|>","",factorLevels2[j]))&as.numeric(pebbleFieldData_D04_sum$value[i])<as.numeric(gsub("<|>","",factorLevels2[j+1]))){
        pebbleFieldData_D04_sum$sizeBin[i] <- factorLevels2[j+1]
      }
    }
  }
}

# Make plots for GUIL
GUIL_events <- events[grepl("GUIL",events)]
GUIL_events <- GUIL_events[!grepl("201708",GUIL_events)]
# Data Wrangling
for(i in 1:length(GUIL_events)){
  temp <- pebbleCount_D04%>%
    filter(eventID%in%GUIL_events[i])%>%
    group_by(eventID,pebbleSize_newNUM)%>%
    summarize(frequency=n()/nrow(pebbleCount_D04%>%filter(eventID%in%GUIL_events[i])))
  missingFactors <- factorLevels2[!factorLevels2%in%temp$pebbleSize_newNUM]
  if(length(missingFactors)>0){
    temp2 <- data.frame(matrix(data=NA,nrow=length(missingFactors),ncol = ncol(temp)))
    names(temp2) <- names(temp)
    temp2$eventID <- unique(temp$eventID)
    temp2$pebbleSize_newNUM <- missingFactors
    temp2$frequency <- 0
    temp <- rbind(temp,temp2)
  }
  if(i==1){
    GUIL_freq_sum <- temp
  }else{
    GUIL_freq_sum <- rbind(GUIL_freq_sum,temp)
  }
}
# Plot particle size distribution as a barplot
pebbleCount_D04%>%
  filter(eventID%in%GUIL_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,level=factorLevels2)))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_wrap(~eventID)
# Plot particle size distribution as a line plot
# Add summary stats as vertical lines
GUIL_freq_sum%>%
  filter(eventID%in%GUIL_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,
                      level=factorLevels2),
             y=frequency,
             group=eventID#,
             # color=eventID
  ))+
  # geom_col()+
  geom_line(stat='summary',
            fun.y='mean')+
  geom_vline(data = pebbleFieldData_D04_sum%>%filter(eventID%in%GUIL_events),
             aes(xintercept=sizeBin,
                 color=distribution),
             size=0.75,
             linetype="dashed")+
  facet_wrap(~eventID,
             #ncol = 1
             )
# Plot particle size distributions as line plot, but all in a single plotting field
GUIL_freq_sum%>%
  filter(eventID%in%GUIL_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,
                      level=factorLevels2),
             y=frequency,
             group=eventID,
             color=eventID
  ))+
  # geom_point(stat='summary',
  #           fun.y='mean')+
  geom_smooth(method="loess",se=F)

# Make plots for CUPE
CUPE_events <- events[grepl("CUPE",events)]
CUPE_events <- CUPE_events[!grepl("201708|202006",CUPE_events)]
# Data wrangling
for(i in 1:length(CUPE_events)){
  temp <- pebbleCount_D04%>%
    filter(eventID%in%CUPE_events[i])%>%
    group_by(eventID,pebbleSize_newNUM)%>%
    summarize(frequency=n()/nrow(pebbleCount_D04%>%filter(eventID%in%CUPE_events[i])))
  missingFactors <- factorLevels2[!factorLevels2%in%temp$pebbleSize_newNUM]
  if(length(missingFactors)>0){
    temp2 <- data.frame(matrix(data=NA,nrow=length(missingFactors),ncol = ncol(temp)))
    names(temp2) <- names(temp)
    temp2$eventID <- unique(temp$eventID)
    temp2$pebbleSize_newNUM <- missingFactors
    temp2$frequency <- 0
    temp <- rbind(temp,temp2)
  }
  if(i==1){
    CUPE_freq_sum <- temp
  }else{
    CUPE_freq_sum <- rbind(CUPE_freq_sum,temp)
  }
}
# Plot particle size distribution as a barplot
pebbleCount_D04%>%
  filter(eventID%in%CUPE_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,level=factorLevels2)))+
  geom_bar(aes(y=(..count..)/sum(..count..)))+
  facet_wrap(~eventID)
# Plot particle size distribution as a line plot
# Add summary stats as vertical lines
CUPE_freq_sum%>%
  filter(eventID%in%CUPE_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,
                      level=factorLevels2),
             y=frequency,
             group=eventID#,
             # color=eventID
             ))+
  # geom_col()+
  geom_line(stat='summary',
            fun.y='mean')+
  geom_vline(data = pebbleFieldData_D04_sum%>%filter(eventID%in%CUPE_events),
             aes(xintercept=sizeBin,
                 color=distribution),
             size=0.75,
             linetype="dashed")+
  facet_wrap(~eventID,
             #ncol=1
             )
# Plot particle size distributions as line plot, but all in a single plotting field
CUPE_freq_sum%>%
  filter(eventID%in%CUPE_events)%>%
  ggplot(aes(x=factor(pebbleSize_newNUM,
                      level=factorLevels2),
             y=frequency,
             group=eventID,
             color=eventID
  ))+
  # geom_point(stat='summary',
  #           fun.y='mean')+
  geom_smooth(method="loess",se=F)
#




#