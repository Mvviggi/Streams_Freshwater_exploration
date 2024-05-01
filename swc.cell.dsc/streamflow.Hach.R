#Combining DSC handheld collection with SWC and microbes
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
library(neonOS)
library(gridExtra)


#Pull data from Portal using neonUtilities

dsc<-loadByProduct(dpID = "DP1.20048.001", site=c("CUPE", "GUIL"), 
                   startdate="2017-01", enddate="2023-04",
                   package= 'expanded', check.size=F, release = 'current')
#write.csv(dsc$dsc_fieldData, "C:/Users/mviggiano/Documents/Github/Fisheries/dsc.csv")

streamFlow<- dsc$dsc_fieldData
short.dsc<- streamFlow%>%
  select(siteID, collectDate, streamStage, finalDischarge)
short.dsc$Date<- as.Date((short.dsc$collectDate), format ="%d-%b-%Y")
short.dsc$date<- as.Date((short.dsc$collectDate), format ="%d-%b-%Y")
#short.dsc<-subset(short.dsc, select = -c(date))
#merge swc.microbes with dsc
dsc.swc.mic<- merge(swc.microbes, short.dsc, by = "Date", all.x =TRUE)

dsc.combo.all<- dsc.swc.mic %>% drop_na(finalDischarge)

dsc.combo.all<- dsc.combo.all %>%
  mutate(logDischarge = log10(finalDischarge))

##plots for dsc and microbres
{
#run linear regression - log10 transform x and y for discharge and microbes
# rmOutlier.DMS<-dsc.combo.all[dsc.combo.all$microAbMl<400000,]

#create df only of discharge and microbes.
dsc.microbes<- merge(ShortAbundanceDF, short.dsc, by= "date" )

plot(microAbMl~finalDischarge, data= dsc.microbes, pch=16)
rmOutlier.dscmic<-dsc.microbes[dsc.microbes$microAbMl<200000,]  

mod<- lm(microAbMl~finalDischarge, data= rmOutlier.dscmic)
summary(mod)
plot(microAbMl~finalDischarge, data= rmOutlier.dscmic, pch=16)
abline(mod)
fit<-lm(log10(dsc.combo.all$microAbMl)~log10(dsc.combo.all$finalDischarge))
summary(fit)
# a<-fit$coefficients[1]
# b<-fit$coefficients[2]
p1<- ggplot(data =rmOutlier.DMS, aes (x= finalDischarge, y = microAbMl)) +
  geom_point(color = 'blue',
             size = 1.5,
             shape = 'square'
             )+
  stat_function(fun=function(x) 10^(a+b*log10(x)),col='red')+
  facet_wrap(~siteID.x , scale= "free")
p1

LogDSCmic<- 
  ggplot(data =rmOutlier.DMS, aes (x= finalDischarge, y = Abundance)) +
  geom_point(color = 'blue',
             size = 1.5,
             shape = 'square'
  )+
  stat_function(fun=function(x) 10^(a+b*log10(x)),col='red')+
  facet_wrap(~siteID.x , scale= "free")
LogDSCmic 

#linear regression equation code
lm_eqn <- function(rmOutlier.DMS){
  m <- lm(y ~ x, rmOutlier.DMS);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

pR <- p1 + geom_text(x = 25, y = 300, label = lm_eqn(rmOutlier.DMS), parse = TRUE)
#make second plot - log scale, use linear model and geom_mooth for regression
fit<-lm(log10(rmOutlier.DMS$microAbMl)~log10(rmOutlier.DMS$finalDischarge))
a<-fit$coefficients[1]
b<-fit$coefficients[2]
p2<- ggplot(data =rmOutlier.DMS, aes (x= finalDischarge, y = microAbMl)) +
  geom_point(color = 'blue',
             size = 1.5,
             shape = 'square'
  )+
  stat_function(fun=function(x) 10^(a+b*log10(x)),col='red')+
  facet_wrap(~siteID.x, ncol = 2, scale = "free")+
  scale_y_log10()+
  scale_x_log10()+
  geom_smooth(method='lm',formula=y~x,se=FALSE)+
  geom_hline(yintercept=40,color=rgb(100,12,77,max=255))+
  geom_vline(xintercept=50,color='#1B9E77')
grid.arrange(p1,p2,ncol=2)
p2

}


