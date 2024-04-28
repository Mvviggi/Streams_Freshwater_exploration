seston.chl<- seston %>%
  select(siteID.x, collectDate.x, parentSampleID, boutNumber, 
         habitatType, analyte, fieldSampleVolume, domainFilterVolume, analyteConcentration)
seston.chl$year<- year(seston.chl$collectDate.x)
seston.chl$year <-as.factor(seston.chl$year)
seston.chla.plot<- seston.chl %>%
  filter(analyte == "chlorophyll a")%>%
  ggplot(aes(x= boutNumber, y = analyteConcentration, color = year, group = year))+
  geom_point() +
  facet_wrap(~siteID.x, scale = "free") 
seston.chla.plot + ggtitle(" Seston chlorophyll concentration by bouts and per site")