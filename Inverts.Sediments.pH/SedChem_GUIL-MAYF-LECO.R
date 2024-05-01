#loadbyproduct
library(neonUtilities)
library(neonOS)
install.packages("raster")
library(raster)
library(ggplot2)
getwd()
setwd("C:/Users/mviggiano/Documents/Github/Mvviggi/Inverts.Sediments.pH")
stringsAsFactors=F

sedChem<- loadByProduct(dpID='DP1.20194.001', 
                        site=c('GUIL','CUPE'), startdate = NA,  enddate = NA,
                        package ='basic', check.size=FALSE)


# names(sedChem478)
# View(sedChem478$asc_externalLabData)
# write.csv(sedChem478$asc_externalLabData,
#           "C:/Users/mviggiano/Documents/R/R courses/mini scripts/asc_externalLabData.csv",
#           row.names=F)

SedData <- sedChem$asc_externalLabData
SedData.Date<- as.Date(SedData$collectDate, format ="%d-%b-%Y")

#filter data to table for only sediment size by  method analysis: ASA No. 9 Pt. 1 Section 15-5
Fe <- SedData %>%
  filter( analyte == "")
  ggplot(aes(x= Date, y =analyteConcentration,
              data=SedData,
              subset=analyte=="Fe",
              xlab="siteID", ylab="Fe mg/kg")
TP <- boxplot(analyteConcentration~siteID,
              data=SedData,
              subset=analyte=="TP",
              xlab="siteID", ylab="TP")
Al <- boxplot(analyteConcentration~siteID,
              data=SedData,
              subset=analyte=="Al",
              xlab="siteID", ylab="Al mg/kg")
Mg <- boxplot(analyteConcentration~siteID,
                       data=SedData,
                       subset=analyte=="Mg",
                       xlab="siteID", ylab="Mg mg/kg ")

NO3.N <- boxplot(analyteConcentration~siteID,
                           data=SedData,
                           subset=analyte=="NO3 - N" ,
                           xlab="siteID", ylab="NO3-N mg/kg ")
Hg <- boxplot(analyteConcentration~siteID,
              data=SedData,
              subset=analyte=="Hg",
              xlab="siteID", ylab="Hg ng/g")
print(SedData)
###boxplot(analyteConcentration~scientificName, 
      #  data=apct, subset=analyte=="d13C", 
      #  xlab=NA, ylab="d13C", 
      #  las=2, cex.axis=0.7)