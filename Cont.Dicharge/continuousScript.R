#continuous discharge

flow<-loadByProduct(dpID = ="DP4.00130.001", site=c("CUPE", "GUIL"), 
                   startdate="2018-01", enddate="2023-01",
                   package= 'basic', check.size=F, release = 'current')

options(stringsAsFactors=F)

list2env(dsc, .GlobalEnv)
dsc<-removeDups(data=csd_continuousDischarge,
                      variables=variables_20138)
fieldcell<-removeDups(data=amc_fieldCellCounts,
                      variables= variables_20138)
joincc<-joinTableNEON(amc_cellCounts,
                      amc_fieldCellCounts)
str(joincc)
parentfield<- joinTableNEON(amc_fieldCellCounts,
                            amc_fieldSuperParent