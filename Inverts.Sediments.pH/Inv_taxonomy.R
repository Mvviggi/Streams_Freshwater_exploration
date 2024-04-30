#data visualization with ggplot##
library(tidyverse)
library(ggplot2)
library(neonUtilities)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(stats)

{options(stringsAsFactors=F)
# Macroinvert dpid
inv_dpid <- 'DP1.20120.001'

# list of sites
my_site_list <- c('CUPE','GUIL')

# get all tables for these sites from the API -- takes < 1 minute
all_tabs_inv <- loadByProduct(
  dpID = inv_dpid ,
  site = my_site_list, 
  startdate="2016-01", enddate="2024-04",
  package= 'basic', check.size=F, release = 'current')

#pull tables in environment
list2env(all_tabs_inv, .GlobalEnv)
e_duped_uids <- inv_fieldData %>% 
  
  # remove records where no sample was collected
  filter(!is.na(sampleID)) %>%  
  group_by(sampleID) %>%
  summarise(n_recs = length(uid),
            n_unique_uids = length(unique(uid)),
            uid_to_keep = dplyr::first(uid)) 

# Are there any records that have more than one unique uid?
max_dups <- max(de_duped_uids$n_unique_uids %>% unique())


# filter data using de-duped uids if they exist

  if(max_dups > 1){
    inv_fieldData <- inv_fieldData %>%
      dplyr::filter(uid %in% de_duped_uids$uid_to_keep)
  }
}
{
  inv_fieldData$Year<- 
  # extract year from date, add it as a new column
  inv_fieldData <- inv_fieldData %>%
    mutate(
      year = collectDate %>% 
        lubridate::as_date() %>% 
        lubridate::year()) 
str(inv_fieldData)
  # extract year from boutNumber, add it as a new column called season
  inv_fieldData$boutNumber<- as.factor(inv_fieldData$boutNumber)
  inv_fieldData <- inv_fieldData %>%
      mutate(season= case_when(
        boutNumber == 1 ~ "spring",
        boutNumber == 2 ~ "summer",
        boutNumber == 3 ~ "fall"))
  
  inv_fieldData<- inv_fieldData %>%
    mutate(season= factor(season, levels= c("spring", "summer", "fall")))

}
  # extract location data into a separate table
 { table_location <- inv_fieldData %>%
    
    # keep only the columns listed below
    select(siteID, 
           domainID,
           namedLocation, 
           decimalLatitude, 
           decimalLongitude, 
           elevation) %>%
    
    # keep rows with unique combinations of values, 
    # i.e., no duplicate records
    distinct()
  
  table_taxon <- inv_taxonomyProcessed %>%
    
    # keep only the coluns listed below
    select(acceptedTaxonID, taxonRank, scientificName,
           order, family, genus, 
           identificationQualifier,
           identificationReferences) %>%
    
    # remove rows with duplicate information
    distinct()
  
  # check for repeated taxa within a sampleID that need to be added together
  inv_taxonomyProcessed_summed <- inv_taxonomyProcessed %>% 
    select(sampleID,
           acceptedTaxonID,
           individualCount,
           estimatedTotalCount) %>%
    group_by(sampleID, acceptedTaxonID) %>%
    summarize(
      across(c(individualCount, estimatedTotalCount), ~sum(.x, na.rm = TRUE)))
 }
  # join summed taxon counts back with sample and field data
  {
    table_observation <- inv_taxonomyProcessed_summed %>%
    
    # Join relevant sample info back in by sampleID
    left_join(inv_taxonomyProcessed %>% 
                select(sampleID,
                       domainID,
                       siteID,
                       namedLocation,
                       collectDate,
                       acceptedTaxonID,
                       order, family, genus, 
                       scientificName,
                       taxonRank) %>%
                distinct()) %>%
    
    # Join the columns selected above with two 
    # columns from inv_fieldData (the two columns 
    # are sampleID and benthicArea)
    left_join(inv_fieldData %>% 
                select(sampleID, eventID, year, 
                       habitatType, samplerType,
                       benthicArea, season )) %>%
    
    # some new columns called 'variable_name', 
    # 'value', and 'unit', and assign values for 
    # all rows in the table.
    # variable_name and unit are both assigned the 
    # same text strint for all rows. 
    mutate(inv_dens = estimatedTotalCount / benthicArea,
           inv_dens_unit = 'count per square meter')
  
  # check for duplicate records, should return a table with 0 rows
  table_observation %>% 
    group_by(sampleID, acceptedTaxonID) %>% 
    summarize(n_obs = length(sampleID)) %>%
    filter(n_obs > 1)
  
  # extract sample info
  table_sample_info <- table_observation %>%
    select(sampleID, siteID, namedLocation, 
           collectDate, eventID, year, season,
           habitatType, samplerType, benthicArea, 
           inv_dens_unit) %>%
    distinct()
  }
{
  # remove singletons and doubletons
  # create an occurrence summary table
  taxa_occurrence_summary <- table_observation %>%
    select(sampleID, acceptedTaxonID) %>%
    distinct() %>%
    group_by(acceptedTaxonID) %>%
    summarize(occurrences = n())
  
  # filter out taxa that are only observed 1 or 2 times
  taxa_list_cleaned <- taxa_occurrence_summary %>%
    filter(occurrences > 2)
  
  str(table_sample_info)
  # filter observation table based on taxon list above
  # table_observation_cleaned <- table_observation %>%
  #   filter(acceptedTaxonID %in%
  #            taxa_list_cleaned$acceptedTaxonID,
  #          !sampleID %in% c("MAYF.20190729.CORE.1",
  #                           "MAYF.20200713.CORE.1",
  #                           "MAYF.20210721.CORE.1",
  #                           "POSE.20160718.HESS.1")) 
  #this is an outlier sampleID
} 
  # some summary data # counts of number of sampling events and number of samples collected
{
  sampling_effort_summary <- table_sample_info %>%
    
    # group by siteID, year
    group_by(siteID, year, samplerType) %>%
    
    # count samples and habitat types, seasons within each event
    summarise(
      event_count = eventID %>% unique() %>% length(),
      sample_count = sampleID %>% unique() %>% length(),
      habitat_count = habitatType %>% unique() %>% length(),
      )
  
}  
  # number taxa by rank by site
#number of taxa rank 
{
  table_observation_cleaned %>% 
    group_by( siteID, taxonRank) %>%
    summarize(
      n_taxa = acceptedTaxonID %>% 
        unique() %>% length()) %>%
    ggplot(aes(n_taxa, taxonRank)) +
    facet_wrap(~ siteID) +
    geom_col()
 
  # number taxa by rank by site per year and season
  table_obs_year <-table_observation_cleaned %>% 
    group_by( siteID, taxonRank, year, season) %>%
    summarize(
      n_taxa = acceptedTaxonID %>% 
        unique() %>% length())
  
  ggplot(table_obs_year) +                     ####Good graph need more aesthetics, size and names etc
    geom_bar(aes(x= n_taxa, y= taxonRank, fill = as.factor(year)), 
             position = "stack", stat = "identity") +
    facet_wrap(season~siteID, nrow= 3, scale ="free_x")
  
  # number taxa by rank by site per collectDate and season
  table_obs_collect <-table_observation_cleaned %>% 
    group_by( siteID, taxonRank, collectDate, season) %>%
    summarize(
      n_taxa = acceptedTaxonID %>% 
        unique() %>% length())
}  

###Setr of grappgs for table of density of order 
{  #Order density bby year per season
  ggplot(data= table_observation_by_order,aes(x=as.factor(year), y= order_dens, color= season, group = season)) +
    geom_col(position = "dodge2")+
    facet_wrap(~siteID, nrow = 2, scale= "free_y")
  head(table_observation_by_order)
  # library(scales)
  # sum densities by order for each sampleID
  table_observation_by_order <- 
    table_observation_cleaned %>% 
    filter(!is.na(order)) %>%
    group_by(siteID, year, season,
             eventID, sampleID, habitatType, order) %>%
    summarize(order_dens = sum(inv_dens, na.rm = TRUE))
  
  #table by order with collectDate column
  table_observation_collectdate <- 
    table_observation_cleaned %>% 
    filter(!is.na(order)) %>%
    group_by(siteID, year, season, collectDate,
             eventID, sampleID, habitatType, order) %>%
    summarize(order_dens = sum(inv_dens, na.rm = TRUE))
  table_observation_collectdate$Date<- as.Date(table_observation_collectdate$collectDate, format ="%d-%b-%Y")
  table_observation_collectdate$year<- as.factor(table_observation_collectdate$year)
  head(table_observation_collectdate)
  
  # boxplot rank occurrence plot
  table_observation_collectdate %>%
    group_by(eventID, siteID, Date, season, habitatType) %>%
    summarize(
      occurrence = (order_dens > 0) %>% sum()) %>%
    ggplot(aes(
      x = Date, 
      y = occurrence,
      color = habitatType,
      fill = habitatType)) +
    geom_col(position= "dodge2") +
    facet_wrap(siteID ~ ., scale = "free_y") +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1,
            ))
  
 ##totaldensity by sampling event
  total_dens_event<- table_observation_collectdate %>%
    group_by(siteID, year, season, Date,
             eventID, habitatType) %>%
    summarize(total_dens = sum(order_dens, na.rm = TRUE))
  head(total_dens_event)
  
  total_dens_date<- table_observation_collectdate %>%
    group_by(siteID, year, season, Date,
             eventID) %>%
    summarize(total_dens = sum(order_dens, na.rm = TRUE))
  head(total_dens_date)
  
 sumamry()
  
  
  #Order density bby year per season
  
  ggplot(data= table_observation_by_order,aes(x=as.factor(year), y= order_dens, color= season, group = season)) +
    geom_col(position = "dodge2")+
    facet_wrap(~siteID, nrow = 2, scale= "free_y")
  total_dens_date %>%
    ggplot(aes(x= Date, y= total_dens, color= season, fill = season)) +
    geom_point(size=2)+
    geom_line(size=1)+
    scale_x_date(date_breaks= "1 year",
                date_labels = "%b-%Y")+
    facet_wrap(~siteID, nrow =2, scale= "free" )
  
  #table for inv_dens
  table_inv_dens <-table_observation %>%
    select(sampleID, siteID,collectDate, eventID, year, season,
           habitatType, samplerType, benthicArea, inv_dens,
           inv_dens_unit) %>%
    distinct()
  str(table_inv_dens)
  
  ggplot(data= table_inv_dens, aes( x= collectDate, y = inv_dens, color = season, group = season)) +
    geom_line()+
    facet_wrap(~siteID)
  
  #Order density bby year per season by habitat type
  total_dens_event %>%
    filter(siteID == c("CUPE"))%>%
  ggplot(aes(x= year, y= total_dens, color= season, fill = season)) +
    geom_col(position = "dodge2")
    #facet_wrap(~Habitat, nrow =3, scale= "free" )
  
  
  # rank occurrence by order
  table_observation_by_order %>% head()
  
  # stacked rank occurrence plot
  table_observation_by_order %>%
    group_by(order, siteID) %>%
    summarize(
      occurrence = (order_dens > 0) %>% sum()) %>%
    ggplot(aes(
      x = reorder(order, -occurrence), 
      y = occurrence,
      color = siteID,
      fill = siteID)) +
    geom_col() +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1,
                         ))
  
  #stacked rank occurance by year plot
  table_observation_by_order %>%
    group_by(order, siteID, year) %>%
    summarize(
      occurrence = (order_dens > 0) %>% sum()) %>%
    ggplot(aes(
      x = reorder(order, -occurrence), 
      y = occurrence,
      color = siteID,
      fill = siteID)) +
    geom_col() +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1, size= 12),
          axis.text.y = element_text(size= 11))+
    facet_wrap(~year, ncol =3,  scale = "free_y")
  
  #stacked rank occurance by year per season
  table_observation_by_order %>%
    filter(siteID ==c("GUIL")) %>%
    group_by(order, season, year) %>%
    summarize(
      occurrence = (order_dens > 0) %>% sum()) %>%
    ggplot(aes(
      x = reorder(order, -occurrence), 
      y = occurrence,
      color = as.factor(year),
      fill = as.factor(year))) +
    geom_col() +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1))+
    facet_wrap(~season, ncol =3,  scale = "free_y")
  
  
  # faceted densities plot
  table_observation_by_order %>%
    ggplot(aes(
      x = reorder(order, -order_dens), 
      y = log10(order_dens),
      color = siteID,
      fill = siteID)) +
    geom_boxplot(alpha = .5) +
    facet_grid(siteID ~ .) +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1))
  
  table_observation_by_order %>%
    filter(siteID == c("CUPE"))%>%
    ggplot(aes(
      x = reorder(order, -order_dens), 
      y = log10(order_dens),
      color = as.factor(year),
      fill = as.factor(year))) +
    geom_boxplot(alpha = .5) +
    facet_grid(season ~ .) +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1)) 
  
  table_observation_by_order %>%
    filter(siteID == c("GUIL"))%>%
    ggplot(aes(
      x = reorder(order, -order_dens), 
      y = log10(order_dens),
      color = season,
      fill = season)) +
    geom_boxplot(alpha = .5) +
    facet_grid(year ~ .) +
    theme(axis.text.x = 
            element_text(angle = 45, hjust = 1))
 
  
                    
  # CUPEorder<- summaryOrder %>%
  #   filter(siteID == "CUPE")
  # summary(CUPEorder)
  # GUILorder<- summaryOrder %>%
  #   filter(siteID == "GUIL")
  # summary(GUILorder)

#########changing factors order for log10order_dens, and order name
#For CUPE samples 
C1<- table_observation_by_order %>%
  filter(siteID == c("CUPE"), year != 2016, order_dens >= 118.3)%>%
  ggplot(aes(
    x = reorder(order, -order_dens), 
    y = log10(order_dens),
    color = season,
    fill = season)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~year, ncol= 1) +
  theme(axis.text.x = 
          element_text( size = 10))+ 
  labs(title = "CUPE Common macroinvertebrates order density by season over the years")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=12))
C1
#For GUIL samples
G1<- table_observation_by_order %>%
  filter(siteID == c("GUIL"), year != 2016, order_dens >= 451.61 )%>%
    ggplot(aes(
    x = reorder(order, -order_dens), 
    y = log10(order_dens),
    color = season,
    fill = season)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~year, ncol= 1) +
  theme(axis.text.x = 
          element_text(angle = 45, hjust = 1, size = 12)) +
    labs(title = "GUIL Macroinvertebrates order density by season over the years")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
G1 

head(table_observation_collectdate)
###Most common order density in time series
summaryOrder<- table_observation_collectdate %>%
  group_by(siteID, year, season, Date,
           eventID, order) %>%
  summarize(total_dens = sum(order_dens, na.rm = TRUE))



C1time<- table_observation_collectdate %>%
  filter(siteID == c("CUPE"), year != 2016, order_dens >= 118.3)%>%
  ggplot(aes(x = Date, 
             y = log10(total_dens),
             color= order, 
             shape = season))+
   geom_line() 
  # facet_wrap(~year, ncol= 1) +
  theme(axis.text.x = 
          element_text( size = 10))+ 
  labs(title = "CUPE Common macroinvertebrates order density over the years")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=12))
C1time
#For GUIL samples
G1<- table_observation_by_order %>%
  filter(siteID == c("GUIL"), year != 2016, order_dens >= 451.61 )%>%
  ggplot(aes(
    x = reorder(order, -order_dens), 
    y = log10(order_dens),
    color = season,
    fill = season)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~year, ncol= 1) +
  theme(axis.text.x = 
          element_text(angle = 45, hjust = 1, size = 12)) +
  labs(title = "GUIL Macroinvertebrates order density by season over the years")+
  theme(plot.title = element_text(hjust =0.5),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
G1 

  
  
}  

{
##### total inv_density by event ID.


 # select only site by species density info and remove duplicate records
 table_sample_by_taxon_density_long <- table_observation_cleaned %>%
   select(sampleID, acceptedTaxonID, inv_dens) %>%
   distinct() %>%
   filter(!is.na(inv_dens))
 # select only site by species density info and remove duplicate records
 table_sample_by_taxon_density_long2 <- table_observation_cleaned %>%
   select(sampleID, season, year, acceptedTaxonID, inv_dens) %>%
   distinct() %>%
   filter(!is.na(inv_dens))
 
 #table_sample_by_taxon_density_long %>% nrow()
 #table_sample_by_taxon_density_long %>% distinct() %>% nrow()
 
 
 
 # pivot to wide format, sum multiple counts per sampleID
 table_sample_by_taxon_density_wide <- table_sample_by_taxon_density_long %>%
   tidyr::pivot_wider(id_cols = sampleID, 
                      names_from = acceptedTaxonID,
                      values_from = inv_dens,
                      values_fill = list(inv_dens = 0),
                      values_fn = list(inv_dens = sum)) %>%
   column_to_rownames(var = "sampleID") 
 
 # check col and row sums -- mins should all be > 0
 colSums(table_sample_by_taxon_density_wide) %>% min()
 rowSums(table_sample_by_taxon_density_wide) %>% min()
 
 #gamma is regional biodiversity
 # alpha is local biodiversity (e.g., the mean diversity at a patch)  ##Alpha diversity is average local richness.
 # and beta diversity is a measure of among-patch variability in community composition.
 #beta = gamma / alpha
 
 # Here we use vegan::renyi to calculate Hill numbers
 # If hill = FALSE, the function returns an entropy
 # If hill = TRUE, the function returns the exponentiated
 # entropy. In other words:
 # exp(renyi entropy) = Hill number = "species equivalent"
 
 # Note that for this function, the "scales" argument 
 # determines the order of q used in the calculation
 
 table_sample_by_taxon_density_wide %>%
   vegan::renyi(scales = 0, hill = TRUE) %>%
   mean()
 
 # even distribution, orders q = 0 and q = 1 for 10 taxa
 vegan::renyi(
   c(spp.a = 10, spp.b = 10, spp.c = 10, 
     spp.d = 10, spp.e = 10, spp.f = 10, 
     spp.g = 10, spp.h = 10, spp.i = 10, 
     spp.j = 10),
   hill = TRUE,
   scales = c(0, 1))
 
 # uneven distribution, orders q = 0 and q = 1 for 10 taxa
 vegan::renyi(
   c(spp.a = 90, spp.b = 2, spp.c = 1, 
     spp.d = 1, spp.e = 1, spp.f = 1, 
     spp.g = 1, spp.h = 1, spp.i = 1, 
     spp.j = 1),
   hill = TRUE,
   scales = c(0, 1)) 
 
 ##Comparing orders of q of NEON data
 # Nest data by siteID
 data_nested_by_siteID <- table_sample_by_taxon_density_wide %>%
   tibble::rownames_to_column("sampleID") %>%
   left_join(table_sample_info %>% 
               select(sampleID, siteID)) %>%
   tibble::column_to_rownames("sampleID") %>%
   nest(data = -siteID)
 
 data_nested_by_siteID$data[[1]] %>%
   vegan::renyi(scales = 0, hill = TRUE) %>%
   mean()
 
 # apply the calculation by site for alpha diversity
 # for each order of q
 #Order q = 0 alpha diversity calculated for our dataset returns a mean local richness (i.e., species counts) 
 #Order q = 1 alpha diversity returns mean number of "species equivalents" per sample in the data set.
 
 data_nested_by_siteID %>% mutate(
   alpha_q0 = purrr::map_dbl(
     .x = data,
     .f = ~ vegan::renyi(x = .,
                         hill = TRUE, 
                         scales = 0) %>% mean()),
   alpha_q1 = purrr::map_dbl(
     .x = data,
     .f = ~ vegan::renyi(x = .,
                         hill = TRUE, 
                         scales = 1) %>% mean()),
   alpha_q2 = purrr::map_dbl(
     .x = data,
     .f = ~ vegan::renyi(x = .,
                         hill = TRUE, 
                         scales = 2) %>% mean())
 )
 
 # To calculate gamma diversity at the site scale,
 # calculate the column means and then calculate 
 # the renyi entropy and Hill number
 # Here we are only calcuating order 
 # q = 0 gamma diversity
 data_nested_by_siteID %>% mutate(
   gamma_q0 = purrr::map_dbl(
     .x = data,
     .f = ~ vegan::renyi(x = colMeans(.),
                         hill = TRUE, 
                         scales = 0)))
 
 # Now calculate alpha, beta, and gamma using orders 0 and 1 
 # for each siteID
 diversity_partitioning_results <- 
   data_nested_by_siteID %>% 
   mutate(
     n_samples = purrr::map_int(data, ~ nrow(.)),
     alpha_q0 = purrr::map_dbl(
       .x = data,
       .f = ~ vegan::renyi(x = .,
                           hill = TRUE, 
                           scales = 0) %>% mean()),
     alpha_q1 = purrr::map_dbl(
       .x = data,
       .f = ~ vegan::renyi(x = .,
                           hill = TRUE, 
                           scales = 1) %>% mean()),
     gamma_q0 = purrr::map_dbl(
       .x = data,
       .f = ~ vegan::renyi(x = colMeans(.),
                           hill = TRUE, 
                           scales = 0)),
     gamma_q1 = purrr::map_dbl(
       .x = data,
       .f = ~ vegan::renyi(x = colMeans(.),
                           hill = TRUE, 
                           scales = 1)),
     beta_q0 = gamma_q0 / alpha_q0,
     beta_q1 = gamma_q1 / alpha_q1)
 
 
 diversity_partitioning_results %>% 
   select(-data) %>% as.data.frame() %>% print()
}

 ###NMDS is the Nonmetric Multidimensional Scaling to ordinate samples
{
 # create ordination using NMDS
 my_nmds_result <- table_sample_by_taxon_density_wide %>% vegan::metaMDS()
 
 # plot stress
 my_nmds_result$stress
 p1 <- vegan::ordiplot(my_nmds_result)
 vegan::ordilabel(p1, "species")
 
 # merge NMDS scores with sampleID information for plotting
 nmds_scores <- my_nmds_result %>% 
   vegan::scores() %>%
   .[["sites"]] %>%
   as.data.frame() %>%
   tibble::rownames_to_column("sampleID") %>%
   left_join(table_sample_info)
 
 # # How I determined the outlier(s)
 nmds_scores %>% arrange(desc(NMDS1)) %>% head()
 nmds_scores %>% arrange(desc(NMDS1)) %>% tail()
 
 # Plot samples in community composition space by year
 nmds_scores %>%
   ggplot(aes(NMDS1, NMDS2, color = siteID, 
              shape = samplerType)) +
   geom_point() +
   facet_wrap(~ as.factor(year))
 
 # Plot samples in community composition space
 # facet by siteID and habitat type
 # color by year
 nmds_scores %>%
   ggplot(aes(NMDS1, NMDS2, color = as.factor(year), 
              shape = samplerType)) +
   geom_point() +
   facet_grid(habitatType ~ siteID, scales = "free")
 
 ##Maria addition
 # color by year and season
 nmds_scores %>%
   ggplot(aes(NMDS1, NMDS2, color = as.factor(year), 
              shape = season, size= 3)) +
   geom_point() +
   facet_grid(season ~ siteID, scales = "free")
}
 
 
 
 
 
 
 #remove unnessesary tables

 
 
 
 
 
 
 
 
 
 
 
    
  
