# PrelimFormatting_2020.R
# Sarah Heidmann
# Created 18 Feb 2021
# Last modified 18 Feb 2021

# Modified from PrelimFormatting_2019.R
# Formatting TCRMP fish data from 36 sites from 2003-2020.
# To make graphs for the 2020 TCRMP report

# Load the libraries
library(tidyverse)

##### Metadata #####
# Each transect is a 25x4 m belt transect of 15 minutes duration
# Transect direction is chosen randomly
# 
# Total number of fish by species is represented by the "SumAbundance column
# 
# Fish biomass per size class has been calculated based upon the length-weight relationship per species
# Biomass is calculated as (g/100m2)
# Total biomass by species is represented by the "SumBiomass" column
# There are no zeros in this data. i.e. if a species was not present it does not exist


##### Import the data #####
fish_old <- read_csv("data/fulldata_2003_2019.csv",
                     col_types = cols(Year=col_character()))
fish_new <- read_csv("data/STT Fish for graphs.csv",
                     col_types = cols(Year = col_factor())) %>%
   rename(ScientificName = `Scientific name`,
          CommonName = `Common name`,
          SumAbundance = `SumOfTotal Number1`,
          SumBiomass = `SumOfTotal Species Biomass`)
sites <- read_csv("data/sitelist_2020.csv")
species <- read_csv("data/speciesgroups_2020.csv")

##### Fix to match old data and primary QAQC #####
#str(fish_old)
#str(fish_new)

# Year as factor
fish_old <- fish_old %>%
   mutate(Year = factor(Year, levels = c(as.character(seq(2003,2016,1)),
                                         "2017/2018",seq(2018,2020,1))))
unique(fish_old$Year)

# Site names to match old ones
fish_new %>%
   filter(!(Site %in% sites$Site)) %>%
   select(Site) %>% unique()
fish_new <- fish_new %>%
   mutate(Site = recode(Site, 
                        # old = new
                        "College Shoal East"= "College Shoal",
                        "Grammanik Tiger FSA" = "Grammanik Tiger",
                        "Hind Bank East FSA" = "Hind Bank FSA",
                        "Savana" = "Savana Island",
                        "St James" = "St. James"))

# Final check- should all be true
summary(fish_new$Site %in% sites$Site)

# Transect is good
summary(as.factor(fish_new$Transect)) #1 thru 9

# Scientific name spelling and updates
fish_new %>%
   select(ScientificName) %>%
   filter(!(ScientificName %in% species$SCIENTIFIC.NAME)) %>% unique()
fish_new <- fish_new %>%
   mutate(ScientificName = recode(ScientificName, 
                                  # old = new
                                  "Scarus iserti"= "Scarus iseri",
                                  "Haemulon plumieri"="Haemulon plumierii",
                                  "Priacanthus cruentatus"="Heteropriacanthus cruentatus",
                                  "Lutjanus mahogani"="Lutjanus mahogoni",
                                  "Dasyatis americana"="Hypanus americanus",
                                  "Ophioblennius atlanticus"="Ophioblennius macclurei",
                                  "Caranx bartholomaei"="Carangoides bartholomaei"))
   
# Update old data with names that changed
fish_old %>%
   select(ScientificName) %>%
   filter(!(ScientificName %in% species$SCIENTIFIC.NAME)) %>% unique()
# There is still an issue with the presence of "Caranx niger"
# This is not a species. Does it mean another Caranx or a black durgon?

# Merge with site and species
fish_new_full <- fish_new %>%
   select(Year,Site,Transect,ScientificName,SumAbundance,SumBiomass) %>%
   left_join(sites, by="Site") %>%
   mutate(SCIENTIFIC.NAME = ScientificName) %>%
   left_join(species, by="SCIENTIFIC.NAME") %>%
   rename(CommonName = COMMON.NAME,
          Family = FAMILY,
          Code = SPP,
          PrimaryTroph = TROPH,
          Commercial = COMMERCIAL)

##### Secondary QAQC- NOAA numbers #####
# Abundance
abund <- fish_new_full %>%
   mutate(flag = as.factor(ifelse(SumAbundance > max_num, "yes","no")))

# Check out the flags
summary(abund$flag)
summary(abund$flag)[2] / nrow(abund) # 4.8% flagged
abund_flag <- abund %>% filter(flag == "yes") %>%
   select(Year,Site,Transect,ScientificName,CommonName,SumAbundance,SumBiomass)
abund_flag
unique(abund_flag$CommonName)
# write_csv(abund_flag, "outputs/flag_abundance_2020.csv")

# Can't check sizes since I don't have the breakdown

##### Join and export #####
# Export full clean 2020 data
# write_csv(fish_new_full,"outputs/2020TCRMPfish_all_clean.csv")

# Reduce and combine with old data
fish <- fish_new_full %>% 
   select(names(fish_old)) %>%
   bind_rows(fish_old, .) %>%
   arrange(Year, Site, Transect, ScientificName)

# Export for easy use
# write_csv(fish,"outputs/fulldata_2003_2020.csv")

##### Basic descriptions #####
unique(fish$Year) # 2003-2020 (18 years)
levels(as.factor(fish$Site)) # 34 sites
nrow(fish[is.na(fish$ScientificName),]) #173 with NAs (data gaps; = no transects at a site-year combo)


# There is still a 2008 Brewers Bay threespot damsel with no transect number.
fish[fish$Year==2008 & fish$Site=="Brewers Bay" & fish$ScientificName=="Stegastes planifrons",]
