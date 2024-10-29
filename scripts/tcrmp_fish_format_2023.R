# tcrmp_fish_format_2023.R
# Sarah Heidmann
# Created 24 Oct 2024
# Last modified 24 Oct 2024

# This script takes csvs exported from our online data entry.
# QAQC has already been done manually on this data.
# It formats it to fit our master spreadsheet and exports.

# To do: 
# - rematch headers to master
# - trophic classification
# - fill empty size bins with 0
# - calculate total number
# - ask nicole about adding size bins >100

# Load libraries
library(tidyverse)
library(lubridate)
library(openxlsx)

##### Import the data #####
reportperiod <- "2023"
period <- "Annual" # Annual or PostHurr or WS

# Fish transects
fts_raw <- read_csv(paste0("data/",reportperiod,"/TCRMP_FishTransects_",reportperiod,"_qaqc.csv"),
                    col_types = cols(Proofed = col_logical(),
                                     `Site name` = col_character(),
                                     `Date completed`=col_date(format = "%m/%d/%y"),
                                     Name = col_character(),
                                     `Oc cc`= col_character(),
                                     Notes = col_character(),
                                     `Common name` = col_character(),
                                     `Scientific name`= col_character(),
                                     .default=col_double()))

# Fish rovers
frs_raw <- read_csv(paste0("data/",reportperiod,"/TCRMP_FishRovers_",reportperiod,"_qaqc.csv"),
                    col_types = cols(`Date completed`=col_date(format = "%m/%d/%y")))
# Diademas
das_raw <- read_csv(paste0("data/",reportperiod,"/TCRMP_Diadema_",reportperiod,"_qaqc.csv"),
                    col_types = cols(`Date completed`=col_date(format = "%m/%d/%y")))

# Site metadata to merge with Diadema data
sites <- read_csv("data/sitelist_2023.csv")

##### Fish Transects #####
# Location, SampleYear, SampleMonth, Period, Transect, ScientificName, CommonName, TrophicGroup, 0-5:91-100, SppTotal
fts_exp <- fts_raw %>% 
   # Split the date
   mutate(SampleYear = year(`Date completed`),
          SampleMonth = month(`Date completed`),
      # Add Period column
          Period = period) %>%
   # Trophic classification
   # Species Total
   mutate(SppTotal = rowSums(select(.,starts_with("X")), na.rm=TRUE)) %>% 
   # Replace sizebin NAs with 0
   mutate(across(starts_with("X"), ~replace_na(.,0))) %>% 
   # Pick only needed columns and rename
   select(Location=`Site name`, SampleYear, SampleMonth, Period, Transect=Rep, 
          ScientificName=`Scientific name`, CommonName=`Common name`, 
          starts_with("X"), SppTotal) %>% 
   # Change gt to >
   rename_with(.data=., .fn = ~ gsub("to","-",gsub("gt","\u003E",.)), 
               .cols = starts_with("X")) %>% 
   # Take out X on size columns
   rename_with(.data=., .fn = ~ gsub("to","-",gsub("X","",.)), 
               .cols = starts_with("X"))
##### Fish Rovers #####
# We don't have a master sheet for this one so let's make it similar to transects
# Location, SampleYear, SampleMonth, Period, Transect, ScientificName, CommonName, TrophicGroup, AbundanceIndex
frs_exp <- frs_raw %>% 
   # Split the date
   mutate(SampleYear = year(`Date completed`),
          SampleMonth = month(`Date completed`),
   # Add Period column
          Period = period) %>% 
   # Pick only needed columns and rename
   select(Location=`Site name`, SampleYear, SampleMonth, Period, Transect=Rep, 
          ScientificName=`Scientific name`, CommonName=`Common name`, 
          AbundanceIndex=`Abundance index`)

##### Diademas #####
# YEAR, ISLAND, LOCATION, DATE, REPORT PERIOD, ORIENTATION, LAND, REEF COMPLEX, TRANSECT, DEPTH, RECORDER, DIADEMA TEST (cm),
das_exp <- das_raw %>% 
   left_join(sites, by =c(`Site name` = "Site")) %>% 
   mutate(YEAR = year(`Date completed`),
          DATE = `Date completed`) %>% 
   add_column(`REPORT PERIOD`=reportperiod) %>% 
   select(YEAR, ISLAND, `Site name`, DATE, `REPORT PERIOD`, ORIENTATION, LAND,
          REEF.COMPLEX, Rep, DEPTH, Name, `Test size cm`) %>% 
   rename(LOCATION = `Site name`, `REEF COMPLEX` = REEF.COMPLEX, 
          TRANSECT = Rep, RECORDER = Name, 
          `DIADEMA TEST (cm)`=`Test size cm`)
# That's the individual sizes, but we also want one with density summarized.
# Location, SampleDate, SampleYear, SampleMonth, Period, Recorder, Transect, TransSize, NoDiad, DiadDens, Notes
das_sum <- das_exp %>% 
   group_by(YEAR, LOCATION, DATE, `REPORT PERIOD`,TRANSECT,RECORDER) %>% 
   summarize(NoDiad = length(which(`DIADEMA TEST (cm)`!=0)), .groups="drop") %>% 
   mutate(SampleMonth=month(DATE),Period="Annual",TranSize=50,Notes=NA) %>% 
   mutate(DiadDens=NoDiad*100/TranSize) %>% 
   select(Location=LOCATION,SampleDate=DATE, SampleYear=YEAR, SampleMonth,
          Period, Recorder=RECORDER, Transect=TRANSECT, TranSize,NoDiad,DiadDens,Notes)

##### Export formatted data #####
write.xlsx(fts_exp, paste0("outputs/TCRMP_FishTransects_",reportperiod,".xlsx"), na="")
write.xlsx(frs_exp, paste0("outputs/TCRMP_FishRovers_",reportperiod,".xlsx"), na="")
write.xlsx(das_exp, paste0("outputs/TCRMP_DiademaSize_",reportperiod,".xlsx"), na="")
write.xlsx(das_sum, paste0("outputs/TCRMP_DiademaDens_",reportperiod,".xlsx"),na="")
