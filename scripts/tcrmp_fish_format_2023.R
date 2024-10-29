# tcrmp_fish_format_2023.R
# Sarah Heidmann
# Created 24 Oct 2024
# Last modified 29 Oct 2024

# This script takes csvs exported from our online data entry.
# QAQC has already been done manually on this data.
# It formats it to fit our master spreadsheet and exports.

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

# Fish trophic data
trophs <- read_csv("data/fish_species_uvimaster.csv") %>% 
   select(`Scientific name`=ScientificName, TrophicGroup=TrophicLevel)

##### Fish Transects #####
# Location, SampleYear, SampleMonth, Period, Transect, ScientificName, CommonName, TrophicGroup, 0-5:91-100, SppTotal
fts_exp <- fts_raw %>% 
   # Fix some spelling mistakes
   mutate(`Scientific name`=gsub("sp.","sp", `Scientific name`),
          `Scientific name`=gsub("Microspthodon chrysurus", 
                                 "Microspathodon chrysurus", `Scientific name`),
          `Scientific name`=gsub("Gymnothorax spcies","Gymnothorax sp", 
                                 `Scientific name`)) %>% 
   # Split the date
   mutate(SampleYear = year(`Date completed`),
          SampleMonth = month(`Date completed`),
      # Add Period column
          Period = period) %>%
   # Trophic classification
   left_join(trophs, by="Scientific name") %>% 
   # Species Total
   mutate(SppTotal = rowSums(select(.,starts_with("X")), na.rm=TRUE)) %>% 
   # Replace sizebin NAs with 0
   mutate(across(starts_with("X"), ~replace_na(.,0))) %>% 
   # Pick only needed columns and rename
   select(Location=`Site name`, SampleYear, SampleMonth, Period, Transect=Rep, 
          ScientificName=`Scientific name`, CommonName=`Common name`, TrophicGroup,
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
   # Fix some spelling mistakes
   mutate(`Scientific name`=gsub("sp.","sp", `Scientific name`),
          `Scientific name`=gsub("Sphoeroides spngleri", 
                                 "Sphoeroides spengleri", `Scientific name`),
          `Scientific name`=gsub("Microspthodon chrysurus", 
                                 "Microspathodon chrysurus", `Scientific name`),
          `Scientific name`=gsub("Paraspyraenops incisus", 
                                 "Parasphyraenops incisus", `Scientific name`),
          `Scientific name`=gsub("Rypticus bistrispnus", 
                                 "Rypticus bistrispinus", `Scientific name`),
          `Scientific name`=gsub("Trachinotus falcatus", "Trachinotus blochii", 
                                 `Scientific name`)) %>% 
   # Split the date
   mutate(SampleYear = year(`Date completed`),
          SampleMonth = month(`Date completed`),
   # Add Period column
          Period = period) %>% 
   # Trophic classification
   left_join(trophs, by="Scientific name") %>% 
   # Pick only needed columns and rename
   select(Location=`Site name`, SampleYear, SampleMonth, Period, Transect=Rep, 
          ScientificName=`Scientific name`, CommonName=`Common name`, TrophicGroup,
          AbundanceIndex=`Abundance index`)

##### Diademas #####
# Location, SampleDate, SampleYear, SampleMonth, Period, Recorder (First Last), Transect, TestSize, Notes
das_exp <- das_raw %>% 
   # Split the date
   mutate(SampleYear = year(`Date completed`),
          SampleMonth = month(`Date completed`),
   # Add period column
          Period = period,
   SampleDate = format(`Date completed`, "%m/%d/%y")) %>% 
   # Reformat observer name
   separate(Name, c("last","first")) %>% 
   mutate(Recorder = str_to_title(paste(first,last,sep=" "))) %>% 
   # Pick only needed columns and rename
   select(Location=`Site name`, SampleDate, SampleYear, SampleMonth, Period,
          Recorder, Transect=Rep, TestSize=`Test size cm`, Notes)
# That's the individual sizes, but we also want one with density summarized.
# Location, SampleDate, SampleYear, SampleMonth, Period, Recorder (First Last), Transect, TransSize, NoDiad, DiadDens, Notes
das_sum <- das_exp %>% 
   # Add transect size
   mutate(TransSize=50) %>% 
   # Summarize by ID variables
   group_by(Location, SampleDate, SampleYear, SampleMonth,Period,Recorder,
            Transect,TransSize,Notes) %>% 
   summarize(NoDiad = length(which(TestSize!=0)), .groups="drop") %>% 
   # Calculate density
   mutate(DiadDens=NoDiad*100/TransSize) %>% 
   # Reorder columns
   select(Location, SampleDate, SampleYear, SampleMonth,Period,Recorder,Transect,
          TransSize,NoDiad,DiadDens,Notes)

##### Export formatted data #####
write.xlsx(fts_exp, paste0("outputs/TCRMP_FishTransects_",reportperiod,".xlsx"), na="")
write.xlsx(frs_exp, paste0("outputs/TCRMP_FishRovers_",reportperiod,".xlsx"), na="")
write.xlsx(das_exp, paste0("outputs/TCRMP_DiademaSize_",reportperiod,".xlsx"), na="")
write.xlsx(das_sum, paste0("outputs/TCRMP_DiademaDens_",reportperiod,".xlsx"),na="")
