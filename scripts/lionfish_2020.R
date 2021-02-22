# Lionfish_2020.R
# Sarah Heidmann
# Modeled after code from Rossie Ennis
# Created 22 Feb 2021
# Last modified 22 Feb 2021

# Graphs for the TCRMP report
# Abundance and biomass of lionfish only at all sites through time

library(tidyverse)
library(cowplot)

##### Read the data #####
dat <- read_csv("data/fulldata_2003_2020.csv",
                col_types = cols(Year=col_character()))

# Make a transect list so can add zeros
tranlist <- dat %>% 
   select(Year, Site, REEF.COMPLEX, Transect) %>%
   unique() %>% na.omit()

# Lionfish only
lion <- dat %>% 
   filter(ScientificName == "Pterois volitans" | is.na(ScientificName)) %>%
   select(Year, Site, REEF.COMPLEX, Transect, SumAbundance, SumBiomass)

# Add the zeros by merging with tranlist
# Need to keep the zeros distinct from the NAs
# Add row with a zero if in tranlist but not in data
zeros <- tibble(Year=character(),
                Site=character(),
                REEF.COMPLEX=character(),
                Transect=integer(),
                SumAbundance=numeric(),
                SumBiomass=numeric())
for(i in 1:nrow(tranlist)){
   year <- as.character(tranlist[i,"Year"])
   site <- as.character(tranlist[i,"Site"])
   reefc <- as.character(tranlist[i,"REEF.COMPLEX"])
   transect <- as.integer(tranlist[i,"Transect"])
   lionsub <- lion %>% filter(Year==year & Site==site & Transect==transect)
   if(nrow(lionsub)==0){
      zeros <- zeros %>% 
         bind_rows(tibble(Year=year,Site=site,REEF.COMPLEX=reefc,
                          Transect=transect,
                          SumAbundance=0,SumBiomass=0))
   }
}
lionz <- lion %>% bind_rows(zeros) %>%
   arrange(Year, Site, Transect)

# write_csv(lionz, "data/lionfish_2003_2020.csv")


##### Set the graph formatting #####
panel<-theme(panel.background = element_blank(), axis.line = element_line())
legend<-theme(legend.position = "left", 
              legend.key=element_rect(color=NA, fill=NA),
              legend.key.size = unit(14,"pt"),
              legend.text = element_text(size=12,face="bold"),
              legend.title = element_text(size=14))

nearshore.sites<-c("Black Point","Botany Bay","Brewers Bay","Cane Bay","Coculus Rock","Coral Bay","Fish Bay","Great Pond","Jacks Bay","Kings Corner","Magens Bay","Salt River West","Sprat Hole")
nearshore.linetype<-c("solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid")
nearshore.color<-c("darkred","darkorange","yellow2","forestgreen","darkcyan","darkblue","darkorchid4","darkred","darkorange","yellow2","forestgreen","darkcyan","darkblue")
nearshore.shape<-c(21,25,22,23,24,21,21,25,22,23,24,21,21)

offshore.sites<-c("Buck Island STT","Buck Island STX","Castle","Eagle Ray","Flat Cay","Mutton Snapper","Savana Island","Seahorse","South Capella","South Water","St. James")
offshore.linetype<-c("solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid")
offshore.color<-c("darkred","darkorange","yellow2","forestgreen","darkcyan","darkblue","darkorchid4","darkred","darkorange","yellow2","forestgreen")
offshore.shape<-c(21,25,22,23,24,21,21,25,22,23,24)

mesophotic.sites<-c("Buck Island STX Deep","Cane Bay Deep","College Shoal","Ginsburgs Fringe","Grammanik Tiger","Hind Bank FSA","Lang Bank EEMP","Lang Bank FSA","Meri Shoal","Salt River Deep")
mesophotic.linetype<-c("solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid")
mesophotic.color<-c("darkred","darkorange","yellow2","forestgreen","darkcyan","darkblue","darkorchid4","darkred","darkorange","yellow2","forestgreen")
mesophotic.shape<-c(21,25,22,23,24,21,21,25,22,23,24)

