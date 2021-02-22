# AbunBioRichOverTime_2020.R
# Sarah Heidmann
# Created 22 Feb 2021
# Last modified 22 Feb 2021

# Modified from AbunBioRichOverTime_2019.R
# Which was modified from Rossie Ennis' code
# Graphs for the TCRMP report
# Abundance, Biomass, and Richness at all sites through time

# Load the libraries
library(tidyverse)
library(cowplot)

# Read the data (from Shaun Kadison)
dat <- read_csv("data/fulldata_2003_2020.csv",
                col_types = cols(Year=col_character()))


# Take out all the gobies (~400 records)
dat <- dat %>% filter(Family!="Gobiidae")

##### Set the graph formatting #####
panel<-theme(panel.background = element_blank(), axis.line = element_line())
legend<-theme(legend.position = "left", legend.key=element_rect(color=NA, fill=NA),
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

##### Abundance #####
# Fish per 100m2 (one transect)
# Sum all fish on each transect, and take the average and SEM for each site
abunsum <- dat %>%
   group_by(Year, Site, REEF.COMPLEX, Transect) %>%
   summarize(totabun = sum(SumAbundance), .groups = "drop_last") %>%
   summarize(meanabun = mean(totabun),
             n = length(Transect),
             sd = sd(totabun), .groups = "drop") %>%
   mutate(sem = sd / sqrt(n))
abunsum

# Nearshore
abun.nearshore <- ggplot(data = filter(abunsum, REEF.COMPLEX == "Nearshore"),
                         aes(x=Year,y=meanabun,group=Site))+
   geom_line(size=1,color="black",aes(linetype=Site))+
   geom_point(size=4.5,color="black",aes(shape=Site,fill=Site))+
   geom_errorbar(aes(ymax=meanabun+sem, ymin=meanabun-sem), width=0.2) +
   scale_linetype_manual(values = nearshore.linetype,
                         labels=nearshore.sites, 
                         name= "Nearshore")+
   scale_fill_manual(values = nearshore.color,
                     labels=nearshore.sites, 
                     name="Nearshore")+
   scale_shape_manual(values = nearshore.shape,
                      labels=nearshore.sites,
                      name="Nearshore")+
   panel+
   legend+
   ylab(expression(bold(paste("Fish/100",m^2))))+
   scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,200)) +
   theme(axis.title.y = element_text(size = 14,face="bold"),
         axis.text.y = element_text(size=12,face="bold"),
         axis.text.x = element_text(angle=60,size=12,vjust=0.6,face="bold"),
         axis.title.x = element_blank())
# Offshore
abun.offshore <- ggplot(data = filter(abunsum, REEF.COMPLEX == "Offshore"),
                        aes(x=Year,y=meanabun,group=Site))+
   geom_line(size=1,color="black",aes(linetype=Site))+
   geom_point(size=4.5,color="black",aes(shape=Site,fill=Site))+
   geom_errorbar(aes(ymax=meanabun+sem, ymin=meanabun-sem), width=0.2) +
   scale_linetype_manual(values = offshore.linetype,
                         labels=offshore.sites, 
                         name= "Offshore")+
   scale_fill_manual(values = offshore.color,
                     labels=offshore.sites, 
                     name="Offshore")+
   scale_shape_manual(values = offshore.shape,
                      labels=offshore.sites,
                      name="Offshore")+
   panel+
   legend+
   ylab(expression(bold(paste("Fish/100",m^2))))+
   scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,200)) +
   theme(axis.title.y = element_text(size = 14,face="bold"),
         axis.text.y = element_text(size=12,face="bold"),
         axis.text.x = element_text(angle=60,size=12,vjust=0.6,face="bold"),
         axis.title.x = element_blank())
# Mesophotic
abun.mesophotic <- ggplot(data = filter(abunsum, REEF.COMPLEX == "Mesophotic"),
                          aes(x=Year,y=meanabun,group=Site))+
   geom_line(size=1,color="black",aes(linetype=Site))+
   geom_point(size=4.5,color="black",aes(shape=Site,fill=Site))+
   geom_errorbar(aes(ymax=meanabun+sem, ymin=meanabun-sem), width=0.2) +
   scale_linetype_manual(values = mesophotic.linetype,
                         labels=mesophotic.sites, 
                         name= "Mesophotic")+
   scale_fill_manual(values = mesophotic.color,
                     labels=mesophotic.sites, 
                     name="Mesophotic")+
   scale_shape_manual(values = mesophotic.shape,
                      labels=mesophotic.sites,
                      name="Mesophotic")+
   panel+
   legend+
   ylab(expression(bold(paste("Fish/100",m^2))))+
   scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000,200)) +
   theme(axis.title.y = element_text(size = 14,face="bold"),
         axis.text.y = element_text(size=12,face="bold"),
         axis.text.x = element_text(angle=60,size=12,vjust=0.6,face="bold"),
         axis.title.x = element_blank())

abunplot <- plot_grid(abun.nearshore,abun.offshore,abun.mesophotic,ncol = 1,align = "v")
save_plot("outputs/abundance_bytime_splitreef_2020.jpeg", abunplot, 
          base_aspect_ratio = 0.8, base_height = 11)
