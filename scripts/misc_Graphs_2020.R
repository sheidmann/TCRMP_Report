# miscGraphs.R
# Sarah Heidmann
# Modeled after code from Rossie Ennis
# Created 22 Feb 2021
# Last modified 22 Feb 2021

# Miscellaneous graphs for the TCRMP report

# Load libraries
library(tidyverse)
library(cowplot)
library(RColorBrewer)

##### Import the data #####
dat <- read_csv("data/fulldata_2003_2020.csv",
                col_types = cols(Year=col_character()))

##### Set graph parameters #####
panel<-theme(panel.background = element_blank(), axis.line = element_line())
legend<-theme(legend.key = element_blank(), legend.position = "left", 
              legend.text = element_text(size=12,face="bold"))

##### total Nassau over time #####
# filter to nassau
epst_sum <- dat %>% 
   filter(ScientificName == "Epinephelus striatus") %>% 
   na.omit() %>% 
   group_by(Year, Site) %>%
   summarize(totepst = sum(SumAbundance), .groups = "drop") %>%
   # none seen in 2004 so add an empty row to keep it in the graph
   bind_rows(tibble(Year="2004", Site="Black Point", totepst=0)) 

epst_color<-c("darkred","darkorange3","gold","forestgreen","darkcyan","darkblue","darkorchid4","tomato3","orange","lightyellow2","peachpuff","seagreen3","lightskyblue","cyan","plum2","darkslategray4")

epst.plot <- ggplot(data = epst_sum, aes(x=Year, y=totepst, fill=Site)) +
   geom_bar(stat = "identity", position=position_stack()) +
   scale_y_continuous(expand = c(0,0)) +
   scale_fill_manual(values = epst_color, name="") +
   ylab("Total Number of Nassau grouper") +
   theme(legend.position = "bottom", legend.title = element_blank(),
         legend.justification = "center",
         axis.text.x = element_text(angle=60,size=12,vjust=0.6,face="bold"),
         axis.title.y = element_text(size = 14,face="bold"),
         axis.text.y = element_text(size=12,face="bold"),
         axis.title.x = element_blank(),
         panel.background = element_blank(), axis.line = element_line())

save_plot("outputs/Nassau_bytime_sitecolor_2020.jpeg", epst.plot, 
          base_aspect_ratio = 1, base_height = 8)
