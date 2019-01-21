
install.packages("tidyverse", dependencies =  TRUE)
install.packages("RColorBrew", dependencies =  TRUE)

library(tidyverse)
library(RColorBrewer)

data00 <- read_csv(file="M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_result_modefreq.csv")
colnames(data00)[1] <- c("Purpose_mode")
data00

data01 <- data00 %>%
  gather(`Monomodal driver`, `Carpooler`, `Active traveler`, `Transit rider`, key = "class", value = "frequency")

data01$Purpose_mode <- factor(data01$Purpose_mode, 
                              levels = c("Commute-Car as a driver",
                                         "Commute-Car as a passenger", 
                                         "Commute-Public transit", 
                                         "Commute-Active modes", 
                                         "Leisure-Car as a driver",  
                                         "Leisure-Car as a passenger", 
                                         "Leisure-Public transit", 
                                         "Leisure-Active modes", 
                                         "Leisure-Emerging modes"), 
                              labels = c("C-driver", 
                                         "C-passenger", 
                                         "C-transit", 
                                         "C-active", 
                                         "L-driver", 
                                         "L-passenger", 
                                         "L-transit", 
                                         "L-active", 
                                         "L-emerging"), ordered = TRUE) 
data01$class <- factor(data01$class, 
                       levels = c("Monomodal driver", 
                                  "Carpooler", 
                                  "Active traveler", 
                                  "Transit rider"), 
                       labels = c("Driver", 
                                  "Carpooler", 
                                  "Active traveler", 
                                  "Transit rider"), ordered = TRUE)

#ggplot(data01) + 
#  geom_bar(
#    mapping = aes(x = Purpose_mode, y = frequency), 
#    stat = "identity") + 
#    facet_wrap( ~ class)

#https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
ggplot(data01, aes(Purpose_mode, frequency)) + 
  geom_bar(aes(fill = class), position = 'dodge', stat = "identity") + 
  scale_fill_brewer(palette = "YlGnBu", direction=-1) + 
  #http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  #https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer
  #scale_fill_grey() + 
  theme_minimal() + 
  theme(legend.position = "bottom", legend.title=element_blank()) + 
  xlab("") + 
  ylab("")



  
