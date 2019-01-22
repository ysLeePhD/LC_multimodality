
install.packages("tidyverse", dependencies =  TRUE)
install.packages("RColorBrew", dependencies =  TRUE)

library(tidyverse)
library(RColorBrewer)

modefreq00 <- read_csv(file="M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_result_modefreq.csv")
colnames(modefreq00)[1] <- c("Purpose_mode")
modefreq00

modefreq01 <- modefreq00 %>%
  gather(`Monomodal driver`, `Carpooler`, `Active traveler`, `Transit rider`, key = "class", value = "frequency")

modefreq01$VMD <- 0

modefreq01[37, ] <- c("Weekly VMD", "Monomodal driver", "", 144)
modefreq01[38, ] <- c("Weekly VMD", "Carpooler", "", 115)
modefreq01[39, ] <- c("Weekly VMD", "Active traveler", "", 47)
modefreq01[40, ] <- c("Weekly VMD", "Transit rider", "", 64)

modefreq01$frequency <- as.numeric(modefreq01$frequency)
modefreq01$VMD <- as.numeric(modefreq01$VMD)

modefreq01


modefreq01$Purpose_mode <- factor(modefreq01$Purpose_mode, 
                              levels = c("Commute-Car as a driver",
                                         "Commute-Car as a passenger", 
                                         "Commute-Public transit", 
                                         "Commute-Active modes", 
                                         "Leisure-Car as a driver",  
                                         "Leisure-Car as a passenger", 
                                         "Leisure-Public transit", 
                                         "Leisure-Active modes", 
                                         "Leisure-Emerging modes", 
                                         "Weekly VMD"), 
                              labels = c("Commute\ndriving", 
                                         "Commute\npassenger", 
                                         "Commute\ntransit", 
                                         "Commute\nactive", 
                                         "Non-commute\ndriving", 
                                         "Non-commute\ncar passenger", 
                                         "Non-commute\ntransit", 
                                         "Non-commute\nactive", 
                                         "Non-commute\nemerging", 
                                         "Weekly VMD"), ordered = TRUE) 
modefreq01$class <- factor(modefreq01$class, 
                       levels = c("Monomodal driver", 
                                  "Carpooler", 
                                  "Active traveler", 
                                  "Transit rider"), 
                       labels = c(" Monomodal driver       ", 
                                  " Carpooler       ", 
                                  " Active traveler       ", 
                                  " Transit rider       "), ordered = TRUE)

#ggplot(data01) + 
#  geom_bar(
#    mapping = aes(x = Purpose_mode, y = frequency), 
#    stat = "identity") + 
#    facet_wrap( ~ class)

#https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
#https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer
#https://rpubs.com/MarkusLoew/226759


ggplot() + 
  geom_bar(data = modefreq01, 
           aes(x=Purpose_mode, y=frequency, fill = class, color = class), 
           position = 'dodge', stat = "identity") + 
  geom_bar(data = modefreq01, 
           aes(x=Purpose_mode, y=VMD/7, fill = class, color = class), 
           position = 'dodge', stat = 'identity') + 
  scale_fill_brewer(palette = "YlGnBu", direction=-1) + 
  scale_y_continuous(sec.axis = sec_axis(~.*7, name = "Weekly VMD")) + 
  theme(legend.position = "bottom", 
        legend.title=element_blank(), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,00,0)) +
  labs(y = "Monthly Frequency", x = "") 

  
ggsave(file="C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/02_Transp_LCA_multimodal/modefreq.jpg", dpi=300)


  
