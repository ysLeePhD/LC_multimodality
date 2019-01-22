
library(tidyverse)


# read in the NHTS trip table 

nhts00 <- read_csv("M:/Uber_NHTS/03_NHTS/Csv/trippub.csv")
colnames(nhts00)



# mode share of cars for commutes in California

nhts01 <- nhts00 %>% 
  select(HHSTATE, TRPTRANS, WHYTO, WTTRDFIN) %>% 
  filter(HHSTATE == "CA" & (WHYTO == "03" | WHYTO == "08")) %>%
  mutate(dr = ifelse(TRPTRANS %in% c("03", "04", "05", "06"), 1, 0))

round(sum(nhts01[nhts01$dr==1, ]$WTTRDFIN) / sum(nhts01$WTTRDFIN) * 100, 1) 



# mode share of cars for all trips in California

nhts02 <- nhts00 %>% 
  select(HHSTATE, TRPTRANS, WHYTO, WTTRDFIN) %>% 
  filter(HHSTATE == "CA") %>%
  mutate(dr = ifelse(TRPTRANS %in% c("03", "04", "05", "06"), 1, 0))

round(sum(nhts02[nhts02$dr==1, ]$WTTRDFIN) / sum(nhts02$WTTRDFIN) * 100, 1) 
