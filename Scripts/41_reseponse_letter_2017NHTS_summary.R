
library(tidyverse)


# read in the NHTS trip table 

nhts00 <- read_csv("M:/Uber_NHTS/03_NHTS/Csv/trippub.csv")
colnames(nhts00)



# A1. mode share of cars for commutes in California

nhts01 <- nhts00 %>% 
  select(HHSTATE, TRPTRANS, WHYTO, WTTRDFIN) %>% 
  filter(HHSTATE == "CA" & (WHYTO == "03" | WHYTO == "08")) %>%
  mutate(dr = ifelse(TRPTRANS %in% c("03", "04", "05", "06"), 1, 0))

round(sum(nhts01[nhts01$dr==1, ]$WTTRDFIN) / sum(nhts01$WTTRDFIN) * 100, 1) 



# A2. mode share of cars for all trips in California

nhts02 <- nhts00 %>% 
  select(HHSTATE, TRPTRANS, WHYTO, WTTRDFIN) %>% 
  filter(HHSTATE == "CA") %>%
  mutate(dr = ifelse(TRPTRANS %in% c("03", "04", "05", "06"), 1, 0))

round(sum(nhts02[nhts02$dr==1, ]$WTTRDFIN) / sum(nhts02$WTTRDFIN) * 100, 1) 



# B1. mode share of cars for school trips in California

nhts03 <- nhts00 %>% 
  select(HHSTATE, R_AGE, TRPTRANS, WHYTO, WTTRDFIN) %>% 
  filter(HHSTATE == "CA" & WHYTO == "08" & R_AGE >=18 & R_AGE <=37) %>%
  mutate(modegroup = ifelse(TRPTRANS %in% c("03", "04", "05", "06"), 1, # cars  
                            ifelse(TRPTRANS %in% c("01", "02"), 2, # active modes 
                                   ifelse(TRPTRANS %in% c("11", "12", "13", "15", "16"), 3, 4)))) %>% #public transit & others
  group_by(modegroup) %>% 
  summarize(sum = sum(WTTRDFIN))

nhts03$pct <- nhts03$sum / sum(nhts03$sum) * 100

nhts03


