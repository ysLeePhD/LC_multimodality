

library(tidyverse) 

stat00 <- read.csv("M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_results.csv")

stat00$wprob1 <- stat00$prob1*stat00$wt 
stat00$wprob2 <- stat00$prob2*stat00$wt 
stat00$wprob3 <- stat00$prob3*stat00$wt 
stat00$wprob4 <- stat00$prob4*stat00$wt 
stat00$PID <- stat00$id 

colnames(stat00)
stat01 <- stat00[, c(41, 37:40, 34)] 
stat02 <- left_join(stat01, data11, by="PID")
colnames(stat02)

stat02$TeleFreq <- 0 
stat02$TeleFreq <- ifelse(stat02$TeleFreq1==1, 1, stat02$TeleFreq)
stat02$TeleFreq <- ifelse(stat02$TeleFreq2==1, 2, stat02$TeleFreq)
stat02$TeleFreq <- factor(stat02$TeleFreq, labels=c("No", "Less than once a week", "At least once a week"), ordered=TRUE)
table(stat02$TeleFreq)
#stat02$TeleFreq0 <- NULL
#stat02$TeleFreq1 <- NULL
#stat02$TeleFreq2 <- NULL

stat02$WorkStudyStat <- 0 
stat02$WorkStudyStat <- ifelse(stat02$FTwork==1, 1, stat02$WorkStudyStat)
stat02$WorkStudyStat <- ifelse(stat02$PTwork==1, 2, stat02$WorkStudyStat)
stat02$WorkStudyStat <- ifelse(stat02$FTstudy==1, 3, stat02$WorkStudyStat)
stat02$WorkStudyStat <- ifelse(stat02$PTstudy==1, 4, stat02$WorkStudyStat)
stat02$WorkStudyStat <- factor(stat02$WorkStudyStat, labels=c("Unpaid work", "Work fulltime", "Work parttime", 
                                                              "Study fulltime", "Study parttime"), ordered=TRUE)
table(stat02$WorkStudyStat)
#stat02$FTwork <- NULL
#stat02$PTwork <- NULL
#stat02$FTstudy <- NULL
#stat02$PTstudy <- NULL

stat02$Education <- 1 
stat02$Education <- ifelse(stat02$ednoanswer==1, 0, stat02$Education)
stat02$Education <- ifelse(stat02$somecoll==1, 2, stat02$Education)
stat02$Education <- ifelse(stat02$bachelor==1, 3, stat02$Education)
stat02$Education <- ifelse(stat02$graduate==1, 4, stat02$Education)
stat02$Education <- factor(stat02$Education, labels=c("Decline to answer", "Up to highschool", "Associate's degree", 
                                                      "Bachelor's degree", "Graduate degree"), ordered=TRUE)
table(stat02$Education)
#stat02$somecoll <- NULL
#stat02$bachelor <- NULL
#stat02$graduate <- NULL

stat02$HHincome <- 0 
stat02$HHincome <- ifelse(stat02$lowhhinc==1, 1, stat02$HHincome)
stat02$HHincome <- ifelse(stat02$midhhinc==1, 2, stat02$HHincome)
stat02$HHincome <- ifelse(stat02$highhhinc==1, 3, stat02$HHincome)
stat02$HHincome <- factor(stat02$HHincome, labels=c("Decline to answer", "~$60,000", 
                                                    "$60,001~$120,000", "More than $120,000"), ordered=TRUE)
table(stat02$HHincome)

stat02$E1car_rating <- factor(stat02$E1car_rating, labels=c("Very bad", "Bad", "Neutral", "Good", "Very good"), ordered=TRUE)
table(stat02$E1car_rating)
stat02$E2pt_rating  <- factor(stat02$E2pt_rating,  labels=c("Very bad", "Bad", "Neutral", "Good", "Very good"), ordered=TRUE)
table(stat02$E2pt_rating)
stat02$E3at_rating  <- factor(stat02$E3at_rating,  labels=c("Very bad", "Bad", "Neutral", "Good", "Very good"), ordered=TRUE)
table(stat02$E3at_rating)

stat02$NHtype <- 0 
stat02$NHtype <- ifelse(stat02$ccity==1, 1, stat02$NHtype) 
stat02$NHtype <- ifelse(stat02$urban==1, 2, stat02$NHtype) 
stat02$NHtype <- ifelse(stat02$suburb==1, 3, stat02$NHtype)
stat02$NHtype <- ifelse(stat02$rural_in_urban==1, 4, stat02$NHtype)
stat02$NHtype <- ifelse(stat02$rural==1, 5, stat02$NHtype)
stat02$NHtype <- factor(stat02$NHtype, labels=c("Central city", "Urban", "Suburban", "Rural in urban", "Rural"), ordered=TRUE)
table(stat02$NHtype)

stat02$ComDist <- exp(stat02$lndistance)-1
stat02$TQ2 <- exp(stat02$lnTQ2)-1

length(colnames(stat02))
stat03a <- stat02[stat02$wprob1>0, c(1, 2, 6:length(colnames(stat02)))]
stat03a$wprob <- stat03a$wprob1  
stat03a$wprob1<- NULL 
stat03a$class <- 1  

stat03b <- stat02[stat02$wprob2>0, c(1, 3, 6:length(colnames(stat02)))]
stat03b$wprob <- stat03b$wprob2 
stat03b$wprob2<- NULL 
stat03b$class <- 2  

stat03c <- stat02[stat02$wprob3>0, c(1, 4, 6:length(colnames(stat02)))]
stat03c$wprob <- stat03c$wprob3  
stat03c$wprob3<- NULL 
stat03c$class <- 3 

stat03d <- stat02[stat02$wprob4>0, c(1, 5, 6:length(colnames(stat02)))]
stat03d$wprob <- stat03d$wprob4  
stat03d$wprob4<- NULL 
stat03d$class <- 4  

stat04 <- rbind(stat03a, stat03b)
stat04 <- rbind(stat04,  stat03c)
stat04 <- rbind(stat04,  stat03d)

colnames(stat04)
stat05 <- merge(stat04, data03[, c(1, 9, 7, 10)], by="PID")
stat06 <- merge(stat05, data01[, c(1, 14, 15)], by="PID")
stat06$Millennials <- ifelse(stat06$Millennials==2, 0, stat06$Millennials)


library(tableone)
library(grid) 
library(Matrix)
library(survival)
library(survey)

xvars <- c("commute_drv", "commute_carpassenger", "commute_pt", "commute_bikewalk", 
           "leisure_drv", "leisure_carpassenger", "leisure_pt", "leisure_bikewalk", "leisure_emerging", 
           "cdaypw", "ComDist", "TeleFreq", "withlicense", "carpadlt", 
           "HHSize", "withParent", "withPartner", "withOwnChild", "withChild", "nchild", 
           "WorkStudyStat", "Education", "HHincome", 
           "Zpro_suburban_18F", "Zlong_term_suburbanite_18F", "Zmust_own_car_18F", "Zcar_as_atool_18F",   
           "Zmaterialism_18F", "Ztech_embracing_18F", "Zinternet_smarthphone_lover_18F", "Zpro_env_policies_18F", 
           "Ztime_mode_constrained_18F", "Zpro_exercise_18F", "ZA3p_likebike", "Zadv_var_seeker_18F", 
           "Zstablished_in_life_18F", "Ztraditional_shopper_18F", "Zpro_social_18F", "ZA3f_uncomfortpeople",
           "E1car_rating", "E2pt_rating", "E3at_rating", 
           "Activity_intensity", "Landuse_diversity", "TQ2",  
           "Age", "Millennials", "wPTpass", "VMDpw", "pctCarAvail", "NHtype")


wt.table1 <- svydesign(ids = ~1, data = stat06, weights = ~wprob)
wt.table2 <- svyCreateTableOne(vars= xvars, strata = "class", data=wt.table1)
