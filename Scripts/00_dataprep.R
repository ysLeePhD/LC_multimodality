#install.packages("tidyverse")

library(tidyverse)
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
#colnames(data00)
data01 <- data00[, c(1, 114:120, 123:138, 599:643, 799, 801, 420, 426, 468, 181:334, 674:675)]
#https://www.r-bloggers.com/subsetting-data/
#colnames(data01)
data01$PID <- data01[, 1] 
data01 <- data01[, c(231, 2:230)]




#A. Sociodemographc/economic characteristics  

table(data01$K11_gender)
data01$Gender <- factor(data01$K11_gender, labels=c("male", "female", "trans", "decline"), ordered=TRUE)

data01$Race <- NA 
data01$Race <- ifelse(data01$K6b_asian==1 & data01$K6e_white+data01$K6c_black+data01$K6a_nativeamerican==0, 1, data01$Race)
data01$Race <- ifelse(data01$K6e_white==1 & data01$K6b_asian+data01$K6c_black+data01$K6a_nativeamerican==0, 2, data01$Race)
data01$Race <- ifelse(data01$K6c_black==1 & data01$K6b_asian+data01$K6e_white+data01$K6a_nativeamerican==0, 3, data01$Race)
data01$Race <- ifelse(data01$K6a_nativeamerican==1 & data01$K6b_asian+data01$K6e_white+data01$K6c_black==0, 4, data01$Race)
data01$Race <- ifelse(is.na(data01$Race)==TRUE, 5, data01$Race)
table(data01$Race)
data01$Race <- factor(data01$Race, labels=c("Asian", "White", "Black", "Native American", "Others"), ordered=TRUE)
table(data01$Race)

data01$Hispanic <- data01$K6d_hispanic

data01$AgeGroup <- NA 
data01$AgeGroup <- ifelse(data01$Age<=34, 1, data01$AgeGroup)
data01$AgeGroup <- ifelse(data01$Age>=35, 2, data01$AgeGroup)
table(data01$AgeGroup)
data01$AgeGroup <- factor(data01$AgeGroup, labels=c("Millennials", "GenXers"), ordered=TRUE)

table(data01$K19_education)
data01$Education <- factor(data01$K19_education, labels=c(
  "Prefer not to answer", "Some grade/high school", "High school/GED", "Some college", 
  "Associate's degree", "Bachelor's degree", "Graduate degree", "Professional degree"), ordered=TRUE)
table(data01$Education)


## Study/work status 
data01$WorkHours <- ifelse(is.na(data01$D7_WorkHours)==TRUE, 0, data01$D7_WorkHours)
table(data01$WorkHours)
data01$StudyWorkStatus <- 0 

data01$Student <- factor(data01$D1_Student, labels=c("Full-time", "Part-time", "Not a student"), ordered=TRUE)
table(data01$Student)
data01$Employment <- factor(data01$D4_Employment, labels=c("Full-time", "Part-time", "Two or more jobs", 
                                                              "Only unpaid work", "Homemaker/unpaid caregiver",
                                                              "Not a worker"), ordered=TRUE)
table(data01$Employment)

## Study/work status 4. full-time students - reference category
# https://www.irs.gov/affordable-care-act/employers/identifying-full-time-employees
# For purposes of the employer shared responsibility provisions, a full-time employee is, for a calendar month, 
# an employee employed on average at least 30 hours of service per week, or 130 hours of service per month.  
data01$StudyWorkStatus <- ifelse(data01$D1_Student==1, 3, data01$StudyWorkStatus) 
data01$StudyWorkStatus <- ifelse(data01$D1_Student==1 & data01$D4_Employment==1, 1, data01$StudyWorkStatus) 
data01$StudyWorkStatus <- ifelse(data01$D1_Student==1 & data01$D4_Employment==3 & 
                                 data01$WorkHours>=30, 1, data01$StudyWorkStatus)   
## Study/work status 1. full-time workers
data01$StudyWorkStatus <- ifelse(data01$D4_Employment==1, 1, data01$StudyWorkStatus) 
data01$StudyWorkStatus <- ifelse(data01$D4_Employment==3 & 
                                 data01$WorkHours>=30, 1, data01$StudyWorkStatus) 
## Study/work status 3. part-time workers
data01$StudyWorkStatus <- ifelse(data01$D4_Employment==2 & data01$D1_Student>1, 2, data01$StudyWorkStatus) 
data01$StudyWorkStatus <- ifelse(data01$D4_Employment==3 & data01$D1_Student>1 & 
                                 data01$WorkHours<30, 2, data01$StudyWorkStatus) 

## Study/work status 2. only part-time students / no work 
data01$StudyWorkStatus <- ifelse(data01$D1_Student==2 & data01$D4_Employment>3, 4, data01$StudyWorkStatus)

## Study/work status 5. no work & no study - drop from the sample  
data01$StudyWorkStatus <- ifelse(data01$D1_Student>2 & data01$D4_Employment>3, 5, data01$StudyWorkStatus)

data01$StudyWorkStatus <- factor(data01$StudyWorkStatus, labels=c("Full-time work", "Part-time work", 
                                                                  "Full-time study", "Part-time study", 
                                                                  "Neither work nor study"), ordered = TRUE)
table(data01$StudyWorkStatus)
by_status <- group_by(data01, StudyWorkStatus)
summarize(by_status, cases=n(), AvgWorkHours=mean(WorkHours, na.rm=TRUE))


data01$HHSize <- data01$K13_HHSize
data01$withParent <- ifelse(data01$C6_ParentsDV>0, 1, 0) 
data01$withPartner <- ifelse(data01$C6_PartnerDV>0, 1, 0) 
data01$withOwnChild <- ifelse(data01$C6_ChildrenDV>0, 1, 0)
data01$nchild <- data01$K14a_kidsunder6 + data01$K14b_kids6to12 + data01$K14c_kids13to17
data01$withChild <- ifelse(data01$nchild>0, 1, 0)

table(data01$K17_hhincome)
data01$hhincome <- factor(data01$K17_hhincome, labels=c("Prefer not to answer", "less than $20,000", "$20,001 to $40,000", 
                                                        "$40,001 to $60,000", "$60,001 to $80,000", "$80,001 to $100,000", 
                                                        "$100,001 to $120,000", "$120,001 to $140,000", "$140,001 to $160,000", 
                                                        "More than $160,000"), ordered=TRUE) 

data01 <- data01[, c(1, 231:233, 240:243, 245, 244, 237, 235, 246, 70, 71)]
colnames(data01)

# categorical variables -> a set of binary variables for Mplus 
colnames(data01)
data01s <- data01

table(data01s$StudyWorkStatus)
data01s$FTwork <- ifelse(data01s$StudyWorkStatus=="Full-time work", 1, 0) # Full-time work 
data01s$PTwork <- ifelse(data01s$StudyWorkStatus=="Part-time work", 1, 0) # Part-time work 
data01s$FTstudy <- ifelse(data01s$StudyWorkStatus=="Full-time study", 1, 0) # Full-time study
data01s$PTstudy <- ifelse(data01s$StudyWorkStatus=="Part-time study", 1, 0) # Part-time study

table(data01s$Education)
data01s$somecoll <- ifelse(data01s$Education=="Some college", 1, 0) 
data01s$somecoll <- ifelse(data01s$Education=="Associate's degree", 1, data01s$somecoll) 
data01s$bachelor <- ifelse(data01s$Education=="Bachelor's degree", 1, 0) 
data01s$graduate <- ifelse(data01s$Education=="Graduate degree", 1, 0) 
data01s$graduate <- ifelse(data01s$Education=="Professional degree", 1, data01s$graduate) 
data01s$ednoanswer <- ifelse(data01s$Education=="Prefer not to answer", 1, 0) 

table(data01s$hhincome) #"prefer not to answer" omitted 
#low:     20k~ 60k 
#middle:  60k~ 120k 
#high:   120k~

data01s$lowhhinc <- ifelse(data01s$hhincome=="less than $20,000", 1, 0) 
data01s$lowhhinc <- ifelse(data01s$hhincome=="$20,001 to $40,000", 1, data01s$lowhhinc) 
data01s$lowhhinc <- ifelse(data01s$hhincome=="$40,001 to $60,000", 1, data01s$lowhhinc) 
table(data01s$lowhhinc)

data01s$midhhinc <- ifelse(data01s$hhincome=="$60,001 to $80,000", 1, 0) 
data01s$midhhinc <- ifelse(data01s$hhincome=="$80,001 to $100,000", 1, data01s$midhhinc) 
data01s$midhhinc <- ifelse(data01s$hhincome=="$100,001 to $120,000", 1, data01s$midhhinc) 
table(data01s$midhhinc)

data01s$highhhinc <- ifelse(data01s$hhincome=="$120,001 to $140,000", 1, 0) 
data01s$highhhinc <- ifelse(data01s$hhincome=="$140,001 to $160,000", 1, data01s$highhhinc) 
data01s$highhhinc <- ifelse(data01s$hhincome=="More than $160,000", 1, data01s$highhhinc) 
table(data01s$highhhinc)

data01s <- data01s[, c(1, 5:10, 16:26)]
colnames(data01s)




#B. Factors & standalone statements 
data02 <- data00[, c(1, 842:891)]
data02$PID <- data02[, 1] 
data02 <- data02[, c(52, 2:51)]
colnames(data02)


 

#C. Travel outcomes (Dependent variables)
data03 <- data00[, c(1, 137:139, 153, 167, 181:346, 420:479, 599:642)]
data03$license <- factor(data03$H1_car, labels=c("No license", "With a driver's license"), ordered=TRUE)
data03$ncar <- data03$H4_NumCar
data03$carpdr <- ifelse(data03$K15_numdrivers>0, data03$H4_NumCar/data03$K15_numdrivers, 0)
data03$nadlt <- data03$K14d_ppl18to26 + data03$K14e_ppl27to34 + data03$K14f_ppl35to50 + 
                data03$K14g_ppl51to65 + data03$K14h_pplover65
data03$nadlt <- ifelse(data03$nadlt==0, 1, data03$nadlt)
data03$carpadlt <- data03$H4_NumCar/data03$nadlt
data03$VMDpw <- ifelse(data03$H11car_VMT>=0, data03$H11car_VMT, 0) 
data03$lnVMDpw <- log(data03$VMDpw+1) 

data03$wPTpass <- ifelse(data03$H14_PTpass>1, 1, 0)
data03$pctCarAvail <- data03$H5_car_avail
data03$pctCarAvail <- ifelse(is.na(data03$H5_car_avail)==TRUE & data03$H4_NumCar==0, 0, data03$pctCarAvail) 

data03$F2_dayswork  <- ifelse(is.na(data03$F2_dayswork) ==TRUE, 0, data03$F2_dayswork )
data03$F2_dayswork  <- ifelse(data03$F2_dayswork<0,             NA, data03$F2_dayswork) # one case missing for commute days for work 
data03$F2_dayschool <- ifelse(is.na(data03$F2_dayschool)==TRUE, 0, data03$F2_dayschool)
data03$cdaypw <- ifelse(data03$F2_dayswork>=data03$F2_dayschool, data03$F2_dayswork, data03$F2_dayschool)

data03$CommuteDist <- NA
data03$CommuteDist <- ifelse(data03$cdaypw==0, 0, data03$CommuteDist) # non-commuters 
data03$CommuteDist <- ifelse(data03$cdaypw>0 & data03$F2_dayswork>=data03$F2_dayschool, data03$F3work_comDist,   data03$CommuteDist)
data03$CommuteDist <- ifelse(data03$cdaypw>0 & data03$F2_dayswork< data03$F2_dayschool, data03$F3school_comdist, data03$CommuteDist)
summary(data03$CommuteDist) # 105 cases missing (they commute regularly, but they did not report the distance)

table(data03$D10_Telecommute) # 1-No, 2-Not sure, 3-Yes 
table(data03$D11_TelecommuteFreq) # 

data03$TeleFreq <- 0 # no telecommuter (including no commuter)
data03$TeleFreq <- ifelse(is.na(data03$D11_TelecommuteFreq)==FALSE & data03$D11_TelecommuteFreq==2, 1, data03$TeleFreq) # less than once a week: less than once a month 
data03$TeleFreq <- ifelse(is.na(data03$D11_TelecommuteFreq)==FALSE & data03$D11_TelecommuteFreq==3, 1, data03$TeleFreq) # less than once a week: 1-3 times a week 
data03$TeleFreq <- ifelse(is.na(data03$D11_TelecommuteFreq)==FALSE & data03$D11_TelecommuteFreq>3, 2, data03$TeleFreq)  # at least once a week: 1-2, 3-4, & 5 or more times a week 
table(data03$TeleFreq)

#if D11_TelecommuteFreq=. then telefreq1=1; 
#if D11_TelecommuteFreq=1 then telefreq1=1; 

#if D11_TelecommuteFreq=2 then telefreq2=1; 
#if D11_TelecommuteFreq=3 then telefreq2=1; 

#if D11_TelecommuteFreq=4 then telefreq3=1; 
#if D11_TelecommuteFreq=5 then telefreq3=1; 
#if D11_TelecommuteFreq=6 then telefreq3=1; 

modefreq <- function(x){
  a <- NA 
  for (i in 1:length(x)){
    if (is.na(x[i])==TRUE) {
      a[i] <- 0
    } else if (x[i]<3) {
      a[i] <- 0 
    } else if (x[i]==3) {
      a[i] <- 0.5 
    } else if (x[i]==4) {
      a[i] <- 2
    } else if (x[i]==5) {
      a[i] <- 6
    } else if (x[i]==6) {
      a[i] <- 14
    } else if (x[i]==7) {
      a[i] <- 20
    }
  }
  return(a) 
}

data03$commute_drv <- round(modefreq(data03$F6school_Drivealone) + modefreq(data03$F6school_Moto) + modefreq(data03$F6school_CarpoolD) +  
  modefreq(data03$F6work_Drivealone) + modefreq(data03$F6work_Moto) + modefreq(data03$F6work_CarpoolD), digits=0)
data03$commute_carpassenger <- round(modefreq(data03$F6school_CarpoolP) +  modefreq(data03$F6school_Uber) + modefreq(data03$F6school_Taxi) + 
    modefreq(data03$F6work_CarpoolP) + modefreq(data03$F6work_Uber) + modefreq(data03$F6work_Taxi), digits=0) 
data03$commute_pt <- round(modefreq(data03$F6school_Shuttle) + modefreq(data03$F6school_Bus) + modefreq(data03$F6school_LR) + modefreq(data03$F6school_Train) + 
  modefreq(data03$F6work_Shuttle) + modefreq(data03$F6work_Bus) + modefreq(data03$F6work_LR) + modefreq(data03$F6work_Train), digits=0)
data03$commute_bikewalk <- round(modefreq(data03$F6school_Bike) + modefreq(data03$F6school_Skateboard) + modefreq(data03$F6school_Walk) + 
  modefreq(data03$F6work_Bike) + modefreq(data03$F6work_Skateboard) + modefreq(data03$F6work_Walk), digits=0)
data03$commute_total <- round(
  modefreq(data03$F6school_Drivealone) + modefreq(data03$F6school_Moto) + modefreq(data03$F6school_CarpoolD)+  
  modefreq(data03$F6work_Drivealone) + modefreq(data03$F6work_Moto) + modefreq(data03$F6work_CarpoolD)+
  modefreq(data03$F6school_CarpoolP) +  modefreq(data03$F6school_Uber) + modefreq(data03$F6school_Taxi)+ 
  modefreq(data03$F6work_CarpoolP) + modefreq(data03$F6work_Uber) + modefreq(data03$F6work_Taxi)+
  modefreq(data03$F6school_Shuttle) + modefreq(data03$F6school_Bus) + modefreq(data03$F6school_LR) + modefreq(data03$F6school_Train)+ 
  modefreq(data03$F6work_Shuttle) + modefreq(data03$F6work_Bus) + modefreq(data03$F6work_LR) + modefreq(data03$F6work_Train)+
  modefreq(data03$F6school_Bike) + modefreq(data03$F6school_Skateboard) + modefreq(data03$F6school_Walk)+ 
  modefreq(data03$F6work_Bike) + modefreq(data03$F6work_Skateboard) + modefreq(data03$F6work_Walk), digits=0)
  
data03$leisure_drv <- round(modefreq(data03$F14leisure_Drivealone) + modefreq(data03$F14leisure_Moto) + modefreq(data03$F14leisure_CarpoolD), digits=0) 
data03$leisure_carpassenger <- round(modefreq(data03$F14leisure_CarpoolP) + modefreq(data03$F14leisure_Taxi), digits=0)
data03$leisure_pt <- round(modefreq(data03$F14leisure_Bus) + modefreq(data03$F14leisure_LR) + modefreq(data03$F14leisure_Train), digits=0)
data03$leisure_bikewalk <- round(modefreq(data03$F14leisure_Bike) + modefreq(data03$F14leisure_Skateboard) + modefreq(data03$F14leisure_Walk), digits=0) 
data03$leisure_emerging <- round(modefreq(data03$F14leisure_Uber) + modefreq(data03$F14leisure_Carsharing), digits=0)
data03$leisure_total <- round(
  modefreq(data03$F14leisure_Drivealone) + modefreq(data03$F14leisure_Moto) + modefreq(data03$F14leisure_CarpoolD)+
  modefreq(data03$F14leisure_CarpoolP) + modefreq(data03$F14leisure_Taxi)+
  modefreq(data03$F14leisure_Bus) + modefreq(data03$F14leisure_LR) + modefreq(data03$F14leisure_Train)+
  modefreq(data03$F14leisure_Bike) + modefreq(data03$F14leisure_Skateboard) + modefreq(data03$F14leisure_Walk)+
  modefreq(data03$F14leisure_Uber) + modefreq(data03$F14leisure_Carsharing), digits=0) 

data03$PID <- data03[, c(1)]
data03 <- data03[, c(300, 277:299, 4:6)]
colnames(data03)

# categorical variables -> a set of binary variables for Mplus 
data03s <- data03

data03s$withlicense <- ifelse(data03s$license=="With a driver's license", 1, 0)
data03s$lnCommuteDist <- ifelse(is.na(data03s$CommuteDist)==FALSE, log(data03s$CommuteDist+1), NA)   
data03s$TeleFreq0 <- ifelse(data03s$TeleFreq==0, 1, 0) # no telecommute
data03s$TeleFreq1 <- ifelse(data03s$TeleFreq==1, 1, 0) # less than once a week 
data03s$TeleFreq2 <- ifelse(data03s$TeleFreq==2, 1, 0) # at least once a week 

data03s <- data03s[, c(1, 14:17, 19:23, 29, 11, 30:32, 28, 6, 25:27)]
colnames(data03s)




#D. BE attributes: Use as it is. 

# most uptodate geocodes  
# geo00 <- read.csv(file="M:/Millennial_CA/03_task/01_geocoding_recheck/Home1975case_xy_region_12132017.csv")
# geo01 <- geo00[c(1:1975), c(1, 2, 6, 10, 17)]
# geo01$Accuracy <- as.integer(geo01[, 2])
# geo01 <- geo01[, c(1, 6, 3:5)]
# head(geo01)

# Neighborhood type by Deborah
#colnames(data00)
data04 <- data00[, c(1, 668, 670, 675)]
data04$PID <- data04[, c(1)]
data04$NHtype <- factor(data04$NHtype5_edited, labels=c("Central city", "Urban", "Suburban", "Rural-In-Urban", "Rural"), ordered=TRUE)
data04$Accuracy <- data04$Geocoding_type
data04 <- data04[, c(5, 6, 7, 4)]
colnames(data04)

# two LU factors: Activity intensity & Balance of various land uses 
geo10 <- read.csv("M:/Millennial_CA/21_RC_LCCM/03_data_process/01_SPSS/Factors_BE2.csv")
geo10$GEOID10 <- geo10[, 1]
geo10 <- geo10[, c(9, 2:3)]
colnames(geo10)

data04 <- left_join(data04, geo10, by="GEOID10")
#data04 <- select(data04, -GEOID10)

# Alltransit measure: Transit Connectivity Index (0-100)
tr00 <- read.csv("M:/Millennial_CA/03_task/04_Alltransit/TRscore_full.csv")
tr00$PID <- tr00[, 1]
tr00$TransitIndex <- as.numeric(as.character(tr00$TQ2))
tr01 <- tr00[, c("PID", "TransitIndex")]
colnames(tr01)

data04 <- left_join(data04, tr01, by="PID")
head(data04)

filter(data04, is.na(Activity_intensity)==TRUE) 
# While two factors are computed at the Census block group level, 
# some variables are not available at that level, or for 93 cases in the sample.
# the best way is to use the previous data file for the two factors or the three LU attributes. 


# use LU attributes + commute distance variable (lndistance) from the previous datafile 
# lndistance: probably used Google Maps API for missing cases, but couldn't find intermediate files on 12/03/2018
data05 <- read.csv("M:/Millennial_CA/15_MC_multimodality/13_analysis/run06/LCA_commuter1071.csv")
data05$PID <- data05[, 1]
colnames(data05)
data05 <- data05[, c(102, 75, 76, 77, 82:86, 91, 74)]
data05$TQ2 <- ifelse(data05$PID==9836757, 3, data05$TQ2) 
# missing: mannually scraped on alltransit.cnt.org 
# revised addresse info come from the two sources (addresses slightly different, but the same location)
# M:/Millennial_CA/03_task/04_Alltransit/TRscore_full.csv 
# M:/Millennial_CA/03_task/01_geocoding_recheck/Home_xy_blkgrp_region_02152018.xlsx 
head(data05)




#E. merge/join 

# check any duplicate with the same PID for data01~05 
data05 %>% 
  group_by(PID) %>% 
  summarize(count=n()) %>% 
  filter(count>1)

data11 <- inner_join(data01s, data03s, by="PID")
data11 <- inner_join(data11, data02, by="PID")
data11 <- inner_join(data11, data05, by="PID") 
# final sample size 1,069 cases - two cases are excluded b/c they are not in the 1975 sample (PID=9365921, 9369844)

data11 <- data11[, c(1, 19:27, 88, 29:34, 2:18, 38:87, 35:37, 89:97)]
colnames(data11)

write.csv(data11, "M:/Millennial_CA/15_MC_multimodality/33_reMplus/data11.csv")
