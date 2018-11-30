#install.packages("tidyverse")

library(tidyverse)
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
#colnames(data00)
data01 <- data00[, c(1, 114:120, 123:138, 599:643, 799, 420, 426, 468, 181:334, 674)]
#https://www.r-bloggers.com/subsetting-data/
#colnames(data01)
data01$PID <- data01[, 1] 
data01 <- data01[, c(229, 2:228)]




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




#B. Factors & standalone statements 
data02 <- data00[, c(1, 842:891)]
data02$PID <- data02[, 1] 
data02 <- data02[, c(52, 2:51)]
colnames(data02)




#C. BE attributes 
geo00 <- read.csv(file="M:/Millennial_CA/03_task/01_geocoding_recheck/Home1975case_xy_region_12132017.csv")
geo01 <- geo00[c(1:1975), c(1, 2, 6, 10, 17)]
geo01$Accuracy <- as.integer(geo01[, 2])
geo01 <- geo01[, c(1, 6, 3:5)]
head(geo01)


 

#D. Travel outcomes (Dependent variables)

data01$license <- factor(data01$H1_car, labels=c("No license", "With a driver's license"), ordered=TRUE)
data01$ncar <- data01$H4_NumCar
data01$carpdr <- ifelse(data01$K15_numdrivers>0, data01$H4_NumCar/data01$K15_numdrivers, 0)
data01$VMDpw <- ifelse(data01$H11car_VMT>=0, data01$H11car_VMT, 0) 
colnames(data01)

summary(data01$F1_commutedest)

summary(data01[is.na(data01$F7work_pmode)==TRUE, ]$F7school_pmode)
summary(data01[is.na(data01$F7work_pmode)==FALSE, ]$F7school_pmode)
summary(data01[is.na(data01$F7school_pmode)==TRUE, ]$F7work_pmode)
summary(data01[is.na(data01$F7school_pmode)==FALSE, ]$F7work_pmode)

data01$commute_pmode <- NA
data01$commute_pmode <- ifelse(is.na(data01$F7work_pmode)==FALSE, data01$F7work_pmode, data01$commute_pmode)
data01$commute_pmode <- ifelse(is.na(data01$F7school_pmode)==FALSE, data01$F7school_pmode, data01$commute_pmode)
data01$commute_pmode <- ifelse(data01$commute_pmode<0, NA, data01$commute_pmode)
table(data01$commute_pmode)

data01$lastcommute_drive   <- ifelse(data01$commute_pmode==1, 1, 0) 
data01$lastcommute_carpool <- 0 
data01$lastcommute_carpool <- ifelse(data01$commute_pmode==2, 1, 0) 
data01$lastcommute_carpool <- ifelse(data01$commute_pmode==3, 1, data01$lastcommute_carpool) 
data01$lastcommute_motor   <- ifelse(data01$commute_pmode==4, 1, 0) 
data01$lastcommute_shuttle <- ifelse(data01$commute_pmode==5, 1, 0) 
data01$lastcommute_transit <- 0 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==6, 1, 0) 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==7, 1, data01$lastcommute_transit) 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==8, 1, data01$lastcommute_transit) 
data01$lastcommute_ridehail<- ifelse(data01$commute_pmode==10, 1, 0) 
data01$lastcommute_bike    <- ifelse(data01$commute_pmode==11, 1, 0) 
data01$lastcommute_walk    <- 0 
data01$lastcommute_walk    <- ifelse(data01$commute_pmode==12, 1, 0) 
data01$lastcommute_walk    <- ifelse(data01$commute_pmode==13, 1, data01$lastcommute_walk) 
data01$lastcommute_other   <- 0 
data01$lastcommute_other   <- ifelse(data01$commute_pmode==9, 1, 0) # taxi
data01$lastcommute_other   <- ifelse(data01$commute_pmode==14, 1, data01$lastcommute_other) 

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

data01$commute_car <- modefreq(data01$F6school_Drivealone) + modefreq(data01$F6school_CarpoolD) + modefreq(data01$F6school_CarpoolP) + 
  modefreq(data01$F6school_Moto) + modefreq(data01$F6work_Drivealone) + modefreq(data01$F6work_CarpoolD) + 
  modefreq(data01$F6work_CarpoolP) + modefreq(data01$F6work_Moto) 
data01$commute_transit <- modefreq(data01$F6school_Bus) + modefreq(data01$F6school_LR) + modefreq(data01$F6school_Train) + 
  modefreq(data01$F6work_Bus) + modefreq(data01$F6work_LR) + modefreq(data01$F6work_Train)
data01$commute_active <- modefreq(data01$F6school_Bike) + modefreq(data01$F6school_Skateboard) + modefreq(data01$F6school_Walk) + 
  modefreq(data01$F6work_Bike) + modefreq(data01$F6work_Skateboard) + modefreq(data01$F6work_Walk)
data01$commute_ridehail <- modefreq(data01$F6school_Uber) + modefreq(data01$F6work_Uber)
data01$commute_other <- modefreq(data01$F6school_Shuttle) + modefreq(data01$F6school_Taxi) + modefreq(data01$F6school_Other) + 
  modefreq(data01$F6work_Shuttle) + modefreq(data01$F6work_Taxi) + modefreq(data01$F6work_Other)

data01$leisure_car <- modefreq(data01$F14leisure_Drivealone) + modefreq(data01$F14leisure_CarpoolD) + 
  modefreq(data01$F14leisure_CarpoolP) + modefreq(data01$F14leisure_Carsharing) + modefreq(data01$F14leisure_Moto) 
data01$leisure_transit <- modefreq(data01$F14leisure_Bus) + modefreq(data01$F14leisure_LR) + modefreq(data01$F14leisure_Train)
data01$leisure_active <- modefreq(data01$F14leisure_Bike) + modefreq(data01$F14leisure_Skateboard) + modefreq(data01$F14leisure_Walk)
data01$leisure_ridehail <- modefreq(data01$F14leisure_Uber)
data01$leisure_other <- modefreq(data01$F14leisure_Taxi) + modefreq(data01$F14leisure_Other) 

