

library(tidyverse) 


#1. read the result of the latent class mplus model    

stat00 <- read.csv("M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_results.csv")



#2. create new categorical variables 
#   also, reverse the log-transformation of commute distance and transit quality measure

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



#3. create a long-formed table to compute weighted summary statistics 

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
stat06$commute_total <- stat06$commute_drv + stat06$commute_carpassenger + stat06$commute_pt + stat06$commute_bikewalk 
stat06$leisure_total <- stat06$leisure_drv + stat06$leisure_carpassenger + stat06$leisure_pt + stat06$leisure_bikewalk + 
  stat06$leisure_emerging

library(tableone)
library(grid) 
library(Matrix)
library(survival)
library(survey)

xvars <- c("commute_drv", "commute_carpassenger", "commute_pt", "commute_bikewalk", "commute_total",
           "leisure_drv", "leisure_carpassenger", "leisure_pt", "leisure_bikewalk", "leisure_emerging", "leisure_total",
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
# https://www.rdocumentation.org/packages/tableone/versions/0.9.3/topics/svyCreateTableOne
print(wt.table2, contDigits = 3, catDigits = 3)


#4. create a chart showing the shares of four classes by age 

colnames(stat02)
stat11 <- merge(stat02, data01[, c(1, 14, 15)], by="PID")
colnames(stat11)

# revise age 17 -> 18 and 51 -> 50 
stat11$Ager <- stat11$Age
stat11$Ager <- ifelse(stat11$Age==17, 18, stat11$Ager)
stat11$Ager <- ifelse(stat11$Age==51, 50, stat11$Ager)

pct_class_by_group <- data.frame(
  AgeNHgroup = "certain group", 
  MultimodalClass = rep(c("Monomodal driver","Carpooler", "Active traveler", "Transit rider"), 34), 
  # 18-22 to 46-50 (29 groups) + NHtype (5 groups) 
  WtProbSum = abs(rnorm(136, 0, 1)), 
  stringsAsFactors = FALSE)
head(pct_class_by_group)
sapply(pct_class_by_group, class)


for (i in 1:29) {
  temp00 <- stat11[stat11$Age>=i+17 & stat11$Age<i+22, ]
  a <- 4*(i-1)+1  
  b <- 4*i  
  pct_class_by_group[a:b, 1] <- paste0(as.character(i+17), "~", as.character(i+21)) 
  pct_class_by_group[a, 3]   <- sum(temp00[, 5])/sum(temp00[, 2:5]) 
  pct_class_by_group[a+1, 3] <- sum(temp00[, 3])/sum(temp00[, 2:5]) 
  pct_class_by_group[a+2, 3] <- sum(temp00[, 2])/sum(temp00[, 2:5])
  pct_class_by_group[a+3, 3] <- sum(temp00[, 4])/sum(temp00[, 2:5]) 
}

NHtypeList <- c("Central city", "Urban", "Suburban", "Rural in urban", "Rural")

for (i in 1:5) {
  temp00 <- stat11[stat11$NHtype == NHtypeList[i], ]
  a <- 4*(i+28)+1  
  b <- 4*(i+29)  
  pct_class_by_group[a:b, 1] <- NHtypeList[i]
  pct_class_by_group[a, 3]   <- sum(temp00[, 5])/sum(temp00[, 2:5]) 
  pct_class_by_group[a+1, 3] <- sum(temp00[, 3])/sum(temp00[, 2:5]) 
  pct_class_by_group[a+2, 3] <- sum(temp00[, 2])/sum(temp00[, 2:5])
  pct_class_by_group[a+3, 3] <- sum(temp00[, 4])/sum(temp00[, 2:5]) 
}

pct_class_by_group$label1 <- paste(round(pct_class_by_group$WtProbSum*100, digits=1), "%")
pct_class_by_group$label1 <- ifelse(pct_class_by_group$MultimodalClass=="Monomodal driver", "", pct_class_by_group$label1)
pct_class_by_group

pct_class_by_group$label2 <- paste(round(pct_class_by_group$WtProbSum*100, digits=1), "%")
pct_class_by_group$label2 <- ifelse(pct_class_by_group$MultimodalClass !="Monomodal driver", "", pct_class_by_group$label2)
pct_class_by_group

library(RColorBrewer)

# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2/
# https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
# https://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
# https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer
# https://ggplot2.tidyverse.org/reference/geom_text.html
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

ggplot(
  pct_class_by_group[1:116, ], 
  aes(
  fill=factor(MultimodalClass, levels=c("Transit rider",  "Active traveler", "Carpooler","Monomodal driver")), 
  y=WtProbSum, 
  x=factor(AgeNHgroup, 
           levels=c(
           "18~22", "19~23", "20~24", "21~25", "22~26", "23~27", "24~28", "25~29",  
           "26~30", "27~31", "28~32", "29~33", "30~34", "31~35", "32~36", "33~37", 
           "34~38", "35~39", "36~40", "37~41", "38~42", "39~43", "40~44", "41~45",  
           "42~46", "43~47", "44~48", "45~49", "46~50", 
           "Central city", "Urban", "Suburban", "Rural in urban", "Rural")))) +
  geom_bar(stat="identity", position="fill") + 
  coord_cartesian(ylim=c(0.7, 1)) + 
  guides(fill=guide_legend(title="")) + 
  ylab("") + 
  xlab("") + 
  theme(legend.position="bottom") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_colour_brewer(palette="RdBu", direction=-1) + # not solved yet. 
  geom_text(aes(label=label1), position=position_stack(vjust=0.5), size=3.5, angle=90)+ 
  geom_text(aes(label=label2), position=position_stack(vjust=0.95), size=3.5, angle=90) 
  # colour="white", , fontface = "bold") +

# https://ggplot2.tidyverse.org/reference/ggsave.html
ggsave(file="M:/Millennial_CA/15_MC_multimodality/31_LC_multimodality/Rplot.jpeg", 
       units=c("in"), dpi=300)

#theme_classic()+   # remove the backgroud grey
# add millennials, gen Xers, and by neighborhood type 
# x/y axis labels
# reorder the legend 
# check if okay in b/w 



