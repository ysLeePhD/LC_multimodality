

install.packages("Hmisc", dependencies = TRUE) # Help generate weighted summary 
install.packages("spatstat", dependencies = TRUE) # weighted.median

library(tidyverse) 

library(lattice)
library(survival)
library(Formula)
library(Hmisc)

library(spatstat.data)
library(nlme)
library(rpart)
library(spatstat)

# Read the result of the latent class mplus model    

stat00 <- read.csv("M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_results.csv")

coef_varlist <- c("t1", "t2", "t4", "t5", "t6", "t7", 
                  "hh4", "p3", "p4", "p5", "p6", "p7", 
                  "at4", "at8", "at9", "at10", "at17", "at18", "at19", "be1", 
                  "prob1", "prob2", "prob3", "prob4", "class", "wt", "id")
stat30 <- stat00[, coef_varlist] 

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
data01 <- data00[, c(1, 114:120, 123:138, 599:643, 799, 801, 420, 426, 468, 181:334, 674:675)]
data01a <- data01[, c(1, 70)]
colnames(data01a) <- c("id", "age")

stat31 <- merge(stat30, data01a, by="id")

stat31$prob1m <- 0 
stat31$prob2m <- 0 
stat31$prob3m <- 0 
stat31$prob4m <- 0 

coef_c <- read.csv("M:/Millennial_CA/15_MC_multimodality/33_reMplus/run32_result_MNL_coeff.csv")
coef_c <- coef_c[,-1]

latent.class.share <- function(x, n=1069){
    data <- x 
  for (i in 1:n){
    b1x <- as.numeric(data[i, 2:21]) %*% as.numeric(coef_c[1, 1:20]) + coef_c[1, 21]
    b2x <- as.numeric(data[i, 2:21]) %*% as.numeric(coef_c[2, 1:20]) + coef_c[2, 21]
    b3x <- as.numeric(data[i, 2:21]) %*% as.numeric(coef_c[3, 1:20]) + coef_c[3, 21]
    sumbx <- exp(b1x) + exp(b2x) + exp(b3x) + 1 
    
    data$prob1m[i] <- exp(b1x)/sumbx 
    data$prob2m[i] <- exp(b2x)/sumbx 
    data$prob3m[i] <- exp(b3x)/sumbx 
    data$prob4m[i] <- 1/sumbx 
  }
    summary <- round(c(wtd.mean(data$prob1m, w = data$wt), 
                       wtd.mean(data$prob2m, w = data$wt),   
                       wtd.mean(data$prob3m, w = data$wt),   
                       wtd.mean(data$prob4m, w = data$wt)), 3)
    print(summary)
}

latent.class.share(stat31) # Active traveler, Carpooler, Transit rider, & Driver 


# only millennials with their original values 

stat32 <- stat31[stat31$age<35, ] # only millennials 
stat32b <- stat31[stat31$age>=35, ] # only Gen Xers  

scenario1 <- latent.class.share(stat32, n=591) # Active traveler, Carpooler, Transit rider, & Driver 
scenario1b <- latent.class.share(stat32b, n=478) 


# A. Test hypothetical scenarios - improving access to cars 

wtd.mean(stat32$t6, w = stat32$wt) # has a drivers' license 
wtd.mean(stat32$t7, w = stat32$wt) # cars per adult in the household 

wtd.mean(stat32b$t6, w = stat32b$wt) # has a drivers' license 
wtd.mean(stat32b$t7, w = stat32b$wt) # cars per adult in the household 

stat33 <- stat32 

table(stat33$t6)
stat33$t6 <- stat33$t6 + wtd.mean(stat32b$t6, w = stat32b$wt) - wtd.mean(stat32$t6, w = stat32$wt)
table(stat33$t6)

wtd.mean(stat33$t7, w = stat33$wt)
stat33$t7 <- stat33$t7 + wtd.mean(stat32b$t7, w = stat32b$wt) - wtd.mean(stat32$t7, w = stat32$wt)
wtd.mean(stat33$t7, w = stat33$wt)

scenario2 <- latent.class.share(stat33, n=591) # Active traveler, Carpooler, Transit rider, & Driver 


# B. Test hypothetical scenarios - finish studying & living with a child

wtd.mean(stat32$hh4, w = stat32$wt) # living with own children 
wtd.mean(stat32$p3, w = stat32$wt) # full-time students 
wtd.mean(stat32$p4, w = stat32$wt) # part-time students 

wtd.mean(stat32b$hh4, w = stat32b$wt) # living with own children 
wtd.mean(stat32b$p3, w = stat32b$wt) # full-time students 
wtd.mean(stat32b$p4, w = stat32b$wt) # part-time students 

stat34 <- stat32 

table(stat34$hh4)
stat34$hh4 <- stat34$hh4 + wtd.mean(stat32b$hh4, w = stat32b$wt) - wtd.mean(stat32$hh4, w = stat32$wt)
table(stat34$hh4)

wtd.mean(stat34$p3, w = stat34$wt)
wtd.mean(stat34$p4, w = stat34$wt)
stat34$p3 <- stat34$p3 + wtd.mean(stat32b$p3, w = stat32b$wt) - wtd.mean(stat32$p3, w = stat32$wt)
stat34$p4 <- stat34$p4 + wtd.mean(stat32b$p4, w = stat32b$wt) - wtd.mean(stat32$p4, w = stat32$wt)
wtd.mean(stat34$p3, w = stat34$wt)
wtd.mean(stat34$p4, w = stat34$wt)

scenario3 <- latent.class.share(stat34, n=591) # Active traveler, Carpooler, Transit rider, & Driver 


# C. Test hypothetical scenarios - attitudes towards cars, environ policies and time/mode constraints

at.factor <- -1 # cancel out the difference between millennials & Gen Xers  
#at.factor <- 1 # strengthen the difference between millennials & Gen Xers  

at4diff <- wtd.mean(stat32$at4, w = stat32$wt) - wtd.mean(stat32b$at4, w = stat32b$wt)
at8diff <- wtd.mean(stat32$at8, w = stat32$wt) - wtd.mean(stat32b$at8, w = stat32b$wt)
at9diff <- wtd.mean(stat32$at9, w = stat32$wt) - wtd.mean(stat32b$at9, w = stat32b$wt)

#weighted.median(stat32$at4, w = stat32$wt) - weighted.median(stat32b$at4, w = stat32b$wt)
#weighted.median(stat32$at8, w = stat32$wt) - weighted.median(stat32b$at8, w = stat32b$wt)
#weighted.median(stat32$at9, w = stat32$wt) - weighted.median(stat32b$at9, w = stat32b$wt)

stat35 <- stat32 

stat35$at4 <- stat35$at4 + at.factor*at4diff # at4 - car as a tool 
stat35$at8 <- stat35$at8 + at.factor*at8diff # at8 - pro-environ
stat35$at9 <- stat35$at9 + at.factor*at9diff # at9 - time/mode constrained 

scenario4 <- latent.class.share(stat35, n=591) # Active traveler, Carpooler, Transit rider, & Driver 


# D. Test hypothetical scenarios - mode-specific perceptions 

at17diff <- wtd.mean(stat32$at17, w = stat32$wt) - wtd.mean(stat32b$at17, w = stat32b$wt)
at18diff <- wtd.mean(stat32$at18, w = stat32$wt) - wtd.mean(stat32b$at18, w = stat32b$wt)
at19diff <- wtd.mean(stat32$at19, w = stat32$wt) - wtd.mean(stat32b$at19, w = stat32b$wt)

stat36 <- stat32 

stat36$at17 <- stat36$at17 + at.factor*at17diff # at17 - overall cars 
stat36$at18 <- stat36$at18 + at.factor*at18diff # at18 - overall public transit 
stat36$at19 <- stat36$at19 + at.factor*at19diff # at19 - overall active modes 

scenario5 <- latent.class.share(stat36, n=591) # Active traveler, Carpooler, Transit rider, & Driver 


# E. Test hypothetical scenarios - relocation to suburbs

be.factor <- -1 

be1diff <- wtd.mean(stat32$be1, w = stat32$wt) - wtd.mean(stat32b$be1, w = stat32b$wt)

stat37 <- stat32

stat37$be1 <- stat37$be1 + be.factor*be1diff #be.factor*be1diff  

scenario6 <- latent.class.share(stat37, n=591) # Active traveler, Carpooler, Transit rider, & Driver 


# F. Test hypothetical scenarios - all changes  

stat38 <- stat32 

stat38$t6 <- stat33$t6
stat38$t7 <- stat33$t7

stat38$hh4 <- stat34$hh4
stat38$p3 <- stat34$p3
stat38$p4 <- stat34$p4

stat38$at4 <- stat35$at4 # at4 - car as a tool 
stat38$at8 <- stat35$at8 # at8 - pro-environ
stat38$at9 <- stat35$at9 # at9 - time/mode constrained 

stat38$at17 <- stat36$at17 # at17 - overall cars 
stat38$at18 <- stat36$at18 # at18 - overall public transit 
stat38$at19 <- stat36$at19 # at19 - overall active modes 

stat38$be1 <- stat37$be1 

scenario7 <- latent.class.share(stat38, n=591) # Active traveler, Carpooler, Transit rider, & Driver 

# Plot a bar chart 

all <- as.data.frame(rbind(scenario1, scenario2, scenario3, scenario4, 
                           scenario5, scenario6, scenario7, scenario1b))
rownames(all) <- NULL
colnames(all) <- c("Active traveler", "Carpooler", "Transit rider", "Driver")
all$scenario <- c("Millennials (now)", "Access to Cars", "Work & Child", "Attitudes", 
                  "Mode-Perception", "Relocation", "All changes", "Gen Xers (now)")
all 

all2 <- all %>% 
  gather(`Active traveler`, `Carpooler`, `Transit rider`, `Driver`, key = "class", value = "share") 

all2$class <- factor(all2$class,
                     levels = c("Active traveler", "Transit rider", "Carpooler", "Driver"), 
                     ordered = TRUE)

all2$scenario <- factor(all2$scenario, 
                        levels = c("Millennials (now)", "Access to Cars", "Work & Child", "Attitudes", 
                                   "Mode-Perception", "Relocation", "All changes", "Gen Xers (now)"), 
                        ordered = TRUE)
all2$pct1 <- ifelse(all2$class != "Driver", paste(as.character(round(all2$share*100, 1)), "%", " "), "")
all2$pct2 <- ifelse(all2$class == "Driver", paste(as.character(round(all2$share*100, 1)), "%", " "), "")

ggplot(data = all2, aes(x = scenario, y = share)) + 
  geom_bar(aes(fill = class), stat = "identity") + 
  coord_cartesian(ylim = c(0.8, 1)) + 
  guides(fill=guide_legend(title="")) + 
  geom_text(size = 3.5, hjust=0.35, vjust=2, aes(label = pct2, fontface=1)) + # 2(bold), 3(italic), 4(bold.italic)
  scale_y_continuous(labels=scales::percent) + #https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
  ylab("") + 
  xlab("") 

ggsave("C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/02_Transp_LCA_multimodal/lca_scenarios.jpg", dpi=300)