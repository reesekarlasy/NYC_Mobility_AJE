library(dplyr)
library(tidycensus)
library(tidyverse)
library(lubridate)
library(tidyr)
library(plyr)
library(data.table)
library(ggplot2)
library(eeptools)
library(lubridate)
library(mapproj)
library(rgdal)
library(tidyverse)
library(ggmap)
library(DT)
library(knitr)
library(maptools)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(sp)
library(RColorBrewer)
library(Hmisc)
library(devtools)
library(fields)
library(lasagnar)   
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(stringr)
library(colorspace)  
library(reshape)
library(reshape2)
library(data.table)
library(MASS)
library(dplyr)
library(ggpubr)
library(segmented)


Packages <- c("plyr","tidyverse", "data.table", "extrafont", "EnvStats", "gam", "gamlss", "gridExtra", "ggsci", "incidence","EpiEstim", 
              "epitools", "MASS", "fitdistrplus","lattice","openintro", "R0", "RColorBrewer", "xlsx")

lapply(Packages, library, character.only = TRUE)


setwd("C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use")

load("Workspace/subway12_workspace.RData")

#nyc_change - final R code about rate of change from 2. SubwayCode
#nyc_matrix_final - from 1. SubwayCode_Borough
#nyc_matrix_final_more - from 2. SubwayCode

setwd("C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use")

previous <- read.csv("NYC_borough_tests_cases.csv") 
nyc <- read.csv("COVID19_NYC_cases_hospitalizations_deaths_by_borough.csv") #Code to pull in boroughs 
state <- read.csv("Apr_28_New_York_State_Statewide_COVID-19_Testing.csv")
state$County <- as.character(state$County)
state_nyc <- subset(state, County=="Bronx" | County=="Queens" | County=="New York" | County=="Kings" )

state_nyc$borough <- state_nyc$County
state_nyc$borough[state_nyc$County=="New York"] <- "Manhattan"
state_nyc$borough[state_nyc$County=="Kings"] <- "Brooklyn"
state_nyc$new.cases <- state_nyc$New.Positives
state_nyc$new.tests <- state_nyc$Total.Number.of.Tests.Performed
state_nyc$date <- as.character(state_nyc$Test.Date)
nyc$date <- as.character(nyc$date)

casetest <- left_join(state_nyc, nyc[,-4], by=c("date","borough"))

#######################################################3

casetest$date<- mdy(casetest$date)

##Pull acs 2018
###CALCULATE OUTCOMES

library(tidycensus)
library(tidyverse)
borough_data<-tidycensus::get_acs(geography = "county",variables = "B01003_001", geometry = FALSE, year = 2018)

#remove so can make long to wide
borough_data<-borough_data %>%
  dplyr::select(-c(moe))
#make wide

borough_demographics<-tidyr::spread(borough_data, key = variable, value = estimate)
colnames(borough_demographics)[3] <- "ZPOP"

borough_demographics$borough <- NA
borough_demographics$borough[borough_demographics$GEOID==36061] <- "Manhattan"
borough_demographics$borough[borough_demographics$GEOID==36005] <- "Bronx"
#borough_demographics$borough[borough_demographics$GEOID==36085] <- "Staten Island"
borough_demographics$borough[borough_demographics$GEOID==36047] <- "Brooklyn"
borough_demographics$borough[borough_demographics$GEOID==36081] <- "Queens"

casetest_b <- left_join(casetest,borough_demographics, by="borough")

casetest_b$positivegiventest <- casetest_b$new.cases/casetest_b$new.tests #P(Positive | Test)
casetest_b$prob_test <- casetest_b$new.tests/casetest_b$ZPOP #P(Test)
casetest_b$prob_pos <- casetest_b$new.cases/casetest_b$ZPOP #P(Positive)

#FOR ANOVA

casetest_b$prob_pos1 <- casetest_b$new.cases/casetest_b$ZPOP*100000 #P(Positive)
casetest_b$prob_pos_giventest1 <- casetest_b$new.cases/casetest_b$ZPOP*100000/casetest_b$new.tests #P(Positive)
casetest_b$positivegiventest1 <- casetest_b$new.cases/casetest_b$new.tests*100 #P(Positive | Test)
casetest_b$prob_test1 <- casetest_b$new.tests/casetest_b$ZPOP*100000 #P(Test)


group_by(casetest_b, borough) %>%
  summarise(
    count = n(),
    mean = mean(prob_pos_giventest1, na.rm = TRUE),
    sd = sd(prob_pos_giventest1, na.rm = TRUE)
  )

hist(casetest_b$prob_pos1)
hist(casetest_b$prob_pos_giventest1 )
hist(casetest_b$positivegiventest1)
hist(casetest_b$prob_test1)

casetest_b$borough <- as.factor(casetest_b$borough)
res.aov1 <- kruskal.test(prob_pos1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- kruskal.test(prob_pos_giventest1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- kruskal.test(positivegiventest1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- kruskal.test(casetest_b$prob_test1 ~ borough, data = casetest_b)
summary(res.aov1)

#create log cases

casetest_b$case_log_wrong <- log(casetest_b$new.cases)
casetest_b$case_log_wrong[casetest_b$case_log_wrong==-Inf] <- 0 # make them 0
casetest_b$case_log <- log(casetest_b$Cumulative.Number.of.Positives)
casetest_b$case_log[casetest_b$case_log==-Inf] <- 0 # make them 0

########TAKE OUT ALL STATEN ISLAND########

casetest_b <- casetest_b[casetest_b$borough!="Staten Island",]


#############BREAKPOINT REGRESSION###############

boroughs <- as.character(unique(casetest_b$borough))

casetest_b$date1 <- as.numeric(casetest_b$date - as.Date("2020-02-15"))
summary(casetest_b$date1)
length(casetest_b$date1)

####LET ME TRY #use quasi binomial GLM for proportion data
hist(exp(casetest_b$positivegiventest))
hist(casetest_b$positivegiventest)
########

casetest_b <- casetest_b[nrow(casetest_b):1,]

queens <- casetest_b[casetest_b$borough=="Queens",]
man <- casetest_b[casetest_b$borough=="Manhattan",]
bronx <- casetest_b[casetest_b$borough=="Bronx",]
brooklyn <- casetest_b[casetest_b$borough=="Brooklyn",]


#########MAIN PLOT#######
#######PROB POS
##positive tests/100k population

p <- ggplot() + geom_smooth(casetest_b, mapping=aes(x=date, y=prob_pos*100000, group=borough, color=borough), span=0.3, show.legend=FALSE, se=FALSE, size=1.2) + geom_smooth(casetest_b, mapping=aes(x=date, y=prob_pos*100000, group=borough, fill=borough), color="transparent", alpha=0.11, size=1, span=0.3, show.legend=FALSE, se=TRUE, size=1.2) + xlab("") + theme_minimal(base_size=12) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = -.24))  
p

p11 <- p + labs(color = "Borough", fill="Borough") + ylab("Rate of COVID-19 cases\nper 100k") + xlab("") + ggtitle("A)") + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + scale_fill_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + ggtitle("                  A)")
p11 

###plot 
#######POSITIVE OVER POPU
#SUPPLEMENT

p <- ggplot() + geom_smooth(casetest_b, mapping=aes(x=date, y=positivegiventest*100, group=borough, color=borough), span=0.3, se=FALSE, size=1.2, show.legend = FALSE) + geom_smooth(casetest_b, mapping=aes(x=date, y=positivegiventest*100, group=borough, fill=borough), color="transparent", alpha=0.11, span=0.3, se=TRUE, size=1, show.legend = FALSE) 
p1_1 <- p + labs(color = "Borough") + ylab("Proportion COVID-19 cases\namong tested") + theme_minimal(base_size=12) + ggtitle("B)") + xlab("") + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + scale_fill_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd"))  + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = -.24)) + ggtitle("                  B)")
p1_1 
#+ xlim(as.Date(c("2020-03-01", "2020-04-27")))

#######PROB POS
##positive tests/100k population

p <- ggplot() + geom_smooth(casetest_b, mapping=aes(x=date, y=prob_test*100000, group=borough, color=borough), span=0.3, show.legend=TRUE, se=FALSE, size=1.2) + geom_smooth(casetest_b, mapping=aes(x=date, y=prob_test*100000, group=borough, fill=borough), color="transparent", alpha=0.11, size=1, span=0.3, show.legend=FALSE, se=TRUE, size=1.2) + xlab("") + theme_minimal(base_size=12) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = -.24)) 
p

p1_2 <- p + labs(color = "Borough", fill="Borough") + ylab("Rate of COVID-19 tests\nper 100k") + xlab("") + ggtitle("A)") + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + scale_fill_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + ggtitle("                  C)")  + theme(legend.position="bottom")
p1_2 


suppfig2 <- ggarrange(p11, p1_1, p1_2, heights = c(2, 2),
           ncol = 1, nrow = 3, align = "v", common.legend = TRUE , legend = "bottom")  + theme_minimal(base_size=12)


pdf("Final Figures/Final/suppfig2.pdf", width=7, height=11)
plot(suppfig2)
graphics.off()

pdf("Final Figures/Final/AppendixFig2a.pdf", width=7, height=4.8)
plot(p11)
graphics.off()

pdf("Final Figures/AppendixFig2b.tiff", width=7, height=4.8)
plot(p1_1)
graphics.off()

pdf("Final Figures/AppendixFig2c.tiff", width=7, height=4.8)
plot(p1_2)
graphics.off()


###########################NYC SUBWAY BY BOROUGH############################
nyc_change_long2_zcta$Borough <- nyc_change_long2_zcta$borough
nyc_change_long2 <- nyc_change_long2_zcta[nyc_change_long2_zcta$Borough!="Staten Island",]
length(unique(nyc_change_long2_zcta$ZCTA)) #UNIQUE IS 214 YAY

nyc_change_long2$date1 <- (nyc_change_long2$dates - as.Date("2020-01-04"))/7
#nyc_change_long2$date2 <- (nyc_change_long2$dates - as.Date("2020-02-15"))

#create subsets of boroughs
queens_subway <- nyc_change_long2[nyc_change_long2$Borough=="Queens",]
man_subway <- nyc_change_long2[nyc_change_long2$Borough=="Manhattan",]
bronx_subway <- nyc_change_long2[nyc_change_long2$Borough=="Bronx",]
bk_subway <- nyc_change_long2[nyc_change_long2$Borough=="Brooklyn",]

lm_q <-lm(MTA~date1, data=queens_subway)
lm_m <-lm(MTA~date1, data=man_subway)
lm_bx <-lm(MTA~date1, data=bronx_subway)
lm_bk <-lm(MTA~date1, data=bk_subway)

#week from 2/15
q<-segmented(lm_q,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
m<-segmented(lm_m,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
bx<-segmented(lm_bx,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
bk<-segmented(lm_bk,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))

#summary(q)
a <- ymd("2020-01-04") + 7*7.574
pscore.test(q)$p.value
#summary(m)
b <-ymd("2020-01-04") + 7*7.034
pscore.test(m)$p.value
#summary(bx)
c <- ymd("2020-01-04") + 7*7.762
pscore.test(bx)$p.value
#summary(bk)
d <- ymd("2020-01-04") + 7*7.382
pscore.test(bk)$p.value

#o<-segmented(out.lm,seg.Z=~date1)
plot.segmented(q)
plot.segmented(m)
plot.segmented(bx)
plot.segmented(bk)

#######get slope of subway

#get values from segmented regression
library(rpsychi)
slopes1 <- rbind(slope(bx)$date1[2,], slope(bk)$date1[2,], slope(m)$date1[2,], slope(q)$date1[2,])
slopes1 <- as.data.frame(slopes1)
slopes1$SE_slope <- as.numeric(slopes1$St.Err.)
slopes1$lCI <- slopes1[,4]
slopes1$uCI <- slopes1[,5]
slopes1$mean1 <- as.numeric(as.character(slopes1$'Est.'))
slopes1 <- subset(slopes1, select=-c(St.Err., Est.))

slopes2 <- cbind(slopes1, rbind(bx$psi, bk$psi, m$psi, q$psi))
Borough <- c("Bronx","Brooklyn","Manhattan","Queens")
slope_b <- cbind(slopes2, Borough)
slope_b$SD <- as.numeric(slope_b$SE_slope)*sqrt(57)

slope_b$date <- ymd("2020-01-04") + slope_b$'Est.'*7
slope_b$date_lCI <- slope_b$date - 1.96*slope_b$St.Err*7
slope_b$date_uCI <- slope_b$date + 1.96*slope_b$St.Err*7

slope_b$Borough = with(slope_b, reorder(Borough, date, median))

p2 <- ggplot(data=slope_b, aes(x = Borough,y = date, ymin = date_lCI, ymax = date_uCI)) + geom_pointrange(aes(col=Borough)) + ylab("Date of mobility decrease (95% CI)")+ geom_errorbar(aes(ymin=date_lCI, ymax=date_uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip() + xlab("") + theme_minimal()

p1 <- ggplot(data=slope_b, aes(x = Borough,y = mean1, ymin = lCI, ymax = uCI)) + geom_pointrange(aes(col=Borough)) + ylab("Change in mobility (95% CI)")+
  geom_errorbar(aes(ymin=lCI, ymax=uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip()+ xlab("")  + theme_minimal()

library(ggpubr)

ggarrange(p2, p1, heights = c(2, 2),
          ncol = 1, nrow = 2, align = "v", common.legend = TRUE) 

##anova showed nothing

a <- with(slope_b, ind.oneway.second(mean1, SD, 57))

####################### SEGMENTED FOR EXPONENTIAL #####################

ggplot(casetest_b, aes(date, case_log, group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + theme_minimal()

a <- lm(case_log ~ date1, data=casetest_b)
all_borough <-segmented(a, seg.Z=~date1, npsi= 1, control=seg.control(display=TRUE))

date <- ymd("2020-02-15") + all_borough$psi[2] #3/24 is the breakpoint

#all boroughs
lm_q_exp <-lm(case_log~date1, data=queens)
lm_m_exp <-lm(case_log~date1, data=man)
lm_bx_exp <-lm(case_log~date1, data=bronx)
lm_bk_exp <-lm(case_log~date1, data=brooklyn)

#week from 2/15
q_exp<-segmented(lm_q_exp,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
m_exp<-segmented(lm_m_exp,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
bx_exp<-segmented(lm_bx_exp,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))
bk_exp<-segmented(lm_bk_exp,seg.Z=~date1, npsi=1,control=seg.control(display=TRUE))

psi_all <- as.data.frame(rbind(bx_exp$psi, bk_exp$psi, m_exp$psi, q_exp$psi))
exp_est <- psi_all$Est.
exp_SE <- psi_all$St.Err

slope_b<- cbind(slope_b, exp_est, exp_SE)
slope_b$dateexp <- ymd("2020-02-15") + slope_b$exp_est
slope_b$dateexp_lCI <- slope_b$dateexp - 1.96*slope_b$St.Err
slope_b$dateexp_uCI <- slope_b$dateexp + 1.96*slope_b$St.Err
View(slope_b)

#NOTE WHAT IS THE SE OF SEGMENTED

bx_exp

################LM FIT##########
##SUBWAY

fitted_q <- broken.line(q)$fit
fitted_bk <- broken.line(bk)$fit
fitted_bx <- broken.line(bx)$fit
fitted_m <- broken.line(m)$fit

########

#use lm

a1 <- (lm_q$fitted.values-(coef(lm_q)[1]))
date_q <- ymd("2020-01-04") + 7*round(a1/coef(lm_q)[2])
a1 <- (lm_m$fitted.values-(coef(lm_m)[1]))
date_m <- ymd("2020-01-04") + 7*round(a1/coef(lm_m)[2])
a1 <- (lm_bk$fitted.values-(coef(lm_bk)[1]))
date_bk <- ymd("2020-01-04") + 7*round(a1/coef(lm_bk)[2])
a1 <- (lm_bx$fitted.values-(coef(lm_bx)[1]))
date_bx <- ymd("2020-01-04") + 7*round(a1/coef(lm_bx)[2])

q_data <- cbind.data.frame(date_q, fitted_q)
bk_data <- cbind.data.frame(date_bk, fitted_bk)
bx_data <- cbind.data.frame(date_bx, fitted_bx)
m_data <- cbind.data.frame(date_m, fitted_m)


################LM FIT##########

fitted_q_exp <- broken.line(q_exp)$fit
fitted_bk_exp <- broken.line(bk_exp)$fit
fitted_bx_exp <- broken.line(bx_exp)$fit
fitted_m_exp <- broken.line(m_exp)$fit

q_data_exp <- cbind.data.frame(queens$date, fitted_q_exp)
bk_data_exp <- cbind.data.frame(queens$date, fitted_bk_exp)
bx_data_exp <- cbind.data.frame(queens$date, fitted_bx_exp)
m_data_exp <- cbind.data.frame(queens$date, fitted_m_exp)

colnames(q_data_exp)[1:2] <- c("Dates",
                           "Pred1")
colnames(bk_data_exp)[1:2] <- c("Dates",
                             "Pred1")
colnames(bx_data_exp)[1:2] <- c("Dates",
                              "Pred1")
colnames(m_data_exp)[1:2] <- c("Dates",
                          "Pred1")
q_data_exp$Borough <- "Queens"
bk_data_exp$Borough <- "Brooklyn"
bx_data_exp$Borough <- "Bronx"
m_data_exp$Borough <- "Manhattan"

seg_exp <- rbind(q_data_exp, bk_data_exp, bx_data_exp, m_data_exp)

######################FIGURES#########################

##CORRECT CODE
dummyData <- nyc_change_long2[1, ] #in an above line, this is the zcta dataset that doesn't have staten
dummyData$MTA <- NaN

#let's add this line on top
Queens <- unique(q_data)
Queens$Borough <- "Queens"
Brooklyn <- unique(bk_data)
Brooklyn$Borough <- "Brooklyn"
Manhattan <- unique(m_data)
Manhattan$Borough <- "Manhattan"
Bronx <- unique(bx_data)
Bronx$Borough <- "Bronx"

colnames(Queens)[1:2] <- c("Dates",
                       "Pred")
colnames(Brooklyn)[1:2] <- c("Dates",
                           "Pred")
colnames(Manhattan)[1:2] <- c("Dates",
                           "Pred")
colnames(Bronx)[1:2] <- c("Dates",
                           "Pred")

seg <- rbind(Queens, Brooklyn, Manhattan, Bronx)
seg <- seg[seg$Dates >= as.Date("2020-02-15"),]

casetest_b$pp <- casetest_b$prob_pos/casetest_b$new.tests*100000
casetest_b$pp1 <- (casetest_b$pp*700)-30

###LOOKING AT GOOD PLOT
l1 <- ggplot(casetest_b, aes(date, log(new.cases), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of incident cases") + xlab("Date")

l2 <- ggplot(casetest_b, aes(date, log(new.cases/new.tests), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of incident cases\nover tests") + xlab("Date")

l3 <- ggplot(casetest_b, aes(date, log(new.cases/new.tests/ZPOP*100000), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of incident cases\nover total tests per 100k") + xlab("Date")

l4 <- ggplot(casetest_b, aes(date, log(Cumulative.Number.of.Positives/Total.Number.of.Tests.Performed), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of cumulative cases") + xlab("Date")

l5 <- ggplot(casetest_b, aes(date, log(Cumulative.Number.of.Positives/Total.Number.of.Tests.Performed), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of cumulative cases\nover total tests") + xlab("Date")

l6 <- ggplot(casetest_b, aes(date, log(Cumulative.Number.of.Positives/Total.Number.of.Tests.Performed/ZPOP*1000), group=borough, color=borough)) + geom_smooth(se=FALSE) + labs(color=Borough) + ylab("Log of cumulative cases\nover total tests per 100k") + xlab("Date")

ggarrange(l1, l2, l3, l4, l5, l6, heights = c(2, 2),
          ncol = 3, nrow = 2, align = "v", common.legend = TRUE) 

##################UPDATED FIGURE 4############
#create labels and lines

cut_subway  <- data.frame(Ref = c("Queens", "Manhattan", "Bronx", "Brooklyn"), vals = c(as.numeric(as.Date(slope_b$date[4])), as.numeric(slope_b$date[3]), as.numeric(slope_b$date[1]), as.numeric(slope_b$date[2])),
                          stringsAsFactors = FALSE)
cuts2 <- data.frame(Ref = c("Queens", "Manhattan", "Bronx", "Brooklyn"),
                    vals = c(as.numeric(as.Date(slope_b$dateexp[4])), as.numeric(slope_b$dateexp[3]), as.numeric(slope_b$dateexp[1]), as.numeric(slope_b$dateexp[2])), stringsAsFactors = FALSE)

breakpoint_label  <- data.frame(Ref = c("    ", "    "), vals = c(as.numeric(as.Date(slope_b$date[4])), as.numeric(as.Date(slope_b$dateexp[4]))), vals2 = c(-20,-2), stringsAsFactors = FALSE)

casetest_b$Borough <- as.factor(casetest_b$borough)
seg_exp$Borough <- as.factor(seg_exp$Borough)
seg$Borough <- as.factor(seg$Borough)
###PLOT

casetest_b$case_log1 <- (casetest_b$case_log*2.9)-31
seg_exp$Pred <- (seg_exp$Pred1*2.9)-31

fig <- ggplot() + geom_line(seg, mapping=aes(x=Dates, y=Pred, group=Borough, color=Borough), size=0.9, show.legend = FALSE)  + scale_y_continuous(name = "Mobility", limits=c(-30,2.2), breaks = c(-30,-20,-10,0), sec.axis = sec_axis(~(.+31)/2.9, name = "log(Cumulative No. of Cases)", breaks = c(0,3,6,9,12))) + theme_classic(base_size=14) + geom_line(seg_exp, mapping=aes(x=Dates, y=Pred, group=Borough, color=Borough), show.legend=TRUE, size=0.9) + geom_smooth(casetest_b, mapping=aes(x=date, y=case_log1, group=Borough, fill=Borough), color="transparent", alpha =.3, size=2, se=TRUE, span=0.45, show.legend = FALSE) + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + scale_fill_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + scale_x_date(date_labels = "%m/%d/%y", limits = c(as.Date("2020-02-22"), as.Date("2020-04-11")), breaks=c(as.Date("2020-02-22"), as.Date("2020-02-29"), as.Date("2020-03-07"), as.Date("2020-03-14"),as.Date("2020-03-21"),as.Date("2020-03-28"),as.Date("2020-04-04"),as.Date("2020-04-11")))


fig4 <- fig + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cut_subway, size=0.70, linetype=2,alpha=0.5, show.legend = FALSE) + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cuts2, size=0.70, linetype=2, alpha=0.5, show.legend = FALSE) + geom_text(aes(x=as.Date(vals,origin = "1969-12-24"), label=Ref, y=vals2), data=breakpoint_label, alpha=0.75, size=4) + xlab("Date") + theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust=1), legend.text=element_text(size=12), legend.position = c(.85, 0.74)) 

fig4 <- fig + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cut_subway, size=0.70, linetype=2,alpha=0.5, show.legend = FALSE) + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cuts2, size=0.70, linetype=2, alpha=0.5, show.legend = FALSE) + geom_text(aes(x=as.Date(vals,origin = "1969-12-24"), label=Ref, y=vals2), data=breakpoint_label, alpha=0.75, size=4) + xlab("Date") + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text=element_text(size=15), axis.text=element_text(size=15, colour="black"), legend.position = c(0.85, 0.58), legend.text=element_text(size=15))
 

tiff("Final Figures/fig4.tiff", width=10, height=5.4, units="in", res=600)
plot(fig4)
graphics.off()

pdf("Final Figures/Final/AJE-00123-2015 Sy Figure 2.pdf", width=9, height=5.5)
plot(fig4)
graphics.off()

#####

dates_diff <- slope_b$dateexp - slope_b$date 
mean(dates_diff)
sd(dates_diff)

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

mean(dates_diff)
mean(dates_diff) + 1.96*stderr(dates_diff)
mean(dates_diff) - 1.96*stderr(dates_diff)

#28.62 (27.94-29.30)

#UPDATED FIGURE 3 #WRONG
casetest_b$case_log1 <- (casetest_b$case_log*5.2)-38

fig3 <- ggplot() + geom_line(seg, mapping=aes(x=Dates, y=Pred, group=Borough, color=Borough), size=1)  + coord_cartesian(ylim=c(-32,1), xlim=c(as.Date("2020-02-15"), as.Date("2020-05-01"))) + scale_y_continuous(name = "Mobility\n", sec.axis = sec_axis(~(.+38)/5.2, name = "Log of incident COVID-19 cases\n")) + geom_smooth(casetest_b, mapping=aes(x=date, y=case_log_wrong, group=borough, color=borough), span=0.5, show.legend=TRUE, se=FALSE, size=1) + theme_minimal()

cut_subway  <- data.frame(Ref = c("Queens", "Manhattan", "Bronx", "Brooklyn"), vals = c(as.numeric(as.Date(slope_b$date[4])), as.numeric(slope_b$date[3]), as.numeric(slope_b$date[1]), as.numeric(slope_b$date[2])),
                   stringsAsFactors = FALSE)

cuts2 <- data.frame(Ref = c("Queens", "Manhattan", "Bronx", "Brooklyn"),
                    vals = c(as.numeric(as.Date(slope_b$dateexp[4])), as.numeric(slope_b$dateexp[3]), as.numeric(slope_b$dateexp[1]), as.numeric(slope_b$dateexp[2])), 
                    stringsAsFactors = FALSE)

fig3 + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cut_subway, size=0.90, linetype=2,alpha=0.7, show.legend = FALSE) + geom_vline(mapping = aes(xintercept = vals, colour = Ref), data = cuts2, size=0.90, linetype=3, alpha=0.7, show.legend = FALSE) +  theme(legend.position="bottom") + xlab("Date") + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

#####

dates_diff <- slope_b$dateexp - slope_b$date 
mean(dates_diff)
sd(dates_diff)

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

mean(dates_diff)
mean(dates_diff) + 1.96*stderr(dates_diff)
mean(dates_diff) - 1.96*stderr(dates_diff)

#27.41 (28.11-26.71)



##############EXPONENTIAL GROWTH PERIOD
#https://www.medrxiv.org/content/10.1101/2020.02.05.20020750v3.full.pdf

incid_q <- queens[,c("date", "new.cases")]
incid_m <- man[,c("date", "new.cases")]
incid_bx <- bronx[,c("date", "new.cases")]
incid_bk <- brooklyn[,c("date", "new.cases")]
#incidence(incid_q)
#colnames(incid_borough) <- c("date", "queens", "manhattan", "bronx", "brooklyn")

queens1 <- queens[queens$date <= as.Date("2020-03-25"),]
man1 <- man[man$date <= as.Date("2020-03-22"),]
bronx1 <- bronx[bronx$date <= as.Date("2020-03-26"),]
brooklyn1 <- brooklyn[brooklyn$date <= as.Date("2020-03-24"),]
queens1$case_log[queens1$case_log==-Inf] <- 0 # make them 0
man1$case_log[man1$case_log==-Inf] <- 0 # make them 0
bronx1$case_log[bronx1$case_log==-Inf] <- 0 # make them 0
brooklyn1$case_log[brooklyn1$case_log==-Inf] <- 0 # make them 0

lm_q_exp1 <-lm(case_log~date1, data=queens1)
lm_m_exp1 <-lm(case_log~date1, data=man1)
lm_bx_exp1 <-lm(case_log~date1, data=bronx1)
lm_bk_exp1 <-lm(case_log~date1, data=brooklyn1)

#lm_q_exp1 <-glm(Cumulative.Number.of.Positives~date1, family="poisson", data=queens1)
#lm_m_exp1 <-glm(Cumulative.Number.of.Positives~date1, family="poisson", data=man1)
#lm_bx_exp1 <-glm(Cumulative.Number.of.Positives~date1,family="poisson", data=bronx1)
#lm_bk_exp1 <-glm(Cumulative.Number.of.Positives~date1,family="poisson", data=brooklyn1)

summary(lm_bx_exp1)
summary(lm_q_exp1)

slope_b$edt[4] <- log(2)/(lm_q_exp1$coefficients[2])
slope_b$edt[3] <- log(2)/(lm_m_exp1$coefficients[2])
slope_b$edt[1] <- log(2)/(lm_bx_exp1$coefficients[2])
slope_b$edt[2] <- log(2)/(lm_bk_exp1$coefficients[2])

slope_b$edt_uCI[4] <- log(2)/(confint(lm_q_exp1)[2,1])
slope_b$edt_uCI[3] <- log(2)/(confint(lm_m_exp1)[2,1])
slope_b$edt_uCI[1] <- log(2)/(confint(lm_bx_exp1)[2,1])
slope_b$edt_uCI[2] <- log(2)/(confint(lm_bk_exp1)[2,1])

slope_b$edt_lCI[4] <- log(2)/(confint(lm_q_exp1)[2,2])
slope_b$edt_lCI[3] <- log(2)/(confint(lm_m_exp1)[2,2])
slope_b$edt_lCI[1] <- log(2)/(confint(lm_bx_exp1)[2,2])
slope_b$edt_lCI[2] <- log(2)/(confint(lm_bk_exp1)[2,2])

p4 <- ggplot(data=slope_b, aes(x = Borough,y = edt, ymin = edt_lCI, ymax = edt_uCI)) + geom_pointrange(aes(col=Borough)) + ylab("Epidemic doubling time (95% CI)")+
  geom_errorbar(aes(ymin=edt_lCI, ymax=edt_uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip()+ xlab("")  + theme_minimal() + ggtitle("C)")

######R

mGT<-generation.time("gamma", c(4.7, 2.9),nrow(incid_bk))
#https://www.ijidonline.com/article/S1201-9712(20)30119-3/pdf
#https://link.springer.com/article/10.1007%2Fs11538-013-9918-2
#The methods used can be as simple as least-squares fitting of an exponential curve to incidence (or cumulative incidence), or of a straight line to the logarithm of incidence (or of cumulative incidence) 

a <- est.R0.EG(epid=incid_q$new.cases, t=incid_q$date, GT=mGT, begin="2020-03-02", end="2020-03-23")
b <- c(a$R,a$conf.int)
a <-est.R0.EG(epid=incid_m$new.cases, t=incid_m$date, GT=mGT, begin="2020-03-02", end="2020-03-23") 
b <- rbind(b, c(a$R,a$conf.int))
a <- est.R0.EG(epid=incid_bx$new.cases, t=incid_bx$date, GT=mGT, begin="2020-03-02", end="2020-03-23")
b <- rbind(b, c(a$R,a$conf.int))
a <- est.R0.EG(epid=incid_bk$new.cases, t=incid_bk$date, GT=mGT, begin="2020-03-02", end="2020-03-23")
b <- rbind(b, c(a$R,a$conf.int))

slope_b <- cbind(slope_b, b) 
colnames(slope_b)[(ncol(slope_b)-2):ncol(slope_b)] <- c("R0", "R0_lCI", "R0_uCI")

##bind

slope_b <- left_join(slope_b, nyc_change_b, by=c("Borough"="borough"))


##################PLOTS#########
slope_b$Borough = with(slope_b, reorder(Borough, mean, median))
slope_b$Borough <- fct_rev(slope_b$Borough)

p1 <- ggplot(data=slope_b, aes(x = Borough,y = mean, ymin = lower, ymax = upper), ) + geom_pointrange(aes(col=Borough),show.legend = FALSE) + ylab("\nMobility compared to baseline (95% CI)")+ geom_errorbar(aes(ymin=lower, ymax=upper,col=Borough),width=0.5,cex=1, show.legend = FALSE) + labs(color = "Borough") + coord_flip() + xlab("") + theme_minimal()+ ggtitle("B)")  + theme(plot.title = element_text(hjust = -.24)) + scale_color_manual(values = c("#1f77b4","#dc5fbd","#2ca02c", "#d62728")) + ggtitle("     B)")


p2 <- ggplot(data=slope_b, aes(x = Borough,y = date, ymin = date_lCI, ymax = date_uCI)) + geom_pointrange(aes(col=Borough), show.legend = FALSE) + ylab("\nDate of mobility decrease (95% CI)")+ geom_errorbar(aes(ymin=date_lCI, ymax=date_uCI,col=Borough),width=0.5,cex=1, show.legend = FALSE) + labs(color = "Borough") + coord_flip() + xlab("") + theme_minimal() + ggtitle("A)")  + theme(plot.title = element_text(hjust = -.24)) + scale_color_manual(values = c("#1f77b4","#dc5fbd","#2ca02c", "#d62728"))  + ggtitle("     A)")


p5 <- ggplot(data=slope_b, aes(x = Borough,y = R0, ymin = R0_lCI, ymax = R0_uCI)) + geom_pointrange(aes(col=Borough)) + ylab("\nR0 estimate (95% CI)")+ geom_errorbar(aes(ymin=R0_lCI, ymax=R0_uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip()+ xlab("")  + theme_minimal()   + theme(plot.title = element_text(hjust = -.24))  + scale_color_manual(values = c("#1f77b4","#dc5fbd","#2ca02c", "#d62728"))


p3 <- ggplot(data=slope_b, aes(x = Borough,y = dateexp, ymin = dateexp_lCI, ymax = dateexp_uCI)) + geom_pointrange(aes(col=Borough), show.legend = FALSE) + ylab("Date of end of exponential\ngrowth period (95% CI)")+ geom_errorbar(aes(ymin=dateexp_lCI, ymax=dateexp_uCI,col=Borough),width=0.5,cex=1, show.legend = FALSE) + labs(color = "Borough") + coord_flip() + xlab("") + theme_minimal() + ggtitle("C)")  + theme(plot.title = element_text(hjust = -.24))  + scale_color_manual(values = c("#1f77b4","#dc5fbd","#2ca02c", "#d62728")) + ggtitle("     C)")


p4 <- ggplot(data=slope_b, aes(x = Borough,y = edt, ymin = edt_lCI, ymax = edt_uCI)) + geom_pointrange(aes(col=Borough)) + ylab("\nEpidemic doubling time (95% CI)")+
  geom_errorbar(aes(ymin=edt_lCI, ymax=edt_uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip()+ xlab("")  + theme_minimal() + ggtitle("D)")  + theme(plot.title = element_text(hjust = -.24))  + scale_color_manual(values = c("#1f77b4","#dc5fbd","#2ca02c", "#d62728"))


library(ggpubr)

fig5 <- ggarrange(p2, p1, p3, heights = c(2, 2),
          ncol = 3, nrow = 1, align = "v", common.legend = TRUE, legend = "bottom") 

tiff("Final Figures/fig5.tiff", width=12, height=6, units="in", res=800)
plot(fig5)
graphics.off()

tiff("Final Figures/Fig5a.tiff", width=8, height=6, units="in", res=800)
plot(p2)
graphics.off()

tiff("Final Figures/Fig5b.tiff", width=8, height=6, units="in", res=800)
plot(p1)
graphics.off()

tiff("Final Figures/Fig5c.tiff", width=8, height=6, units="in", res=800)
plot(p3)
graphics.off()

tiff("Final Figures/Fig5d.tiff", width=8, height=6, units="in", res=800)
plot(p4)
graphics.off()

###Add more epidemiological parameters
#View(slope_b)


######PLOT

p2 <- ggplot(data=slope_b, aes(x = Borough,y = date, ymin = date_lCI, ymax = date_uCI)) + geom_pointrange(aes(col=Borough)) + ylab("Date of mobility decrease (95% CI)")+ geom_errorbar(aes(ymin=date_lCI, ymax=date_uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip() + xlab("") + theme_minimal()

p1 <- ggplot(data=slope_b, aes(x = Borough,y = mean1, ymin = lCI, ymax = uCI)) + geom_pointrange(aes(col=Borough)) + ylab("Change in mobility (95% CI)")+
  geom_errorbar(aes(ymin=lCI, ymax=uCI,col=Borough),width=0.5,cex=1) + labs(color = "Borough") + coord_flip()+ xlab("")  + theme_minimal() 

library(ggpubr)

ggarrange(p2, p1, heights = c(2, 2),
          ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom") 



###NUMBERS FOR THE PAPER

slope_b$borough <- slope_b$Borough
View(slope_b)

####R estimation

T <- length(queens$date)
t_start <- seq(7, T-6-10) # starting at timepoint 7 (pre-ECQ period, 02/11/2020) as Re is conditional on the past observations 
t_end <- t_start+6

mGT<-R0::generation.time("gamma", c(4.7, 2.9),nrow(queens))
mGT_v <- as.vector(mGT$GT)

library(EpiEstim)
Rt_est_queens <- estimate_R(queens$new.cases, 
                          method="non_parametric_si",
                          config = make_config(list(
                            t_start = t_start,
                            t_end = t_end,
                            si_distr= mGT_v)))

Rt_est_man <- estimate_R(man$new.cases, 
                            method="non_parametric_si",
                            config = make_config(list(
                              t_start = t_start,
                              t_end = t_end,
                              si_distr= mGT_v)))

Rt_est_bk <- estimate_R(brooklyn$new.cases, 
                         method="non_parametric_si",
                         config = make_config(list(
                           t_start = t_start,
                           t_end = t_end,
                           si_distr= mGT_v)))

Rt_est_bx <- estimate_R(bronx$new.cases, 
                         method="non_parametric_si",
                         config = make_config(list(
                           t_start = t_start,
                           t_end = t_end,
                           si_distr= mGT_v)))


Rt_plot_queens <- plot(Rt_est_queens, "R")
Rt_plot_man <- plot(Rt_est_man, "R")
Rt_plot_queens <- plot(Rt_est_queens, "R")
Rt_plot_man <- plot(Rt_est_man, "R")

###plot
R_param_q <- cbind.data.frame(queens$date[t_start], Rt_est_queens$R[,3], Rt_est_queens$R[,4],"Queens")
colnames(R_param_q) <- c("time","R","sd","Borough")

R_param_bx <- cbind.data.frame(bronx$date[t_start], Rt_est_bx$R[,3], Rt_est_bx$R[,4],"Bronx")
colnames(R_param_bx) <- c("time","R","sd","Borough")

R_param_man <- cbind.data.frame(man$date[t_start], Rt_est_man$R[,3], Rt_est_man$R[,4], "Manhattan")
colnames(R_param_man) <- c("time","R","sd","Borough")

R_param_bk <- cbind.data.frame(brooklyn$date[t_start], Rt_est_bk$R[,3], Rt_est_bk$R[,4], "Brooklyn")
colnames(R_param_bk) <- c("time","R","sd", "Borough")

R_param <- rbind(R_param_bx, R_param_bk, R_param_man, R_param_q)

suppfig3 <- ggplot() + geom_smooth(R_param, mapping=aes(x=time, y=R, group=Borough, color=Borough), se=FALSE, size=1.2) + geom_smooth(R_param, mapping=aes(x=time, y=R, group=Borough, fill=Borough),color="transparent", alpha =.08, size=1, se=TRUE, show.legend = FALSE) + ylab("Effective reproductive number\n") + theme_minimal() + theme_minimal(base_size=11) + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + theme(legend.position = "bottom")  + scale_fill_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd"))

tiff("Final Figures/suppfig3.tiff", width=8, height=6, units="in", res=500)
plot(suppfig3)
graphics.off()

##Get cum on 4/26 (end date of the study)

casetest_lastday <- subset(casetest_b, Test.Date=="04/26/2020")
View(cbind(as.character(casetest_lastday$borough),casetest_lastday$Cumulative.Number.of.Positives/casetest_lastday$ZPOP*100))

View(cbind(as.character(casetest_lastday$borough),casetest_lastday$Cumulative.Number.of.Positives/casetest_lastday$ZPOP*100000))


##ADDED 8/28

casetest_b_final <- dplyr::select(casetest_b, Test.Date, County, New.Positives, Cumulative.Number.of.Positives, prob_pos, positivegiventest, prob_test)
colnames(casetest_b_final) <- c("date","borough","incidence","cumincidence","rate_positive","percent_positive_over_tests","rate_tests")
write.csv(casetest_b_final,"Dryad/COVID_borough.csv")

