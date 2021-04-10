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

setwd("C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use")

load("Workspace/subway12_workspace.RData")

tests_zcta <- read.csv("tests-by-zcta.csv") #tests in zcta to ZCTA
tests_zcta <- tests_zcta[!is.na(tests_zcta$MODZCTA),] #take out the missing
zcta_tract <- read.csv("zcta_info.csv")
zcta_tract <- zcta_tract[,c("ZCTA5","COUNTY","TRACT", "ZAREALAND","ZPOP")] #MULTIPLE CENSUS TRACKS TO 1 ZCTA #WILL JUST PICK ONE TRACT TO USE TO MATCH TO ZCTA 
zcta_tract1 <- zcta_tract

######MERGE 

coefficients_m <- as.data.frame(coefficients_m)
coefficients_m$ZCTA <- as.integer(as.character(coefficients_m$ZCTA))
zcta_tests_subway <- left_join(zcta_tract1, coefficients_m, by=c("ZCTA5" = "ZCTA"))
zcta_tests_subway <- left_join(zcta_tests_subway, tests_zcta, by=c("ZCTA5" = "MODZCTA"))

zcta_tests_subway <- unique(zcta_tests_subway[,-3]) #took out the county and kept the unique ones
#zcta_tests_subway$Beta <- as.numeric(as.character(zcta_tests_subway$Beta))


#THOSE WITH GREATEST DIP IN SUBWAY USE HAD THE LOWEST %POSITIVE AMONG THOSE TESTED
######PULL ACS

v17 <- load_variables(2018, "acs5", cache = TRUE)
#View(v17)
lf_m <- paste("B23001_", formatC(seq(4,67,7), width=3, flag="0"), sep="") # Males. Check to make sure these are indeed the variables representing the labor force for men in different age categories
lf_f <- paste("B23001_", formatC(seq(90,153,7), width=3, flag="0"), sep="") # Females

#county_data<-tidycensus::get_acs(geography = "zcta", variables = c("B01001_002","B01001H_001","B01002_001","B01003_001", "B02001_002","B02001_003","B02001_004","B02001_005","B02001_006","B02001_007","B02001_008", "B03001_003",   "B06009_001","B06009_002","B06009_003","B06009_004","B06009_005","B06009_006", "B06011_001", "B19013_001", "C24060_001","C24060_003","C24060_005","C24060_006","B27010_001", "B27010_017", "B27010_033", "B27010_050", "B27010_066","B03002_003", "B25018_001", "B08126_001", "B08126_002", "B08126_003","B08126_004", "B08126_005", "B08126_006", "B08126_007", "B08126_011", "C24010_001", "C24010_052", "C24010_016" , "B01001_022" , "B01001_023", "B01001_024", "B01001_025",  "B01001_046", "B01001_047", "B01001_048", "B01001_049"), geometry = FALSE, year = 2018)

#74
#AFTER B03002_003 is added

#save(county_data,file="Workspace/county_data.RData")

load("Workspace/county_data.RData")

#remove so can make long to wide
county_data<-county_data %>%
  dplyr::select(-c(moe))

length(unique(county_data$variable))
#make wide

county_demographics<-tidyr::spread(county_data, key = variable, value = estimate)

#move around variables
county_demographics<- select(county_demographics, GEOID, NAME , B01001_002,B01001H_001,B01002_001,B01003_001, B02001_002,B02001_003,B02001_004,B02001_005,B02001_006,B02001_007,B02001_008, B03001_003, B06009_001,B06009_002,B06009_003,B06009_004,B06009_005,B06009_006, B06011_001, B19013_001, C24060_001,C24060_003,C24060_005,C24060_006,B27010_001, B27010_017, B27010_033, B27010_050, B27010_066, B03002_003, B25018_001, B08126_001, B08126_002, B08126_003, B08126_004, B08126_005, B08126_006, B08126_007, B08126_011, C24010_001, C24010_052, C24010_016, B01001_022 , B01001_023, B01001_024, B01001_025,  B01001_046, B01001_047, B01001_048, B01001_049)

#AFTER B03002_003 is added 

#OCCUPATION BY CLASS OF WORKER FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER - C24060_001
#current total
#OCCUPATION

#NOTES
#Estimate!!Total!!35 to 64 years!!No health insurance coverage
#The last term "No health insurance coverage" is what you care about
#TYPES OF HEALTH INSURANCE COVERAGE BY AGE - B27010_050
#Number of 35-64 with no health insurance

#B27010_001 - give total who answered health insurance question

#change names
names(county_demographics)<-c("ZCTA","location", "males","white_not_hispanic","median_age1","total_population","white","black","AI_AN","Asian","H_PI","Other_Race","Two_or_More_Race","Total_His_or_Lat","Total_ed","Less_than_High","High_School","Some_College","Bachelors","Graduate","Median_Income","Median_household_income", "Total_occupation_counted","Number_Service_Occ","Number_Nat_Rea","Total_Total_Transport","Total_Insured_Info","Under_19_NO_I","A19_34_NO_I", "A35_64_NO_I", "Over_65_NO_I","White_NH", "Med_Rm", "Total_Industry", "B08126_002", "B08126_003","B08126_004", "B08126_005", "B08126_006", "B08126_007", "B08126_011", "Total_Occupation", "Med_F", "Med_M" , "B01001_022" , "B01001_023", "B01001_024", "B01001_025",  "B01001_046", "B01001_047", "B01001_048", "B01001_049")

#create employment force var
#county_demographics$labor <- rowSums(county_demographics[,45:ncol(county_demographics)])

cbind(county_demographics$Total_Industry, county_demographics$Total_Occupation) #ok
#cor.test(county_demographics$Total_Industry, county_demographics$labor)
cbind(county_demographics$Total_Insured_Info, county_demographics$total_population) #ok

length(county_demographics$ZCTA)
length(unique(county_demographics$ZCTA))

county_demographics$ZCTA <- as.numeric(county_demographics$ZCTA)
write.csv(county_demographics, "county_demographics.csv")
zcta_tests_mta_acs1 <- left_join(zcta_tests_subway, county_demographics, by=c("ZCTA5" = "ZCTA"))


#pull in SDI
#SDI <- read.csv("ACS2015_zctaallvars.csv") #tests in zcta to ZCTA

#zcta_tests_mta_acs1 <- left_join(zcta_tests_mta_acs11, SDI[,c(sdi_score)], by=c("ZCTA5" = "zcta"))
 #ACS2015_zctaallvars


###########THERE ARE MANY POPULATIONS THAT ARE 0
zcta_tests_mta_acs <- subset(zcta_tests_mta_acs1, zcta_tests_mta_acs1$total_population!=0) #187 zctas that have populations
write.csv(zcta_tests_mta_acs1, "Output/all.csv")
nrow(zcta_tests_mta_acs1) #214
nrow(zcta_tests_mta_acs) #185 #29 don't have populations

#WE USE TOTAL POPULATION AS THE POPULATION, NOT ZPOP

##########################FINAL DATASET TO USE##################
###FIX

zcta_tests_mta_acs$positivegiventest <- zcta_tests_mta_acs$Positive/zcta_tests_mta_acs$Total #P(Positive | Test)
zcta_tests_mta_acs$prob_test <- zcta_tests_mta_acs$Total/zcta_tests_mta_acs$total_population #P(Test)
zcta_tests_mta_acs$prob_pos <- zcta_tests_mta_acs$Positive/zcta_tests_mta_acs$total_population #P(Positive)

zcta_tests_mta_acs$prob_pos1 <- zcta_tests_mta_acs$Positive/zcta_tests_mta_acs$total_population*100000
zcta_tests_mta_acs$prob_pos_giventest1 <- zcta_tests_mta_acs$Positive/zcta_tests_mta_acs$Total/zcta_tests_mta_acs$total_population*100000 #P(Positive | Test)
zcta_tests_mta_acs$positivegiventest1 <- zcta_tests_mta_acs$Positive/zcta_tests_mta_acs$Total*100 #P(Positive)
zcta_tests_mta_acs$prob_test1 <- zcta_tests_mta_acs$Total/zcta_tests_mta_acs$total_population*100000 #P(Test)

hist(zcta_tests_mta_acs$prob_pos1)
hist(zcta_tests_mta_acs$prob_pos_giventest1)
hist(zcta_tests_mta_acs$positivegiventest1)
hist(zcta_tests_mta_acs$prob_test1)

summary(zcta_tests_mta_acs$prob_pos1, na.rm=TRUE)
summary(zcta_tests_mta_acs$prob_pos_giventest1, na.rm=TRUE)
summary(zcta_tests_mta_acs$positivegiventest1, na.rm=TRUE)
summary(zcta_tests_mta_acs$prob_test1, na.rm=TRUE)


###Describing areas with largest cases
#use zcta_tests_mta_acs #WHAT I USED FOR PAPER SUBMITTED
nrow(zcta_tests_mta_acs) #185
max(zcta_tests_mta_acs$prob_pos1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$prob_pos1==max(zcta_tests_mta_acs$prob_pos1, na.rm=TRUE),"ZCTA5"]
max(zcta_tests_mta_acs$positivegiventest1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$positivegiventest1==max(zcta_tests_mta_acs$positivegiventest1, na.rm=TRUE),"ZCTA5"]
max(zcta_tests_mta_acs$prob_test1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$prob_test1==max(zcta_tests_mta_acs$prob_test1, na.rm=TRUE),"ZCTA5"]

min(zcta_tests_mta_acs$prob_pos1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$prob_pos1==min(zcta_tests_mta_acs$prob_pos1, na.rm=TRUE),"ZCTA5"]
min(zcta_tests_mta_acs$positivegiventest1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$positivegiventest1==min(zcta_tests_mta_acs$positivegiventest1, na.rm=TRUE),"ZCTA5"]
min(zcta_tests_mta_acs$prob_test1, na.rm=TRUE)
zcta_tests_mta_acs[zcta_tests_mta_acs$prob_test1==min(zcta_tests_mta_acs$prob_test1, na.rm=TRUE),"ZCTA5"]

###anova

zcta_tests_mta_acs$ZCTA5 <- as.factor(zcta_tests_mta_acs$ZCTA5)
res.aov1 <- aov(prob_pos1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- aov(prob_pos_giventest1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- aov(positivegiventest1 ~ borough, data = casetest_b)
summary(res.aov1)
res.aov1 <- aov(casetest_b$prob_test1 ~ borough, data = casetest_b)
summary(res.aov1)


zcta_tests_mta_acs$borough[zcta_tests_mta_acs$COUNTY==61] <- "Manhattan"
zcta_tests_mta_acs$borough[zcta_tests_mta_acs$COUNTY==5] <- "Bronx"
zcta_tests_mta_acs$borough[zcta_tests_mta_acs$COUNTY==85] <- "Staten Island"
zcta_tests_mta_acs$borough[zcta_tests_mta_acs$COUNTY==47] <- "Brooklyn"
zcta_tests_mta_acs$borough[zcta_tests_mta_acs$COUNTY==81] <- "Queens"

############################
#zcta_tests_mta_acs
#create variables

hist(zcta_tests_mta_acs$prob_test)
hist(zcta_tests_mta_acs$prob_test)
hist(zcta_tests_mta_acs$positivegiventest)
length(zcta_tests_mta_acs$prob_test) #216

##############CREATING ADDTL VARIABLES
#>75 years old #zcta_tests_mta_acs$B01001_022 # zcta_tests_mta_acs$B01001_046
zcta_tests_mta_acs$median_age <- (zcta_tests_mta_acs$B01001_023 + zcta_tests_mta_acs$B01001_024 + zcta_tests_mta_acs$B01001_025   + zcta_tests_mta_acs$B01001_047 + zcta_tests_mta_acs$B01001_048 + zcta_tests_mta_acs$B01001_049)/zcta_tests_mta_acs$total_population

#Median household income
zcta_tests_mta_acs$medianinc <- zcta_tests_mta_acs$Median_household_income

#Median income
zcta_tests_mta_acs$Median_Income1 <- zcta_tests_mta_acs$Median_Income
zcta_tests_mta_acs$Median_Income <- zcta_tests_mta_acs$Median_Income1

#Employment

zcta_tests_mta_acs$essential_both <- (zcta_tests_mta_acs$B08126_002 + zcta_tests_mta_acs$B08126_003 + zcta_tests_mta_acs$B08126_004 + zcta_tests_mta_acs$B08126_005 + zcta_tests_mta_acs$B08126_006 + zcta_tests_mta_acs$B08126_007 + zcta_tests_mta_acs$Med_F + zcta_tests_mta_acs$Med_M)/(zcta_tests_mta_acs$Total_Industry+ zcta_tests_mta_acs$Total_Occupation)
zcta_tests_mta_acs$essential_both <- zcta_tests_mta_acs$essential_both

write.csv(zcta_tests_mta_acs, "county_demographics.csv")

#Employment

zcta_tests_mta_acs$essential <- (zcta_tests_mta_acs$B08126_002 + zcta_tests_mta_acs$B08126_003 + zcta_tests_mta_acs$B08126_004 + zcta_tests_mta_acs$B08126_005 + zcta_tests_mta_acs$B08126_006 + zcta_tests_mta_acs$B08126_007)/zcta_tests_mta_acs$Total_Industry
zcta_tests_mta_acs$essential <- zcta_tests_mta_acs$essential
summary(zcta_tests_mta_acs$essential)

#Employment - Only Health

zcta_tests_mta_acs$health <- (zcta_tests_mta_acs$Med_F + zcta_tests_mta_acs$Med_M)/zcta_tests_mta_acs$Total_Occupation
zcta_tests_mta_acs$health <- zcta_tests_mta_acs$health

#Employment - Only Health

#zcta_tests_mta_acs$essential_health <- (zcta_tests_mta_acs$B08126_011)/zcta_tests_mta_acs$Total_Industry
#zcta_tests_mta_acs$essential_health <- zcta_tests_mta_acs$essential_health*10
#summary(zcta_tests_mta_acs$essential_health)

#Health Insurance
zcta_tests_mta_acs$healthins <- (zcta_tests_mta_acs$Under_19_NO_I + zcta_tests_mta_acs$A19_34_NO_I + zcta_tests_mta_acs$A35_64_NO_I +  zcta_tests_mta_acs$Over_65_NO_I)/zcta_tests_mta_acs$Total_Insured_Info
zcta_tests_mta_acs$healthins <- zcta_tests_mta_acs$healthins
summary(zcta_tests_mta_acs$healthins)

#Density
zcta_tests_mta_acs$density <- as.numeric((zcta_tests_mta_acs$total_population/(zcta_tests_mta_acs$ZAREALAND/1e6))/1000) # area in square meters is ZAREALAND LAND  #increase in 1000 people/km3 
summary(zcta_tests_mta_acs$density)

#Minority
zcta_tests_mta_acs$minority <- (zcta_tests_mta_acs$total_population-zcta_tests_mta_acs$White_NH)/zcta_tests_mta_acs$total_population
zcta_tests_mta_acs$minority <- zcta_tests_mta_acs$minority
hist(zcta_tests_mta_acs$minority)

#Education
zcta_tests_mta_acs$hs <- (zcta_tests_mta_acs$Less_than_High+zcta_tests_mta_acs$High_School)/(zcta_tests_mta_acs$total_population)
zcta_tests_mta_acs$hs <- zcta_tests_mta_acs$hs
hist(zcta_tests_mta_acs$hs)

detach("package:dplyr")
library(dplyr)

#fix 
colnames(zcta_tests_mta_acs)
zcta_tests_mta_acs$Intercept <- NA
zcta_tests_mta_acs$Beta <- NA
zcta_tests_mta_acs<- select(zcta_tests_mta_acs, ZCTA5, COUNTY, ZAREALAND, ZPOP, Intercept, Beta, April_11_Reduction, Positive, Total, zcta_cum.perc_pos, location, males, white_not_hispanic, total_population, white, black, AI_AN, Asian, H_PI, Other_Race, Two_or_More_Race, Total_His_or_Lat, Total_ed, Less_than_High, High_School, Some_College, Bachelors, Graduate, medianinc, Median_household_income, Total_occupation_counted, Number_Service_Occ, Number_Nat_Rea, Total_Total_Transport, Total_Insured_Info, Under_19_NO_I,A19_34_NO_I, A35_64_NO_I, Over_65_NO_I, White_NH, positivegiventest, prob_test, prob_pos, borough, Median_Income, median_age, essential_both, health, essential, minority, healthins, hs, density, Med_Rm)
#zcta_tests_mta_acs<- select(zcta_tests_mta_acs, ZCTA5, COUNTY, ZAREALAND, ZPOP, April_11_Reduction, Positive, Total, zcta_cum.perc_pos, location, males, white_not_hispanic, total_population, white, black, AI_AN, Asian, H_PI, Other_Race, Two_or_More_Race, Total_His_or_Lat, Total_ed, Less_than_High, High_School, Some_College, Bachelors, Graduate, medianinc, Median_household_income, Total_occupation_counted, Number_Service_Occ, Number_Nat_Rea, Total_Total_Transport, Total_Insured_Info, Under_19_NO_I,A19_34_NO_I, A35_64_NO_I, Over_65_NO_I, White_NH, positivegiventest, prob_test, prob_pos, borough, Median_Income, median_age, essential_both, health, essential, minority, healthins, hs, density, Med_Rm)

#TAKE OUT DUPLICATE ZCTA
zcta_tests_mta_acs$ZCTA5 <- as.numeric(as.character(zcta_tests_mta_acs$ZCTA5))
zcta_tests_mta_acs2 <- zcta_tests_mta_acs[!((zcta_tests_mta_acs$ZCTA5==10463) & (zcta_tests_mta_acs$COUNTY==61)),]  #11370 not in Manhattan
zcta_tests_mta_acs3 <- zcta_tests_mta_acs2[!((zcta_tests_mta_acs$ZCTA5==11370) & (zcta_tests_mta_acs$COUNTY==5)),]  #11370 not in Manhattan

#zcta_tests_mta_acs[!(zcta_tests_mta_acs$ZCTA5="11370" & zcta_tests_mta_acs$COUNTY="5"),] #11370 not in Bronx

zcta_tests_mta_acs_nosi <- zcta_tests_mta_acs3[zcta_tests_mta_acs$borough!="Staten Island",] #173 without SI
zcta_tests_mta_acs_nosi <-  zcta_tests_mta_acs_nosi[!is.na(zcta_tests_mta_acs_nosi$April_11_Reduction),] #124 without April 11 reduction

write.csv(zcta_tests_mta_acs_nosi, "Output/Final ZCTA.csv")
write.csv(zcta_tests_mta_acs1, "Output/Final ZCTA_all.csv")
nrow(zcta_tests_mta_acs1)
nrow(zcta_tests_mta_acs)
nrow(zcta_tests_mta_acs_nosi)

#CHECKING COVID CASES ON 124 ZCTAS
max(zcta_tests_mta_acs_nosi$prob_pos, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$prob_pos==max(zcta_tests_mta_acs_nosi$prob_pos, na.rm=TRUE),"ZCTA5"]
max(zcta_tests_mta_acs_nosi$positivegiventest, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$positivegiventest==max(zcta_tests_mta_acs_nosi$positivegiventest, na.rm=TRUE),"ZCTA5"]
max(zcta_tests_mta_acs_nosi$prob_test, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$prob_test==max(zcta_tests_mta_acs_nosi$prob_test, na.rm=TRUE),"ZCTA5"]

min(zcta_tests_mta_acs_nosi$prob_pos, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$prob_pos==min(zcta_tests_mta_acs_nosi$prob_pos, na.rm=TRUE),"ZCTA5"]
min(zcta_tests_mta_acs_nosi$positivegiventest, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$positivegiventest==min(zcta_tests_mta_acs_nosi$positivegiventest, na.rm=TRUE),"ZCTA5"]
min(zcta_tests_mta_acs_nosi$prob_test, na.rm=TRUE)
zcta_tests_mta_acs_nosi[zcta_tests_mta_acs_nosi$prob_test==min(zcta_tests_mta_acs_nosi$prob_test, na.rm=TRUE),"ZCTA5"]


## CORRELATION
library(ggcorrplot)

corr_matrix <- select(zcta_tests_mta_acs_nosi, hs,
healthins,
minority,
essential,
health,
essential_both,
median_age,
Median_Income)

colnames(corr_matrix) <- c("% HS or less",
                           "% uninsured",
                           "% non-white/Hispanic",
                           "% in non-healthcare",
                           "% healthcare",
                           "% essential services",
                           "% >75 years",
                           "Median income")

corr <- cor(corr_matrix, use= "complete.obs")
p.mat <- cor_pmat(corr_matrix)

corrplot <- ggcorrplot(corr, type = "lower",
           lab = TRUE,
           colors = c("#1f77b4", "white", "#d62728"),
           ggtheme = ggplot2::theme_minimal)

corrplot

tiff("Final Figures/corrplot.tiff", width=9, height=8, units="in", res=500)
plot(corrplot)
graphics.off()


#all very correlated to each other
#health insurance is the least correlated

###############################START############################

#SES
#########Median Income (not household) - Median_Income
#########% of essential workers #nothing - essential
#########% of insurance #nothing - healthins
#########% of high school - hs
#########% of minority - minority
#########% density

#Subway
#April_11_Reduction

#COVID19
#positivegiventest
#prob_test
#prob_pos

#summary(lm(regression))
#se_or_pv <- 2 #put 2 if you want se, put 4 if pvalue

#Change subway use to 10 units

zcta_tests_mta_acs_nosi$April_11_Reduction <- zcta_tests_mta_acs_nosi$April_11_Reduction
#######SES determinants of subway use################
#autoplot(lm(log(abs(zcta_tests_mta_acs_nosi$April_11_Reduction)) ~ zcta_tests_mta_acs_nosi$Median_Income))

ses_subway_unadj <- NULL
ses_subway_adj <- NULL

ses_subway <- matrix(nrow=10, ncol=8)
for(i in 45:54) {
  a <- (lm(zcta_tests_mta_acs_nosi$April_11_Reduction ~ zcta_tests_mta_acs_nosi[,i]))
  ses_subway[(i-44),1:4] <- c(a$coefficients[2], confint(a)[2,], summary(a)$coefficients[,4][2])
  ses_subway_unadj[[i-44]] <- a
  c <- (lm(zcta_tests_mta_acs_nosi$April_11_Reduction ~ zcta_tests_mta_acs_nosi[,i] + zcta_tests_mta_acs_nosi$essential_both))
  ses_subway[(i-44),5:8] <- c(c$coefficients[2], confint(c)[2,], summary(c)$coefficients[,4][2])
  ses_subway_adj[[i-44]] <- c
}
lapply(ses_subway_unadj, summary)
lapply(ses_subway_adj, summary)

#Parameter names - columns 
colnames(ses_subway) <- c("Unadjusted","LowCI_U","HighCI_U", "Pvalue", "Adjusted","LowCI_A","HighCI_A", "Pvalue")
rownames(ses_subway) <- colnames(zcta_tests_mta_acs_nosi)[45:54]
ses_subway <- as.data.frame(ses_subway)

ses_subway$final_u <- paste(round(ses_subway$Unadjusted,2)," (",round(ses_subway$LowCI_U,2),",",round(ses_subway$HighCI_U,2), ")", sep="")
ses_subway$final_a <- paste(round(ses_subway$Adjusted,2)," (",round(ses_subway$LowCI_A,2),",",round(ses_subway$HighCI_A,2), ")" , sep="")

write.csv(ses_subway, "C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use/Tables/ses_subway.csv")

#glm(zcta_tests_mta_acs_nosi$positivegiventest ~ zcta_tests_mta_acs_nosi$April_11_Reduction, family = binomial(link = "logit"), weights = zcta_tests_mta_acs_nosi$Total)


#######Subway use and COVID 19################
###negative binomial#########https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/#########
#summary(glm(Positive ~ April_11_Reduction + offset(log(total_population)), family = poisson(link = "log"), data=zcta_tests_mta_acs_nosi))
#summary(glm(Total ~ April_11_Reduction + offset(log(total_population)), family=quasipoisson, data=zcta_tests_mta_acs_nosi))
subway_covid <- matrix(nrow=3, ncol=12)

library(MASS)

subway_covid_unadj <- NULL
subway_covid_adj_test <- NULL
subway_covid_adj_conf <- NULL
subway_totalrate <- NULL

#UNADJUSTED

c <- glm(zcta_tests_mta_acs_nosi$positivegiventest ~ zcta_tests_mta_acs_nosi$April_11_Reduction, family = binomial(link = "logit"), weights = zcta_tests_mta_acs_nosi$Total)
subway_covid[1,1:4] <- c(exp(c$coefficients[2]), exp(confint(c)[2,]), summary(c)$coefficients[,4][2])
a <- glm.nb(Total ~ April_11_Reduction + offset(log(total_population)), data=zcta_tests_mta_acs_nosi, link=log)
subway_covid[2,1:4] <- c(exp(a$coefficients[2]), exp(confint(a)[2,]), summary(a)$coefficients[,4][2])
b <- glm.nb(Positive ~ April_11_Reduction + offset(log(total_population)), data=zcta_tests_mta_acs_nosi, link=log)
subway_covid[3,1:4] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])

subway_covid_unadj[[1]] <- c
subway_covid_unadj[[2]] <- a
subway_covid_unadj[[3]] <- b

#Adjusted for Testing Only
b <- glm.nb(Positive ~ April_11_Reduction + Total + offset(log(total_population)), data=zcta_tests_mta_acs_nosi, link=log)
subway_covid[3,5:8] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])

subway_covid_adj_test[[1]] <- b
subway_totalrate <- b


#Adjusted for Confounders and Testing

c <- glm(zcta_tests_mta_acs_nosi$positivegiventest ~ zcta_tests_mta_acs_nosi$April_11_Reduction + zcta_tests_mta_acs_nosi$density + zcta_tests_mta_acs_nosi$Med_Rm + zcta_tests_mta_acs_nosi$Median_Income, family = binomial(link = "logit"), weights = zcta_tests_mta_acs_nosi$Total)
subway_covid[1,9:12] <- c(exp(c$coefficients[2]), exp(confint(c)[2,]), summary(c)$coefficients[,4][2])
a <- glm.nb(Total ~ April_11_Reduction + zcta_tests_mta_acs_nosi$Median_Income + offset(log(total_population)), data=zcta_tests_mta_acs_nosi, link=log)
subway_covid[2,9:12] <- c(exp(a$coefficients[2]), exp(confint(a)[2,]), summary(a)$coefficients[,4][2])
b <- glm.nb(Positive ~ April_11_Reduction + zcta_tests_mta_acs_nosi$essential_both + zcta_tests_mta_acs_nosi$Median_Income + Total + offset(log(total_population)), data=zcta_tests_mta_acs_nosi, link=log)
subway_covid[3,9:12] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])

subway_covid_adj_conf[[1]] <- c
subway_covid_adj_conf[[2]] <- a
subway_covid_adj_conf[[3]] <- b

#1 - Unadjusted, Adjusted testing, then Adjusted for all

#Parameter names - columns 
colnames(subway_covid) <- c("Unadjusted","LowCI_U","HighCI_U", "Pvalue", "Adjusted_test","LowCI_A_test","HighCI_A_test", "Pvalue_test", "Adjusted","LowCI_A","HighCI_A", "Pvalue")
#rownames(subway_covid) <- colnames(zcta_tests_mta_acs_nosi)[41:43]
rownames(subway_covid) <- c("posgiventest","prob_test","prob_pos")
subway_covid <- as.data.frame(subway_covid)

subway_covid$final_u <- paste(round(subway_covid$Unadjusted,2)," (",round(subway_covid$LowCI_U,2),",",round(subway_covid$HighCI_U,2), ")", sep="")
subway_covid$final_a_test <- paste(round(subway_covid$Adjusted_test,2)," (",round(subway_covid$LowCI_A_test,2),",",round(subway_covid$HighCI_A_test,2), ")" , sep="")
subway_covid$final_a1_testconf <- paste(round(subway_covid$Adjusted,2)," (",round(subway_covid$LowCI_A,2),",",round(subway_covid$HighCI_A,2), ")" , sep="")

write.csv(subway_covid, "C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use/Tables/subway_covid.csv")

######################SES and COVID outcomes###################
###THIS CURRENTLY DOES NOT INCLUDE STATEN

ses_covid_unadj <- NULL
ses_covid_adj_conf <- NULL #confounder plus test
ses_covid_adj_test <- NULL #only test
ses_totalrate <- NULL #only test

####positivegiventest
ses_covid <- matrix(nrow=10, ncol=12)
for(i in 45:54) {
  a <- (glm(zcta_tests_mta_acs_nosi$positivegiventest ~ zcta_tests_mta_acs_nosi[,i], family = binomial(link = "logit"), weights = zcta_tests_mta_acs_nosi$Total))
  ses_covid[(i-44),1:4] <- c(exp(a$coefficients[2]), exp(confint(a)[2,]), summary(a)$coefficients[,4][2])
  ses_covid_unadj[[i-44]] <- a
  b <- (glm(zcta_tests_mta_acs_nosi$positivegiventest ~ zcta_tests_mta_acs_nosi[,i] + zcta_tests_mta_acs_nosi$Median_Income, family = binomial(link = "logit"), weights = zcta_tests_mta_acs_nosi$Total))
  ses_covid[(i-44),5:8] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])
  ses_covid_adj_conf[[i-44]] <- b
}

####Total Tests
ses_covid_proptest <- matrix(nrow=10, ncol=12)
for(i in 45:54) {
  a <- (glm.nb(zcta_tests_mta_acs_nosi$Total ~ zcta_tests_mta_acs_nosi[,i]+ offset(log(zcta_tests_mta_acs_nosi$total_population)), link=log))
  ses_covid_proptest[(i-44),1:4] <- c(exp(a$coefficients[2]), exp(confint(a)[2,]), summary(a)$coefficients[,4][2])
  ses_covid_unadj[[i-44+9]] <- a
  b <- (glm.nb(zcta_tests_mta_acs_nosi$Total ~ zcta_tests_mta_acs_nosi[,i] + zcta_tests_mta_acs_nosi$Median_Income+ offset(log(zcta_tests_mta_acs_nosi$total_population)), link=log))
  ses_covid_proptest[(i-44),5:8] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])
  ses_covid_adj_conf[[i-44+9]] <- b
}


####Positive
ses_covid_proppos <- matrix(nrow=10, ncol=12)
for(i in 45:54) {
  a <- (glm.nb(zcta_tests_mta_acs_nosi$Positive ~ zcta_tests_mta_acs_nosi[,i]+ offset(log(zcta_tests_mta_acs_nosi$total_population)), link=log))
  ses_covid_proppos[(i-44),1:4] <- c(exp(a$coefficients[2]), exp(confint(a)[2,]), summary(a)$coefficients[,4][2])
  ses_totalrate[[i-44]] <- a
  b <- (glm.nb(zcta_tests_mta_acs_nosi$Positive ~ zcta_tests_mta_acs_nosi[,i] + zcta_tests_mta_acs_nosi$Median_Income + zcta_tests_mta_acs_nosi$Total + offset(log(zcta_tests_mta_acs_nosi$total_population)), link=log))
  ses_covid_proppos[(i-44),5:8] <- c(exp(b$coefficients[2]), exp(confint(b)[2,]), summary(b)$coefficients[,4][2])
  ses_covid_adj_conf[[i-44+18]] <- b
  c <- (glm.nb(zcta_tests_mta_acs_nosi$Positive ~ zcta_tests_mta_acs_nosi[,i] + zcta_tests_mta_acs_nosi$Total + offset(log(zcta_tests_mta_acs_nosi$total_population)), link=log))
  ses_covid_proppos[(i-44),9:12] <- c(exp(c$coefficients[2]), exp(confint(c)[2,]), summary(c)$coefficients[,4][2])
  ses_covid_adj_test[[i-44+18]] <- c
  ses_totalrate[[i-44]] <- b
}

lapply(ses_totalrate, summary)

#ses_totalrate 1:10 - unadjusted
#ses_totalrate 11:20 - adjusted for only testing
#ses_totalrate 21:30 - adjusted for all

#Parameter names - columns 
ses_covid_all <- rbind(ses_covid, ses_covid_proptest, ses_covid_proppos)

colnames(ses_covid_all) <- c("Unadjusted","LowCI_U","HighCI_U", "Pvalue", "Adjusted","LowCI_A","HighCI_A", "Pvalue", "Adjusted_test","LowCI_A_test","HighCI_A_test", "Pvalue_test")
rownames(ses_covid_all) <- c(colnames(zcta_tests_mta_acs_nosi)[45:54],colnames(zcta_tests_mta_acs_nosi)[45:54],colnames(zcta_tests_mta_acs_nosi)[45:54])
ses_covid_all <- as.data.frame(ses_covid_all)

ses_covid_all$final_u <- paste(round(ses_covid_all$Unadjusted,2)," (",round(ses_covid_all$LowCI_U,2),",",round(ses_covid_all$HighCI_U,2), ")", sep="")
ses_covid_all$final_a_test <- paste(round(ses_covid_all$Adjusted_test,2)," (",round(ses_covid_all$LowCI_A_test,2),",",round(ses_covid_all$HighCI_A_test,2), ")" , sep="")
ses_covid_all$final_a_testconf <- paste(round(ses_covid_all$Adjusted,2)," (",round(ses_covid_all$LowCI_A,2),",",round(ses_covid_all$HighCI_A,2), ")" , sep="")

#View(ses_covid_all)
#View(ses_subway)
#View(subway_covid)

#NEGATIVE BINOMIAL DIAGNOSTICS - ok 
#lapply(ses_covid_unadj, countreg::rootogram)
#pscl::odTest(ses_covid_unadj[[11]])
#lapply(ses_covid_unadj, summary)

write.csv(ses_covid_all, "C:/Users/karla/OneDrive/Documents/Boston University/COVID/NYC Subway/Data/Datasets to Use/Tables/ses_covid_all.csv")


#FOR BEN FOREST PLOT

rm(list=setdiff(ls(), c("ses_subway_unadj", "subway_totalrate", "ses_totalrate", "ses_subway", "subway_covid", "ses_covid_all")))

save.image(file = "forest plot workspace.RData")

#NOTE - for all SES exposures, take out #9 (density) and #10 (median # of rooms)

#ses_subway_unadj - SES (10 exposures) and Subway (1 outcome)
#List of 10 - Median_Income, age>70, essential_both, health, essential_nothealth, minority, healthins, hs, density, Med_Rm

#subway_totalrate - Subway (1 exposure) and rate per 100k (1 outcome), adjusting for total tested
#just one object

#ses_totalrate - SES (10 exposures) and rate per 100k (1 outcome)
#1:10 - adjusted for only testing
# 10 variables 

#"ses_subway", "subway_covid", "ses_covid_all" - final dataframes I used 




##ADDED 8/28
glimpse(zcta_tests_mta_acs_nosi)
zcta_tests_mta_acs_nosi_final <- dplyr::select(zcta_tests_mta_acs_nosi, borough,  ZCTA5, Positive, April_11_Reduction, Median_Income, median_age, essential_both, health, essential, minority, healthins, hs, prob_pos, positivegiventest, prob_test)
colnames(zcta_tests_mta_acs_nosi_final) <- c("borough","ZCTA","cumincidence","mobility_April_11", "median_income", "gt_75_yrs", "all_essential","health_essential", "nonhealth_essential", "nonwhite_Hispanic", "uninsured","le_highschool", "rate_positive", "percent_positive_over_tests","rate_tests")
write.csv(zcta_tests_mta_acs_nosi_final,"Dryad/Cross-sectional_COVID_mobility_zcta.csv")

length(unique(zcta_tests_mta_acs_nosi_final$ZCTA))
