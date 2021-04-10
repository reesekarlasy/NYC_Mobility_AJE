
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
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
reported <- read.csv("Fare_Card_History_for_Metropolitan_Transportation_Authority__MTA___Beginning_2010.csv")
reported$From.Date<- mdy(reported$From.Date)
nyc <- reported[year(reported$From.Date)==2020,]

nyc[,5:26] <- sapply(nyc[,5:26], as.character)
nyc[,5:26] <- sapply(nyc[,5:26],function(x){as.numeric(gsub(",", "", x))})
nyc[,5:26] <- sapply(nyc[,5:26], as.numeric)
nyc$total <- rowSums(nyc[,5:26], na.rm=TRUE)
nyc$Remote.Station.ID <- as.character(nyc$Remote.Station.ID)
nyc <- nyc[!grepl("LIVINGSTON PLAZA", nyc$Station),]  #take out any place with Livingston Plaza
nyc <- nyc[!grepl("PATH", nyc$Station),]  #take out any place with Livingston Plaza

library(ggmap)
nyc$Station <- as.character(nyc$Station)
nyc_station <- unique(nyc$Station)
geo_stat2 <- read.csv('nyc_geocode_2020only.csv')
nyc_station_all <- cbind(geo_stat2, nyc_station)

nyc_station_all$lon[nyc_station_all$lon <= -74.3 | nyc_station_all$lon >= -73.67 | nyc_station_all$lat <= 40.45 | nyc_station_all$lat >= 40.95] <- NA #take out wrong long and lat
nyc_station_all$lat[nyc_station_all$lon <= -74.3 | nyc_station_all$lon >= -73.67 | nyc_station_all$lat <= 40.45 | nyc_station_all$lat >= 40.95] <- NA #take out wrong long and lat

missing_fixed <- read.csv("missing_fixed.csv")

nyc_station_all1 <- merge(nyc_station_all, missing_fixed, by="nyc_station", all.x=TRUE)
nyc_station_all1$lon[is.na(nyc_station_all1$lon)] <- nyc_station_all1$lon1[is.na(nyc_station_all1$lon)]
nyc_station_all1$lat[is.na(nyc_station_all1$lat)] <- nyc_station_all1$lat1[is.na(nyc_station_all1$lat)]

#remove nyc station
nyc_station1 <- nyc_station_all1[!is.na(nyc_station_all1$lon),]

#Manually fix some nyc station
nyc_station1$lon[nyc_station1$nyc_station=="42ND STREET AND GRAND CENTRAL"] <- -73.9772
nyc_station1$lat[nyc_station1$nyc_station=="42ND STREET AND GRAND CENTRAL"] <- 40.7527
nyc_station1$lon[nyc_station1$nyc_station=="53RD STREET-4TH AVENUE"] <- -73.9691
nyc_station1$lat[nyc_station1$nyc_station=="53RD STREET-4TH AVENUE"] <- 40.7577
nyc_station1$lon[nyc_station1$nyc_station=="96TH STREET-LEXINGTON AVE"] <- -73.95107
nyc_station1$lat[nyc_station1$nyc_station=="96TH STREET-LEXINGTON AVE"] <- 40.785672
nyc_station1$lon[nyc_station1$nyc_station=="96TH STREET - 2 AVENUE"] <- -73.947152
nyc_station1$lat[nyc_station1$nyc_station=="96TH STREET - 2 AVENUE"] <- 40.784318

### merge
colnames(nyc_station1)[1] <- "Station"
nyc_longlat <- merge(nyc, nyc_station1[,c(1,3,4)], by="Station", all.x=TRUE) #RIGHT JOIN
nyc_long_lat <- nyc_longlat[!is.na(nyc_longlat$lon),] #initial dataset but with lat and long
nyc_long_lat2 <- nyc_long_lat ####we will turn nyc_long_lat into a SpatialObject

#get unique dates
dates <- (as.data.frame(unique(nyc_long_lat$From.Date))) %>% arrange(unique(nyc_long_lat$From.Date))
dates1 <- dates[,1]

#SHAPEFILES
#All boroughs - shp1
shp1 <- readOGR("cb_2018_us_zcta510_500k", "cb_2018_us_zcta510_500k")
zcta_tract <- read.csv("zcta_info.csv")
shp1 <- shp1[shp1@data$ZCTA5CE10 %in% zcta_tract$ZCTA5,]
shp1_f <- fortify(shp1)

########################PLOT###########
detach(package:plyr)

nyc_agg <- nyc_long_lat %>%
  group_by(Remote.Station.ID) %>%
  summarise(sum=sum(total,na.rm=TRUE))

nyc_agg2 <- unique(merge(nyc_agg, nyc_long_lat[,c("Remote.Station.ID", "lon","lat")], by="Remote.Station.ID", all.x=TRUE))
################################NUMBER OF PEOPLE PER BOROUGH

coordinates(nyc_long_lat)<-c("lon","lat")  ##Coerces this to a SpatialPointsDataFrame
proj4string(nyc_long_lat)<-proj4string(shp1)

polygon <- over(as(nyc_long_lat,"SpatialPoints"), as(shp1,"SpatialPolygons"))
nyc_ll_poly <- cbind(nyc_long_lat2, polygon)
nyc_ll_poly$polygon <- as.factor(nyc_ll_poly$polygon)

all_nyc <- nyc_ll_poly[!is.na(nyc_ll_poly$polygon),] %>%
  group_by(polygon) %>%
  summarise(sum=sum(total,na.rm=TRUE))

matrix_nyc <- matrix(NA, nrow(dates), length(unique(zcta_tract$ZCTA5)))
zctacode <- (as(shp1,"data.frame"))[1] #colnames of matrix_nyc
colnames(matrix_nyc) <- t(zctacode)

for (i in 1:nrow(dates)) {
  nyc_long_lat3 <- nyc_long_lat2[nyc_long_lat2$From.Date==dates1[i],]
  nyc_long_lat4 <- nyc_long_lat3
  coordinates(nyc_long_lat3)<-c("lon","lat")  ##Assigning coordinates coerces this to a SpatialPointsDataFrame
  proj4string(nyc_long_lat3)<-proj4string(shp1)
  polygon <- over(as(nyc_long_lat3,"SpatialPoints"), as(shp1,"SpatialPolygons"))
  nyc_ll_poly <- cbind(nyc_long_lat4, polygon)
  nyc_ll_poly$polygon <- as.factor(nyc_ll_poly$polygon)
  a <- nyc_ll_poly[!is.na(nyc_ll_poly$polygon),] %>%
    group_by(polygon) %>%
    summarise(sum=sum(total,na.rm=TRUE))
  #a has a lot of missing polygons, need to become 0
  numbers <- as.data.frame(seq(1,214))
  a$polygon <- as.numeric(as.character(a$polygon))
  colnames(numbers) <- "polygon"
  a2 <- left_join(numbers,a, by="polygon")
  #
  a3 <- pull(a2,sum)   #just get the "sum" column of a (but a tibble)
  a3[is.na(a3)] <- 0
  matrix_nyc[i,1:214] <- a3
}

nyc_matrix_final <- cbind(dates,matrix_nyc)
colnames(nyc_matrix_final)[1] <- "dates"

a<- (c(TRUE, colMeans(nyc_matrix_final[2:ncol(nyc_matrix_final)])!=0))
length(a)
nyc_matrix_final_more <- nyc_matrix_final  #MAYBE TAKE OUT THE 0s

zctacode <- as.data.frame(names(nyc_matrix_final_more)[2:ncol(nyc_matrix_final_more)])
nrow(zctacode)

nyc_matrix_final2 <-reshape(nyc_matrix_final_more, 
                            direction = "long",
                            varying = list(names(nyc_matrix_final_more)[2:ncol(nyc_matrix_final_more)]),
                            v.names = "MTA",
                            idvar = "dates",
                            timevar = "ZCTA",
                            times = t(zctacode))

nyc_matrix_final2$ZCTA <- as.factor(nyc_matrix_final2$ZCTA)


##nyc_final is the final wide matrix
nyc_final <- dcast.data.table(setDT(nyc_matrix_final2), 
                                      ZCTA ~ dates, 
                                      value.var = "MTA")

################CALCULATE PERCENT CHANGE STANDARDIZED #################

nyc_change <- cbind(nyc_final[,1], (nyc_final[,3:ncol(nyc_final)] - nyc_final[,2:(ncol(nyc_final)-1)])/(nyc_final[,2:(ncol(nyc_final)-1)]))

nyc_change_long <-reshape(nyc_change, 
                            direction = "long",
                            varying = list(names(nyc_change)[2:ncol(nyc_change)]),
                            v.names = "MTA",
                            idvar = "ZCTA",
                            timevar = "dates",
                            times = names(nyc_change)[2:ncol(nyc_change)])

###ADD IN BOROUGH###
county_demographics <- read.csv("county_demographics.csv")
borough_zcta <- unique(zcta_tract[,c("ZCTA5","COUNTY")])
colnames(borough_zcta) <- c("ZCTA","Borough")
borough_zcta
#borough_zcta <- borough_zcta[-c(96,176),]
nrow(borough_zcta)

borough_zcta$ZCTA <- as.character(borough_zcta$ZCTA)
borough_zcta<- as.data.frame(borough_zcta)
nyc_final_borough <- merge(nyc_final, unique(borough_zcta), by="ZCTA", all.x=TRUE)

##########STANDARDIZE 

nyc_change2 <- ((nyc_final_borough[,2:ncol(nyc_final)])-rowMeans(nyc_final_borough[,2:10]))/apply(nyc_final_borough[,2:10],1,sd)
nyc_change_zcta <- cbind(nyc_final_borough$ZCTA,nyc_change2,nyc_final_borough$Borough)

library(Rmisc)
summary(nyc_change_zcta$April_11_Reduction)
detach(package:dplyr)
library(dplyr)

nyc_change_long2_zcta <- melt(nyc_change_zcta, id=c("V1","V3"))
colnames(nyc_change_long2_zcta) <- c("V1", "V3", "dates", "MTA")

#SUMMARY OF APRIL 11
subway <- read.csv("subway.csv") #tests in zcta to ZCTA
colnames(subway) <- "ZCTA"
nyc_change_zcta_124 <- nyc_change_zcta[(nyc_change_zcta$V1 %in% subway$ZCTA),]

summary(nyc_change_zcta_124$'2020-04-11')


#nyc_change_long2_zcta <- reshape(nyc_change_zcta, 
#                          direction = "long",
#                          varying = list(names(nyc_change_zcta)[2:(ncol#(nyc_change_zcta)-1)]),
#                          v.names = "MTA",
#                          idvar = c("V1","V3"),
 #                         timevar = "dates",
#                          times = names(nyc_change_zcta)[2:(ncol(nyc_change#_zcta)-1)])

nyc_change_long2_zcta$V1 <- as.factor(nyc_change_long2_zcta$V1)
nyc_change_long2_zcta$dates <- ymd(nyc_change_long2_zcta$dates)

colnames(nyc_change_long2_zcta)[1:2] <- c("ZCTA","Borough")
nyc_change_long2_zcta$Borough <- as.factor(nyc_change_long2_zcta$Borough)

nyc_change_long2_zcta$borough[nyc_change_long2_zcta$Borough=="61"] <- "Manhattan"
nyc_change_long2_zcta$borough[nyc_change_long2_zcta$Borough=="5"] <- "Bronx"
nyc_change_long2_zcta$borough[nyc_change_long2_zcta$Borough=="85"] <- "Staten Island"
nyc_change_long2_zcta$borough[nyc_change_long2_zcta$Borough=="47"] <- "Brooklyn"
nyc_change_long2_zcta$borough[nyc_change_long2_zcta$Borough=="81"] <- "Queens"
table(nyc_change_long2_zcta$borough)


#SUPPLEMENTARY FIGURE

nyc_change_long2_zcta_nosi <- nyc_change_long2_zcta[nyc_change_long2_zcta$Borough!="85",]
nyc_change_long2_zcta_nosi <- nyc_change_long2_zcta_nosi[nyc_change_long2_zcta_nosi$Borough!="Staten Island",]
length(unique(nyc_change_long2_zcta_nosi$ZCTA))

###WHAT I USED###
p <- ggplot() + geom_line(nyc_change_long2_zcta_nosi, mapping=aes(x=dates, y=MTA, group=ZCTA, color=borough), size=0.4, alpha=0.20) + geom_smooth(nyc_change_long2_zcta_nosi, mapping=aes(x=dates, y=MTA, group=borough, color=borough), span=0.5, size=1, se=FALSE) + ylab("Mobility") + ggtitle("") + scale_fill_distiller(palette="Spectral") + xlab("")
#suppfig1 <- p + theme_minimal() + labs(color="Borough") + xlim(as.Date("2020-02-22"), as.Date("2020-04-11")) + scale_color_manual(values = c("#59A14F","#4E79A7","#E15759", "#F28E2B"))
suppfig1 <- p + theme_minimal() + labs(color="Borough") + xlim(as.Date("2020-02-22"), as.Date("2020-04-11")) + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + theme_minimal(base_size=13) + theme(legend.position="bottom", panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.margin = margin(0, 15, 0, -15))
#+ scale_color_manual(values = c("#4E79A7","#E15759","#76B7B2", "#F28E2B"))
#+ scale_color_manual(values = c("orchid3","dodgerblue2","#E31A1C", "#FF7F00"))

pdf("Final Figures/Final/suppfig1.pdf", width=7, height=5)
plot(suppfig1)
graphics.off()

tiff("Final Figures/suppfig1.tiff", width=7, height=5, units="in", res=500)
plot(suppfig1)
graphics.off()


####Create subway quantiles###

nyc_change_long2_zcta_nosi$ZCTA <- as.integer(as.character(nyc_change_long2_zcta_nosi$ZCTA))
length(unique(nyc_change_long2_zcta_nosi$ZCTA)) #199
#county_demographics$essential_both <- county_demographics$essential_both*10
nyc_change_long2_zcta_nosi1 <- left_join(nyc_change_long2_zcta_nosi, county_demographics[,c("ZCTA5","Median_Income1")], by=c("ZCTA"="ZCTA5"))
#where is ZCTA 10301?

#only include GEOIDS included in the final analytical sample
subway <- read.csv("subway.csv") #tests in zcta to ZCTA
colnames(subway) <- "ZCTA"
nyc_change_long2_zcta_nosi1 <- nyc_change_long2_zcta_nosi1[(nyc_change_long2_zcta_nosi1$ZCTA %in% subway$ZCTA),]

unique(nyc_change_long2_zcta_nosi1$ZCTA) %>% write.csv("fig2.csv")
length(unique(nyc_change_long2_zcta_nosi1$ZCTA))

library(StatMeasures)

#SUPPLEMENTARY FIGURE

nyc_change_long2_zcta_nosi <- nyc_change_long2_zcta[nyc_change_long2_zcta$Borough!="85",]
nyc_change_long2_zcta_nosi <- nyc_change_long2_zcta_nosi[nyc_change_long2_zcta_nosi$Borough!="Staten Island",]
length(unique(nyc_change_long2_zcta$ZCTA))
length(unique(nyc_change_long2_zcta_nosi$ZCTA))



#FIGURE 2 FOR DECILES
nyc_change_long2_zcta_nosi11 <- nyc_change_long2_zcta_nosi1[!is.na(nyc_change_long2_zcta_nosi1$Median_Income1),]
nyc_change_long2_zcta_nosi11 <- nyc_change_long2_zcta_nosi11 %>% mutate(income_d = ntile(Median_Income1, 5))

table(nyc_change_long2_zcta_nosi11$income_d)

length(unique(nyc_change_long2_zcta_nosi1$ZCTA))
length(unique(nyc_change_long2_zcta_nosi11$ZCTA))

###
###FIXED TO USE ONLY 124###
p <- ggplot() + geom_line(nyc_change_long2_zcta_nosi11, mapping=aes(x=dates, y=MTA, group=ZCTA, color=borough), size=0.4, alpha=0.20) + geom_smooth(nyc_change_long2_zcta_nosi11, mapping=aes(x=dates, y=MTA, group=borough, color=borough), span=0.5, size=1, se=FALSE) + ylab("Mobility") + ggtitle("") + scale_fill_distiller(palette="Spectral") + xlab("")
#suppfig1 <- p + theme_minimal() + labs(color="Borough") + xlim(as.Date("2020-02-22"), as.Date("2020-04-11")) + scale_color_manual(values = c("#59A14F","#4E79A7","#E15759", "#F28E2B"))
suppfig1 <- p + theme_minimal() + labs(color="Borough") + xlim(as.Date("2020-02-22"), as.Date("2020-04-11")) + scale_color_manual(values = c("#1f77b4","#2ca02c","#d62728", "#dc5fbd")) + theme_minimal(base_size=13) + theme(legend.position="bottom", panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.margin = margin(0, 15, 0, -15))
#+ scale_color_manual(values = c("#4E79A7","#E15759","#76B7B2", "#F28E2B"))
#+ scale_color_manual(values = c("orchid3","dodgerblue2","#E31A1C", "#FF7F00"))

tiff("Final Figures/suppfig1_true.tiff", width=7, height=5, units="in", res=500)
plot(suppfig1)
graphics.off()

pdf("Final Figures/Final/suppfig1_true.pdf", width=7, height=5)
plot(suppfig1)
graphics.off()


nyc_change_long2_zcta_nosi11 %>%
  group_by(income_d) %>%
  summarise(mean=mean(MTA,na.rm=TRUE))

nyc_change_long2_zcta_nosi11 %>%
  group_by(income_d) %>%
  summarise(mean=mean(Median_Income1,na.rm=TRUE))

sextiles <- nyc_change_long2_zcta_nosi11 %>%
  group_by(income_d) %>%
  summarise(mean=mean(Median_Income1,na.rm=TRUE), min=min(Median_Income1,na.rm=TRUE), max=max(Median_Income1,na.rm=TRUE))
sextiles

#write.csv(sextiles_essen,"sextile_e.csv")
#saveRDS(fig2, "fig2.rds")
#saveRDS(nyc_change_long2_zcta_nosi1, "dataset.rds")
#View(nyc_change_long2_zcta_nosi1)

#FINAL FIG 2 FROM BEN
#Intervention line
cuts_5 <- data.frame(Executive_Order= c("      ","     ","    "), vals = c(as.numeric(as.Date("2020-03-07")), as.numeric(as.Date("2020-03-14")), as.numeric(as.Date("2020-03-21"))), stringsAsFactors = FALSE)

p <- ggplot() +
  geom_smooth(nyc_change_long2_zcta_nosi11, mapping=aes(x=dates, y=MTA, group=income_d, fill = income_d), color="transparent", alpha =0.10, size=1, se=TRUE, span=0.45) +
  geom_smooth(nyc_change_long2_zcta_nosi11, mapping=aes(x=dates, y=MTA, group=income_d, color=income_d),size=1.2, se=FALSE, span=0.5) + ylab("Mobility") + xlab("Date") +
  ggtitle("") +
  scale_color_distiller(palette="RdYlBu", trans="reverse", labels=c("15,778-22,544","22,545-27,588", "27,589-34,186","34,187-57,987","57,988-107,138"), guide = "legend") + scale_fill_distiller(palette="RdYlBu", trans="reverse", labels=c("15,778-22,544","22,545-27,588", "27,589-34,186","34,187-57,987","57,988-107,138"), guide = "none") 

fig2 <- p + theme_classic(base_size=12) + labs(color="Median Income, $") + geom_vline(mapping = aes(xintercept = vals, linetype=Executive_Order), data = cuts_5, size=0.7, alpha=0.40, show.legend = FALSE) + scale_linetype_manual(values=c(4, 5, 3)) + geom_text(aes(x=as.Date(vals,origin = "1970-01-01")-c(7,5.5,5.5), label=Executive_Order, y=c(-18,-21,-24)), data=cuts_5, alpha=0.7, size=4)  + theme(
  axis.text.x = element_text(angle = 45, hjust=1),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.margin = margin(0, 15, 0, -15),
  text=element_text(size=14),
  axis.text=element_text(size=14, colour="black"),
  legend.text=element_text(size=14),
  legend.position = c(0.8, 0.7)) + scale_x_date(date_labels = "%m/%d/%y", limits = c(as.Date("2020-02-22"), as.Date("2020-04-11")), breaks=c(as.Date("2020-02-22"), as.Date("2020-02-29"), as.Date("2020-03-07"), as.Date("2020-03-14"),as.Date("2020-03-21"),as.Date("2020-03-28"),as.Date("2020-04-04"),as.Date("2020-04-11"))) + scale_y_continuous(breaks=c(0,-5,-10,-15,-20,-25)) 

plot(fig2)


png("Final Figures/AJE-00123-2015 Sy Figure 3.png", width=7.8, height=5.5, units="in", res=500)
plot(fig2)
graphics.off()

pdf("Final Figures/Final/AJE-00123-2015 Sy Figure 3.pdf", width=7.8, height=5.5)
plot(fig2)
graphics.off()


##################

nyc_change_long2_zcta$dates <- as.Date(nyc_change_long2_zcta$dates)
nyc_change_long2_zcta1111 <-nyc_change_long2_zcta[nyc_change_long2_zcta$dates >= as.Date("2020-02-1"),]

p <- ggplot(nyc_change_long2_zcta1111[nyc_change_long2_zcta1111$Borough!="85",], aes(x=dates, y=MTA, group=borough, color=borough)) + geom_smooth(se=FALSE, size=1, span=0.5) + ylab("Mobility") + scale_fill_manual(palette="Spectral") + labs(color = "Borough")
p + theme_minimal()

p <- ggplot(nyc_change_long2_zcta, aes(x=dates, y=MTA, group=ZCTA, color=Borough)) + geom_line() +   xlab("") + ggtitle("NYC MTA Station Standardized by ZCTA")
p + theme_minimal() + xlab("Date") + ylab("value")  

p <- ggplot(nyc_change_long2_zcta[nyc_change_long2_zcta$Borough!="85",], aes(x=dates, y=MTA, group=ZCTA, color=borough)) + geom_line(size=0.5) + geom_smooth() + xlab("") + ggtitle("NYC MTA Station Standardized by ZCTA") + scale_fill_distiller(palette="Spectral")
p + theme_minimal() 


#DESCRIPTIVES

summary(nyc_change_zcta[nyc_change_zcta$Borough!=85,"April_11_Reduction"])


#####################COMBINE TO SHAPEFILE to plot March 28 decline
colnames(nyc_change_zcta) <- c("ZCTA5CE10", "a","b","c","d","e","f","g","h","i","j","k","l","m","April_11_Reduction","Borough")
nyc_change_zcta$April_11_Reduction <- as.numeric(nyc_change_zcta$April_11_Reduction)
mynewspdf <- merge(shp1, nyc_change_zcta, by="ZCTA5CE10", duplicateGeoms = T)

mynewspdf@data$id <- rownames((as(shp1,"data.frame")))
shp2  <- fortify(shp1)
shp3  <- merge(shp2,mynewspdf@data, by="id", all.x=TRUE)

plot_2 <- ggplot()
plot_2a <- plot_2 +  geom_polygon(data=shp3, aes(x=long, y=lat, group=group, fill=April_11_Reduction), color = "black", size=0.7) 
plot_2b <- plot_2a + xlim(-74.3, -73.67) +  ylim(40.45, 40.95) 
plot_2b + theme_minimal()

##mean standardized change## by borough #126 total

nyc_change_zcta_nomissing <- nyc_change_zcta[!is.na(nyc_change_zcta$April_11_Reduction)]
nyc_change_zcta_nomissing$borough[nyc_change_zcta_nomissing$Borough=="61"] <- "Manhattan"
nyc_change_zcta_nomissing$borough[nyc_change_zcta_nomissing$Borough=="5"] <- "Bronx"
nyc_change_zcta_nomissing$borough[nyc_change_zcta_nomissing$Borough=="47"] <- "Brooklyn"
nyc_change_zcta_nomissing$borough[nyc_change_zcta_nomissing$Borough=="81"] <- "Queens"

nyc_change_b <- nyc_change_zcta_nomissing %>%
  group_by(borough) %>%
  summarise(mean=mean(April_11_Reduction,na.rm=TRUE), lower=CI(April_11_Reduction)[3], upper=CI(April_11_Reduction)[1])

nyc_change_b1 <- nyc_change_zcta_nomissing %>%
  group_by(borough) %>%
  summarise(median=median(April_11_Reduction,na.rm=TRUE), lower=summary(April_11_Reduction)[2], upper=summary(April_11_Reduction)[5])
#View(nyc_change_b1)

#####################COMBINE TO SHAPEFILE
nyc_change_zcta <- as.data.frame(nyc_change_zcta)
nyc_change_zcta$ZCTA <- as.character(nyc_change_zcta$ZCTA5CE10)
nyc_1 <- nyc_change_zcta
mynewspdf1 <- merge(shp1, nyc_1, by="ZCTA5CE10", duplicateGeoms = T)

mynewspdf1@data$id <- rownames((as(shp1,"data.frame")))
shp2  <- fortify(shp1)
shp3  <- merge(shp2,mynewspdf1@data, by="id", all.x=TRUE)

shp_nyc1 <- shp3[shp3$Borough!=85,]
shp_nyc <- shp_nyc1 %>% filter(!is.nan(April_11_Reduction))
length(unique(shp_nyc$ZCTA))

#125 should be 124
a222 <- unique(shp_nyc$ZCTA)
a222[!(unique(shp_nyc$ZCTA) %in% COVID_mobility_zcta_c$ZCTA)]
#"10020"ZIP code 10020 is located in southeast New York and covers an extremely small land area compared to other ZIP codes in the United States. Population, 0.
#TAKE OUT

shp_nyc <- shp_nyc %>% filter(ZCTA!="10020")
shp_nyc <- shp_nyc %>% select(c(ZCTA, long, lat, i, j, k, l, m, April_11_Reduction))
colnames(shp_nyc)[4:9] <- c("")  


write.csv(shp_nyc, "4._Map_Mobility_zcta.borough.csv") 

#####################################################

plot_2 <- ggplot()
plot_2a <- plot_2 +  geom_polygon(data=shp3[shp3$Borough!=85,], aes(x=long, y=lat, group=group, fill=April_11_Reduction), color = "black", size=0.7) + scale_fill_distiller(palette="Blues",na.value="gray92")
plot_2b <- plot_2a + xlim(-74.08, -73.63) +  ylim(40.55, 40.92) + xlab("") + ylab("") 
aaaaa <- plot_2b + theme_minimal()  + labs(fill = "Decrease in \nMobility")  + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank())

#######PLOT BUT CHANGE IN TIME

loop <- colnames(shp3)[21:26] #2/29 - 4/11 (missing 4/3)
for(a in loop) { 
  plot_2 <- ggplot()
  fill_plot <- sym(a)
  plot_2a <- plot_2 +  geom_polygon(data=shp3[shp3$Borough!=85,], aes(x=long, y=lat, group=group, fill=!!fill_plot), color = "black", size=0.7) + scale_fill_distiller(palette="YlGnBu", na.value="gray92", trans="reverse", limits=c(1,-42), breaks=c(1,-42), labels=c("Least\ndecrease","\n\nGreatest\ndecrease"))
  plot_2b <- plot_2a + xlim(-74.08, -73.63) +  ylim(40.55, 40.92) + xlab("") + ylab("") 
  plot_2c <- plot_2b + theme_minimal()  + labs(fill = "Decrease in \nMobility")  + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
  tiff(paste("Figs/",a,".tiff", sep=""), width=10, height=8, units="in", res=600)
  plot(plot_2c)
  dev.off()
}


loop <- colnames(shp3)[21:26] #2/29 - 4/11 (missing 4/3)
for(a in loop) { 
  plot_2 <- ggplot()
  fill_plot <- sym(a)
  plot_2a <- plot_2 +  geom_polygon(data=shp3[shp3$Borough!=85,], aes(x=long, y=lat, group=group, fill=!!fill_plot), color = "black", size=0.7, show.legend=FALSE) + scale_fill_distiller(palette="YlGnBu", na.value="gray92", trans="reverse", limits=c(1,-42), breaks=c(1,-42), labels=c("Least\ndecrease","\n\nGreatest\ndecrease"))
  plot_2b <- plot_2a + xlim(-74.08, -73.63) +  ylim(40.55, 40.92) + xlab("") + ylab("") 
  plot_2c <- plot_2b + theme_minimal()  + labs(fill = "Decrease in \nMobility")  + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
  pdf(paste("Final Figures/Final/",a,".pdf", sep=""), width=9, height=8)
  plot(plot_2c)
  dev.off()
}

############SAVE##################
coefficients_m <- nyc_1
#nyc_final until end is from here

rm(list=setdiff(ls(), c("nyc_matrix_borough", "nyc_change_long2", "nyc_final", "nyc_matrix_final_more", "nyc_change_zcta", "nyc_change_long2_zcta", "nyc_change_long2_zcta_nosi", "nyc_change_long2_zcta_nosi1", "subset_nyc","coefficients_m", "nyc_change_b")))

save.image(file = "Workspace/subway12_workspace.RData")


##ADDED 8/28

nyc_change_long2_zcta_nosi1_final <- dplyr::select(nyc_change_long2_zcta_nosi1, dates, ZCTA, borough, MTA)
colnames(nyc_change_long2_zcta_nosi1_final) <- c("date","ZCTA","borough","mobility")
write.csv(nyc_change_long2_zcta_nosi1_final,"Dryad/Mobility.csv")