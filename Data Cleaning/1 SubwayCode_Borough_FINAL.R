######################################
# Note from Micaela:
# it is not clear to me what this file was for, there didn't seem to be any resulting csv file or pdf result output of this R file
# it is good practice to export any results that will be called by subsequent files
######################################

# install.packages('lubridate', repos='http://cran.us.r-project.org')
# install.packages('rgdal', repos='http://cran.us.r-project.org')
# install.packages('ggplot2', repos='http://cran.us.r-project.org')
# install.packages('mapproj', repos='http://cran.us.r-project.org')
# install.packages('dplyr', repos='http://cran.us.r-project.org')
# install.packages('plyr', repos='http://cran.us.r-project.org')

library(lubridate)
library(plyr)
library(dplyr)
library(rgdal)  
library(ggplot2)
library(mapproj)

######################################
# set working directory here
######################################
setwd('/Users/Micaela/Documents/NY_COVID19/MTA_paper/AJE_proofs/FINAL_R_CODE_32321/Files_[Micaela_edits]')

######################################
# load the MTA ridership data from 2010-2020
######################################
reported <- read.csv("Fare_Card_History_for_Metropolitan_Transportation_Authority__MTA___Beginning_2010.csv")
reported$From.Date<- mdy(reported$From.Date)

######################################
# clean up the ridership data
######################################
# select the 2020 data
nyc <- reported[year(reported$From.Date)==2020,]
# total fare
nyc$total <- rowSums(nyc[,5:26], na.rm=TRUE)
# format columns
nyc$Remote.Station.ID <- as.character(nyc$Remote.Station.ID)
nyc$Station <- as.character(nyc$Station)
# list of stations, there are 473 stations
nyc_station <- unique(nyc$Station)

######################################
# load the geographic coordinates for the stations
# note, these are appoximate locations based on google maps
######################################
geo_stat2 <- read.csv('nyc_geocode_2020.csv')
nyc_station <- cbind(geo_stat2, nyc_station)

# replace incorrect lat/longs with NAs
nyc_station$lon[nyc_station$lon <= -74.3 | nyc_station$lon >= -73.67 | nyc_station$lat <= 40.45 | nyc_station$lat >= 40.95] <- NA #take out wrong long and lat
nyc_station$lat[nyc_station$lon <= -74.3 | nyc_station$lon >= -73.67 | nyc_station$lat <= 40.45 | nyc_station$lat >= 40.95] <- NA #take out wrong long and lat
# there are 49 stations with NAs that need to be corrected

######################################
# manually fix some of the station lat/longs
######################################
# lines 84-89 above are overridden by this line
# so we should be able to delete lines 83-92
nyc_station1 <- nyc_station[!is.na(nyc_station$lon),]

# fix some lat/longs manually
nyc_station1$lon[nyc_station1$nyc_station=="42ND STREET AND GRAND CENTRAL"] <- -73.9772
nyc_station1$lat[nyc_station1$nyc_station=="42ND STREET AND GRAND CENTRAL"] <- 40.7527
nyc_station1$lon[nyc_station1$nyc_station=="53RD STREET-4TH AVENUE"] <- -73.9691
nyc_station1$lat[nyc_station1$nyc_station=="53RD STREET-4TH AVENUE"] <- 40.7577
nyc_station1$lon[nyc_station1$nyc_station=="96TH STREET-LEXINGTON AVE"] <- -73.95107
nyc_station1$lat[nyc_station1$nyc_station=="96TH STREET-LEXINGTON AVE"] <- 40.785672
nyc_station1$lon[nyc_station1$nyc_station=="96TH STREET - 2 AVENUE"] <- -73.947152
nyc_station1$lat[nyc_station1$nyc_station=="96TH STREET - 2 AVENUE"] <- 40.784318

# Savepoint added by Micaela
station_info<- nyc_station1[,c('nyc_station','lat','lon')]
write.csv(station_info,file='NYC_subway_station_lat_long.csv',row.names=FALSE)

######################################
# load the shapefile for NYC
######################################
# NYC shapefile with all 5 boroughs
shp1 <- readOGR("NYC_shapefile", "geo_export_3517d3a1-f228-4112-9379-f8c33f1ff7e7")

# Checkpoint added by Micaela
# check out the plot
plot(shp1) # it has 5 polygons, one per borough

# define the 4 boroughs we are working with, Staten Island is being excluded because they don't have subway stations
si <- c("Manhattan", "Queens", "Bronx", "Brooklyn")
shp <- shp1[shp1@data$boro_name %in% si,]
shp_f <- fortify(shp)

######################################
# merge the 2020 subway rideership data with with station lat/longs
######################################
colnames(nyc_station1)[ncol(nyc_station1)] <- "Station"
nyc_longlat <- merge(nyc, nyc_station1, by="Station", all.x=TRUE) # RIGHT JOIN
# we didn't have lat/longs for all the stations in the nyc dataframe, so remove those we don't have locations for
nyc_long_lat <- nyc_longlat[!is.na(nyc_longlat$lon),] # initial dataset but with lat and long
# make a copy of the data frame to turn into a spatial object
nyc_long_lat2 <- nyc_long_lat 

######################################
# get unique dates and arrange them from earlies to latest
######################################
# our data spans Jan 4th, 2020 - April 11, 2020
dates <- (as.data.frame(unique(nyc_long_lat$From.Date))) %>% arrange(unique(nyc_long_lat$From.Date))
dates1 <- dates[,1]

######################################
# calculate the total rides per station
######################################
# aggragate the total rides for each station
detach(package:plyr)
nyc_agg <- nyc_long_lat %>%
  group_by(Remote.Station.ID) %>%
  summarise(sum=sum(total,na.rm=TRUE))

# add the lat/long to the aggregated data
nyc_agg2 <- unique(merge(nyc_agg, nyc_long_lat[,c("Remote.Station.ID", "lon","lat")], by="Remote.Station.ID", all.x=TRUE))

######################################
# calculate the number of swipes per borough each week 
######################################
matrix_nyc <- matrix(NA, nrow(dates), 5)
colnames(matrix_nyc) <- c("Bronx", "Staten Island", "Brooklyn", "Queens", "Manhattan")

######################################
# Checkpoint: Micaela added this section to check the loop that follows
# check how we can group the subway stations to borough based
######################################
# copy the ridership data
nyc_long_lat3 <- nyc_long_lat2
nyc_long_lat4 <- nyc_long_lat3
# coerces this to a SpatialPointsDataFrame
coordinates(nyc_long_lat3)<-c("lon","lat")  
# use the shp1 projection atributes 
proj4string(nyc_long_lat3)<-proj4string(shp1)
# figure out which borough polygon each lat/long is in
nyc.polygon <- over(as(nyc_long_lat3,"SpatialPoints"), as(shp1,"SpatialPolygons"))
# add the polygon to the weekly rideship data
nyc_ll_poly <- cbind(nyc_long_lat4, nyc.polygon)
nyc_ll_poly$nyc.polygon <- as.factor(nyc_ll_poly$nyc.polygon)

# make a plot to check the points were categorized correctly by borough
data2020<- nyc_ll_poly[c('Station','From.Date','total','lon','lat','nyc.polygon')]
data2020$col<- NA 
data2020$col[which(data2020$nyc.polygon==1)]<- 'blue' # Bronx
data2020$col[which(data2020$nyc.polygon==2)]<- 'black' # Staten Island
data2020$col[which(data2020$nyc.polygon==3)]<- 'green' # Brooklyn
data2020$col[which(data2020$nyc.polygon==4)]<- 'purple' # Queens
data2020$col[which(data2020$nyc.polygon==5)]<- 'red' # Manhattan

plot(shp1) # it has 5 polygons, one per borough
points(data2020$lon,data2020$lat,col=data2020$col)

# make a plot to check the points were categorized correctly by borough
data2020$borough<- NA
data2020$borough[which(data2020$nyc.polygon==1)]<- 'Bronx' # Bronx
data2020$borough[which(data2020$nyc.polygon==2)]<- 'Staten.Island' # Staten Island
data2020$borough[which(data2020$nyc.polygon==3)]<- 'Brooklyn' # Brooklyn
data2020$borough[which(data2020$nyc.polygon==4)]<- 'Queens' # Queens
data2020$borough[which(data2020$nyc.polygon==5)]<- 'Manhattan' # Manhattan

# check if the stations looks like they were binned proper
# they look correct
manhattan<- subset(data2020,borough=='Manhattan')
unique(manhattan$Station)

bronx<- subset(data2020,borough=='Bronx')
unique(bronx$Station)

brooklyn<- subset(data2020,borough=='Brooklyn')
unique(brooklyn$Station)

queens<- subset(data2020,borough=='Queens')
unique(queens$Station)

######################################
# loop through the dates and put the total weekly rides per borough in a data frame
######################################

for (i in 1:nrow(dates)) {
  # subset the ridership data by week
  nyc_long_lat3 <- nyc_long_lat2[nyc_long_lat2$From.Date==dates1[i],]
  nyc_long_lat4 <- nyc_long_lat3
  # coerces this to a SpatialPointsDataFrame
  coordinates(nyc_long_lat3)<-c("lon","lat")  
  # use the shp1 projection atributes 
  proj4string(nyc_long_lat3)<-proj4string(shp1)

  # figure out which borough polygon each lat/long is in
  polygon <- over(as(nyc_long_lat3,"SpatialPoints"), as(shp1,"SpatialPolygons"))
  # add the polygon to the weekly rideship data
  nyc_ll_poly <- cbind(nyc_long_lat4, polygon)
  nyc_ll_poly$polygon <- as.factor(nyc_ll_poly$polygon)

  # make a plot to check the points were categorized correctly to borough

  # summarize the total weekly ridership data by borough (i.e. polygon)
  a <- nyc_ll_poly[!is.na(nyc_ll_poly$polygon),] %>%
    group_by(polygon) %>%
    summarise(sum=sum(total,na.rm=TRUE))
  # total rides by borough for the current week
  a2 <- pull(a,sum)
  # put the total weekly rides in the matrix of weekly rides by borough
  matrix_nyc[i,1:5] <- a2
}
# add the dates to the matrix
nyc_matrix_final <- cbind(dates,matrix_nyc)
colnames(nyc_matrix_final)[1] <- "dates"

# Checkpoint added by Micaela
# check out the plots
par(mfrow=c(2,3))
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Bronx'])
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Staten Island'])
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Brooklyn'])
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Queens'])
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Manhattan'])

# Checkpoint added by Micaela
plot(nyc_matrix_final$dates,nyc_matrix_final[,'Manhattan'],type='l')
lines(nyc_matrix_final$dates,nyc_matrix_final[,'Bronx'])
lines(nyc_matrix_final$dates,nyc_matrix_final[,'Staten Island'])
lines(nyc_matrix_final$dates,nyc_matrix_final[,'Brooklyn'])
lines(nyc_matrix_final$dates,nyc_matrix_final[,'Queens'])

# rearrange the weekly ridership by borough in a dataframe
nyc_matrix_final2 <-reshape(nyc_matrix_final, 
        direction = "long",
        varying = list(names(nyc_matrix_final)[2:6]),
        v.names = "MTA",
        idvar = c("dates", "Manhattan"),
        timevar = "Borough",
        times = c("Bronx", "Staten Island", "Brooklyn", "Queens", "Manhattan"))

######################################
# check the pre-pandemic and pandemic mean weekly ridership for 4 boroughs (BK, Manhattan, BX, and Queens)
######################################
# subset the data to remove staten island and get the total for the 4 boroughs each week
all <- rowSums(nyc_matrix_final[,c(2,4,5,6)])
a <- mean(all[1:9]) # prepandemic mean rides for 4 boroughs
b <- mean(all[10:14]) # pandemic mean rides for 4 boroughs

######################################
# calculate the weekly subway ridership measured as standard deviations from the pre-pandemic mean
######################################
# prepandemic means and sd for each of 5 boroughs
means_b <- colMeans(nyc_matrix_final[1:9,2:ncol(nyc_matrix_final)])
sd_b <- apply(nyc_matrix_final[1:9,2:ncol(nyc_matrix_final)],2,sd)

# create a matrix that has the prepandemic mean for each borough repeated weekly 
means_b1 <- matrix(data=rep(means_b,nrow(nyc_matrix_final)),nrow=nrow(nyc_matrix_final),ncol=5, byrow=TRUE)
# create a matrix that has the prepandemic mean for each borough repeated weekly 
sd_b1 <- matrix(rep(sd_b, nrow(nyc_matrix_final)),nrow(nyc_matrix_final), 5, byrow=TRUE)
# the column order is Bronx, Staten Island, BK, Queens, Manhattan

# caululate the deviation in subway ridership from the prepandemic mean, standardized by the prepandemic standard deviation
nyc_change2 <- ((nyc_matrix_final[,-1])- means_b1)/sd_b1
# add the dates to the prepandemic standardized ridership matrix
nyc_change3 <- cbind.data.frame(nyc_matrix_final$dates,nyc_change2)
colnames(nyc_change3)[1] <- "dates"

# Checkpoint added by Micaela
# plot the standardized ridership, standardized by the prepandemic mean and sd
# I am not getting the same pattern as Reese showed in the supp material
par(plt=c(0.2,0.9,0.1,0.9))
plot(nyc_change3$dates,nyc_change3[,'Manhattan'],lwd=2,col='red',type='l',xlab='date',bty='n',ylab='weekly subway ridership\n(standard deviation from the pre-pandemic mean)',ylim=c(-30,5))
lines(nyc_change3$dates,nyc_change3[,'Bronx'],lwd=2,col='blue')
lines(nyc_change3$dates,nyc_change3[,'Staten Island'],lwd=2,col='black')
lines(nyc_change3$dates,nyc_change3[,'Brooklyn'],lwd=2,col='green')
lines(nyc_change3$dates,nyc_change3[,'Queens'],lwd=2,col='purple')
legend(nyc_change3$dates[2],-15,bty='n',legend=c('Manhattan','Bronx','Staten Island','Brooklyn','Queens'),col=c('red','blue','black','green','purple'),lwd=1)

# put the data in long form
nyc_change_long2 <-reshape(nyc_change3, 
                           direction = "long",
                           varying = list(names(nyc_change3)[2:ncol(nyc_change3)]),
                           v.names = "MTA",
                           idvar = "dates",
                           timevar = "Borough",
                           times = names(nyc_change3)[2:ncol(nyc_change3)])


nyc_matrix_borough <- nyc_matrix_final

# Checkpoint added by Micaela
pdf(file='subway_change_by_borough.pdf',height=6,width=6)
  # I am not getting the same pattern as Reese showed in the supp material
  index1<- which(nyc_change_long2$Borough=='Manhattan')
  plot(nyc_change_long2$dates[index1],nyc_change_long2$MTA[index1],type='l',col='red',ylim=c(-30,5),ylab='standardized ridership',xlab='date')
  index2<- which(nyc_change_long2$Borough=='Bronx')
  lines(nyc_change_long2$dates[index2],nyc_change_long2$MTA[index2],col='blue')
  index3<- which(nyc_change_long2$Borough=='Staten Island')
  lines(nyc_change_long2$dates[index3],nyc_change_long2$MTA[index3],col='black')
  index4<- which(nyc_change_long2$Borough=='Brooklyn')
  lines(nyc_change_long2$dates[index4],nyc_change_long2$MTA[index4],col='green')
  index5<- which(nyc_change_long2$Borough=='Queens')
  lines(nyc_change_long2$dates[index5],nyc_change_long2$MTA[index5],col='purple')
  legend(nyc_change_long2$dates[2],-15,bty='n',legend=c('Manhattan','Bronx','Staten Island','Brooklyn','Queens'),col=c('red','blue','black','green','purple'),lwd=1)
dev.off()

# clear the workstation with everything except the weekly ridership matrix and the data on the standardized change by borough
rm(list=setdiff(ls(), c("nyc_matrix_borough", "nyc_change_long2")))

# Savepoint added by Micaela
# write the important files to move to next step rather than keep them in the workspace
write.csv(nyc_matrix_borough,file='nyc_matrix_borough.csv',row.names=FALSE)
write.csv(nyc_change_long2,file='weekly_standardized_subway_use_by_borough.csv',row.names=FALSE)

# end file



