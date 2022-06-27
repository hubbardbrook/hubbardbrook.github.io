


#  Alex Y go at HW chapter for HB online book
# June 27, 2022  
  
  
####### HB online book HW chapter 
  ####################################################

library(lubridate) # for handling date-times
library(ggplot2) # for visualizing
library(tidyr) # for data formatting
library(dplyr) # for data formatting
library(plotly)# for making interactive graphs
  
####################################################     ##
# Chapter 2 Hydrology                           ##########
####################################################     ##


## Chapter 2.1
####################################################

# read in streamflow data

# Package ID: knb-lter-hbr.2.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Daily Streamflow by Watershed, 1956 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/2/11/1254d17cbd381556c05afa740d380e78" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "DATE",     
                 "WS",     
                 "Streamflow",     
                 "Flag"    ), check.names=TRUE)
unlink(infile1)

# view the dataset, 182,000 rows
str(dt1)

# Notice that WS is numeric, and we want the watersheds to be factors
dt1$WS<-as.factor(dt1$WS)

# Provide columns interpreted as a Date by R
dt1$DATE<-ymd(dt1$DATE) # change how R interprets Date to be a date
dt1$Year<-year(dt1$DATE)   #add a column for 'year' from date
dt1$doy<-yday(dt1$DATE) # add day of year
head(dt1)

# Quick summary of the dataset
table( dt1$WS, dt1$Year) # view how many Watersheds, and # of observations
num_days_WS<-as.data.frame(table(dt1$WS, dt1$Year))
ggplot(num_days_WS, aes(x=Var1, y=Freq, fill=Var2))+
  geom_bar(stat="identity", col="black")+theme_classic()


# add in water year
w.year <- as.numeric(format(dt1$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt1$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1

dt1$wyear<-w.year
head(dt1)

# calculate the annual sum of streamflow measurements
annstr<-aggregate(list(Streamflow=dt1$Streamflow), by=list(WS=dt1$WS, wyear=dt1$wyear), FUN="sum")

# this makes it nicer for working with other HB datasets
annstr$WS<-sub("^","W",annstr$WS)


#  Data currently go to 22
g1<-ggplot(annstr, aes(x=wyear, y=Streamflow, col=WS))+
  geom_point()+geom_line()+
  ylab("Steamflow (mm)")+xlab("Water year (June 1)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g1


## Chapter 2.2
####################################################

# read in preciptation data

# Package ID: knb-lter-hbr.14.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Total Daily Precipitation by Watershed, 1956 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/14/14/c606bfe2f2deb3fa3eabf692ae15f02d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "DATE",     
                 "watershed",     
                 "Precip"    ), check.names=TRUE)

unlink(infile1)

# conduct data formatting
str(dt2)

# We have a data column, but its not formatted as a date
dt2$DATE<-ymd(dt2$DATE) # change how R interprets Date to be a date
dt2$Year<-year(dt2$DATE)

# add in water year
w.year <- as.numeric(format(dt2$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt2$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1

dt2$wyear<-w.year # add water year as a column to precip dataset
head(dt2)

# get annual sums
annpre<-aggregate(list(Precip=dt2$Precip), by=list(WS=dt2$watershed, wyear=dt2$wyear), FUN="sum")
head(annpre) 

g2<-ggplot(annpre, aes(x=wyear, y=Precip, col=WS))+
  geom_point()+geom_line()+
  ylab("Precip (mm)")+xlab("Water year (June 1)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g2


## Chapter 2.3
####################################################

# calculate Actual EvapoTranspiration (AET) #  Precip - Streamflow = AET

### give the watersheds and years a unique ID. watershed-year, wsy
annstr$wsy<-paste(annstr$WS, annstr$wyear)
annpre$wsy<-paste(annpre$WS, annpre$wyear)

# bring in precip data to streamflow object
annstr$Precip<-annpre$Precip[match(annstr$wsy, annpre$wsy)]

# calculate AET, assuming no change in storage
annstr$AET<-annstr$Precip - annstr$Streamflow


g3<-ggplot(annstr, aes(x=wyear, y=AET, col=WS))+
  geom_point()+geom_line()+
  ylab("AET (mm)")+xlab("Water year (June 1st)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g3


library(plotly)
p1<-ggplotly(g1)
p2<-ggplotly(g2)
p3<-ggplotly(g3)

plotfinal<-subplot(p1, p2, p3, nrows=3,
                   shareX = T, shareY = T)
plotfinal

htmlwidgets::saveWidget(as_widget(plotfinal), "hwChapter/hydrology_2_precip_stream_AET.html")



## Chapter 2.4
####################################################

# # read in air temperature data,  compare AET with Average annual air temp

# Package ID: knb-lter-hbr.59.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Daily Temperature Record, 1955 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/59/10/9723086870f14b48409869f6c06d6aa8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt3 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "date",     
                 "STA",     
                 "MAX",     
                 "MIN",     
                 "AVE",     
                 "Flag"    ), check.names=TRUE)

unlink(infile1)

head(dt3)


# We have a data column, but its not formatted as a date
dt3$DATE<-ymd(dt3$date) # change how R interprets Date to be a date
dt3$Year<-year(dt3$DATE)

# add in water year
w.year <- as.numeric(format(dt3$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt3$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1

dt3$wyear<-w.year # add water year as a column to precip dataset

dt3$month<-month(dt3$date)
head(dt3)

# get annual averages of air

growseas<-dt3[dt3$month >="6" & dt3$month <=9 ,]
annairgrow<-aggregate(list(avtemp=growseas$AVE), by=list( wyear=growseas$wyear), FUN="mean")
head(annairgrow) 




#visualize to become familiar with data
ggplot(annairgrow, aes(x=wyear, y=avtemp))+
  geom_point()+geom_line()+
  ylab("Growing season air temperature (C)")+xlab("Water year (June 1st)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# view trend from 1995-2010 of growing season temp and AET

### bring in av temp to annstr
annstr$avtemp<-annairgrow$avtemp[match(annstr$wyear, annairgrow$wyear)]

#subset to 1995-2010
temp_aet<-annstr[annstr$wyear >= "1995" & annstr$wyear <="2010" ,]


ch2<-ggplot(temp_aet, aes(x=avtemp, y=AET, col=WS))+geom_point()+
  geom_smooth(method="lm", se=F)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Average growing season temperature (C) June 1 - Sep 30")+
  ylab("Actual Evapotranspiration")


pch2<-ggplotly(ch2)
pch2
htmlwidgets::saveWidget(as_widget(pch2), "hwChapter/hydrology_2_temp_aet.html")



## Chapter 2.5
####################################################

# Forest cutting to increase water supply?

# WS5 clearcut in whole tree harvest 1984-1985.
head(annstr)

# separate out WS5
WS5<-annstr[annstr$WS=="W5",]  
  
# subset years for periof of time in question
harvest<-WS5[WS5$wyear >= "1980" & WS5$wyear <="1990" ,]

# Use precip and stream discharge to evaluate effect on water yield and AET for 1983 (pre) - 1986 (post clear cut)

harvest$wyear<-as.integer(harvest$wyear)

gharv<-ggplot(harvest, aes(x=wyear, y=AET))+geom_point()+
  geom_vline(xintercept=1983, linetype="dashed", col="red")+
  geom_vline(xintercept=1985, linetype="solid", col="red")+
  geom_line()+scale_x_continuous(limits = c(1980, 1990), breaks = seq(1980, 1990, 1))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Actual Evapotranspiration")

pharv<-ggplotly(gharv)
pharv

htmlwidgets::saveWidget(as_widget(pharv), "hwChapter/hydrology_2_WS2_harvest.html")


####################################################     ##
  # Chapter 3 Streamwater nutrient fluxes         ##########
####################################################     ##


## Chapter 3.1
####################################################

# read in Chemistry of streamwater data

# conduct data formatting

# perform unit calibrations



## Chapter 3.2
####################################################

# Sum the 12 months
##  watch units!  mg/L, convert to kg/ha/year/

# has WS6 declined 

WS2 view for increased leaching Ca 1965 onward.


## Chapter 3.3 
####################################################