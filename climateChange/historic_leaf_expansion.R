##
##
## Leaf Phenology  condition 4 leaf expansion
##    Alex Young 7/5/2022

library(ggplot2) # graphing
library(tidyr) # dataframe manipulation
library(lubridate) # for handling date times
library(plotly)

# Package ID: knb-lter-hbr.51.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Routine Seasonal Phenology Measurements, 1989 - present.


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/51/12/9f623c83fb1da7595c6d2d498bde15df" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "DATE",     
                 "DAY",     
                 "SEASON",     
                 "SPECIES",     
                 "SITE",     
                 "Phenology_Stage"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$DAY)=="factor") dt1$DAY <-as.numeric(levels(dt1$DAY))[as.integer(dt1$DAY) ]               
if (class(dt1$DAY)=="character") dt1$DAY <-as.numeric(dt1$DAY)
if (class(dt1$SEASON)!="factor") dt1$SEASON<- as.factor(dt1$SEASON)
if (class(dt1$SPECIES)!="factor") dt1$SPECIES<- as.factor(dt1$SPECIES)
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$Phenology_Stage)=="factor") dt1$Phenology_Stage <-as.numeric(levels(dt1$Phenology_Stage))[as.integer(dt1$Phenology_Stage) ]               
if (class(dt1$Phenology_Stage)=="character") dt1$Phenology_Stage <-as.numeric(dt1$Phenology_Stage)


## 
head(dt1)

table(dt1$SITE)

table(dt1$SEASON)


hist(dt1$Phenology_Stage)
expand<-dt1[dt1$Phenology_Stage==4,]

# We have a data column, but its not formatted as a date
expand$DATE<-ymd(expand$DATE) # change how R interprets Date to be a date
expand$Year<-year(expand$DATE)
expand$Day<-day(expand$DATE)
head(expand,20)

# huh, DAY is not DOY, its day of water year

# add in water year
w.year <- as.numeric(format(expand$DATE, "%Y"))
june.july.sept <- as.numeric(format(expand$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
expand$wyear<-w.year # add water year as a column to precip dataset

head(expand,50)





expav<-aggregate(list(DAY=expand$DAY), by=list(SEASON=expand$SEASON, Year=expand$Year, SPECIES=expand$SPECIES, SITE=expand$SITE), FUN="mean", na.rm=T)

# We have a data column, but its not formatted as a date
air$DATE<-ymd(air$date) # change how R interprets Date to be a date
air$Year<-year(air$DATE)

expav$seasp<-paste( expav$SPECIES, expav$SEASON)
expav$seasi<-paste( expav$SPECIES, expav$SITE,  expav$SEASON)

head(expav)

# give top bottom code
expav[expav$SITE=="1B","Elevation"]<-"Bottom"
expav[expav$SITE=="4B","Elevation"]<-"Bottom"
expav[expav$SITE=="5B","Elevation"]<-"Bottom"
expav[expav$SITE=="7B","Elevation"]<-"Bottom"
expav[expav$SITE=="4T","Elevation"]<-"Top"
expav[expav$SITE=="5T","Elevation"]<-"Top"
expav[expav$SITE=="6T","Elevation"]<-"Top"
expav[expav$SITE=="7T","Elevation"]<-"Top"
expav[expav$SITE=="HQ","Elevation"]<-"HQ"

expav$Elevation<-factor(expav$Elevation, levels=c("Top","Bottom","HQ"))

table(expav$SPECIES)
expav[expav$SPECIES=="ACSA3", "Species"]<-"Sugar maple"
expav[expav$SPECIES=="BEAL2", "Species"]<-"Yellow birch"
expav[expav$SPECIES=="FAGR", "Species"]<-"American beech"


expav$SEASON<-factor(expav$SEASON, levels=c("SPRING","FALL"))


e1<-ggplot(expav, aes(x=Year, y=DAY, col=Elevation, group=seasp))+geom_point()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Date of year")+ labs(col='Site ID')+
  facet_wrap(Species~ SEASON, ncol=2, scales="free_y")+
  geom_smooth(method="lm")
e1
p1<-ggplotly(e1)
p1



av<-aggregate(list(DAY=expav$DAY), by=list(Year=expav$Year, SEASON=expav$SEASON, Species=expav$Species), FUN="mean")
st.err <- function(x) {  sd(x)/sqrt(length(x))}
se<-aggregate(list(DAYse=expav$DAY), by=list(Year=expav$Year, SEASON=expav$SEASON, Species=expav$Species), function(x) sd(x))
head(se)
av$SE<-se$DAYse


head(av)

av$seasp<-paste( av$SPECIES, expav$SEASON)
av$Year<-as.integer(av$Year)
av$SEASON<-factor(av$SEASON, levels=c("SPRING","FALL"))



#




ggplot(av, aes(Year))+
  geom_ribbon(aes( ymin = DAY-SE, ymax = DAY + SE), fill = "grey70") +
  geom_line(aes(x=Year, y = DAY))+facet_wrap(Species~SEASON, ncol=2, scales="free_y")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(expav, mapping=aes(x=Year, y=DAY, col=SITE))+
  

str(av)

av$Elevation<-"average"
av$SITE<-"average"

head(expav)
## try 3!
ggplot(expav, aes(x=Year, y=DAY, col=SITE, group=seasp))+
  geom_smooth(method="lm", se=F)+
  geom_ribbon(data=av, mapping=aes(x=Year, ymin = DAY-SE, ymax = DAY + SE), fill = "grey70") +
  geom_line(data=av, mapping=aes(x=Year, y = DAY))+
  geom_point()+
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Date of year")+ labs(col='Site ID')+
  facet_wrap(Species~ SEASON, ncol=2, scales="free_y")+
scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                            "purple","black","red"))
  

  
  
  
## sO i have a ssy, season, species, year average...

   htmlwidgets::saveWidget(as_widget(p1), "climateChange/Leaf_expansion_R.html")
