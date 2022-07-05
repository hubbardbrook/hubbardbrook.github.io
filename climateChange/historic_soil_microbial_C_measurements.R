##
##
## Microbial Biomass C
##    Alex Young 7/5/2022

library(ggplot2) # graphing
library(tidyr) # dataframe manipulation
library(lubridate) # for handling date times
library(plotly)


# Package ID: knb-lter-hbr.67.23 Cataloging System:https://pasta.edirepository.org.
# Data set title: Long-term measurements of microbial biomass and activity at the Hubbard Brook Experimental Forest 1994 â present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/67/23/e37c133ebf3f4ac6c1f8c2f7e612ea86" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Project",     
                 "Date",     
                 "Year",     
                 "Se",     
                 "Treatment",     
                 "El",     
                 "Plot",     
                 "Hor",     
                 "BIOC",     
                 "RESPC",     
                 "BION",     
                 "NO3",     
                 "NH4",     
                 "NIT",     
                 "MIN",     
                 "DEA",     
                 "H2O",     
                 "pH"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Project)!="factor") dt1$Project<- as.factor(dt1$Project)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Se)!="factor") dt1$Se<- as.factor(dt1$Se)
if (class(dt1$Treatment)!="factor") dt1$Treatment<- as.factor(dt1$Treatment)
if (class(dt1$El)!="factor") dt1$El<- as.factor(dt1$El)
if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Hor)!="factor") dt1$Hor<- as.factor(dt1$Hor)
if (class(dt1$BIOC)=="factor") dt1$BIOC <-as.numeric(levels(dt1$BIOC))[as.integer(dt1$BIOC) ]               
if (class(dt1$BIOC)=="character") dt1$BIOC <-as.numeric(dt1$BIOC)

# Convert Missing Values to NA for non-dates
dt1$BIOC <- ifelse((trimws(as.character(dt1$BIOC))==trimws("-9999.99")),NA,dt1$BIOC)               
suppressWarnings(dt1$BIOC <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$BIOC))==as.character(as.numeric("-9999.99"))),NA,dt1$BIOC))

head(dt1)


## 
# Package ID: knb-lter-hbr.252.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Measurements of microbial biomass and activity in two snow manipulation experiments at Hubbard Brook Experimental Forest 1998 â 2004.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/252/1/feb35c5409291d7f2d4ebfb1a16241f4" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Project",     
                 "Date",     
                 "Year",     
                 "Season",     
                 "Site",     
                 "Plot",     
                 "Treatment",     
                 "Horizon",     
                 "Lab_Rep",     
                 "BIOC",     
                 "RESPC",     
                 "BION",     
                 "NO3",     
                 "NH4",     
                 "NIT",     
                 "MIN",     
                 "DEA",     
                 "H2O",     
                 "pH"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Project)!="factor") dt2$Project<- as.factor(dt2$Project)                                   
# attempting to convert dt2$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt2$Date,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt2$Date <- tmp1Date } else {print("Date conversion failed for dt2$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt2$Season)!="factor") dt2$Season<- as.factor(dt2$Season)
if (class(dt2$Site)!="factor") dt2$Site<- as.factor(dt2$Site)
if (class(dt2$Plot)=="factor") dt2$Plot <-as.numeric(levels(dt2$Plot))[as.integer(dt2$Plot) ]               
if (class(dt2$Plot)=="character") dt2$Plot <-as.numeric(dt2$Plot)
if (class(dt2$Treatment)!="factor") dt2$Treatment<- as.factor(dt2$Treatment)
if (class(dt2$Horizon)!="factor") dt2$Horizon<- as.factor(dt2$Horizon)
if (class(dt2$Lab_Rep)!="factor") dt2$Lab_Rep<- as.factor(dt2$Lab_Rep)
if (class(dt2$BIOC)=="factor") dt2$BIOC <-as.numeric(levels(dt2$BIOC))[as.integer(dt2$BIOC) ]               
if (class(dt2$BIOC)=="character") dt2$BIOC <-as.numeric(dt2$BIOC)

# Convert Missing Values to NA for non-dates
dt2$Plot <- ifelse((trimws(as.character(dt2$Plot))==trimws("NA")),NA,dt2$Plot)               
suppressWarnings(dt2$Plot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Plot))==as.character(as.numeric("NA"))),NA,dt2$Plot))
dt2$BIOC <- ifelse((trimws(as.character(dt2$BIOC))==trimws("-9999")),NA,dt2$BIOC)               
suppressWarnings(dt2$BIOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt2$BIOC))==as.character(as.numeric("-9999"))),NA,dt2$BIOC))

#####

head(dt2)
head(dt1)

table(dt1$Project)
table(dt2$Project)

names(dt1)
names(dt2)

table(dt1$Treatment)
dt1$doy<-yday(dt1$Date)

table(dt1$Hor)

dt1[dt1$El=="U", "Elevation"]<-"Upper"
dt1[dt1$El=="SF", "Elevation"]<-"Spruce Fir"
dt1[dt1$El=="H", "Elevation"]<-"High"
dt1[dt1$El=="M", "Elevation"]<-"Mid"
dt1[dt1$El=="L", "Elevation"]<-"Low"

table(dt1$Elevation)


dt1$Elevation<-factor(dt1$Elevation, levels=c("Low","Mid","High","Spruce Fir","Upper"))

dt1$Hor<-factor(dt1$Hor, levels=c("Oi/Oe","Oa/A", "Min"))

table(dt1$Hor)

microC<-ggplot(dt1, aes(x=Year, y=BIOC, col=doy,group=Plot))+
  geom_point()+facet_grid(Hor ~ Elevation, scales="free_y")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(col='Day of year')+
  ylab("Microbial Biomass C (mg/kg)")+
  ggtitle("Sampling locations by elevation and soil horizon for measurements of microbial biomass C")

picroC<-ggplotly(microC)

picroC



htmlwidgets::saveWidget(as_widget(picroC), "climateChange/microbial_biomass_C.html")
