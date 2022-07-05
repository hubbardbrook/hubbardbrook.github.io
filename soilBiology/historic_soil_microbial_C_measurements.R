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
if (class(dt1$RESPC)=="factor") dt1$RESPC <-as.numeric(levels(dt1$RESPC))[as.integer(dt1$RESPC) ]               
if (class(dt1$RESPC)=="character") dt1$RESPC <-as.numeric(dt1$RESPC)
if (class(dt1$BION)=="factor") dt1$BION <-as.numeric(levels(dt1$BION))[as.integer(dt1$BION) ]               
if (class(dt1$BION)=="character") dt1$BION <-as.numeric(dt1$BION)
if (class(dt1$NO3)=="factor") dt1$NO3 <-as.numeric(levels(dt1$NO3))[as.integer(dt1$NO3) ]               
if (class(dt1$NO3)=="character") dt1$NO3 <-as.numeric(dt1$NO3)
if (class(dt1$NH4)=="factor") dt1$NH4 <-as.numeric(levels(dt1$NH4))[as.integer(dt1$NH4) ]               
if (class(dt1$NH4)=="character") dt1$NH4 <-as.numeric(dt1$NH4)
if (class(dt1$NIT)=="factor") dt1$NIT <-as.numeric(levels(dt1$NIT))[as.integer(dt1$NIT) ]               
if (class(dt1$NIT)=="character") dt1$NIT <-as.numeric(dt1$NIT)
if (class(dt1$MIN)=="factor") dt1$MIN <-as.numeric(levels(dt1$MIN))[as.integer(dt1$MIN) ]               
if (class(dt1$MIN)=="character") dt1$MIN <-as.numeric(dt1$MIN)
if (class(dt1$DEA)=="factor") dt1$DEA <-as.numeric(levels(dt1$DEA))[as.integer(dt1$DEA) ]               
if (class(dt1$DEA)=="character") dt1$DEA <-as.numeric(dt1$DEA)
if (class(dt1$H2O)=="factor") dt1$H2O <-as.numeric(levels(dt1$H2O))[as.integer(dt1$H2O) ]               
if (class(dt1$H2O)=="character") dt1$H2O <-as.numeric(dt1$H2O)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)

# Convert Missing Values to NA for non-dates

dt1$BIOC <- ifelse((trimws(as.character(dt1$BIOC))==trimws("-9999.99")),NA,dt1$BIOC)               
suppressWarnings(dt1$BIOC <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$BIOC))==as.character(as.numeric("-9999.99"))),NA,dt1$BIOC))
dt1$RESPC <- ifelse((trimws(as.character(dt1$RESPC))==trimws("-9999.99")),NA,dt1$RESPC)               
suppressWarnings(dt1$RESPC <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$RESPC))==as.character(as.numeric("-9999.99"))),NA,dt1$RESPC))
dt1$BION <- ifelse((trimws(as.character(dt1$BION))==trimws("-9999.99")),NA,dt1$BION)               
suppressWarnings(dt1$BION <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$BION))==as.character(as.numeric("-9999.99"))),NA,dt1$BION))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("-9999.99")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("-9999.99"))),NA,dt1$NO3))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("-9999.99")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("-9999.99"))),NA,dt1$NH4))
dt1$NIT <- ifelse((trimws(as.character(dt1$NIT))==trimws("-9999.99")),NA,dt1$NIT)               
suppressWarnings(dt1$NIT <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NIT))==as.character(as.numeric("-9999.99"))),NA,dt1$NIT))
dt1$MIN <- ifelse((trimws(as.character(dt1$MIN))==trimws("-9999.99")),NA,dt1$MIN)               
suppressWarnings(dt1$MIN <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$MIN))==as.character(as.numeric("-9999.99"))),NA,dt1$MIN))
dt1$DEA <- ifelse((trimws(as.character(dt1$DEA))==trimws("-9999.99")),NA,dt1$DEA)               
suppressWarnings(dt1$DEA <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$DEA))==as.character(as.numeric("-9999.99"))),NA,dt1$DEA))
dt1$H2O <- ifelse((trimws(as.character(dt1$H2O))==trimws("-9999.99")),NA,dt1$H2O)               
suppressWarnings(dt1$H2O <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$H2O))==as.character(as.numeric("-9999.99"))),NA,dt1$H2O))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("-9999.99")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("-9999.99"))),NA,dt1$pH))

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

table(dt1$s)
dt1[dt1$Se=="F", "Season"]<-"Fall"
dt1[dt1$Se=="W", "Season"]<-"Winter"
dt1[dt1$Se=="SP", "Season"]<-"Spring"
dt1[dt1$Se=="SU", "Season"]<-"Summer"
table(dt1$Season)





# now factor in the right order
dt1$Elevation<-factor(dt1$Elevation, levels=c("Low","Mid","High","Spruce Fir","Upper"))

dt1$Hor<-factor(dt1$Hor, levels=c("Oi/Oe","Oa/A", "Min"))



table(dt1$Hor)

microC<-ggplot(dt1, aes(x=Year, y=BIOC, col=doy,group=Plot))+
  geom_jitter()+facet_grid(Hor ~ Elevation, scales="free_y")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(col='Day of year')+
  ylab("Microbial Biomass C (mg/kg)")+
  ggtitle("Sampling locations by elevation and soil horizon for measurements of microbial biomass C")

picroC<-ggplotly(microC)

picroC



htmlwidgets::saveWidget(as_widget(picroC), "climateChange/microbial_biomass_C.html")
head(dt1)

table(dt1$Se)




## now Nmin.  There are soem high values. 
# microN<-ggplot(dt1, aes(x=Year, y=MIN, col=doy,group=Plot))+
#   geom_point()+facet_grid(Hor ~ Elevation, scales="free_y")+
#   theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   labs(col='Day of year')+
#   ylab("N mineralization (mg/kg/day)")+
#   ggtitle("Sampling locations by elevation and soil horizon for measurements of N mineralization)")
# microN
# 
# 
# picroN<-ggplotly(microN)
# 
# picroN


####  Average to the year level
nminav<-aggregate(list(Nmin=dt1$MIN), by=list(Year=dt1$Year, Hor=dt1$Hor), FUN="mean", na.rm=T)
head(nminav)



microN<-ggplot(nminav, aes(x=Year, y=Nmin, col=Hor))+
  geom_point()+geom_line()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(col='Day of year')+
  scale_color_manual(values=c("deepskyblue2","orange","grey"))+
  ylab("N mineralization (mg/kg/day)")+
  ggtitle("Microbial biomass N in the Oie, Oa, and mineral soil horizons in the Bear Brook watershed (west of the reference watershed 6)")
microN


htmlwidgets::saveWidget(as_widget(picroN), "climateChange/N_mineralization.html")





