


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

# add in 'water year station'. 
dt1$wys<-paste(dt1$wyear, dt1$WS)

head(dt1)

# make sure you are only using complete years for the record
str.obs<-as.data.frame(table(dt1$WS, dt1$wyear))
str.obs$wys<- paste(str.obs$Var2, str.obs$Var1)
str.obs[str.obs$Freq<350, "Use"]<-"incomplete wyear" # complete is 350 or more days
str.obs[is.na(str.obs$Use),"Use"]<-"complete"
head(str.obs, 50)


dt1$Use<-str.obs$Use[match(dt1$wys, str.obs$wys)]

dt1.complete<-dt1[dt1$Use=="complete",]

dt1.complete
# calculate the annual sum of streamflow measurements
annstr<-aggregate(list(Streamflow=dt1.complete$Streamflow), by=list(WS=dt1.complete$WS, wyear=dt1.complete$wyear), FUN="sum")

# this makes it nicer for working with other HB datasets
annstr$WS<-sub("^","W",annstr$WS)
head(annstr)




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

# add in water year-station
dt2$wys<-paste(dt2$wyear, dt2$watershed)
head(dt2)


# make sure you are only using complete years for the record
pre.obs<-as.data.frame(table(dt2$watershed , dt2$wyear))
pre.obs$wys<- paste(pre.obs$Var2, pre.obs$Var1)
pre.obs[pre.obs$Freq<350, "Use"]<-"incomplete wyear" # complete is 350 or more days
pre.obs[is.na(pre.obs$Use),"Use"]<-"complete"
head(pre.obs, 50)


dt2$Use<-pre.obs$Use[match(dt2$wys, pre.obs$wys)]

dt2.complete<-dt2[dt2$Use=="complete",]


# get annual sums
annpre<-aggregate(list(Precip=dt2.complete$Precip), by=list(WS=dt2.complete$watershed, wyear=dt2.complete$wyear), FUN="sum")
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


## create gg plotly objects  
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

# add in water year-station
dt3$wys<-paste(dt3$wyear, dt3$STA)
head(dt3)


# make sure you are only using complete years for the record
pre.obs<-as.data.frame(table(dt3$STA , dt3$wyear))
pre.obs$wys<- paste(pre.obs$Var2, pre.obs$Var1)
pre.obs[pre.obs$Freq<350, "Use"]<-"incomplete wyear" # complete is 350 or more days
pre.obs[is.na(pre.obs$Use),"Use"]<-"complete"
head(pre.obs, 50)


dt3$Use<-pre.obs$Use[match(dt3$wys, pre.obs$wys)]

dt3.complete<-dt3[dt3$Use=="complete",]


head(dt3)

# get annual averages of air

growseas<-dt3.complete[dt3.complete$month >="6" & dt3.complete$month <=9 ,]
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
  head(WS5)
# subset years for periof of time in question
harvest<-WS5[WS5$wyear >= "1970" & WS5$wyear <="1994" ,]
head(harvest)

# Use precip and stream discharge to evaluate effect on water yield and AET for 1983 (pre) - 1986 (post clear cut)

harvest$wyear<-as.integer(harvest$wyear)

gharv<-ggplot(harvest, aes(x=wyear, y=AET))+geom_point()+
  geom_vline(xintercept=1983, linetype="dashed", col="red")+
  geom_vline(xintercept=1985, linetype="solid", col="red")+
  geom_line()+scale_x_continuous(limits = c(1970, 1994), breaks = seq(1970, 1994, 5))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Actual Evapotranspiration")+
  ggtitle("Hubbard Brook Watershed 5: Whole tree harvest 1983-1985")

gharv

# # a different look at the relationship
# ggplot(harvest, aes(x=Streamflow, y=Precip, col=wyear))+geom_point()+
#   theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Streamflow (mm)")+
#   ylab("Precipitation (mm)")+
#   xlim(0,2000)+ylim(0,2000)

pharv<-ggplotly(gharv)
pharv

htmlwidgets::saveWidget(as_widget(pharv), "hwChapter/hydrology_2_WS5_harvest.html")


####################################################     ##
  # Chapter 3 Stream water nutrient fluxes        ##########
####################################################     ##


## Chapter 3.1
####################################################

# read in monthly fluxes for Chemistry of streamwater data for WS6

# Package ID: knb-lter-hbr.20.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Precipitation â Monthly Fluxes, Watershed 6, 1964 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/20/11/76b46d5bd60e4912406726ec71610d0a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
dt4 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Year_Month",     
                 "precip_mm",     
                 "Ca_flux",     
                 "Mg_flux",     
                 "K_flux",     
                 "Na_flux",     
                 "Al_Ferron_flux",     
                 "TMAl_flux",     
                 "OMAl_flux",     
                 "Al_ICP_flux",     
                 "NH4_flux",     
                 "SO4_flux",     
                 "NO3_flux",     
                 "Cl_flux",     
                 "PO4_flux",     
                 "DOC_flux",     
                 "TDN_flux",     
                 "DON_flux",     
                 "SiO2_flux",     
                 "Mn_flux",     
                 "Fe_flux",     
                 "F_flux",     
                 "H_flux",     
                 "pH_volwt",     
                 "SpecCond_volwt"    ), check.names=TRUE)
unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Month)!="factor") dt4$Month<- as.factor(dt4$Month)
if (class(dt4$Year_Month)!="factor") dt4$Year_Month<- as.factor(dt4$Year_Month)
if (class(dt4$precip_mm)=="factor") dt4$precip_mm <-as.numeric(levels(dt4$precip_mm))[as.integer(dt4$precip_mm) ]               
if (class(dt4$precip_mm)=="character") dt4$precip_mm <-as.numeric(dt4$precip_mm)
if (class(dt4$Ca_flux)=="factor") dt4$Ca_flux <-as.numeric(levels(dt4$Ca_flux))[as.integer(dt4$Ca_flux) ]               
if (class(dt4$Ca_flux)=="character") dt4$Ca_flux <-as.numeric(dt4$Ca_flux)
if (class(dt4$Mg_flux)=="factor") dt4$Mg_flux <-as.numeric(levels(dt4$Mg_flux))[as.integer(dt4$Mg_flux) ]               
if (class(dt4$Mg_flux)=="character") dt4$Mg_flux <-as.numeric(dt4$Mg_flux)
if (class(dt4$K_flux)=="factor") dt4$K_flux <-as.numeric(levels(dt4$K_flux))[as.integer(dt4$K_flux) ]               
if (class(dt4$K_flux)=="character") dt4$K_flux <-as.numeric(dt4$K_flux)
if (class(dt4$Na_flux)=="factor") dt4$Na_flux <-as.numeric(levels(dt4$Na_flux))[as.integer(dt4$Na_flux) ]               
if (class(dt4$Na_flux)=="character") dt4$Na_flux <-as.numeric(dt4$Na_flux)
if (class(dt4$Al_Ferron_flux)=="factor") dt4$Al_Ferron_flux <-as.numeric(levels(dt4$Al_Ferron_flux))[as.integer(dt4$Al_Ferron_flux) ]               
if (class(dt4$Al_Ferron_flux)=="character") dt4$Al_Ferron_flux <-as.numeric(dt4$Al_Ferron_flux)
if (class(dt4$TMAl_flux)=="factor") dt4$TMAl_flux <-as.numeric(levels(dt4$TMAl_flux))[as.integer(dt4$TMAl_flux) ]               
if (class(dt4$TMAl_flux)=="character") dt4$TMAl_flux <-as.numeric(dt4$TMAl_flux)
if (class(dt4$OMAl_flux)=="factor") dt4$OMAl_flux <-as.numeric(levels(dt4$OMAl_flux))[as.integer(dt4$OMAl_flux) ]               
if (class(dt4$OMAl_flux)=="character") dt4$OMAl_flux <-as.numeric(dt4$OMAl_flux)
if (class(dt4$Al_ICP_flux)=="factor") dt4$Al_ICP_flux <-as.numeric(levels(dt4$Al_ICP_flux))[as.integer(dt4$Al_ICP_flux) ]               
if (class(dt4$Al_ICP_flux)=="character") dt4$Al_ICP_flux <-as.numeric(dt4$Al_ICP_flux)
if (class(dt4$NH4_flux)=="factor") dt4$NH4_flux <-as.numeric(levels(dt4$NH4_flux))[as.integer(dt4$NH4_flux) ]               
if (class(dt4$NH4_flux)=="character") dt4$NH4_flux <-as.numeric(dt4$NH4_flux)
if (class(dt4$SO4_flux)=="factor") dt4$SO4_flux <-as.numeric(levels(dt4$SO4_flux))[as.integer(dt4$SO4_flux) ]               
if (class(dt4$SO4_flux)=="character") dt4$SO4_flux <-as.numeric(dt4$SO4_flux)
if (class(dt4$NO3_flux)=="factor") dt4$NO3_flux <-as.numeric(levels(dt4$NO3_flux))[as.integer(dt4$NO3_flux) ]               
if (class(dt4$NO3_flux)=="character") dt4$NO3_flux <-as.numeric(dt4$NO3_flux)
if (class(dt4$Cl_flux)=="factor") dt4$Cl_flux <-as.numeric(levels(dt4$Cl_flux))[as.integer(dt4$Cl_flux) ]               
if (class(dt4$Cl_flux)=="character") dt4$Cl_flux <-as.numeric(dt4$Cl_flux)
if (class(dt4$PO4_flux)=="factor") dt4$PO4_flux <-as.numeric(levels(dt4$PO4_flux))[as.integer(dt4$PO4_flux) ]               
if (class(dt4$PO4_flux)=="character") dt4$PO4_flux <-as.numeric(dt4$PO4_flux)
if (class(dt4$DOC_flux)=="factor") dt4$DOC_flux <-as.numeric(levels(dt4$DOC_flux))[as.integer(dt4$DOC_flux) ]               
if (class(dt4$DOC_flux)=="character") dt4$DOC_flux <-as.numeric(dt4$DOC_flux)
if (class(dt4$TDN_flux)=="factor") dt4$TDN_flux <-as.numeric(levels(dt4$TDN_flux))[as.integer(dt4$TDN_flux) ]               
if (class(dt4$TDN_flux)=="character") dt4$TDN_flux <-as.numeric(dt4$TDN_flux)
if (class(dt4$DON_flux)=="factor") dt4$DON_flux <-as.numeric(levels(dt4$DON_flux))[as.integer(dt4$DON_flux) ]               
if (class(dt4$DON_flux)=="character") dt4$DON_flux <-as.numeric(dt4$DON_flux)
if (class(dt4$SiO2_flux)=="factor") dt4$SiO2_flux <-as.numeric(levels(dt4$SiO2_flux))[as.integer(dt4$SiO2_flux) ]               
if (class(dt4$SiO2_flux)=="character") dt4$SiO2_flux <-as.numeric(dt4$SiO2_flux)
if (class(dt4$Mn_flux)=="factor") dt4$Mn_flux <-as.numeric(levels(dt4$Mn_flux))[as.integer(dt4$Mn_flux) ]               
if (class(dt4$Mn_flux)=="character") dt4$Mn_flux <-as.numeric(dt4$Mn_flux)
if (class(dt4$Fe_flux)=="factor") dt4$Fe_flux <-as.numeric(levels(dt4$Fe_flux))[as.integer(dt4$Fe_flux) ]               
if (class(dt4$Fe_flux)=="character") dt4$Fe_flux <-as.numeric(dt4$Fe_flux)
if (class(dt4$F_flux)=="factor") dt4$F_flux <-as.numeric(levels(dt4$F_flux))[as.integer(dt4$F_flux) ]               
if (class(dt4$F_flux)=="character") dt4$F_flux <-as.numeric(dt4$F_flux)
if (class(dt4$H_flux)=="factor") dt4$H_flux <-as.numeric(levels(dt4$H_flux))[as.integer(dt4$H_flux) ]               
if (class(dt4$H_flux)=="character") dt4$H_flux <-as.numeric(dt4$H_flux)
if (class(dt4$pH_volwt)=="factor") dt4$pH_volwt <-as.numeric(levels(dt4$pH_volwt))[as.integer(dt4$pH_volwt) ]               
if (class(dt4$pH_volwt)=="character") dt4$pH_volwt <-as.numeric(dt4$pH_volwt)
if (class(dt4$SpecCond_volwt)=="factor") dt4$SpecCond_volwt <-as.numeric(levels(dt4$SpecCond_volwt))[as.integer(dt4$SpecCond_volwt) ]               
if (class(dt4$SpecCond_volwt)=="character") dt4$SpecCond_volwt <-as.numeric(dt4$SpecCond_volwt)

# Convert Missing Values to NA for non-dates

dt4$precip_mm <- ifelse((trimws(as.character(dt4$precip_mm))==trimws("-888.88")),NA,dt4$precip_mm)               
suppressWarnings(dt4$precip_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$precip_mm))==as.character(as.numeric("-888.88"))),NA,dt4$precip_mm))
dt4$Ca_flux <- ifelse((trimws(as.character(dt4$Ca_flux))==trimws("-888.88")),NA,dt4$Ca_flux)               
suppressWarnings(dt4$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Ca_flux))
dt4$Mg_flux <- ifelse((trimws(as.character(dt4$Mg_flux))==trimws("-888.88")),NA,dt4$Mg_flux)               
suppressWarnings(dt4$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Mg_flux))
dt4$K_flux <- ifelse((trimws(as.character(dt4$K_flux))==trimws("-888.88")),NA,dt4$K_flux)               
suppressWarnings(dt4$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$K_flux))==as.character(as.numeric("-888.88"))),NA,dt4$K_flux))
dt4$Na_flux <- ifelse((trimws(as.character(dt4$Na_flux))==trimws("-888.88")),NA,dt4$Na_flux)               
suppressWarnings(dt4$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Na_flux))
dt4$Al_Ferron_flux <- ifelse((trimws(as.character(dt4$Al_Ferron_flux))==trimws("-888.88")),NA,dt4$Al_Ferron_flux)               
suppressWarnings(dt4$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Al_Ferron_flux))
dt4$TMAl_flux <- ifelse((trimws(as.character(dt4$TMAl_flux))==trimws("-888.88")),NA,dt4$TMAl_flux)               
suppressWarnings(dt4$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt4$TMAl_flux))
dt4$OMAl_flux <- ifelse((trimws(as.character(dt4$OMAl_flux))==trimws("-888.88")),NA,dt4$OMAl_flux)               
suppressWarnings(dt4$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt4$OMAl_flux))
dt4$Al_ICP_flux <- ifelse((trimws(as.character(dt4$Al_ICP_flux))==trimws("-888.88")),NA,dt4$Al_ICP_flux)               
suppressWarnings(dt4$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Al_ICP_flux))
dt4$NH4_flux <- ifelse((trimws(as.character(dt4$NH4_flux))==trimws("-888.88")),NA,dt4$NH4_flux)               
suppressWarnings(dt4$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt4$NH4_flux))
dt4$SO4_flux <- ifelse((trimws(as.character(dt4$SO4_flux))==trimws("-888.88")),NA,dt4$SO4_flux)               
suppressWarnings(dt4$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt4$SO4_flux))
dt4$NO3_flux <- ifelse((trimws(as.character(dt4$NO3_flux))==trimws("-888.88")),NA,dt4$NO3_flux)               
suppressWarnings(dt4$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt4$NO3_flux))
dt4$Cl_flux <- ifelse((trimws(as.character(dt4$Cl_flux))==trimws("-888.88")),NA,dt4$Cl_flux)               
suppressWarnings(dt4$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Cl_flux))
dt4$PO4_flux <- ifelse((trimws(as.character(dt4$PO4_flux))==trimws("-888.88")),NA,dt4$PO4_flux)               
suppressWarnings(dt4$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt4$PO4_flux))
dt4$DOC_flux <- ifelse((trimws(as.character(dt4$DOC_flux))==trimws("-888.88")),NA,dt4$DOC_flux)               
suppressWarnings(dt4$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt4$DOC_flux))
dt4$TDN_flux <- ifelse((trimws(as.character(dt4$TDN_flux))==trimws("-888.88")),NA,dt4$TDN_flux)               
suppressWarnings(dt4$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt4$TDN_flux))
dt4$DON_flux <- ifelse((trimws(as.character(dt4$DON_flux))==trimws("-888.88")),NA,dt4$DON_flux)               
suppressWarnings(dt4$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt4$DON_flux))
dt4$SiO2_flux <- ifelse((trimws(as.character(dt4$SiO2_flux))==trimws("-888.88")),NA,dt4$SiO2_flux)               
suppressWarnings(dt4$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt4$SiO2_flux))
dt4$Mn_flux <- ifelse((trimws(as.character(dt4$Mn_flux))==trimws("-888.88")),NA,dt4$Mn_flux)               
suppressWarnings(dt4$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Mn_flux))
dt4$Fe_flux <- ifelse((trimws(as.character(dt4$Fe_flux))==trimws("-888.88")),NA,dt4$Fe_flux)               
suppressWarnings(dt4$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt4$Fe_flux))
dt4$F_flux <- ifelse((trimws(as.character(dt4$F_flux))==trimws("-888.88")),NA,dt4$F_flux)               
suppressWarnings(dt4$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$F_flux))==as.character(as.numeric("-888.88"))),NA,dt4$F_flux))
dt4$H_flux <- ifelse((trimws(as.character(dt4$H_flux))==trimws("-888.88")),NA,dt4$H_flux)               
suppressWarnings(dt4$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$H_flux))==as.character(as.numeric("-888.88"))),NA,dt4$H_flux))
dt4$pH_volwt <- ifelse((trimws(as.character(dt4$pH_volwt))==trimws("-888.88")),NA,dt4$pH_volwt)               
suppressWarnings(dt4$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt4$pH_volwt))
dt4$SpecCond_volwt <- ifelse((trimws(as.character(dt4$SpecCond_volwt))==trimws("-888.888")),NA,dt4$SpecCond_volwt)               
suppressWarnings(dt4$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.888")) & (trimws(as.character(dt4$SpecCond_volwt))==as.character(as.numeric("-888.888"))),NA,dt4$SpecCond_volwt))


# conduct data formatting
head(dt4)
dt4$DATE<-paste0(dt4$Year_Month,"-01") 
dt4$DATE<-ymd(dt4$DATE) # change how R interprets Date to be a date
# add in water year
w.year <- as.numeric(format(dt4$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt4$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
dt4$wyear<-w.year
head(dt4)

# make sure you are only using complete years for the record
monchem<-as.data.frame(table( dt4$wyear))

monchem$wys<- paste(monchem$Var1)
monchem[monchem$Freq<12, "Use"]<-"incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use),"Use"]<-"complete"
head(monchem, 50)

dt4$Use<-monchem$Use[match(dt4$wyear, monchem$wys)]

dt4.complete<-dt4[dt4$Use=="complete",]


# Sum the 12 months to get annual totals
## Ca is in g/ha

annCaWS6<-aggregate(list(Ca.g.ha=dt4.complete$Ca_flux), by=list(wyear=dt4.complete$wyear), FUN="sum")
head(annCaWS6)

# has Ca flux from WS6 changed since the 1960s?
gca1<-ggplot(annCaWS6, aes(x=wyear, y=Ca.g.ha))+geom_point()+geom_line()+
  ggtitle("Reference WS6: annual flux of Ca ")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Ca (g/ha)")
gca1




     ## Chapter 3.2
####################################################


## read in monthly flux of stream chemistry for WS2

# Package ID: knb-lter-hbr.16.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Precipitation â Monthly Fluxes, Watershed 2, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/16/11/907d535e812f3141942ac7274f268710" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt5<-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Year_Month",     
                 "precip_mm",     
                 "Ca_flux",     
                 "Mg_flux",     
                 "K_flux",     
                 "Na_flux",     
                 "Al_Ferron_flux",     
                 "TMAl_flux",     
                 "OMAl_flux",     
                 "Al_ICP_flux",     
                 "NH4_flux",     
                 "SO4_flux",     
                 "NO3_flux",     
                 "Cl_flux",     
                 "PO4_flux",     
                 "DOC_flux",     
                 "TDN_flux",     
                 "DON_flux",     
                 "SiO2_flux",     
                 "Mn_flux",     
                 "Fe_flux",     
                 "F_flux",     
                 "H_flux",     
                 "pH_volwt",     
                 "SpecCond_volwt"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Month)!="factor") dt5$Month<- as.factor(dt5$Month)
if (class(dt5$Year_Month)!="factor") dt5$Year_Month<- as.factor(dt5$Year_Month)
if (class(dt5$precip_mm)=="factor") dt5$precip_mm <-as.numeric(levels(dt5$precip_mm))[as.integer(dt5$precip_mm) ]               
if (class(dt5$precip_mm)=="character") dt5$precip_mm <-as.numeric(dt5$precip_mm)
if (class(dt5$Ca_flux)=="factor") dt5$Ca_flux <-as.numeric(levels(dt5$Ca_flux))[as.integer(dt5$Ca_flux) ]               
if (class(dt5$Ca_flux)=="character") dt5$Ca_flux <-as.numeric(dt5$Ca_flux)
if (class(dt5$Mg_flux)=="factor") dt5$Mg_flux <-as.numeric(levels(dt5$Mg_flux))[as.integer(dt5$Mg_flux) ]               
if (class(dt5$Mg_flux)=="character") dt5$Mg_flux <-as.numeric(dt5$Mg_flux)
if (class(dt5$K_flux)=="factor") dt5$K_flux <-as.numeric(levels(dt5$K_flux))[as.integer(dt5$K_flux) ]               
if (class(dt5$K_flux)=="character") dt5$K_flux <-as.numeric(dt5$K_flux)
if (class(dt5$Na_flux)=="factor") dt5$Na_flux <-as.numeric(levels(dt5$Na_flux))[as.integer(dt5$Na_flux) ]               
if (class(dt5$Na_flux)=="character") dt5$Na_flux <-as.numeric(dt5$Na_flux)
if (class(dt5$Al_Ferron_flux)=="factor") dt5$Al_Ferron_flux <-as.numeric(levels(dt5$Al_Ferron_flux))[as.integer(dt5$Al_Ferron_flux) ]               
if (class(dt5$Al_Ferron_flux)=="character") dt5$Al_Ferron_flux <-as.numeric(dt5$Al_Ferron_flux)
if (class(dt5$TMAl_flux)=="factor") dt5$TMAl_flux <-as.numeric(levels(dt5$TMAl_flux))[as.integer(dt5$TMAl_flux) ]               
if (class(dt5$TMAl_flux)=="character") dt5$TMAl_flux <-as.numeric(dt5$TMAl_flux)
if (class(dt5$OMAl_flux)=="factor") dt5$OMAl_flux <-as.numeric(levels(dt5$OMAl_flux))[as.integer(dt5$OMAl_flux) ]               
if (class(dt5$OMAl_flux)=="character") dt5$OMAl_flux <-as.numeric(dt5$OMAl_flux)
if (class(dt5$Al_ICP_flux)=="factor") dt5$Al_ICP_flux <-as.numeric(levels(dt5$Al_ICP_flux))[as.integer(dt5$Al_ICP_flux) ]               
if (class(dt5$Al_ICP_flux)=="character") dt5$Al_ICP_flux <-as.numeric(dt5$Al_ICP_flux)
if (class(dt5$NH4_flux)=="factor") dt5$NH4_flux <-as.numeric(levels(dt5$NH4_flux))[as.integer(dt5$NH4_flux) ]               
if (class(dt5$NH4_flux)=="character") dt5$NH4_flux <-as.numeric(dt5$NH4_flux)
if (class(dt5$SO4_flux)=="factor") dt5$SO4_flux <-as.numeric(levels(dt5$SO4_flux))[as.integer(dt5$SO4_flux) ]               
if (class(dt5$SO4_flux)=="character") dt5$SO4_flux <-as.numeric(dt5$SO4_flux)
if (class(dt5$NO3_flux)=="factor") dt5$NO3_flux <-as.numeric(levels(dt5$NO3_flux))[as.integer(dt5$NO3_flux) ]               
if (class(dt5$NO3_flux)=="character") dt5$NO3_flux <-as.numeric(dt5$NO3_flux)
if (class(dt5$Cl_flux)=="factor") dt5$Cl_flux <-as.numeric(levels(dt5$Cl_flux))[as.integer(dt5$Cl_flux) ]               
if (class(dt5$Cl_flux)=="character") dt5$Cl_flux <-as.numeric(dt5$Cl_flux)
if (class(dt5$PO4_flux)=="factor") dt5$PO4_flux <-as.numeric(levels(dt5$PO4_flux))[as.integer(dt5$PO4_flux) ]               
if (class(dt5$PO4_flux)=="character") dt5$PO4_flux <-as.numeric(dt5$PO4_flux)
if (class(dt5$DOC_flux)=="factor") dt5$DOC_flux <-as.numeric(levels(dt5$DOC_flux))[as.integer(dt5$DOC_flux) ]               
if (class(dt5$DOC_flux)=="character") dt5$DOC_flux <-as.numeric(dt5$DOC_flux)
if (class(dt5$TDN_flux)=="factor") dt5$TDN_flux <-as.numeric(levels(dt5$TDN_flux))[as.integer(dt5$TDN_flux) ]               
if (class(dt5$TDN_flux)=="character") dt5$TDN_flux <-as.numeric(dt5$TDN_flux)
if (class(dt5$DON_flux)=="factor") dt5$DON_flux <-as.numeric(levels(dt5$DON_flux))[as.integer(dt5$DON_flux) ]               
if (class(dt5$DON_flux)=="character") dt5$DON_flux <-as.numeric(dt5$DON_flux)
if (class(dt5$SiO2_flux)=="factor") dt5$SiO2_flux <-as.numeric(levels(dt5$SiO2_flux))[as.integer(dt5$SiO2_flux) ]               
if (class(dt5$SiO2_flux)=="character") dt5$SiO2_flux <-as.numeric(dt5$SiO2_flux)
if (class(dt5$Mn_flux)=="factor") dt5$Mn_flux <-as.numeric(levels(dt5$Mn_flux))[as.integer(dt5$Mn_flux) ]               
if (class(dt5$Mn_flux)=="character") dt5$Mn_flux <-as.numeric(dt5$Mn_flux)
if (class(dt5$Fe_flux)=="factor") dt5$Fe_flux <-as.numeric(levels(dt5$Fe_flux))[as.integer(dt5$Fe_flux) ]               
if (class(dt5$Fe_flux)=="character") dt5$Fe_flux <-as.numeric(dt5$Fe_flux)
if (class(dt5$F_flux)=="factor") dt5$F_flux <-as.numeric(levels(dt5$F_flux))[as.integer(dt5$F_flux) ]               
if (class(dt5$F_flux)=="character") dt5$F_flux <-as.numeric(dt5$F_flux)
if (class(dt5$H_flux)=="factor") dt5$H_flux <-as.numeric(levels(dt5$H_flux))[as.integer(dt5$H_flux) ]               
if (class(dt5$H_flux)=="character") dt5$H_flux <-as.numeric(dt5$H_flux)
if (class(dt5$pH_volwt)=="factor") dt5$pH_volwt <-as.numeric(levels(dt5$pH_volwt))[as.integer(dt5$pH_volwt) ]               
if (class(dt5$pH_volwt)=="character") dt5$pH_volwt <-as.numeric(dt5$pH_volwt)
if (class(dt5$SpecCond_volwt)=="factor") dt5$SpecCond_volwt <-as.numeric(levels(dt5$SpecCond_volwt))[as.integer(dt5$SpecCond_volwt) ]               
if (class(dt5$SpecCond_volwt)=="character") dt5$SpecCond_volwt <-as.numeric(dt5$SpecCond_volwt)

# Convert Missing Values to NA for non-dates

dt5$precip_mm <- ifelse((trimws(as.character(dt5$precip_mm))==trimws("-888.88")),NA,dt5$precip_mm)               
suppressWarnings(dt5$precip_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$precip_mm))==as.character(as.numeric("-888.88"))),NA,dt5$precip_mm))
dt5$Ca_flux <- ifelse((trimws(as.character(dt5$Ca_flux))==trimws("-888.88")),NA,dt5$Ca_flux)               
suppressWarnings(dt5$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Ca_flux))
dt5$Mg_flux <- ifelse((trimws(as.character(dt5$Mg_flux))==trimws("-888.88")),NA,dt5$Mg_flux)               
suppressWarnings(dt5$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Mg_flux))
dt5$K_flux <- ifelse((trimws(as.character(dt5$K_flux))==trimws("-888.88")),NA,dt5$K_flux)               
suppressWarnings(dt5$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$K_flux))==as.character(as.numeric("-888.88"))),NA,dt5$K_flux))
dt5$Na_flux <- ifelse((trimws(as.character(dt5$Na_flux))==trimws("-888.88")),NA,dt5$Na_flux)               
suppressWarnings(dt5$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Na_flux))
dt5$Al_Ferron_flux <- ifelse((trimws(as.character(dt5$Al_Ferron_flux))==trimws("-888.88")),NA,dt5$Al_Ferron_flux)               
suppressWarnings(dt5$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Al_Ferron_flux))
dt5$TMAl_flux <- ifelse((trimws(as.character(dt5$TMAl_flux))==trimws("-888.88")),NA,dt5$TMAl_flux)               
suppressWarnings(dt5$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt5$TMAl_flux))
dt5$OMAl_flux <- ifelse((trimws(as.character(dt5$OMAl_flux))==trimws("-888.88")),NA,dt5$OMAl_flux)               
suppressWarnings(dt5$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt5$OMAl_flux))
dt5$Al_ICP_flux <- ifelse((trimws(as.character(dt5$Al_ICP_flux))==trimws("-888.88")),NA,dt5$Al_ICP_flux)               
suppressWarnings(dt5$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Al_ICP_flux))
dt5$NH4_flux <- ifelse((trimws(as.character(dt5$NH4_flux))==trimws("-888.88")),NA,dt5$NH4_flux)               
suppressWarnings(dt5$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt5$NH4_flux))
dt5$SO4_flux <- ifelse((trimws(as.character(dt5$SO4_flux))==trimws("-888.88")),NA,dt5$SO4_flux)               
suppressWarnings(dt5$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt5$SO4_flux))
dt5$NO3_flux <- ifelse((trimws(as.character(dt5$NO3_flux))==trimws("-888.88")),NA,dt5$NO3_flux)               
suppressWarnings(dt5$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt5$NO3_flux))
dt5$Cl_flux <- ifelse((trimws(as.character(dt5$Cl_flux))==trimws("-888.88")),NA,dt5$Cl_flux)               
suppressWarnings(dt5$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Cl_flux))
dt5$PO4_flux <- ifelse((trimws(as.character(dt5$PO4_flux))==trimws("-888.88")),NA,dt5$PO4_flux)               
suppressWarnings(dt5$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt5$PO4_flux))
dt5$DOC_flux <- ifelse((trimws(as.character(dt5$DOC_flux))==trimws("-888.88")),NA,dt5$DOC_flux)               
suppressWarnings(dt5$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt5$DOC_flux))
dt5$TDN_flux <- ifelse((trimws(as.character(dt5$TDN_flux))==trimws("-888.88")),NA,dt5$TDN_flux)               
suppressWarnings(dt5$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt5$TDN_flux))
dt5$DON_flux <- ifelse((trimws(as.character(dt5$DON_flux))==trimws("-888.88")),NA,dt5$DON_flux)               
suppressWarnings(dt5$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt5$DON_flux))
dt5$SiO2_flux <- ifelse((trimws(as.character(dt5$SiO2_flux))==trimws("-888.88")),NA,dt5$SiO2_flux)               
suppressWarnings(dt5$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt5$SiO2_flux))
dt5$Mn_flux <- ifelse((trimws(as.character(dt5$Mn_flux))==trimws("-888.88")),NA,dt5$Mn_flux)               
suppressWarnings(dt5$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Mn_flux))
dt5$Fe_flux <- ifelse((trimws(as.character(dt5$Fe_flux))==trimws("-888.88")),NA,dt5$Fe_flux)               
suppressWarnings(dt5$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Fe_flux))
dt5$F_flux <- ifelse((trimws(as.character(dt5$F_flux))==trimws("-888.88")),NA,dt5$F_flux)               
suppressWarnings(dt5$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$F_flux))==as.character(as.numeric("-888.88"))),NA,dt5$F_flux))
dt5$H_flux <- ifelse((trimws(as.character(dt5$H_flux))==trimws("-888.88")),NA,dt5$H_flux)               
suppressWarnings(dt5$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$H_flux))==as.character(as.numeric("-888.88"))),NA,dt5$H_flux))
dt5$pH_volwt <- ifelse((trimws(as.character(dt5$pH_volwt))==trimws("-888.88")),NA,dt5$pH_volwt)               
suppressWarnings(dt5$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt5$pH_volwt))
dt5$SpecCond_volwt <- ifelse((trimws(as.character(dt5$SpecCond_volwt))==trimws("-888.88")),NA,dt5$SpecCond_volwt)               
suppressWarnings(dt5$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt5$SpecCond_volwt))


# Here is the structure of the input data frame:
str(dt5)                            

# conduct data formatting
head(dt5)
dt5$DATE<-paste0(dt5$Year_Month,"-01") 
dt5$DATE<-ymd(dt5$DATE) # change how R interprets Date to be a date
# add in water year
w.year <- as.numeric(format(dt5$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt5$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
dt5$wyear<-w.year

# make sure you are only using complete years for the record
monchem<-as.data.frame(table( dt5$wyear))

monchem$wys<- paste(monchem$Var1)
monchem[monchem$Freq<12, "Use"]<-"incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use),"Use"]<-"complete"
head(monchem, 50)

dt5$Use<-monchem$Use[match(dt5$wyear, monchem$wys)]

dt5.complete<-dt5[dt5$Use=="complete",]

# Sum the 12 months to get annual totals
## Ca is in g/ha

annCaws2<-aggregate(list(Ca.g.ha=dt5.complete$Ca_flux), by=list(wyear=dt5.complete$wyear), FUN="sum")
head(annCaws2)


# Combine WS6 and WS2
annCaws2$WS<-"W2"
annCaWS6$WS<-"W6"

annCa26<-rbind(annCaws2,annCaWS6)

# has Ca flux from WS6 changed since the 1960s?
gca2<-ggplot(annCa26, aes(x=wyear, y=Ca.g.ha, col=WS))+geom_point()+geom_line(size=2)+
  ggtitle("Decreasing annual Ca flux in devegetated watershed 2 and reference watershed 6  ")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Ca (g/ha)")+
  geom_vline(xintercept=1965, linetype="dashed", col="red")+
  geom_vline(xintercept=1968, linetype="solid", col="red")+
  scale_y_log10()
gca2

pca2<-ggplotly(gca2)


# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(plotfinal), "hwChapter/Streamwater_nutrient_flux_3.html")


####################################################     ##
# Chapter 4 Element input in bulk precip        ##########
####################################################     ##


## Chapter 4.1
####################################################

# read in bulk precip chem data
# Package ID: knb-lter-hbr.24.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Chemistry of Bulk Precipitation at Hubbard Brook     Experimental Forest, 1969 - present, Robert S. Pierce Ecosystem     Laboratory Facility.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/24/8/03013580f8955256d4c53b28b5d51c48" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt6 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep="," 
               , col.names=c(
                 "rg",     
                 "year",     
                 "mo",     
                 "precip",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "Al",     
                 "NH4",     
                 "pH",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "SiO2"    ), check.names=TRUE)

unlink(infile1)

if (class(dt6$rg)!="factor") dt6$rg<- as.factor(dt6$rg)
if (class(dt6$precip)=="factor") dt6$precip <-as.numeric(levels(dt6$precip))[as.integer(dt6$precip) ]               
if (class(dt6$precip)=="character") dt6$precip <-as.numeric(dt6$precip)
if (class(dt6$Ca)=="factor") dt6$Ca <-as.numeric(levels(dt6$Ca))[as.integer(dt6$Ca) ]               
if (class(dt6$Ca)=="character") dt6$Ca <-as.numeric(dt6$Ca)
if (class(dt6$Mg)=="factor") dt6$Mg <-as.numeric(levels(dt6$Mg))[as.integer(dt6$Mg) ]               
if (class(dt6$Mg)=="character") dt6$Mg <-as.numeric(dt6$Mg)
if (class(dt6$K)=="factor") dt6$K <-as.numeric(levels(dt6$K))[as.integer(dt6$K) ]               
if (class(dt6$K)=="character") dt6$K <-as.numeric(dt6$K)
if (class(dt6$Na)=="factor") dt6$Na <-as.numeric(levels(dt6$Na))[as.integer(dt6$Na) ]               
if (class(dt6$Na)=="character") dt6$Na <-as.numeric(dt6$Na)
if (class(dt6$Al)=="factor") dt6$Al <-as.numeric(levels(dt6$Al))[as.integer(dt6$Al) ]               
if (class(dt6$Al)=="character") dt6$Al <-as.numeric(dt6$Al)
if (class(dt6$NH4)=="factor") dt6$NH4 <-as.numeric(levels(dt6$NH4))[as.integer(dt6$NH4) ]               
if (class(dt6$NH4)=="character") dt6$NH4 <-as.numeric(dt6$NH4)
if (class(dt6$pH)=="factor") dt6$pH <-as.numeric(levels(dt6$pH))[as.integer(dt6$pH) ]               
if (class(dt6$pH)=="character") dt6$pH <-as.numeric(dt6$pH)
if (class(dt6$SO4)=="factor") dt6$SO4 <-as.numeric(levels(dt6$SO4))[as.integer(dt6$SO4) ]               
if (class(dt6$SO4)=="character") dt6$SO4 <-as.numeric(dt6$SO4)
if (class(dt6$NO3)=="factor") dt6$NO3 <-as.numeric(levels(dt6$NO3))[as.integer(dt6$NO3) ]               
if (class(dt6$NO3)=="character") dt6$NO3 <-as.numeric(dt6$NO3)
if (class(dt6$Cl)=="factor") dt6$Cl <-as.numeric(levels(dt6$Cl))[as.integer(dt6$Cl) ]               
if (class(dt6$Cl)=="character") dt6$Cl <-as.numeric(dt6$Cl)
if (class(dt6$PO4)=="factor") dt6$PO4 <-as.numeric(levels(dt6$PO4))[as.integer(dt6$PO4) ]               
if (class(dt6$PO4)=="character") dt6$PO4 <-as.numeric(dt6$PO4)
if (class(dt6$SiO2)=="factor") dt6$SiO2 <-as.numeric(levels(dt6$SiO2))[as.integer(dt6$SiO2) ]               
if (class(dt6$SiO2)=="character") dt6$SiO2 <-as.numeric(dt6$SiO2)

# Convert Missing Values to NA for non-dates

dt6$precip <- ifelse((trimws(as.character(dt6$precip))==trimws("-3.000")),NA,dt6$precip)               
suppressWarnings(dt6$precip <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$precip))==as.character(as.numeric("-3.000"))),NA,dt6$precip))
dt6$Ca <- ifelse((trimws(as.character(dt6$Ca))==trimws("-3.000")),NA,dt6$Ca)               
suppressWarnings(dt6$Ca <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$Ca))==as.character(as.numeric("-3.000"))),NA,dt6$Ca))
dt6$Mg <- ifelse((trimws(as.character(dt6$Mg))==trimws("-3.000")),NA,dt6$Mg)               
suppressWarnings(dt6$Mg <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$Mg))==as.character(as.numeric("-3.000"))),NA,dt6$Mg))
dt6$K <- ifelse((trimws(as.character(dt6$K))==trimws("-3.000")),NA,dt6$K)               
suppressWarnings(dt6$K <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$K))==as.character(as.numeric("-3.000"))),NA,dt6$K))
dt6$Na <- ifelse((trimws(as.character(dt6$Na))==trimws("-3.000")),NA,dt6$Na)               
suppressWarnings(dt6$Na <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$Na))==as.character(as.numeric("-3.000"))),NA,dt6$Na))
dt6$Al <- ifelse((trimws(as.character(dt6$Al))==trimws("-3.000")),NA,dt6$Al)               
suppressWarnings(dt6$Al <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$Al))==as.character(as.numeric("-3.000"))),NA,dt6$Al))
dt6$NH4 <- ifelse((trimws(as.character(dt6$NH4))==trimws("-3.000")),NA,dt6$NH4)               
suppressWarnings(dt6$NH4 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$NH4))==as.character(as.numeric("-3.000"))),NA,dt6$NH4))
dt6$pH <- ifelse((trimws(as.character(dt6$pH))==trimws("-3.00")),NA,dt6$pH)               
suppressWarnings(dt6$pH <- ifelse(!is.na(as.numeric("-3.00")) & (trimws(as.character(dt6$pH))==as.character(as.numeric("-3.00"))),NA,dt6$pH))
dt6$SO4 <- ifelse((trimws(as.character(dt6$SO4))==trimws("-3.000")),NA,dt6$SO4)               
suppressWarnings(dt6$SO4 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$SO4))==as.character(as.numeric("-3.000"))),NA,dt6$SO4))
dt6$NO3 <- ifelse((trimws(as.character(dt6$NO3))==trimws("-3.000")),NA,dt6$NO3)               
suppressWarnings(dt6$NO3 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$NO3))==as.character(as.numeric("-3.000"))),NA,dt6$NO3))
dt6$Cl <- ifelse((trimws(as.character(dt6$Cl))==trimws("-3.000")),NA,dt6$Cl)               
suppressWarnings(dt6$Cl <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$Cl))==as.character(as.numeric("-3.000"))),NA,dt6$Cl))
dt6$PO4 <- ifelse((trimws(as.character(dt6$PO4))==trimws("-3.0000")),NA,dt6$PO4)               
suppressWarnings(dt6$PO4 <- ifelse(!is.na(as.numeric("-3.0000")) & (trimws(as.character(dt6$PO4))==as.character(as.numeric("-3.0000"))),NA,dt6$PO4))
dt6$SiO2 <- ifelse((trimws(as.character(dt6$SiO2))==trimws("-3.000")),NA,dt6$SiO2)               
suppressWarnings(dt6$SiO2 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt6$SiO2))==as.character(as.numeric("-3.000"))),NA,dt6$SiO2))




# inspect data
str(dt6)
dt6$month<-paste0("0",dt6$mo)
## annoyingly, we don't want the extra 0 to be in the last 3 months
dt6[dt6$month=="010","month"]<-"10"
dt6[dt6$month=="011","month"]<-"11"
dt6[dt6$month=="012","month"]<-"12"
head(dt6)

# add in date
dt6$DATE<-paste0(dt6$year,"-", dt6$month,"-01") 

dt6$DATE<-ymd(dt6$DATE) # change how R interprets Date to be a date
# add in water year
w.year <- as.numeric(format(dt6$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt6$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
dt6$wyear<-w.year


# make sure you are only using complete years for the record
monchem<-as.data.frame(table( dt6$wyear))

monchem$wys<- paste(monchem$Var1)
monchem[monchem$Freq<12, "Use"]<-"incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use),"Use"]<-"complete"
head(monchem, 50)

dt6$Use<-monchem$Use[match(dt6$wyear, monchem$wys)]

dt6.complete<-dt6[dt6$Use=="complete",]

# calculate annual input of Ca into WS6
head(dt6.complete)


# Sum the 12 months to get annual totals
## Ca is in milligramPerLiter

annCa_bulk<-aggregate(list(Ca.mg.L=dt6.complete$Ca), by=list(wyear=dt6.complete$wyear), FUN="sum")

ginputca1<-ggplot(annCa_bulk, aes(x=wyear, y=Ca.mg.L))+geom_point()+geom_line()+
  ggtitle("Rain gauge 22? : annual input of Ca ")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Ca (mg/L)")
ginputca1

pinputca1<-ggplotly(ginputca1)

# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(pinputca1), "hwChapter/bulk_precip_4_Ca.html")


####################################################     ##
# Chapter 5 Nutrient accumulation               ##########
####################################################     ##

## Chapter 5.1
####################################################

# read in vegetation data for WS6 (received from Mary Martin 2022-06-29 by email)


dt7<-read.csv("C:\\Users\\bears\\Downloads\\HubbardBrookWS6_Tree_Biomass_1997.csv")


# Here is the structure of the input data frame:
str(dt7)       


tr<-aggregate( list(abv=dt7$ABOVE.kg , blw=dt7$BELOW.kg), by=list(Plot=dt7$Plot, Sp=dt7$SppName), FUN="sum", na.rm=T)
dim(tr)
head(tr)

# gather above and below to make graphing easier
trg<-gather(tr, "abe","value", 3:4)
head(trg)

ggplot(trg, aes(x=Plot, y=value, fill=Sp))+geom_bar(stat="identity", position="stack", col="black")

## calculate the per area value of biomass on WS6 in 1974.

####  The planimetric area is 13.2 ha
sum(trg$value) # this is the biomass of the entire region?

w6biomass<-sum(trg$value) / 13.2
w6biomass # kg of biomass per hectare

# for every gram of tree, there is 3.13 mg of Ca,  based on dry weight.
##   unit conversions 3.13 mg / g * 1000 g / kg * w6biomass kg / ha.   End is mg of Ca per hectare

w6ca.mg.ha<-3.13*1000 * w6biomass 

w6ca.kg.ha <-w6ca.mg.ha / 1000000 # mg in a kg

w6ca.kg.ha   # ~700 kg/ha Ca

## Chapter 5.2
####################################################

# how does this value of Ca in the vegetation from 1965 differ from 1997?

########## no data yet from Mary for 1965, only 1997

# read in tree biomass information for 1997
 
 


# Chapter 6 Net soil release                    ##########
####################################################     ##

## Chapter 6.1
####################################################

# read in  

### bulk precip input
### stream output.
  
# this is to get net Ca soil release.



## Chapter 6.2
####################################################

## calculate weathering input for 1965

## weathering input = stream output - precip input +- storage
  



## Chapter 6.3
####################################################

## calculate for 1974. 
# work with 1965 - 1997 data.     Use the tree biomass answers



## Chapter 6.4
####################################################

### An independent estimate of primary mineral weathering was 2.8 kg/ha/yr. 
###   What does this say about overall changes in soil pool during this period of time? Acid rain context.


# compare of Ca removed in the WS5 whole tree harvest compared to net soil depletion from 1965-1997 (previously calculated)


# simplify by assuming Ca content of WS5 prior to harvest was equal to that on Ws6 in 1997


# what would you conclude about acid deposition vs forest harvest as causes of Ca depletion in the 20th century?

