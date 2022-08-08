


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

#htmlwidgets::saveWidget(as_widget(plotfinal), "hwChapter/hydrology_2_precip_stream_AET.html")



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
# Package ID: knb-lter-hbr.4.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater – Monthly Fluxes, Watershed 2, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/4/17/a6aeef15070be913ee2f06f431b9b7a7" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt5 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Year_Month",     
                 "flow_mm",     
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
                 "DIC_flux",     
                 "SiO2_flux",     
                 "Mn_flux",     
                 "Fe_flux",     
                 "F_flux",     
                 "H_flux",     
                 "pH_volwt",     
                 "SpecCond_volwt",     
                 "ANC_volwt"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Month)!="factor") dt5$Month<- as.factor(dt5$Month)
if (class(dt5$Year_Month)!="factor") dt5$Year_Month<- as.factor(dt5$Year_Month)
if (class(dt5$flow_mm)=="factor") dt5$flow_mm <-as.numeric(levels(dt5$flow_mm))[as.integer(dt5$flow_mm) ]               
if (class(dt5$flow_mm)=="character") dt5$flow_mm <-as.numeric(dt5$flow_mm)
if (class(dt5$Ca_flux)=="factor") dt5$Ca_flux <-as.numeric(levels(dt5$Ca_flux))[as.integer(dt5$Ca_flux) ]               
if (class(dt5$Ca_flux)=="character") dt5$Ca_flux <-as.numeric(dt5$Ca_flux)

# Convert Missing Values to NA for non-dates
dt5$flow_mm <- ifelse((trimws(as.character(dt5$flow_mm))==trimws("-888.88")),NA,dt5$flow_mm)               
suppressWarnings(dt5$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt5$flow_mm))
dt5$Ca_flux <- ifelse((trimws(as.character(dt5$Ca_flux))==trimws("-888.88")),NA,dt5$Ca_flux)               
suppressWarnings(dt5$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt5$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt5$Ca_flux))

# Here is the structure of the input data frame:
#str(dt5)                            

# conduct data formatting
#head(dt5)
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
#head(monchem, 50)

dt5$Use<-monchem$Use[match(dt5$wyear, monchem$wys)]

dt5.complete<-dt5[dt5$Use=="complete",]

head(dt5)
# Sum the 12 months to get annual totals
## Ca is in g/ha
annCa_str_WS2<-aggregate(list(Ca.g.ha=dt5.complete$Ca_flux), by=list(wyear=dt5.complete$wyear), FUN="sum")
#head(annCa_str_WS6)


# Combine WS6 and WS2
annCa_str_WS2$Watershed<-"WS2"

####################################################     ##
# Chapter 4 Element input in bulk precip        ##########
####################################################     ##


## Chapter 4.1
####################################################

# read in chemistry of precipitation for W6

# Package ID: knb-lter-hbr.20.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Precipitation  Monthly Fluxes, Watershed 6, 1964 - present.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/20/11/76b46d5bd60e4912406726ec71610d0a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt6 <-read.csv(infile1,header=F 
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

head(dt6)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(dt6$Month)!="factor") dt6$Month<- as.factor(dt6$Month)
if (class(dt6$Year_Month)!="factor") dt6$Year_Month<- as.factor(dt6$Year_Month)
if (class(dt6$precip_mm)=="factor") dt6$precip_mm <-as.numeric(levels(dt6$precip_mm))[as.integer(dt6$precip_mm) ]               
if (class(dt6$precip_mm)=="character") dt6$precip_mm <-as.numeric(dt6$precip_mm)
if (class(dt6$Ca_flux)=="factor") dt6$Ca_flux <-as.numeric(levels(dt6$Ca_flux))[as.integer(dt6$Ca_flux) ]               
if (class(dt6$Ca_flux)=="character") dt6$Ca_flux <-as.numeric(dt6$Ca_flux)

# Convert Missing Values to NA for non-dates
dt6$precip_mm <- ifelse((trimws(as.character(dt6$precip_mm))==trimws("-888.88")),NA,dt6$precip_mm)               
dt6$Ca_flux <- ifelse((trimws(as.character(dt6$Ca_flux))==trimws("-888.88")),NA,dt6$Ca_flux)               

head(dt6)


# add in date
dt6$DATE<-paste0(dt6$Year_Month,"-01") 
head(dt6)
dt6$DATE<-ymd(dt6$DATE) # change how R interprets Date to be a date

# add in water year
w.year <- as.numeric(format(dt6$DATE, "%Y"))
june.july.sept <- as.numeric(format(dt6$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
dt6$wyear<-w.year


# make sure you are only using complete years for the record
monchem<-as.data.frame(table( dt6$wyear))
monchem

monchem$wys<- paste(monchem$Var1)
monchem[monchem$Freq<10, "Use"]<-"incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use),"Use"]<-"complete"
head(monchem, 50)

dt6$Use<-monchem$Use[match(dt6$wyear, monchem$wys)]

dt6.complete<-dt6[dt6$Use=="complete",]

# calculate annual input of Ca into WS6
head(dt6.complete)


# Sum the 12 months to get annual totals
## Ca is in milligramPerLiter

annCa_precip<-aggregate(list(Ca.g.ha=dt6.complete$Ca_flux, precip_mm=dt6.complete$precip_mm), by=list(wyear=dt6.complete$wyear), FUN="sum")




ginputca1<-ggplot(annCa_precip, aes(x=wyear, y=Ca.g.ha))+geom_point()+geom_line()+
  ggtitle("Rain gauge 22? : annual input of Ca ")+xlim(1964,2020)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+
  ylab("Ca (mg/L)")
ginputca1

pinputca1<-ggplotly(ginputca1)

# this line writes the html file to create interactive graphs for the online book
#htmlwidgets::saveWidget(as_widget(pinputca1), "hwChapter/bulk_precip_4_Ca.html")







### An independent estimate of primary mineral weathering was 2.8 kg/ha/yr. 

###   What does this say about overall changes in soil pool during this period of time? Acid rain context.














# What single dataset would have the most explanatory power for primary productivity at Hubbard Brook?   Where do trees grow the fastest. 


  
  


head(annstr)

table(annstr$WS)
jc<-annstr[annstr$WS==c("W5","W6"),]
ja<-jc[,c(-4,-5,-6)]
ja
j<-spread(ja, "WS","Streamflow")


  ggplot(annstr, aes(x=Streamflow, y=Precip, col=wyear))+geom_point()
  
  
  
  t.test( ja$Streamflow ~ ja$WS, paired=T)
  
  
  # stream flow volume
annstr$all_ymean<-mean(annstr$Streamflow)
annstr$diff<-annstr$Streamflow - annstr$all_ymean  


dim(annstr)
n<- 493

str(annstr) 
sqrt(sum((annstr$diff^2))   /  (n-1))








# calculate the standard error between years


  
  
  # stream flow concentration
  
  
  


