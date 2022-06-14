
# Package ID: knb-lter-hbr.208.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Continuous precipitation and stream chemistry data, Hubbard Brook Ecosystem Study, 1963 – present..
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 



# Alex Young, 2022-06-14
# Hubbard Brook Online Book

# Transforming from static to interactive figures

#make sure you have these packages. If you don't, use e.g. install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

##################################################


## read in the data from EDI
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/7/75239939a03d0c623ccd99b3f10b5e9c" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site",     
                 "date",     
                 "timeEST",     
                 "barcode",     
                 "pH",     
                 "DIC",     
                 "spCond",     
                 "temp",     
                 "ANC960",     
                 "ANCMet",     
                 "gageHt",     
                 "hydroGraph",     
                 "flowGageHt",     
                 "fieldCode",     
                 "notes",     
                 "uniqueID",     
                 "waterYr",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "TMAl",     
                 "OMAl",     
                 "Al_ICP",     
                 "NH4",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "DOC",     
                 "TDN",     
                 "DON",     
                 "SiO2",     
                 "Mn",     
                 "Fe",     
                 "F",     
                 "cationCharge",     
                 "anionCharge",     
                 "ionError",     
                 "duplicate",     
                 "sampleType",     
                 "ionBalance",     
                 "canonical",     
                 "pHmetrohm"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$barcode)!="factor") dt2$barcode<- as.factor(dt2$barcode)
if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]               
if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
if (class(dt2$DIC)=="factor") dt2$DIC <-as.numeric(levels(dt2$DIC))[as.integer(dt2$DIC) ]               
if (class(dt2$DIC)=="character") dt2$DIC <-as.numeric(dt2$DIC)
if (class(dt2$spCond)=="factor") dt2$spCond <-as.numeric(levels(dt2$spCond))[as.integer(dt2$spCond) ]               
if (class(dt2$spCond)=="character") dt2$spCond <-as.numeric(dt2$spCond)
if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]               
if (class(dt2$temp)=="character") dt2$temp <-as.numeric(dt2$temp)
if (class(dt2$ANC960)=="factor") dt2$ANC960 <-as.numeric(levels(dt2$ANC960))[as.integer(dt2$ANC960) ]               
if (class(dt2$ANC960)=="character") dt2$ANC960 <-as.numeric(dt2$ANC960)
if (class(dt2$ANCMet)=="factor") dt2$ANCMet <-as.numeric(levels(dt2$ANCMet))[as.integer(dt2$ANCMet) ]               
if (class(dt2$ANCMet)=="character") dt2$ANCMet <-as.numeric(dt2$ANCMet)
if (class(dt2$gageHt)=="factor") dt2$gageHt <-as.numeric(levels(dt2$gageHt))[as.integer(dt2$gageHt) ]               
if (class(dt2$gageHt)=="character") dt2$gageHt <-as.numeric(dt2$gageHt)
if (class(dt2$hydroGraph)!="factor") dt2$hydroGraph<- as.factor(dt2$hydroGraph)
if (class(dt2$flowGageHt)=="factor") dt2$flowGageHt <-as.numeric(levels(dt2$flowGageHt))[as.integer(dt2$flowGageHt) ]               
if (class(dt2$flowGageHt)=="character") dt2$flowGageHt <-as.numeric(dt2$flowGageHt)
if (class(dt2$fieldCode)!="factor") dt2$fieldCode<- as.factor(dt2$fieldCode)
if (class(dt2$notes)!="factor") dt2$notes<- as.factor(dt2$notes)
if (class(dt2$uniqueID)!="factor") dt2$uniqueID<- as.factor(dt2$uniqueID)
if (class(dt2$Ca)=="factor") dt2$Ca <-as.numeric(levels(dt2$Ca))[as.integer(dt2$Ca) ]               
if (class(dt2$Ca)=="character") dt2$Ca <-as.numeric(dt2$Ca)
if (class(dt2$Mg)=="factor") dt2$Mg <-as.numeric(levels(dt2$Mg))[as.integer(dt2$Mg) ]               
if (class(dt2$Mg)=="character") dt2$Mg <-as.numeric(dt2$Mg)
if (class(dt2$K)=="factor") dt2$K <-as.numeric(levels(dt2$K))[as.integer(dt2$K) ]               
if (class(dt2$K)=="character") dt2$K <-as.numeric(dt2$K)
if (class(dt2$Na)=="factor") dt2$Na <-as.numeric(levels(dt2$Na))[as.integer(dt2$Na) ]               
if (class(dt2$Na)=="character") dt2$Na <-as.numeric(dt2$Na)
if (class(dt2$TMAl)=="factor") dt2$TMAl <-as.numeric(levels(dt2$TMAl))[as.integer(dt2$TMAl) ]               
if (class(dt2$TMAl)=="character") dt2$TMAl <-as.numeric(dt2$TMAl)
if (class(dt2$OMAl)=="factor") dt2$OMAl <-as.numeric(levels(dt2$OMAl))[as.integer(dt2$OMAl) ]               
if (class(dt2$OMAl)=="character") dt2$OMAl <-as.numeric(dt2$OMAl)
if (class(dt2$Al_ICP)=="factor") dt2$Al_ICP <-as.numeric(levels(dt2$Al_ICP))[as.integer(dt2$Al_ICP) ]               
if (class(dt2$Al_ICP)=="character") dt2$Al_ICP <-as.numeric(dt2$Al_ICP)
if (class(dt2$NH4)=="factor") dt2$NH4 <-as.numeric(levels(dt2$NH4))[as.integer(dt2$NH4) ]               
if (class(dt2$NH4)=="character") dt2$NH4 <-as.numeric(dt2$NH4)
if (class(dt2$SO4)=="factor") dt2$SO4 <-as.numeric(levels(dt2$SO4))[as.integer(dt2$SO4) ]               
if (class(dt2$SO4)=="character") dt2$SO4 <-as.numeric(dt2$SO4)
if (class(dt2$NO3)=="factor") dt2$NO3 <-as.numeric(levels(dt2$NO3))[as.integer(dt2$NO3) ]               
if (class(dt2$NO3)=="character") dt2$NO3 <-as.numeric(dt2$NO3)
if (class(dt2$Cl)=="factor") dt2$Cl <-as.numeric(levels(dt2$Cl))[as.integer(dt2$Cl) ]               
if (class(dt2$Cl)=="character") dt2$Cl <-as.numeric(dt2$Cl)
if (class(dt2$PO4)=="factor") dt2$PO4 <-as.numeric(levels(dt2$PO4))[as.integer(dt2$PO4) ]               
if (class(dt2$PO4)=="character") dt2$PO4 <-as.numeric(dt2$PO4)
if (class(dt2$DOC)=="factor") dt2$DOC <-as.numeric(levels(dt2$DOC))[as.integer(dt2$DOC) ]               
if (class(dt2$DOC)=="character") dt2$DOC <-as.numeric(dt2$DOC)
if (class(dt2$TDN)=="factor") dt2$TDN <-as.numeric(levels(dt2$TDN))[as.integer(dt2$TDN) ]               
if (class(dt2$TDN)=="character") dt2$TDN <-as.numeric(dt2$TDN)
if (class(dt2$DON)=="factor") dt2$DON <-as.numeric(levels(dt2$DON))[as.integer(dt2$DON) ]               
if (class(dt2$DON)=="character") dt2$DON <-as.numeric(dt2$DON)
if (class(dt2$SiO2)=="factor") dt2$SiO2 <-as.numeric(levels(dt2$SiO2))[as.integer(dt2$SiO2) ]               
if (class(dt2$SiO2)=="character") dt2$SiO2 <-as.numeric(dt2$SiO2)
if (class(dt2$Mn)=="factor") dt2$Mn <-as.numeric(levels(dt2$Mn))[as.integer(dt2$Mn) ]               
if (class(dt2$Mn)=="character") dt2$Mn <-as.numeric(dt2$Mn)
if (class(dt2$Fe)=="factor") dt2$Fe <-as.numeric(levels(dt2$Fe))[as.integer(dt2$Fe) ]               
if (class(dt2$Fe)=="character") dt2$Fe <-as.numeric(dt2$Fe)
if (class(dt2$F)=="factor") dt2$F <-as.numeric(levels(dt2$F))[as.integer(dt2$F) ]               
if (class(dt2$F)=="character") dt2$F <-as.numeric(dt2$F)
if (class(dt2$cationCharge)=="factor") dt2$cationCharge <-as.numeric(levels(dt2$cationCharge))[as.integer(dt2$cationCharge) ]               
if (class(dt2$cationCharge)=="character") dt2$cationCharge <-as.numeric(dt2$cationCharge)
if (class(dt2$anionCharge)=="factor") dt2$anionCharge <-as.numeric(levels(dt2$anionCharge))[as.integer(dt2$anionCharge) ]               
if (class(dt2$anionCharge)=="character") dt2$anionCharge <-as.numeric(dt2$anionCharge)
if (class(dt2$ionError)=="factor") dt2$ionError <-as.numeric(levels(dt2$ionError))[as.integer(dt2$ionError) ]               
if (class(dt2$ionError)=="character") dt2$ionError <-as.numeric(dt2$ionError)
if (class(dt2$duplicate)!="factor") dt2$duplicate<- as.factor(dt2$duplicate)
if (class(dt2$sampleType)!="factor") dt2$sampleType<- as.factor(dt2$sampleType)
if (class(dt2$ionBalance)=="factor") dt2$ionBalance <-as.numeric(levels(dt2$ionBalance))[as.integer(dt2$ionBalance) ]               
if (class(dt2$ionBalance)=="character") dt2$ionBalance <-as.numeric(dt2$ionBalance)
if (class(dt2$canonical)!="factor") dt2$canonical<- as.factor(dt2$canonical)
if (class(dt2$pHmetrohm)!="factor") dt2$pHmetrohm<- as.factor(dt2$pHmetrohm)

# Convert Missing Values to NA for non-dates

dt2$site <- as.factor(ifelse((trimws(as.character(dt2$site))==trimws("NA")),NA,as.character(dt2$site)))
dt2$barcode <- as.factor(ifelse((trimws(as.character(dt2$barcode))==trimws("NA")),NA,as.character(dt2$barcode)))
dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)               
suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
dt2$DIC <- ifelse((trimws(as.character(dt2$DIC))==trimws("NA")),NA,dt2$DIC)               
suppressWarnings(dt2$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DIC))==as.character(as.numeric("NA"))),NA,dt2$DIC))
dt2$spCond <- ifelse((trimws(as.character(dt2$spCond))==trimws("NA")),NA,dt2$spCond)               
suppressWarnings(dt2$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$spCond))==as.character(as.numeric("NA"))),NA,dt2$spCond))
dt2$temp <- ifelse((trimws(as.character(dt2$temp))==trimws("NA")),NA,dt2$temp)               
suppressWarnings(dt2$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temp))==as.character(as.numeric("NA"))),NA,dt2$temp))
dt2$ANC960 <- ifelse((trimws(as.character(dt2$ANC960))==trimws("NA")),NA,dt2$ANC960)               
suppressWarnings(dt2$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANC960))==as.character(as.numeric("NA"))),NA,dt2$ANC960))
dt2$ANCMet <- ifelse((trimws(as.character(dt2$ANCMet))==trimws("NA")),NA,dt2$ANCMet)               
suppressWarnings(dt2$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANCMet))==as.character(as.numeric("NA"))),NA,dt2$ANCMet))
dt2$gageHt <- ifelse((trimws(as.character(dt2$gageHt))==trimws("NA")),NA,dt2$gageHt)               
suppressWarnings(dt2$gageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$gageHt))==as.character(as.numeric("NA"))),NA,dt2$gageHt))
dt2$hydroGraph <- as.factor(ifelse((trimws(as.character(dt2$hydroGraph))==trimws("NA")),NA,as.character(dt2$hydroGraph)))
dt2$flowGageHt <- ifelse((trimws(as.character(dt2$flowGageHt))==trimws("NA")),NA,dt2$flowGageHt)               
suppressWarnings(dt2$flowGageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$flowGageHt))==as.character(as.numeric("NA"))),NA,dt2$flowGageHt))
dt2$fieldCode <- as.factor(ifelse((trimws(as.character(dt2$fieldCode))==trimws("NA")),NA,as.character(dt2$fieldCode)))
dt2$notes <- as.factor(ifelse((trimws(as.character(dt2$notes))==trimws("NA")),NA,as.character(dt2$notes)))
dt2$uniqueID <- as.factor(ifelse((trimws(as.character(dt2$uniqueID))==trimws("NA")),NA,as.character(dt2$uniqueID)))
dt2$Ca <- ifelse((trimws(as.character(dt2$Ca))==trimws("NA")),NA,dt2$Ca)               
suppressWarnings(dt2$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Ca))==as.character(as.numeric("NA"))),NA,dt2$Ca))
dt2$Mg <- ifelse((trimws(as.character(dt2$Mg))==trimws("NA")),NA,dt2$Mg)               
suppressWarnings(dt2$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mg))==as.character(as.numeric("NA"))),NA,dt2$Mg))
dt2$K <- ifelse((trimws(as.character(dt2$K))==trimws("NA")),NA,dt2$K)               
suppressWarnings(dt2$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$K))==as.character(as.numeric("NA"))),NA,dt2$K))
dt2$Na <- ifelse((trimws(as.character(dt2$Na))==trimws("NA")),NA,dt2$Na)               
suppressWarnings(dt2$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Na))==as.character(as.numeric("NA"))),NA,dt2$Na))
dt2$TMAl <- ifelse((trimws(as.character(dt2$TMAl))==trimws("NA")),NA,dt2$TMAl)               
suppressWarnings(dt2$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TMAl))==as.character(as.numeric("NA"))),NA,dt2$TMAl))
dt2$OMAl <- ifelse((trimws(as.character(dt2$OMAl))==trimws("NA")),NA,dt2$OMAl)               
suppressWarnings(dt2$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$OMAl))==as.character(as.numeric("NA"))),NA,dt2$OMAl))
dt2$Al_ICP <- ifelse((trimws(as.character(dt2$Al_ICP))==trimws("NA")),NA,dt2$Al_ICP)               
suppressWarnings(dt2$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ICP))==as.character(as.numeric("NA"))),NA,dt2$Al_ICP))
dt2$NH4 <- ifelse((trimws(as.character(dt2$NH4))==trimws("NA")),NA,dt2$NH4)               
suppressWarnings(dt2$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NH4))==as.character(as.numeric("NA"))),NA,dt2$NH4))
dt2$SO4 <- ifelse((trimws(as.character(dt2$SO4))==trimws("NA")),NA,dt2$SO4)               
suppressWarnings(dt2$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SO4))==as.character(as.numeric("NA"))),NA,dt2$SO4))
dt2$NO3 <- ifelse((trimws(as.character(dt2$NO3))==trimws("NA")),NA,dt2$NO3)               
suppressWarnings(dt2$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NO3))==as.character(as.numeric("NA"))),NA,dt2$NO3))
dt2$Cl <- ifelse((trimws(as.character(dt2$Cl))==trimws("NA")),NA,dt2$Cl)               
suppressWarnings(dt2$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Cl))==as.character(as.numeric("NA"))),NA,dt2$Cl))
dt2$PO4 <- ifelse((trimws(as.character(dt2$PO4))==trimws("NA")),NA,dt2$PO4)               
suppressWarnings(dt2$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$PO4))==as.character(as.numeric("NA"))),NA,dt2$PO4))
dt2$DOC <- ifelse((trimws(as.character(dt2$DOC))==trimws("NA")),NA,dt2$DOC)               
suppressWarnings(dt2$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DOC))==as.character(as.numeric("NA"))),NA,dt2$DOC))
dt2$TDN <- ifelse((trimws(as.character(dt2$TDN))==trimws("NA")),NA,dt2$TDN)               
suppressWarnings(dt2$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TDN))==as.character(as.numeric("NA"))),NA,dt2$TDN))
dt2$DON <- ifelse((trimws(as.character(dt2$DON))==trimws("NA")),NA,dt2$DON)               
suppressWarnings(dt2$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DON))==as.character(as.numeric("NA"))),NA,dt2$DON))
dt2$SiO2 <- ifelse((trimws(as.character(dt2$SiO2))==trimws("NA")),NA,dt2$SiO2)               
suppressWarnings(dt2$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SiO2))==as.character(as.numeric("NA"))),NA,dt2$SiO2))
dt2$Mn <- ifelse((trimws(as.character(dt2$Mn))==trimws("NA")),NA,dt2$Mn)               
suppressWarnings(dt2$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mn))==as.character(as.numeric("NA"))),NA,dt2$Mn))
dt2$Fe <- ifelse((trimws(as.character(dt2$Fe))==trimws("NA")),NA,dt2$Fe)               
suppressWarnings(dt2$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Fe))==as.character(as.numeric("NA"))),NA,dt2$Fe))
dt2$F <- ifelse((trimws(as.character(dt2$F))==trimws("NA")),NA,dt2$F)               
suppressWarnings(dt2$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$F))==as.character(as.numeric("NA"))),NA,dt2$F))
dt2$cationCharge <- ifelse((trimws(as.character(dt2$cationCharge))==trimws("NA")),NA,dt2$cationCharge)               
suppressWarnings(dt2$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$cationCharge))==as.character(as.numeric("NA"))),NA,dt2$cationCharge))
dt2$anionCharge <- ifelse((trimws(as.character(dt2$anionCharge))==trimws("NA")),NA,dt2$anionCharge)               
suppressWarnings(dt2$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$anionCharge))==as.character(as.numeric("NA"))),NA,dt2$anionCharge))
dt2$ionError <- ifelse((trimws(as.character(dt2$ionError))==trimws("NA")),NA,dt2$ionError)               
suppressWarnings(dt2$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionError))==as.character(as.numeric("NA"))),NA,dt2$ionError))
dt2$duplicate <- as.factor(ifelse((trimws(as.character(dt2$duplicate))==trimws("NA")),NA,as.character(dt2$duplicate)))
dt2$sampleType <- as.factor(ifelse((trimws(as.character(dt2$sampleType))==trimws("NA")),NA,as.character(dt2$sampleType)))
dt2$ionBalance <- ifelse((trimws(as.character(dt2$ionBalance))==trimws("NA")),NA,dt2$ionBalance)               
suppressWarnings(dt2$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionBalance))==as.character(as.numeric("NA"))),NA,dt2$ionBalance))
dt2$canonical <- as.factor(ifelse((trimws(as.character(dt2$canonical))==trimws("NA")),NA,as.character(dt2$canonical)))
dt2$pHmetrohm <- as.factor(ifelse((trimws(as.character(dt2$pHmetrohm))==trimws("NA")),NA,as.character(dt2$pHmetrohm)))

names(dt2)


###########################


##
# make a new DATE column, formatted as a date.
dt2$DATE<-as.Date(dt2$date, format="%y/%m/%d")
dt2$Year<-year(dt2$DATE)
dt2$DOY<-yday(dt2$DATE)
dt2$water_year<-as.factor(dt2$waterYr)



##
# this shows all the data available for DOC values

streamview<-ggplot(dt2, aes(x=waterYr, y=DOC, group=waterYr, col=DOY))+geom_boxplot+geom_line()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~site, scales="free_y")+
  ggtitle("DOC data availability. Each line is a year. Figure shows data going back to 1980 ")+
  xlab("Water year (June 1)")

streamview






# subset the record for only watershed 6 stream gauge
W6<-dt2[dt2$site=="W6",]
W1<-dt2[dt2$site=="W1",]

f6<-rbind(W6, W1)


# Calculate the annual means
dsum<-aggregate(list(annCa=f6$Ca, annSiO2=f6$SiO2, annpH=f6$pH, annANC=f6$ANC960, annAl=f6$Al_ICP, annDOC=f6$DOC), by=list(waterYr=f6$waterYr,site=f6$site ),FUN="mean", na.rm=T)
head(dsum)


#####################################################

# try graph with individual panels, not facet_wrapped
head(dsum)

dsum[dsum$site=="W6","Site"]<-"W6 reference watershed"
dsum[dsum$site=="W1","Site"]<-"W1 Calcium treatment"

table(dsum$site)

d1<-ggplot(dsum, aes(x=waterYr, y=annCa ,group=Site))+geom_line()+geom_point(aes(shape=Site), size=3)+
  scale_shape_manual(values=c(1,19))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(aes(xintercept=1999), linetype="dashed")+
  theme(legend.position = "bottom")  +
  xlab("Year")+ylab("Ca2+ (ueq/L)")
d1

d2<-ggplot(dsum, aes(x=waterYr, y=annCa ,group=Site))+geom_line()+geom_point(aes(shape=Site), size=3)+
  scale_shape_manual(values=c(1,19))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(aes(xintercept=1999), linetype="dashed")+
  theme(legend.position = "bottom")  +
  xlab("Year")+ylab("Ca2+ (ueq/L)")
d2



#########################


# this line writes the html file to create interactive graphs for the online book
#htmlwidgets::saveWidget(as_widget(ddd), "  .html")

######################

## with the facet_wrap

library(tidyr)
head(dsum)
d<-gather(dsum, "vars","value",3:8)



head(d)
d[d$vars=="annCa","Variable"]<-"Mean annual Ca"
d[d$vars=="annSiO2","Variable"]<-"Mean annual SiO2"
d[d$vars=="annpH","Variable"]<-"Mean annual pH"
d[d$vars=="annANC","Variable"]<-"Mean annual ANC"
d[d$vars=="annAl","Variable"]<-"Mean annual Al"
d[d$vars=="annDOC","Variable"]<-"Mean annual DOC"

d[d$site=="W6","Site"]<-"W6 reference watershed"
d[d$site=="W1","Site"]<-"W1 Calcium treatment"

d$Variable<-factor(d$Variable, levels=c("Mean annual Ca","Mean annual SiO2","Mean annual pH","Mean annual ANC","Mean annual Al", "Mean annual DOC" ))

# this shows all the data available for nitrate for the stream gauge in watershed 6
dd<-ggplot(d, aes(x=waterYr, y=value ,group=Site, col=Site))+geom_point(aes(shape=Site))+geom_line()+
  scale_shape_manual(values=c(1,19))+guides(col=FALSE)+
  scale_color_manual(values=c("grey","black"))+
  geom_vline(aes(xintercept=1999), linetype="dashed")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Similar magnitude analyte values")+ylab("Analyte ")+
  facet_wrap(~Variable, nrow=6, scales="free_y")

ddd<-ggplotly(dd)
ddd

htmlwidgets::saveWidget(as_widget(ddd), "acidDeposition/fig6_steam_chem_facet_wrapped.html")



#For figure 5
# dsum<-aggregate(list(annSO4=f6$SO4, annNO3=f6$NO3, annpH=f6$pH, annANC=f6$ANC960, annAl=f6$Al_ICP, ann2DOC=f6$DOC), by=list(waterYr=f6$waterYr,site=f6$site ),FUN="mean", na.rm=T)
# head(dsum)
# 
# 
# library(tidyr)
# dsug<-gather(dsum, "vars","value", 3:8)
# 
# table(dsug$vars)
# 
# dsug[dsug$vars=="annSO4","Variable"]<-"SO4 (eq/L)"
# dsug[dsug$vars=="annNO3","Variable"]<-"NO3 (eq/L)"
# dsug[dsug$vars=="annpH","Variable"]<-"pH"
# dsug[dsug$vars=="annANC","Variable"]<-"ANC (umol/L)"
# dsug[dsug$vars=="annAl","Variable"]<-"Al (umol/L)"
# dsug[dsug$vars=="ann2DOC","Variable"]<-"DOC (umol/L)"
