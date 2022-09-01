

##  Fig 8 forest management



# watershed read in: Ws2, ws4, ws5

# 8/24/2022 - Alex Yo0ung alexyoung.116@gmail.com


library(lubridate)
library(tidyr)
library(ggplot2)

##################   WS2     ###############################################

# Package ID: knb-lter-hbr.4.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 2, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/4/17/a6aeef15070be913ee2f06f431b9b7a7" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
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

if (class(dt1$Month)!="factor") dt1$Month<- as.factor(dt1$Month)
if (class(dt1$Year_Month)!="factor") dt1$Year_Month<- as.factor(dt1$Year_Month)
if (class(dt1$flow_mm)=="factor") dt1$flow_mm <-as.numeric(levels(dt1$flow_mm))[as.integer(dt1$flow_mm) ]               
if (class(dt1$flow_mm)=="character") dt1$flow_mm <-as.numeric(dt1$flow_mm)
if (class(dt1$Ca_flux)=="factor") dt1$Ca_flux <-as.numeric(levels(dt1$Ca_flux))[as.integer(dt1$Ca_flux) ]               
if (class(dt1$Ca_flux)=="character") dt1$Ca_flux <-as.numeric(dt1$Ca_flux)
if (class(dt1$Mg_flux)=="factor") dt1$Mg_flux <-as.numeric(levels(dt1$Mg_flux))[as.integer(dt1$Mg_flux) ]               
if (class(dt1$Mg_flux)=="character") dt1$Mg_flux <-as.numeric(dt1$Mg_flux)
if (class(dt1$K_flux)=="factor") dt1$K_flux <-as.numeric(levels(dt1$K_flux))[as.integer(dt1$K_flux) ]               
if (class(dt1$K_flux)=="character") dt1$K_flux <-as.numeric(dt1$K_flux)
if (class(dt1$Na_flux)=="factor") dt1$Na_flux <-as.numeric(levels(dt1$Na_flux))[as.integer(dt1$Na_flux) ]               
if (class(dt1$Na_flux)=="character") dt1$Na_flux <-as.numeric(dt1$Na_flux)
if (class(dt1$Al_Ferron_flux)=="factor") dt1$Al_Ferron_flux <-as.numeric(levels(dt1$Al_Ferron_flux))[as.integer(dt1$Al_Ferron_flux) ]               
if (class(dt1$Al_Ferron_flux)=="character") dt1$Al_Ferron_flux <-as.numeric(dt1$Al_Ferron_flux)
if (class(dt1$TMAl_flux)=="factor") dt1$TMAl_flux <-as.numeric(levels(dt1$TMAl_flux))[as.integer(dt1$TMAl_flux) ]               
if (class(dt1$TMAl_flux)=="character") dt1$TMAl_flux <-as.numeric(dt1$TMAl_flux)
if (class(dt1$OMAl_flux)=="factor") dt1$OMAl_flux <-as.numeric(levels(dt1$OMAl_flux))[as.integer(dt1$OMAl_flux) ]               
if (class(dt1$OMAl_flux)=="character") dt1$OMAl_flux <-as.numeric(dt1$OMAl_flux)
if (class(dt1$Al_ICP_flux)=="factor") dt1$Al_ICP_flux <-as.numeric(levels(dt1$Al_ICP_flux))[as.integer(dt1$Al_ICP_flux) ]               
if (class(dt1$Al_ICP_flux)=="character") dt1$Al_ICP_flux <-as.numeric(dt1$Al_ICP_flux)
if (class(dt1$NH4_flux)=="factor") dt1$NH4_flux <-as.numeric(levels(dt1$NH4_flux))[as.integer(dt1$NH4_flux) ]               
if (class(dt1$NH4_flux)=="character") dt1$NH4_flux <-as.numeric(dt1$NH4_flux)
if (class(dt1$SO4_flux)=="factor") dt1$SO4_flux <-as.numeric(levels(dt1$SO4_flux))[as.integer(dt1$SO4_flux) ]               
if (class(dt1$SO4_flux)=="character") dt1$SO4_flux <-as.numeric(dt1$SO4_flux)
if (class(dt1$NO3_flux)=="factor") dt1$NO3_flux <-as.numeric(levels(dt1$NO3_flux))[as.integer(dt1$NO3_flux) ]               
if (class(dt1$NO3_flux)=="character") dt1$NO3_flux <-as.numeric(dt1$NO3_flux)
if (class(dt1$Cl_flux)=="factor") dt1$Cl_flux <-as.numeric(levels(dt1$Cl_flux))[as.integer(dt1$Cl_flux) ]               
if (class(dt1$Cl_flux)=="character") dt1$Cl_flux <-as.numeric(dt1$Cl_flux)
if (class(dt1$PO4_flux)=="factor") dt1$PO4_flux <-as.numeric(levels(dt1$PO4_flux))[as.integer(dt1$PO4_flux) ]               
if (class(dt1$PO4_flux)=="character") dt1$PO4_flux <-as.numeric(dt1$PO4_flux)
if (class(dt1$DOC_flux)=="factor") dt1$DOC_flux <-as.numeric(levels(dt1$DOC_flux))[as.integer(dt1$DOC_flux) ]               
if (class(dt1$DOC_flux)=="character") dt1$DOC_flux <-as.numeric(dt1$DOC_flux)
if (class(dt1$TDN_flux)=="factor") dt1$TDN_flux <-as.numeric(levels(dt1$TDN_flux))[as.integer(dt1$TDN_flux) ]               
if (class(dt1$TDN_flux)=="character") dt1$TDN_flux <-as.numeric(dt1$TDN_flux)
if (class(dt1$DON_flux)=="factor") dt1$DON_flux <-as.numeric(levels(dt1$DON_flux))[as.integer(dt1$DON_flux) ]               
if (class(dt1$DON_flux)=="character") dt1$DON_flux <-as.numeric(dt1$DON_flux)
if (class(dt1$DIC_flux)=="factor") dt1$DIC_flux <-as.numeric(levels(dt1$DIC_flux))[as.integer(dt1$DIC_flux) ]               
if (class(dt1$DIC_flux)=="character") dt1$DIC_flux <-as.numeric(dt1$DIC_flux)
if (class(dt1$SiO2_flux)=="factor") dt1$SiO2_flux <-as.numeric(levels(dt1$SiO2_flux))[as.integer(dt1$SiO2_flux) ]               
if (class(dt1$SiO2_flux)=="character") dt1$SiO2_flux <-as.numeric(dt1$SiO2_flux)
if (class(dt1$Mn_flux)=="factor") dt1$Mn_flux <-as.numeric(levels(dt1$Mn_flux))[as.integer(dt1$Mn_flux) ]               
if (class(dt1$Mn_flux)=="character") dt1$Mn_flux <-as.numeric(dt1$Mn_flux)
if (class(dt1$Fe_flux)=="factor") dt1$Fe_flux <-as.numeric(levels(dt1$Fe_flux))[as.integer(dt1$Fe_flux) ]               
if (class(dt1$Fe_flux)=="character") dt1$Fe_flux <-as.numeric(dt1$Fe_flux)
if (class(dt1$F_flux)=="factor") dt1$F_flux <-as.numeric(levels(dt1$F_flux))[as.integer(dt1$F_flux) ]               
if (class(dt1$F_flux)=="character") dt1$F_flux <-as.numeric(dt1$F_flux)
if (class(dt1$H_flux)=="factor") dt1$H_flux <-as.numeric(levels(dt1$H_flux))[as.integer(dt1$H_flux) ]               
if (class(dt1$H_flux)=="character") dt1$H_flux <-as.numeric(dt1$H_flux)
if (class(dt1$pH_volwt)=="factor") dt1$pH_volwt <-as.numeric(levels(dt1$pH_volwt))[as.integer(dt1$pH_volwt) ]               
if (class(dt1$pH_volwt)=="character") dt1$pH_volwt <-as.numeric(dt1$pH_volwt)
if (class(dt1$SpecCond_volwt)=="factor") dt1$SpecCond_volwt <-as.numeric(levels(dt1$SpecCond_volwt))[as.integer(dt1$SpecCond_volwt) ]               
if (class(dt1$SpecCond_volwt)=="character") dt1$SpecCond_volwt <-as.numeric(dt1$SpecCond_volwt)
if (class(dt1$ANC_volwt)=="factor") dt1$ANC_volwt <-as.numeric(levels(dt1$ANC_volwt))[as.integer(dt1$ANC_volwt) ]               
if (class(dt1$ANC_volwt)=="character") dt1$ANC_volwt <-as.numeric(dt1$ANC_volwt)

# Convert Missing Values to NA for non-dates

dt1$flow_mm <- ifelse((trimws(as.character(dt1$flow_mm))==trimws("-888.88")),NA,dt1$flow_mm)               
suppressWarnings(dt1$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt1$flow_mm))
dt1$Ca_flux <- ifelse((trimws(as.character(dt1$Ca_flux))==trimws("-888.88")),NA,dt1$Ca_flux)               
suppressWarnings(dt1$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_flux))
dt1$Mg_flux <- ifelse((trimws(as.character(dt1$Mg_flux))==trimws("-888.88")),NA,dt1$Mg_flux)               
suppressWarnings(dt1$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_flux))
dt1$K_flux <- ifelse((trimws(as.character(dt1$K_flux))==trimws("-888.88")),NA,dt1$K_flux)               
suppressWarnings(dt1$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_flux))==as.character(as.numeric("-888.88"))),NA,dt1$K_flux))
dt1$Na_flux <- ifelse((trimws(as.character(dt1$Na_flux))==trimws("-888.88")),NA,dt1$Na_flux)               
suppressWarnings(dt1$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Na_flux))
dt1$Al_Ferron_flux <- ifelse((trimws(as.character(dt1$Al_Ferron_flux))==trimws("-888.88")),NA,dt1$Al_Ferron_flux)               
suppressWarnings(dt1$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_Ferron_flux))
dt1$TMAl_flux <- ifelse((trimws(as.character(dt1$TMAl_flux))==trimws("-888.88")),NA,dt1$TMAl_flux)               
suppressWarnings(dt1$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TMAl_flux))
dt1$OMAl_flux <- ifelse((trimws(as.character(dt1$OMAl_flux))==trimws("-888.88")),NA,dt1$OMAl_flux)               
suppressWarnings(dt1$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$OMAl_flux))
dt1$Al_ICP_flux <- ifelse((trimws(as.character(dt1$Al_ICP_flux))==trimws("-888.88")),NA,dt1$Al_ICP_flux)               
suppressWarnings(dt1$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_ICP_flux))
dt1$NH4_flux <- ifelse((trimws(as.character(dt1$NH4_flux))==trimws("-888.88")),NA,dt1$NH4_flux)               
suppressWarnings(dt1$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NH4_flux))
dt1$SO4_flux <- ifelse((trimws(as.character(dt1$SO4_flux))==trimws("-888.88")),NA,dt1$SO4_flux)               
suppressWarnings(dt1$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SO4_flux))
dt1$NO3_flux <- ifelse((trimws(as.character(dt1$NO3_flux))==trimws("-888.88")),NA,dt1$NO3_flux)               
suppressWarnings(dt1$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NO3_flux))
dt1$Cl_flux <- ifelse((trimws(as.character(dt1$Cl_flux))==trimws("-888.88")),NA,dt1$Cl_flux)               
suppressWarnings(dt1$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Cl_flux))
dt1$PO4_flux <- ifelse((trimws(as.character(dt1$PO4_flux))==trimws("-888.88")),NA,dt1$PO4_flux)               
suppressWarnings(dt1$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$PO4_flux))
dt1$DOC_flux <- ifelse((trimws(as.character(dt1$DOC_flux))==trimws("-888.88")),NA,dt1$DOC_flux)               
suppressWarnings(dt1$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DOC_flux))
dt1$TDN_flux <- ifelse((trimws(as.character(dt1$TDN_flux))==trimws("-888.88")),NA,dt1$TDN_flux)               
suppressWarnings(dt1$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TDN_flux))
dt1$DON_flux <- ifelse((trimws(as.character(dt1$DON_flux))==trimws("-888.88")),NA,dt1$DON_flux)               
suppressWarnings(dt1$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DON_flux))
dt1$DIC_flux <- ifelse((trimws(as.character(dt1$DIC_flux))==trimws("-888.88")),NA,dt1$DIC_flux)               
suppressWarnings(dt1$DIC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DIC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DIC_flux))
dt1$SiO2_flux <- ifelse((trimws(as.character(dt1$SiO2_flux))==trimws("-888.88")),NA,dt1$SiO2_flux)               
suppressWarnings(dt1$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SiO2_flux))
dt1$Mn_flux <- ifelse((trimws(as.character(dt1$Mn_flux))==trimws("-888.88")),NA,dt1$Mn_flux)               
suppressWarnings(dt1$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_flux))
dt1$Fe_flux <- ifelse((trimws(as.character(dt1$Fe_flux))==trimws("-888.88")),NA,dt1$Fe_flux)               
suppressWarnings(dt1$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_flux))
dt1$F_flux <- ifelse((trimws(as.character(dt1$F_flux))==trimws("-888.88")),NA,dt1$F_flux)               
suppressWarnings(dt1$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$F_flux))==as.character(as.numeric("-888.88"))),NA,dt1$F_flux))
dt1$H_flux <- ifelse((trimws(as.character(dt1$H_flux))==trimws("-888.88")),NA,dt1$H_flux)               
suppressWarnings(dt1$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$H_flux))==as.character(as.numeric("-888.88"))),NA,dt1$H_flux))
dt1$pH_volwt <- ifelse((trimws(as.character(dt1$pH_volwt))==trimws("-888.88")),NA,dt1$pH_volwt)               
suppressWarnings(dt1$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$pH_volwt))
dt1$SpecCond_volwt <- ifelse((trimws(as.character(dt1$SpecCond_volwt))==trimws("-888.88")),NA,dt1$SpecCond_volwt)               
suppressWarnings(dt1$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$SpecCond_volwt))
dt1$ANC_volwt <- ifelse((trimws(as.character(dt1$ANC_volwt))==trimws("-888.88")),NA,dt1$ANC_volwt)               
suppressWarnings(dt1$ANC_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ANC_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$ANC_volwt))


# Here is the structure of the input data frame:
str(dt1)                            

W2<-dt1
W2$Watershed<-"W2"
head(W2)

### Now read in WS4, strip cutting

############################    WS4   ##########################################
# Package ID: knb-lter-hbr.6.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 4, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/6/17/54b3ae4a45a2bb6c7006c2ab45cf63b9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
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

if (class(dt1$Month)!="factor") dt1$Month<- as.factor(dt1$Month)
if (class(dt1$Year_Month)!="factor") dt1$Year_Month<- as.factor(dt1$Year_Month)
if (class(dt1$flow_mm)=="factor") dt1$flow_mm <-as.numeric(levels(dt1$flow_mm))[as.integer(dt1$flow_mm) ]               
if (class(dt1$flow_mm)=="character") dt1$flow_mm <-as.numeric(dt1$flow_mm)
if (class(dt1$Ca_flux)=="factor") dt1$Ca_flux <-as.numeric(levels(dt1$Ca_flux))[as.integer(dt1$Ca_flux) ]               
if (class(dt1$Ca_flux)=="character") dt1$Ca_flux <-as.numeric(dt1$Ca_flux)
if (class(dt1$Mg_flux)=="factor") dt1$Mg_flux <-as.numeric(levels(dt1$Mg_flux))[as.integer(dt1$Mg_flux) ]               
if (class(dt1$Mg_flux)=="character") dt1$Mg_flux <-as.numeric(dt1$Mg_flux)
if (class(dt1$K_flux)=="factor") dt1$K_flux <-as.numeric(levels(dt1$K_flux))[as.integer(dt1$K_flux) ]               
if (class(dt1$K_flux)=="character") dt1$K_flux <-as.numeric(dt1$K_flux)
if (class(dt1$Na_flux)=="factor") dt1$Na_flux <-as.numeric(levels(dt1$Na_flux))[as.integer(dt1$Na_flux) ]               
if (class(dt1$Na_flux)=="character") dt1$Na_flux <-as.numeric(dt1$Na_flux)
if (class(dt1$Al_Ferron_flux)=="factor") dt1$Al_Ferron_flux <-as.numeric(levels(dt1$Al_Ferron_flux))[as.integer(dt1$Al_Ferron_flux) ]               
if (class(dt1$Al_Ferron_flux)=="character") dt1$Al_Ferron_flux <-as.numeric(dt1$Al_Ferron_flux)
if (class(dt1$TMAl_flux)=="factor") dt1$TMAl_flux <-as.numeric(levels(dt1$TMAl_flux))[as.integer(dt1$TMAl_flux) ]               
if (class(dt1$TMAl_flux)=="character") dt1$TMAl_flux <-as.numeric(dt1$TMAl_flux)
if (class(dt1$OMAl_flux)=="factor") dt1$OMAl_flux <-as.numeric(levels(dt1$OMAl_flux))[as.integer(dt1$OMAl_flux) ]               
if (class(dt1$OMAl_flux)=="character") dt1$OMAl_flux <-as.numeric(dt1$OMAl_flux)
if (class(dt1$Al_ICP_flux)=="factor") dt1$Al_ICP_flux <-as.numeric(levels(dt1$Al_ICP_flux))[as.integer(dt1$Al_ICP_flux) ]               
if (class(dt1$Al_ICP_flux)=="character") dt1$Al_ICP_flux <-as.numeric(dt1$Al_ICP_flux)
if (class(dt1$NH4_flux)=="factor") dt1$NH4_flux <-as.numeric(levels(dt1$NH4_flux))[as.integer(dt1$NH4_flux) ]               
if (class(dt1$NH4_flux)=="character") dt1$NH4_flux <-as.numeric(dt1$NH4_flux)
if (class(dt1$SO4_flux)=="factor") dt1$SO4_flux <-as.numeric(levels(dt1$SO4_flux))[as.integer(dt1$SO4_flux) ]               
if (class(dt1$SO4_flux)=="character") dt1$SO4_flux <-as.numeric(dt1$SO4_flux)
if (class(dt1$NO3_flux)=="factor") dt1$NO3_flux <-as.numeric(levels(dt1$NO3_flux))[as.integer(dt1$NO3_flux) ]               
if (class(dt1$NO3_flux)=="character") dt1$NO3_flux <-as.numeric(dt1$NO3_flux)
if (class(dt1$Cl_flux)=="factor") dt1$Cl_flux <-as.numeric(levels(dt1$Cl_flux))[as.integer(dt1$Cl_flux) ]               
if (class(dt1$Cl_flux)=="character") dt1$Cl_flux <-as.numeric(dt1$Cl_flux)
if (class(dt1$PO4_flux)=="factor") dt1$PO4_flux <-as.numeric(levels(dt1$PO4_flux))[as.integer(dt1$PO4_flux) ]               
if (class(dt1$PO4_flux)=="character") dt1$PO4_flux <-as.numeric(dt1$PO4_flux)
if (class(dt1$DOC_flux)=="factor") dt1$DOC_flux <-as.numeric(levels(dt1$DOC_flux))[as.integer(dt1$DOC_flux) ]               
if (class(dt1$DOC_flux)=="character") dt1$DOC_flux <-as.numeric(dt1$DOC_flux)
if (class(dt1$TDN_flux)=="factor") dt1$TDN_flux <-as.numeric(levels(dt1$TDN_flux))[as.integer(dt1$TDN_flux) ]               
if (class(dt1$TDN_flux)=="character") dt1$TDN_flux <-as.numeric(dt1$TDN_flux)
if (class(dt1$DON_flux)=="factor") dt1$DON_flux <-as.numeric(levels(dt1$DON_flux))[as.integer(dt1$DON_flux) ]               
if (class(dt1$DON_flux)=="character") dt1$DON_flux <-as.numeric(dt1$DON_flux)
if (class(dt1$DIC_flux)=="factor") dt1$DIC_flux <-as.numeric(levels(dt1$DIC_flux))[as.integer(dt1$DIC_flux) ]               
if (class(dt1$DIC_flux)=="character") dt1$DIC_flux <-as.numeric(dt1$DIC_flux)
if (class(dt1$SiO2_flux)=="factor") dt1$SiO2_flux <-as.numeric(levels(dt1$SiO2_flux))[as.integer(dt1$SiO2_flux) ]               
if (class(dt1$SiO2_flux)=="character") dt1$SiO2_flux <-as.numeric(dt1$SiO2_flux)
if (class(dt1$Mn_flux)=="factor") dt1$Mn_flux <-as.numeric(levels(dt1$Mn_flux))[as.integer(dt1$Mn_flux) ]               
if (class(dt1$Mn_flux)=="character") dt1$Mn_flux <-as.numeric(dt1$Mn_flux)
if (class(dt1$Fe_flux)=="factor") dt1$Fe_flux <-as.numeric(levels(dt1$Fe_flux))[as.integer(dt1$Fe_flux) ]               
if (class(dt1$Fe_flux)=="character") dt1$Fe_flux <-as.numeric(dt1$Fe_flux)
if (class(dt1$F_flux)=="factor") dt1$F_flux <-as.numeric(levels(dt1$F_flux))[as.integer(dt1$F_flux) ]               
if (class(dt1$F_flux)=="character") dt1$F_flux <-as.numeric(dt1$F_flux)
if (class(dt1$H_flux)=="factor") dt1$H_flux <-as.numeric(levels(dt1$H_flux))[as.integer(dt1$H_flux) ]               
if (class(dt1$H_flux)=="character") dt1$H_flux <-as.numeric(dt1$H_flux)
if (class(dt1$pH_volwt)=="factor") dt1$pH_volwt <-as.numeric(levels(dt1$pH_volwt))[as.integer(dt1$pH_volwt) ]               
if (class(dt1$pH_volwt)=="character") dt1$pH_volwt <-as.numeric(dt1$pH_volwt)
if (class(dt1$SpecCond_volwt)=="factor") dt1$SpecCond_volwt <-as.numeric(levels(dt1$SpecCond_volwt))[as.integer(dt1$SpecCond_volwt) ]               
if (class(dt1$SpecCond_volwt)=="character") dt1$SpecCond_volwt <-as.numeric(dt1$SpecCond_volwt)
if (class(dt1$ANC_volwt)=="factor") dt1$ANC_volwt <-as.numeric(levels(dt1$ANC_volwt))[as.integer(dt1$ANC_volwt) ]               
if (class(dt1$ANC_volwt)=="character") dt1$ANC_volwt <-as.numeric(dt1$ANC_volwt)

# Convert Missing Values to NA for non-dates

dt1$flow_mm <- ifelse((trimws(as.character(dt1$flow_mm))==trimws("-888.88")),NA,dt1$flow_mm)               
suppressWarnings(dt1$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt1$flow_mm))
dt1$Ca_flux <- ifelse((trimws(as.character(dt1$Ca_flux))==trimws("-888.88")),NA,dt1$Ca_flux)               
suppressWarnings(dt1$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_flux))
dt1$Mg_flux <- ifelse((trimws(as.character(dt1$Mg_flux))==trimws("-888.88")),NA,dt1$Mg_flux)               
suppressWarnings(dt1$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_flux))
dt1$K_flux <- ifelse((trimws(as.character(dt1$K_flux))==trimws("-888.88")),NA,dt1$K_flux)               
suppressWarnings(dt1$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_flux))==as.character(as.numeric("-888.88"))),NA,dt1$K_flux))
dt1$Na_flux <- ifelse((trimws(as.character(dt1$Na_flux))==trimws("-888.88")),NA,dt1$Na_flux)               
suppressWarnings(dt1$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Na_flux))
dt1$Al_Ferron_flux <- ifelse((trimws(as.character(dt1$Al_Ferron_flux))==trimws("-888.88")),NA,dt1$Al_Ferron_flux)               
suppressWarnings(dt1$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_Ferron_flux))
dt1$TMAl_flux <- ifelse((trimws(as.character(dt1$TMAl_flux))==trimws("-888.88")),NA,dt1$TMAl_flux)               
suppressWarnings(dt1$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TMAl_flux))
dt1$OMAl_flux <- ifelse((trimws(as.character(dt1$OMAl_flux))==trimws("-888.88")),NA,dt1$OMAl_flux)               
suppressWarnings(dt1$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$OMAl_flux))
dt1$Al_ICP_flux <- ifelse((trimws(as.character(dt1$Al_ICP_flux))==trimws("-888.88")),NA,dt1$Al_ICP_flux)               
suppressWarnings(dt1$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_ICP_flux))
dt1$NH4_flux <- ifelse((trimws(as.character(dt1$NH4_flux))==trimws("-888.88")),NA,dt1$NH4_flux)               
suppressWarnings(dt1$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NH4_flux))
dt1$SO4_flux <- ifelse((trimws(as.character(dt1$SO4_flux))==trimws("-888.88")),NA,dt1$SO4_flux)               
suppressWarnings(dt1$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SO4_flux))
dt1$NO3_flux <- ifelse((trimws(as.character(dt1$NO3_flux))==trimws("-888.88")),NA,dt1$NO3_flux)               
suppressWarnings(dt1$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NO3_flux))
dt1$Cl_flux <- ifelse((trimws(as.character(dt1$Cl_flux))==trimws("-888.88")),NA,dt1$Cl_flux)               
suppressWarnings(dt1$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Cl_flux))
dt1$PO4_flux <- ifelse((trimws(as.character(dt1$PO4_flux))==trimws("-888.88")),NA,dt1$PO4_flux)               
suppressWarnings(dt1$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$PO4_flux))
dt1$DOC_flux <- ifelse((trimws(as.character(dt1$DOC_flux))==trimws("-888.88")),NA,dt1$DOC_flux)               
suppressWarnings(dt1$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DOC_flux))
dt1$TDN_flux <- ifelse((trimws(as.character(dt1$TDN_flux))==trimws("-888.88")),NA,dt1$TDN_flux)               
suppressWarnings(dt1$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TDN_flux))
dt1$DON_flux <- ifelse((trimws(as.character(dt1$DON_flux))==trimws("-888.88")),NA,dt1$DON_flux)               
suppressWarnings(dt1$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DON_flux))
dt1$DIC_flux <- ifelse((trimws(as.character(dt1$DIC_flux))==trimws("-888.88")),NA,dt1$DIC_flux)               
suppressWarnings(dt1$DIC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DIC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DIC_flux))
dt1$SiO2_flux <- ifelse((trimws(as.character(dt1$SiO2_flux))==trimws("-888.88")),NA,dt1$SiO2_flux)               
suppressWarnings(dt1$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SiO2_flux))
dt1$Mn_flux <- ifelse((trimws(as.character(dt1$Mn_flux))==trimws("-888.88")),NA,dt1$Mn_flux)               
suppressWarnings(dt1$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_flux))
dt1$Fe_flux <- ifelse((trimws(as.character(dt1$Fe_flux))==trimws("-888.88")),NA,dt1$Fe_flux)               
suppressWarnings(dt1$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_flux))
dt1$F_flux <- ifelse((trimws(as.character(dt1$F_flux))==trimws("-888.88")),NA,dt1$F_flux)               
suppressWarnings(dt1$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$F_flux))==as.character(as.numeric("-888.88"))),NA,dt1$F_flux))
dt1$H_flux <- ifelse((trimws(as.character(dt1$H_flux))==trimws("-888.88")),NA,dt1$H_flux)               
suppressWarnings(dt1$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$H_flux))==as.character(as.numeric("-888.88"))),NA,dt1$H_flux))
dt1$pH_volwt <- ifelse((trimws(as.character(dt1$pH_volwt))==trimws("-888.88")),NA,dt1$pH_volwt)               
suppressWarnings(dt1$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$pH_volwt))
dt1$SpecCond_volwt <- ifelse((trimws(as.character(dt1$SpecCond_volwt))==trimws("-888.88")),NA,dt1$SpecCond_volwt)               
suppressWarnings(dt1$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$SpecCond_volwt))
dt1$ANC_volwt <- ifelse((trimws(as.character(dt1$ANC_volwt))==trimws("-888.88")),NA,dt1$ANC_volwt)               
suppressWarnings(dt1$ANC_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ANC_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$ANC_volwt))


# Here is the structure of the input data frame:
str(dt1)                            

W4<-dt1
W4$Watershed<-"W4"


### Now Read in WS5, which had the whole tree harvest in 1983-84.

########################   WS5   ########################################
# Package ID: knb-lter-hbr.7.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 5, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/7/17/c08ebaccab4fee5fb60f4eee77f06cb3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
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

if (class(dt1$Month)!="factor") dt1$Month<- as.factor(dt1$Month)
if (class(dt1$Year_Month)!="factor") dt1$Year_Month<- as.factor(dt1$Year_Month)
if (class(dt1$flow_mm)=="factor") dt1$flow_mm <-as.numeric(levels(dt1$flow_mm))[as.integer(dt1$flow_mm) ]               
if (class(dt1$flow_mm)=="character") dt1$flow_mm <-as.numeric(dt1$flow_mm)
if (class(dt1$Ca_flux)=="factor") dt1$Ca_flux <-as.numeric(levels(dt1$Ca_flux))[as.integer(dt1$Ca_flux) ]               
if (class(dt1$Ca_flux)=="character") dt1$Ca_flux <-as.numeric(dt1$Ca_flux)
if (class(dt1$Mg_flux)=="factor") dt1$Mg_flux <-as.numeric(levels(dt1$Mg_flux))[as.integer(dt1$Mg_flux) ]               
if (class(dt1$Mg_flux)=="character") dt1$Mg_flux <-as.numeric(dt1$Mg_flux)
if (class(dt1$K_flux)=="factor") dt1$K_flux <-as.numeric(levels(dt1$K_flux))[as.integer(dt1$K_flux) ]               
if (class(dt1$K_flux)=="character") dt1$K_flux <-as.numeric(dt1$K_flux)
if (class(dt1$Na_flux)=="factor") dt1$Na_flux <-as.numeric(levels(dt1$Na_flux))[as.integer(dt1$Na_flux) ]               
if (class(dt1$Na_flux)=="character") dt1$Na_flux <-as.numeric(dt1$Na_flux)
if (class(dt1$Al_Ferron_flux)=="factor") dt1$Al_Ferron_flux <-as.numeric(levels(dt1$Al_Ferron_flux))[as.integer(dt1$Al_Ferron_flux) ]               
if (class(dt1$Al_Ferron_flux)=="character") dt1$Al_Ferron_flux <-as.numeric(dt1$Al_Ferron_flux)
if (class(dt1$TMAl_flux)=="factor") dt1$TMAl_flux <-as.numeric(levels(dt1$TMAl_flux))[as.integer(dt1$TMAl_flux) ]               
if (class(dt1$TMAl_flux)=="character") dt1$TMAl_flux <-as.numeric(dt1$TMAl_flux)
if (class(dt1$OMAl_flux)=="factor") dt1$OMAl_flux <-as.numeric(levels(dt1$OMAl_flux))[as.integer(dt1$OMAl_flux) ]               
if (class(dt1$OMAl_flux)=="character") dt1$OMAl_flux <-as.numeric(dt1$OMAl_flux)
if (class(dt1$Al_ICP_flux)=="factor") dt1$Al_ICP_flux <-as.numeric(levels(dt1$Al_ICP_flux))[as.integer(dt1$Al_ICP_flux) ]               
if (class(dt1$Al_ICP_flux)=="character") dt1$Al_ICP_flux <-as.numeric(dt1$Al_ICP_flux)
if (class(dt1$NH4_flux)=="factor") dt1$NH4_flux <-as.numeric(levels(dt1$NH4_flux))[as.integer(dt1$NH4_flux) ]               
if (class(dt1$NH4_flux)=="character") dt1$NH4_flux <-as.numeric(dt1$NH4_flux)
if (class(dt1$SO4_flux)=="factor") dt1$SO4_flux <-as.numeric(levels(dt1$SO4_flux))[as.integer(dt1$SO4_flux) ]               
if (class(dt1$SO4_flux)=="character") dt1$SO4_flux <-as.numeric(dt1$SO4_flux)
if (class(dt1$NO3_flux)=="factor") dt1$NO3_flux <-as.numeric(levels(dt1$NO3_flux))[as.integer(dt1$NO3_flux) ]               
if (class(dt1$NO3_flux)=="character") dt1$NO3_flux <-as.numeric(dt1$NO3_flux)
if (class(dt1$Cl_flux)=="factor") dt1$Cl_flux <-as.numeric(levels(dt1$Cl_flux))[as.integer(dt1$Cl_flux) ]               
if (class(dt1$Cl_flux)=="character") dt1$Cl_flux <-as.numeric(dt1$Cl_flux)
if (class(dt1$PO4_flux)=="factor") dt1$PO4_flux <-as.numeric(levels(dt1$PO4_flux))[as.integer(dt1$PO4_flux) ]               
if (class(dt1$PO4_flux)=="character") dt1$PO4_flux <-as.numeric(dt1$PO4_flux)
if (class(dt1$DOC_flux)=="factor") dt1$DOC_flux <-as.numeric(levels(dt1$DOC_flux))[as.integer(dt1$DOC_flux) ]               
if (class(dt1$DOC_flux)=="character") dt1$DOC_flux <-as.numeric(dt1$DOC_flux)
if (class(dt1$TDN_flux)=="factor") dt1$TDN_flux <-as.numeric(levels(dt1$TDN_flux))[as.integer(dt1$TDN_flux) ]               
if (class(dt1$TDN_flux)=="character") dt1$TDN_flux <-as.numeric(dt1$TDN_flux)
if (class(dt1$DON_flux)=="factor") dt1$DON_flux <-as.numeric(levels(dt1$DON_flux))[as.integer(dt1$DON_flux) ]               
if (class(dt1$DON_flux)=="character") dt1$DON_flux <-as.numeric(dt1$DON_flux)
if (class(dt1$DIC_flux)=="factor") dt1$DIC_flux <-as.numeric(levels(dt1$DIC_flux))[as.integer(dt1$DIC_flux) ]               
if (class(dt1$DIC_flux)=="character") dt1$DIC_flux <-as.numeric(dt1$DIC_flux)
if (class(dt1$SiO2_flux)=="factor") dt1$SiO2_flux <-as.numeric(levels(dt1$SiO2_flux))[as.integer(dt1$SiO2_flux) ]               
if (class(dt1$SiO2_flux)=="character") dt1$SiO2_flux <-as.numeric(dt1$SiO2_flux)
if (class(dt1$Mn_flux)=="factor") dt1$Mn_flux <-as.numeric(levels(dt1$Mn_flux))[as.integer(dt1$Mn_flux) ]               
if (class(dt1$Mn_flux)=="character") dt1$Mn_flux <-as.numeric(dt1$Mn_flux)
if (class(dt1$Fe_flux)=="factor") dt1$Fe_flux <-as.numeric(levels(dt1$Fe_flux))[as.integer(dt1$Fe_flux) ]               
if (class(dt1$Fe_flux)=="character") dt1$Fe_flux <-as.numeric(dt1$Fe_flux)
if (class(dt1$F_flux)=="factor") dt1$F_flux <-as.numeric(levels(dt1$F_flux))[as.integer(dt1$F_flux) ]               
if (class(dt1$F_flux)=="character") dt1$F_flux <-as.numeric(dt1$F_flux)
if (class(dt1$H_flux)=="factor") dt1$H_flux <-as.numeric(levels(dt1$H_flux))[as.integer(dt1$H_flux) ]               
if (class(dt1$H_flux)=="character") dt1$H_flux <-as.numeric(dt1$H_flux)
if (class(dt1$pH_volwt)=="factor") dt1$pH_volwt <-as.numeric(levels(dt1$pH_volwt))[as.integer(dt1$pH_volwt) ]               
if (class(dt1$pH_volwt)=="character") dt1$pH_volwt <-as.numeric(dt1$pH_volwt)
if (class(dt1$SpecCond_volwt)=="factor") dt1$SpecCond_volwt <-as.numeric(levels(dt1$SpecCond_volwt))[as.integer(dt1$SpecCond_volwt) ]               
if (class(dt1$SpecCond_volwt)=="character") dt1$SpecCond_volwt <-as.numeric(dt1$SpecCond_volwt)
if (class(dt1$ANC_volwt)=="factor") dt1$ANC_volwt <-as.numeric(levels(dt1$ANC_volwt))[as.integer(dt1$ANC_volwt) ]               
if (class(dt1$ANC_volwt)=="character") dt1$ANC_volwt <-as.numeric(dt1$ANC_volwt)

# Convert Missing Values to NA for non-dates

dt1$flow_mm <- ifelse((trimws(as.character(dt1$flow_mm))==trimws("-888.88")),NA,dt1$flow_mm)               
suppressWarnings(dt1$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt1$flow_mm))
dt1$Ca_flux <- ifelse((trimws(as.character(dt1$Ca_flux))==trimws("-888.88")),NA,dt1$Ca_flux)               
suppressWarnings(dt1$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_flux))
dt1$Mg_flux <- ifelse((trimws(as.character(dt1$Mg_flux))==trimws("-888.88")),NA,dt1$Mg_flux)               
suppressWarnings(dt1$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_flux))
dt1$K_flux <- ifelse((trimws(as.character(dt1$K_flux))==trimws("-888.88")),NA,dt1$K_flux)               
suppressWarnings(dt1$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_flux))==as.character(as.numeric("-888.88"))),NA,dt1$K_flux))
dt1$Na_flux <- ifelse((trimws(as.character(dt1$Na_flux))==trimws("-888.88")),NA,dt1$Na_flux)               
suppressWarnings(dt1$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Na_flux))
dt1$Al_Ferron_flux <- ifelse((trimws(as.character(dt1$Al_Ferron_flux))==trimws("-888.88")),NA,dt1$Al_Ferron_flux)               
suppressWarnings(dt1$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_Ferron_flux))
dt1$TMAl_flux <- ifelse((trimws(as.character(dt1$TMAl_flux))==trimws("-888.88")),NA,dt1$TMAl_flux)               
suppressWarnings(dt1$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TMAl_flux))
dt1$OMAl_flux <- ifelse((trimws(as.character(dt1$OMAl_flux))==trimws("-888.88")),NA,dt1$OMAl_flux)               
suppressWarnings(dt1$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$OMAl_flux))
dt1$Al_ICP_flux <- ifelse((trimws(as.character(dt1$Al_ICP_flux))==trimws("-888.88")),NA,dt1$Al_ICP_flux)               
suppressWarnings(dt1$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_ICP_flux))
dt1$NH4_flux <- ifelse((trimws(as.character(dt1$NH4_flux))==trimws("-888.88")),NA,dt1$NH4_flux)               
suppressWarnings(dt1$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NH4_flux))
dt1$SO4_flux <- ifelse((trimws(as.character(dt1$SO4_flux))==trimws("-888.88")),NA,dt1$SO4_flux)               
suppressWarnings(dt1$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SO4_flux))
dt1$NO3_flux <- ifelse((trimws(as.character(dt1$NO3_flux))==trimws("-888.88")),NA,dt1$NO3_flux)               
suppressWarnings(dt1$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NO3_flux))
dt1$Cl_flux <- ifelse((trimws(as.character(dt1$Cl_flux))==trimws("-888.88")),NA,dt1$Cl_flux)               
suppressWarnings(dt1$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Cl_flux))
dt1$PO4_flux <- ifelse((trimws(as.character(dt1$PO4_flux))==trimws("-888.88")),NA,dt1$PO4_flux)               
suppressWarnings(dt1$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$PO4_flux))
dt1$DOC_flux <- ifelse((trimws(as.character(dt1$DOC_flux))==trimws("-888.88")),NA,dt1$DOC_flux)               
suppressWarnings(dt1$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DOC_flux))
dt1$TDN_flux <- ifelse((trimws(as.character(dt1$TDN_flux))==trimws("-888.88")),NA,dt1$TDN_flux)               
suppressWarnings(dt1$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TDN_flux))
dt1$DON_flux <- ifelse((trimws(as.character(dt1$DON_flux))==trimws("-888.88")),NA,dt1$DON_flux)               
suppressWarnings(dt1$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DON_flux))
dt1$DIC_flux <- ifelse((trimws(as.character(dt1$DIC_flux))==trimws("-888.88")),NA,dt1$DIC_flux)               
suppressWarnings(dt1$DIC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DIC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DIC_flux))
dt1$SiO2_flux <- ifelse((trimws(as.character(dt1$SiO2_flux))==trimws("-888.88")),NA,dt1$SiO2_flux)               
suppressWarnings(dt1$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SiO2_flux))
dt1$Mn_flux <- ifelse((trimws(as.character(dt1$Mn_flux))==trimws("-888.88")),NA,dt1$Mn_flux)               
suppressWarnings(dt1$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_flux))
dt1$Fe_flux <- ifelse((trimws(as.character(dt1$Fe_flux))==trimws("-888.88")),NA,dt1$Fe_flux)               
suppressWarnings(dt1$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_flux))
dt1$F_flux <- ifelse((trimws(as.character(dt1$F_flux))==trimws("-888.88")),NA,dt1$F_flux)               
suppressWarnings(dt1$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$F_flux))==as.character(as.numeric("-888.88"))),NA,dt1$F_flux))
dt1$H_flux <- ifelse((trimws(as.character(dt1$H_flux))==trimws("-888.88")),NA,dt1$H_flux)               
suppressWarnings(dt1$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$H_flux))==as.character(as.numeric("-888.88"))),NA,dt1$H_flux))
dt1$pH_volwt <- ifelse((trimws(as.character(dt1$pH_volwt))==trimws("-888.88")),NA,dt1$pH_volwt)               
suppressWarnings(dt1$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$pH_volwt))
dt1$SpecCond_volwt <- ifelse((trimws(as.character(dt1$SpecCond_volwt))==trimws("-888.88")),NA,dt1$SpecCond_volwt)               
suppressWarnings(dt1$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$SpecCond_volwt))
dt1$ANC_volwt <- ifelse((trimws(as.character(dt1$ANC_volwt))==trimws("-888.88")),NA,dt1$ANC_volwt)               
suppressWarnings(dt1$ANC_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ANC_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$ANC_volwt))


# Here is the structure of the input data frame:
str(dt1)                            
W5<-dt1
W5$Watershed<-"W5"


### Now read in WS6

#######################################  WS6   ############################################
# Package ID: knb-lter-hbr.8.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 6, 1963 - present.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/8/17/3312389e77cc5fd06bc8a7c9019de0ed" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
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

if (class(dt1$Month)!="factor") dt1$Month<- as.factor(dt1$Month)
if (class(dt1$Year_Month)!="factor") dt1$Year_Month<- as.factor(dt1$Year_Month)
if (class(dt1$flow_mm)=="factor") dt1$flow_mm <-as.numeric(levels(dt1$flow_mm))[as.integer(dt1$flow_mm) ]               
if (class(dt1$flow_mm)=="character") dt1$flow_mm <-as.numeric(dt1$flow_mm)
if (class(dt1$Ca_flux)=="factor") dt1$Ca_flux <-as.numeric(levels(dt1$Ca_flux))[as.integer(dt1$Ca_flux) ]               
if (class(dt1$Ca_flux)=="character") dt1$Ca_flux <-as.numeric(dt1$Ca_flux)
if (class(dt1$Mg_flux)=="factor") dt1$Mg_flux <-as.numeric(levels(dt1$Mg_flux))[as.integer(dt1$Mg_flux) ]               
if (class(dt1$Mg_flux)=="character") dt1$Mg_flux <-as.numeric(dt1$Mg_flux)
if (class(dt1$K_flux)=="factor") dt1$K_flux <-as.numeric(levels(dt1$K_flux))[as.integer(dt1$K_flux) ]               
if (class(dt1$K_flux)=="character") dt1$K_flux <-as.numeric(dt1$K_flux)
if (class(dt1$Na_flux)=="factor") dt1$Na_flux <-as.numeric(levels(dt1$Na_flux))[as.integer(dt1$Na_flux) ]               
if (class(dt1$Na_flux)=="character") dt1$Na_flux <-as.numeric(dt1$Na_flux)
if (class(dt1$Al_Ferron_flux)=="factor") dt1$Al_Ferron_flux <-as.numeric(levels(dt1$Al_Ferron_flux))[as.integer(dt1$Al_Ferron_flux) ]               
if (class(dt1$Al_Ferron_flux)=="character") dt1$Al_Ferron_flux <-as.numeric(dt1$Al_Ferron_flux)
if (class(dt1$TMAl_flux)=="factor") dt1$TMAl_flux <-as.numeric(levels(dt1$TMAl_flux))[as.integer(dt1$TMAl_flux) ]               
if (class(dt1$TMAl_flux)=="character") dt1$TMAl_flux <-as.numeric(dt1$TMAl_flux)
if (class(dt1$OMAl_flux)=="factor") dt1$OMAl_flux <-as.numeric(levels(dt1$OMAl_flux))[as.integer(dt1$OMAl_flux) ]               
if (class(dt1$OMAl_flux)=="character") dt1$OMAl_flux <-as.numeric(dt1$OMAl_flux)
if (class(dt1$Al_ICP_flux)=="factor") dt1$Al_ICP_flux <-as.numeric(levels(dt1$Al_ICP_flux))[as.integer(dt1$Al_ICP_flux) ]               
if (class(dt1$Al_ICP_flux)=="character") dt1$Al_ICP_flux <-as.numeric(dt1$Al_ICP_flux)
if (class(dt1$NH4_flux)=="factor") dt1$NH4_flux <-as.numeric(levels(dt1$NH4_flux))[as.integer(dt1$NH4_flux) ]               
if (class(dt1$NH4_flux)=="character") dt1$NH4_flux <-as.numeric(dt1$NH4_flux)
if (class(dt1$SO4_flux)=="factor") dt1$SO4_flux <-as.numeric(levels(dt1$SO4_flux))[as.integer(dt1$SO4_flux) ]               
if (class(dt1$SO4_flux)=="character") dt1$SO4_flux <-as.numeric(dt1$SO4_flux)
if (class(dt1$NO3_flux)=="factor") dt1$NO3_flux <-as.numeric(levels(dt1$NO3_flux))[as.integer(dt1$NO3_flux) ]               
if (class(dt1$NO3_flux)=="character") dt1$NO3_flux <-as.numeric(dt1$NO3_flux)
if (class(dt1$Cl_flux)=="factor") dt1$Cl_flux <-as.numeric(levels(dt1$Cl_flux))[as.integer(dt1$Cl_flux) ]               
if (class(dt1$Cl_flux)=="character") dt1$Cl_flux <-as.numeric(dt1$Cl_flux)
if (class(dt1$PO4_flux)=="factor") dt1$PO4_flux <-as.numeric(levels(dt1$PO4_flux))[as.integer(dt1$PO4_flux) ]               
if (class(dt1$PO4_flux)=="character") dt1$PO4_flux <-as.numeric(dt1$PO4_flux)
if (class(dt1$DOC_flux)=="factor") dt1$DOC_flux <-as.numeric(levels(dt1$DOC_flux))[as.integer(dt1$DOC_flux) ]               
if (class(dt1$DOC_flux)=="character") dt1$DOC_flux <-as.numeric(dt1$DOC_flux)
if (class(dt1$TDN_flux)=="factor") dt1$TDN_flux <-as.numeric(levels(dt1$TDN_flux))[as.integer(dt1$TDN_flux) ]               
if (class(dt1$TDN_flux)=="character") dt1$TDN_flux <-as.numeric(dt1$TDN_flux)
if (class(dt1$DON_flux)=="factor") dt1$DON_flux <-as.numeric(levels(dt1$DON_flux))[as.integer(dt1$DON_flux) ]               
if (class(dt1$DON_flux)=="character") dt1$DON_flux <-as.numeric(dt1$DON_flux)
if (class(dt1$DIC_flux)=="factor") dt1$DIC_flux <-as.numeric(levels(dt1$DIC_flux))[as.integer(dt1$DIC_flux) ]               
if (class(dt1$DIC_flux)=="character") dt1$DIC_flux <-as.numeric(dt1$DIC_flux)
if (class(dt1$SiO2_flux)=="factor") dt1$SiO2_flux <-as.numeric(levels(dt1$SiO2_flux))[as.integer(dt1$SiO2_flux) ]               
if (class(dt1$SiO2_flux)=="character") dt1$SiO2_flux <-as.numeric(dt1$SiO2_flux)
if (class(dt1$Mn_flux)=="factor") dt1$Mn_flux <-as.numeric(levels(dt1$Mn_flux))[as.integer(dt1$Mn_flux) ]               
if (class(dt1$Mn_flux)=="character") dt1$Mn_flux <-as.numeric(dt1$Mn_flux)
if (class(dt1$Fe_flux)=="factor") dt1$Fe_flux <-as.numeric(levels(dt1$Fe_flux))[as.integer(dt1$Fe_flux) ]               
if (class(dt1$Fe_flux)=="character") dt1$Fe_flux <-as.numeric(dt1$Fe_flux)
if (class(dt1$F_flux)=="factor") dt1$F_flux <-as.numeric(levels(dt1$F_flux))[as.integer(dt1$F_flux) ]               
if (class(dt1$F_flux)=="character") dt1$F_flux <-as.numeric(dt1$F_flux)
if (class(dt1$H_flux)=="factor") dt1$H_flux <-as.numeric(levels(dt1$H_flux))[as.integer(dt1$H_flux) ]               
if (class(dt1$H_flux)=="character") dt1$H_flux <-as.numeric(dt1$H_flux)
if (class(dt1$pH_volwt)=="factor") dt1$pH_volwt <-as.numeric(levels(dt1$pH_volwt))[as.integer(dt1$pH_volwt) ]               
if (class(dt1$pH_volwt)=="character") dt1$pH_volwt <-as.numeric(dt1$pH_volwt)
if (class(dt1$SpecCond_volwt)=="factor") dt1$SpecCond_volwt <-as.numeric(levels(dt1$SpecCond_volwt))[as.integer(dt1$SpecCond_volwt) ]               
if (class(dt1$SpecCond_volwt)=="character") dt1$SpecCond_volwt <-as.numeric(dt1$SpecCond_volwt)
if (class(dt1$ANC_volwt)=="factor") dt1$ANC_volwt <-as.numeric(levels(dt1$ANC_volwt))[as.integer(dt1$ANC_volwt) ]               
if (class(dt1$ANC_volwt)=="character") dt1$ANC_volwt <-as.numeric(dt1$ANC_volwt)

# Convert Missing Values to NA for non-dates

dt1$flow_mm <- ifelse((trimws(as.character(dt1$flow_mm))==trimws("-888.88")),NA,dt1$flow_mm)               
suppressWarnings(dt1$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt1$flow_mm))
dt1$Ca_flux <- ifelse((trimws(as.character(dt1$Ca_flux))==trimws("-888.88")),NA,dt1$Ca_flux)               
suppressWarnings(dt1$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_flux))
dt1$Mg_flux <- ifelse((trimws(as.character(dt1$Mg_flux))==trimws("-888.88")),NA,dt1$Mg_flux)               
suppressWarnings(dt1$Mg_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_flux))
dt1$K_flux <- ifelse((trimws(as.character(dt1$K_flux))==trimws("-888.88")),NA,dt1$K_flux)               
suppressWarnings(dt1$K_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_flux))==as.character(as.numeric("-888.88"))),NA,dt1$K_flux))
dt1$Na_flux <- ifelse((trimws(as.character(dt1$Na_flux))==trimws("-888.88")),NA,dt1$Na_flux)               
suppressWarnings(dt1$Na_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Na_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Na_flux))
dt1$Al_Ferron_flux <- ifelse((trimws(as.character(dt1$Al_Ferron_flux))==trimws("-888.88")),NA,dt1$Al_Ferron_flux)               
suppressWarnings(dt1$Al_Ferron_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_Ferron_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_Ferron_flux))
dt1$TMAl_flux <- ifelse((trimws(as.character(dt1$TMAl_flux))==trimws("-888.88")),NA,dt1$TMAl_flux)               
suppressWarnings(dt1$TMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TMAl_flux))
dt1$OMAl_flux <- ifelse((trimws(as.character(dt1$OMAl_flux))==trimws("-888.88")),NA,dt1$OMAl_flux)               
suppressWarnings(dt1$OMAl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$OMAl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$OMAl_flux))
dt1$Al_ICP_flux <- ifelse((trimws(as.character(dt1$Al_ICP_flux))==trimws("-888.88")),NA,dt1$Al_ICP_flux)               
suppressWarnings(dt1$Al_ICP_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_ICP_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Al_ICP_flux))
dt1$NH4_flux <- ifelse((trimws(as.character(dt1$NH4_flux))==trimws("-888.88")),NA,dt1$NH4_flux)               
suppressWarnings(dt1$NH4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NH4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NH4_flux))
dt1$SO4_flux <- ifelse((trimws(as.character(dt1$SO4_flux))==trimws("-888.88")),NA,dt1$SO4_flux)               
suppressWarnings(dt1$SO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SO4_flux))
dt1$NO3_flux <- ifelse((trimws(as.character(dt1$NO3_flux))==trimws("-888.88")),NA,dt1$NO3_flux)               
suppressWarnings(dt1$NO3_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$NO3_flux))==as.character(as.numeric("-888.88"))),NA,dt1$NO3_flux))
dt1$Cl_flux <- ifelse((trimws(as.character(dt1$Cl_flux))==trimws("-888.88")),NA,dt1$Cl_flux)               
suppressWarnings(dt1$Cl_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cl_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Cl_flux))
dt1$PO4_flux <- ifelse((trimws(as.character(dt1$PO4_flux))==trimws("-888.88")),NA,dt1$PO4_flux)               
suppressWarnings(dt1$PO4_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PO4_flux))==as.character(as.numeric("-888.88"))),NA,dt1$PO4_flux))
dt1$DOC_flux <- ifelse((trimws(as.character(dt1$DOC_flux))==trimws("-888.88")),NA,dt1$DOC_flux)               
suppressWarnings(dt1$DOC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DOC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DOC_flux))
dt1$TDN_flux <- ifelse((trimws(as.character(dt1$TDN_flux))==trimws("-888.88")),NA,dt1$TDN_flux)               
suppressWarnings(dt1$TDN_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TDN_flux))==as.character(as.numeric("-888.88"))),NA,dt1$TDN_flux))
dt1$DON_flux <- ifelse((trimws(as.character(dt1$DON_flux))==trimws("-888.88")),NA,dt1$DON_flux)               
suppressWarnings(dt1$DON_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DON_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DON_flux))
dt1$DIC_flux <- ifelse((trimws(as.character(dt1$DIC_flux))==trimws("-888.88")),NA,dt1$DIC_flux)               
suppressWarnings(dt1$DIC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$DIC_flux))==as.character(as.numeric("-888.88"))),NA,dt1$DIC_flux))
dt1$SiO2_flux <- ifelse((trimws(as.character(dt1$SiO2_flux))==trimws("-888.88")),NA,dt1$SiO2_flux)               
suppressWarnings(dt1$SiO2_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SiO2_flux))==as.character(as.numeric("-888.88"))),NA,dt1$SiO2_flux))
dt1$Mn_flux <- ifelse((trimws(as.character(dt1$Mn_flux))==trimws("-888.88")),NA,dt1$Mn_flux)               
suppressWarnings(dt1$Mn_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_flux))
dt1$Fe_flux <- ifelse((trimws(as.character(dt1$Fe_flux))==trimws("-888.88")),NA,dt1$Fe_flux)               
suppressWarnings(dt1$Fe_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_flux))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_flux))
dt1$F_flux <- ifelse((trimws(as.character(dt1$F_flux))==trimws("-888.88")),NA,dt1$F_flux)               
suppressWarnings(dt1$F_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$F_flux))==as.character(as.numeric("-888.88"))),NA,dt1$F_flux))
dt1$H_flux <- ifelse((trimws(as.character(dt1$H_flux))==trimws("-888.88")),NA,dt1$H_flux)               
suppressWarnings(dt1$H_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$H_flux))==as.character(as.numeric("-888.88"))),NA,dt1$H_flux))
dt1$pH_volwt <- ifelse((trimws(as.character(dt1$pH_volwt))==trimws("-888.88")),NA,dt1$pH_volwt)               
suppressWarnings(dt1$pH_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pH_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$pH_volwt))
dt1$SpecCond_volwt <- ifelse((trimws(as.character(dt1$SpecCond_volwt))==trimws("-888.88")),NA,dt1$SpecCond_volwt)               
suppressWarnings(dt1$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$SpecCond_volwt))
dt1$ANC_volwt <- ifelse((trimws(as.character(dt1$ANC_volwt))==trimws("-888.88")),NA,dt1$ANC_volwt)               
suppressWarnings(dt1$ANC_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ANC_volwt))==as.character(as.numeric("-888.88"))),NA,dt1$ANC_volwt))


# Here is the structure of the input data frame:
str(dt1)                      

W6<-dt1
W6$Watershed<-"W6 (Reference)"
###

mean(W2$Ca_flux)
mean(W4$Ca_flux)
mean(W5$Ca_flux)
mean(W6$Ca_flux)


names(W2)
names(W4)
names(W5)
names(W6)

## W1 - 1999, Wollastonite, evaluate the role of Ca supply
## W2 - 65-68 was the cut and left in place in 65, herbicided from 66-68.  Eliminate transpiring vegetation.
## W4 - 70, 72, 74 strip cut.
## W5 - 1983-84 whole tree harvest


library(lubridate)

fm<-rbind(W2, W4, W5, W6)
head(fm)

table(fm$Watershed)






# add in date
fm$DATE<-paste0(fm$Year_Month,"-01")  # in in day to year month string
fm$DATE<-ymd(fm$DATE) # change how R interprets Date to be a date

# add in water year
w.year <- as.numeric(format(fm$DATE, "%Y"))

june.july.sept <- as.numeric(format(fm$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
fm$wyear<-w.year


# make sure you are only using complete years for the record
monchem<-as.data.frame(table( fm$wyear))

monchem
monchem$wys<- paste(monchem$Var1)
monchem[monchem$Freq<40, "Use"]<-"incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use),"Use"]<-"complete"
fm$Use<-monchem$Use[match(fm$wyear, monchem$wys)]
fm.complete<-fm[fm$Use=="complete",]



manage<-aggregate(list(Ca_flux=fm.complete$Ca_flux, flow_mm=fm.complete$flow_mm), by=list(Watershed=fm.complete$Watershed, wyear=fm.complete$wyear), FUN="sum")


## check units here!
head(manage)

manage$flow_m<-manage$flow_mm/1000
manage$Ca_mg<-manage$Ca_flux*1000

## spread manage, then subset each watershed.
Ca<-spread(manage,"Watershed","Ca_mg")

Fl<-spread(manage, "Watershed","flow_m")

head(Fl)
head(Ca)

# Ca conc  (is in grams, multiply by hectares to just get grams)
Ca$W2<-Ca$W2*15.6
Ca$W4<-Ca$W4*36.1
Ca$W5<-Ca$W5*21.9
Ca$`W6 (Reference)`<-Ca$`W6 (Reference)`*13.2


# water.  Is in meters, multiply by area of watershed in meters.
Fl$W2<-Fl$W2*15.6*10000
Fl$W4<-Fl$W4*36.1*10000
Fl$W5<-Fl$W5*21.9*10000
Fl$`W6 (Reference)`<-Fl$`W6 (Reference)`*13.2*10000

head(Ca)
head(Fl)


cag<-gather(Ca, "Watershed","Ca_mg", 5:8)
flag<-gather(Fl, "Watershed","flow_m3", 5:8)

head(flag)
head(cag)
flag$flow_L<-flag$flow_m3*1000


dim(cag)

cag$flow_L<-flag$flow_L

cag$camgL<-cag$Ca_mg/cag$flow_L



fa<-spread(cag, "Watershed","camgL")




str(fa)
fa1<-gather(fa, "Watershed","camgL",c(7,10))
fa2<-gather(fa, "Watershed","camgL",c(8,10))
fa3<-gather(fa, "Watershed","camgL",c(9,10))


##################

#Maybe this will work?

fa1<-fa1[,c(1,9,10)]
fa1<-na.omit(fa1)
fa1<-spread(fa1, "Watershed","camgL")

head(fa2)
fa2<-fa2[,c(1,9,10)]
fa2<-na.omit(fa2)
fa2<-spread(fa2, "Watershed","camgL")

fa3<-fa3[,c(1,9,10)]
fa3<-na.omit(fa3)
fa3<-spread(fa3, "Watershed","camgL")



g1<- ggplot(fa1) +
  geom_line(aes(x = wyear, y =`W6 (Reference)`), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W2),              size = 0.8, color = "black") +
  geom_point(aes(x = wyear, y = `W6 (Reference)`),  shape = 21, size = 4, stroke = 1.5, color = "black", fill = "white") +
  geom_point(aes(x = wyear, y = W2),                shape = 21, size = 4, stroke = 1.5, color = "black", fill = "black")+
  theme_bw()+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y     = element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text      =element_text(size=22),
        plot.title = element_text(size = 26))+
  geom_text(aes(x=2008, y=7, label="treatment"),size=6 ,color="black") +  
  geom_text(aes(x=2008, y=6, label="reference"),size=6, color="black") +ylab("Ca (mg/L)")+xlab("Water Year (June 1)")+
  scale_x_continuous(expand = c(0, 0), limits=c(1960,2023), breaks=seq(1960,2020,5 ))+
  scale_y_continuous(limits=c(0,9), breaks=seq(0,8,2 ))+
  annotate("point", x = 2004, y = 7, size=4, shape=21, stroke=1.5, color="black",fill="black")+
  annotate("point", x = 2004, y = 6, size=4, shape=21, stroke=1.5, color="black",fill="white")+
  geom_text(aes(x=1971, y=8.5, label="W2: devegetated in 1965-1968"),size=7,color="black") 
g1


g2<-ggplot(fa2) +
  geom_line(aes(x = wyear, y =`W6 (Reference)`), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W4),              size = 0.8, color = "black") +
  geom_point(aes(x = wyear, y = `W6 (Reference)`),  shape = 21, size = 4, stroke = 1.5, color = "black", fill = "white") +
  geom_point(aes(x = wyear, y = W4),                shape = 21, size = 4, stroke = 1.5, color = "black", fill = "black")+
  theme_bw()+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y     = element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text      =element_text(size=22),
        plot.title = element_text(size = 26))+
  geom_text(aes(x=2008, y=3, label="treatment"),size=6 ,color="black") +  
  geom_text(aes(x=2008, y=2.6, label="reference"),size=6, color="black") +ylab("Ca (mg/L)")+xlab("Water Year (June 1)")+
  scale_x_continuous(expand = c(0, 0), limits=c(1960,2023), breaks=seq(1960,2020,5 ))+
  annotate("point", x = 2004, y = 3, size=4, shape=21, stroke=1.5, color="black",fill="black")+
  annotate("point", x = 2004, y = 2.6, size=4, shape=21, stroke=1.5, color="black",fill="white")+
  ylim(0,3.5)+
  geom_text(aes(x=1972, y=3.2, label="W4: strip cut in 1970, 1972, 1974"),size=7 ,color="black") 
g2


g3<-ggplot(fa3) +
  geom_line(aes(x = wyear, y =`W6 (Reference)`), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W5),              size = 0.8, color = "black") +
  geom_point(aes(x = wyear, y = `W6 (Reference)`),  shape = 21, size = 4, stroke = 1.5, color = "black", fill = "white") +
  geom_point(aes(x = wyear, y = W5),                shape = 21, size = 4, stroke = 1.5, color = "black", fill = "black")+
  theme_bw()+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y     = element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text      =element_text(size=22),
        plot.title = element_text(size = 26))+
  geom_text(aes(x=2008, y=3, label="treatment"),size=6 ,color="black") +  
  geom_text(aes(x=2008, y=2.6, label="reference"),size=6, color="black") +ylab("Ca (mg/L)")+xlab("Water Year (June 1)")+
  scale_x_continuous(expand = c(0, 0), limits=c(1960,2023), breaks=seq(1960,2020,5 ))+
  annotate("point", x = 2004, y = 3, size=4, shape=21, stroke=1.5, color="black",fill="black")+
  annotate("point", x = 2004, y = 2.6, size=4, shape=21, stroke=1.5, color="black",fill="white")+
  ylim(0,3.5)+
  geom_text(aes(x=1973, y=3.2, label="W5: whole tree harvest in 1983, 1984"),size=7 ,color="black") 
g3

  
library(ggpubr)
library(plotly)

ggarrange(g1, g2 ,g3, nrow=3, legend="none")
  
############################################



p1<-ggplotly(g1)
p2<-ggplotly(g2)
p3<-ggplotly(g3)

p123<-subplot(p1, p2, p3, shareX=T, nrows=3)

p123
htmlwidgets::saveWidget(as_widget(p123), "management/streamCa_W2_W4_W5_W6.html")
