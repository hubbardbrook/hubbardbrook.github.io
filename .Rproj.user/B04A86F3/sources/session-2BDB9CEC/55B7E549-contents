## Make a figure of phosphate export from W5 and W6

## Alex Young  alexyoung.116@gmail.com

library(ggplot2)
library(tidyr)
library(plotly)
library(lubridate)

# read in monthly fluxes for Chemistry of streamwater data for WS6
# Package ID: knb-lter-hbr.8.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 6, 1963 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/8/17/3312389e77cc5fd06bc8a7c9019de0ed" 
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

if (class(dt4$Month)!="factor") dt4$Month<- as.factor(dt4$Month)
if (class(dt4$Year_Month)!="factor") dt4$Year_Month<- as.factor(dt4$Year_Month)
if (class(dt4$flow_mm)=="factor") dt4$flow_mm <-as.numeric(levels(dt4$flow_mm))[as.integer(dt4$flow_mm) ]               
if (class(dt4$flow_mm)=="character") dt4$flow_mm <-as.numeric(dt4$flow_mm)
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
if (class(dt4$DIC_flux)=="factor") dt4$DIC_flux <-as.numeric(levels(dt4$DIC_flux))[as.integer(dt4$DIC_flux) ]               
if (class(dt4$DIC_flux)=="character") dt4$DIC_flux <-as.numeric(dt4$DIC_flux)
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
if (class(dt4$ANC_volwt)=="factor") dt4$ANC_volwt <-as.numeric(levels(dt4$ANC_volwt))[as.integer(dt4$ANC_volwt) ]               
if (class(dt4$ANC_volwt)=="character") dt4$ANC_volwt <-as.numeric(dt4$ANC_volwt)

# Convert Missing Values to NA for non-dates

dt4$flow_mm <- ifelse((trimws(as.character(dt4$flow_mm))==trimws("-888.88")),NA,dt4$flow_mm)               
suppressWarnings(dt4$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt4$flow_mm))
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
dt4$DIC_flux <- ifelse((trimws(as.character(dt4$DIC_flux))==trimws("-888.88")),NA,dt4$DIC_flux)               
suppressWarnings(dt4$DIC_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$DIC_flux))==as.character(as.numeric("-888.88"))),NA,dt4$DIC_flux))
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
dt4$SpecCond_volwt <- ifelse((trimws(as.character(dt4$SpecCond_volwt))==trimws("-888.88")),NA,dt4$SpecCond_volwt)               
suppressWarnings(dt4$SpecCond_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$SpecCond_volwt))==as.character(as.numeric("-888.88"))),NA,dt4$SpecCond_volwt))
dt4$ANC_volwt <- ifelse((trimws(as.character(dt4$ANC_volwt))==trimws("-888.88")),NA,dt4$ANC_volwt)               
suppressWarnings(dt4$ANC_volwt <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$ANC_volwt))==as.character(as.numeric("-888.88"))),NA,dt4$ANC_volwt))

W6<-dt4

W6

### Now Read in WS5, which had the whole tree harvest in 1983-84.

# Package ID: knb-lter-hbr.7.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Streamwater â Monthly Fluxes, Watershed 5, 1963 - present.
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

W5<-dt1

###


# give the watershed name
W5$Watershed<-"W5"
W6$Watershed<-"W6"





## W1 - 1999, Wollastonite, evaluate the role of Ca supply
## W2 - 65-68 was the cut and left in place in 65, herbicided from 66-68.  Eliminate transpiring vegetation.
## W4 - 70, 72, 74 strip cut.
## W5 - 1983-84 whole tree harvest


#Reference: 
##  W3 1957 Hydrologic reference
##  W6 1963 Biogeochemical reference watershed

## W7 in 1965, W8 in 1968, W9 in 1995



## combine the watersheds

w<-rbind(W5,W6)
library(lubridate)
library(ggplot2)
library(plotly)


# conduct data formatting

w$DATE<-paste0(w$Year_Month,"-01") 
w$DATE<-ymd(w$DATE) # change how R interprets Date to be a date



# We have a data column, but its not formatted as a date
w$DATE<-ymd(w$DATE) # change how R interprets Date to be a date
w$Year<-year(w$DATE)

# add in water year
w.year <- as.numeric(format(w$DATE, "%Y"))
june.july.sept <- as.numeric(format(w$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
w$wyear<-w.year # 

str(w)


## troubleshoot
ggplot(w, aes(x=wyear, y=flow_mm, col=Watershed ))+geom_point()

ggplot(w, aes(x=wyear, y=PO4_flux, col=Watershed ))+geom_point()

ggplot(w.complete, aes(x=PO4_flux, y=flow_mm, col=Watershed))+geom_point()


# make sure you are only using complete years for the record
po4.d <-as.data.frame(table(w$Watershed , w$wyear , w$Month, is.na(w$PO4_flux)))



hist(w$PO4_flux, breaks=200)

summary(w$PO4_flux)
po4.d<-po4.d[po4.d$Freq!=0,]

po4.na<-po4.d[is.na(po4.d$Freq),]

table(po4.d$Var4)

po4<-po4.d[po4.d$Var4=="FALSE",]
table(po4$Var1, po4$Var2)




pre.use<-as.data.frame(table(po4$Var2, po4$Var1))
head(pre.use)
pre.use$wys<-paste(pre.use$Var2, pre.use$Var1)
pre.use
pre.use[pre.use$Freq<1, "Use"]<-"incomplete wyear" # complete is 350 or more days
pre.use[pre.use$Freq>1,"Use"]<-"complete"
pre.use

head(pre.use)
head(w)
w$wys<-paste(w$Watershed,w$wyear)

w$Use<-pre.use$Use[match(w$wys, pre.use$wys)]
w

w.complete<-w[w$Use=="complete",]

## troubleshoot
w.complete
ggplot(w.complete, aes(x=wyear, y=flow_mm, col=Watershed ))+geom_point()

ggplot(w, aes(x=wyear, y=PO4_flux, col=Watershed ))+geom_point()

m1<-ggplot(w.complete, aes(x=flow_mm, y=PO4_flux, col=Watershed))+geom_point()+
  ggtitle("Monthly PO4 fluxes  are often 0?")+theme_bw()
m1
summary(w.complete$PO4_flux)

0.04/.012

30/94
# 
# # get po4 p- from po4  g/ha.   
# w.complete$po4p <- w.complete$PO4_flux * 0.319
# w.complete$po4P <- w.complete$po4p / 3.066188 
# 

# get the volume weighted concentration in mg/L
pex<-aggregate(list(PO4_flux=w.complete$PO4_flux, flow_mm=w.complete$flow_mm), by=list(Watershed = w.complete$Watershed, wyear=w.complete$wyear), FUN="mean", na.rm=T)


pex$flow_m<-pex$flow_mm/1000
pex$PO4mg<-pex$PO4_flux*1000

## spread manage, then subset each watershed.
p04<-spread(pex,"Watershed","PO4mg")

Fl<-spread(pex, "Watershed","flow_m")


#######

head(p04)
head(Fl)

# p04 conc  (is in grams, multiply by hectares to just get grams)
p04$W5<-p04$W5*21.9
p04$W6<-p04$W6*13.2

# water.  Is in meters, multiply by area of watershed in meters.
Fl$W5<-Fl$W5*21.9*10000
Fl$W6<-Fl$W6*13.2*10000


head(p04)
head(Fl)

pag<-gather(p04, "Watershed","P04_mg", 5:6)
flag<-gather(Fl, "Watershed","flow_m3", 5:6)

head(flag)
head(pag)
flag$flow_L<-flag$flow_m3*1000



pag$flow_L<-flag$flow_L

pag$P04mgL<-pag$P04_mg/pag$flow_L


fa<-spread(pag, "Watershed","P04mgL")
head(fa)
fa1<-gather(fa, "Watershed","P04mgL",c(7,8))

head(fa1)
fa1<-fa1[,c(1,7,8)]
fa1<-na.omit(fa1)
head(fa1)
##


head(pex)
head(pag)



go<-ggplot(fa1[fa1$wyear>1971, ], aes(x=wyear, y=P04mgL, col=Watershed, group=Watershed))+
  geom_point()+geom_line()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Water year (June 1)")+scale_color_manual(values=c("green","black"))+
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.014), breaks=seq(0,0.014,0.002 ))+
  ylab("Volume weighted reactive P (mg / L)")+geom_vline(xintercept=1984, linetype="dashed",col="purple")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y     = element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text      =element_text(size=22),
        plot.title = element_text(size = 26))+
  geom_text(aes(x=1980, y=0.012, label="whole tree"),size=6 ,color="black") +
  geom_text(aes(x=1980, y=0.0115, label="harvest"),size=6 ,color="black") +
  ggtitle("Comparing volume weighted P04 stream concentrations W6 vs W5")
go
po<-ggplotly(go)
po

#annual average, volume weighted concentration of soluble reactive P (mg/L) in stream draining from W5 and W6

# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(po), "phosphorus/P_exportW5_W6_Driscoll.html")






