













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

# Convert Missing Values to NA for non-dates
dt4$flow_mm <- ifelse((trimws(as.character(dt4$flow_mm))==trimws("-888.88")),NA,dt4$flow_mm)               
suppressWarnings(dt4$flow_mm <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$flow_mm))==as.character(as.numeric("-888.88"))),NA,dt4$flow_mm))
dt4$Ca_flux <- ifelse((trimws(as.character(dt4$Ca_flux))==trimws("-888.88")),NA,dt4$Ca_flux)               
suppressWarnings(dt4$Ca_flux <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt4$Ca_flux))==as.character(as.numeric("-888.





### add in reading WS5







# comparing annual, Vol-wt avg PO4 concentrations

#Fig2 annual average, volume weighted concentrations of soluble reactive P (mg/L) in stream draining from W5 and W6 (Likens, unpublished data)


# add line for Whole tree harvest in 1983


