
###

# Alex Young 8/2/2022
# Figure 4 and 5 of soil decomp, Chris Johnson, Mary Margaret Koppers




# Package ID: knb-lter-hbr.172.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Mass and Chemistry of Organic Horizons and Surface Mineral Soils on Watershed 6 at the Hubbard Brook Experimental Forest, 1976 - present
inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/172/3/775e243b7a2b67c0047498533bf5b9d1" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Sample_ID",     
                 "Year",     
                 "Watershed",     
                 "Plot",     
                 "Horizon",     
                 "OM_TM",     
                 "OM_OM",     
                 "OM_LOI"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Sample_ID)!="factor") dt4$Sample_ID<- as.factor(dt4$Sample_ID)
if (class(dt4$Watershed)!="factor") dt4$Watershed<- as.factor(dt4$Watershed)
if (class(dt4$Plot)!="factor") dt4$Plot<- as.factor(dt4$Plot)
if (class(dt4$Horizon)!="factor") dt4$Horizon<- as.factor(dt4$Horizon)
if (class(dt4$OM_TM)=="factor") dt4$OM_TM <-as.numeric(levels(dt4$OM_TM))[as.integer(dt4$OM_TM) ]               
if (class(dt4$OM_TM)=="character") dt4$OM_TM <-as.numeric(dt4$OM_TM)
if (class(dt4$OM_OM)=="factor") dt4$OM_OM <-as.numeric(levels(dt4$OM_OM))[as.integer(dt4$OM_OM) ]               
if (class(dt4$OM_OM)=="character") dt4$OM_OM <-as.numeric(dt4$OM_OM)
if (class(dt4$OM_LOI)=="factor") dt4$OM_LOI <-as.numeric(levels(dt4$OM_LOI))[as.integer(dt4$OM_LOI) ]               
if (class(dt4$OM_LOI)=="character") dt4$OM_LOI <-as.numeric(dt4$OM_LOI)

# Convert Missing Values to NA for non-dates

dt4$OM_TM <- ifelse((trimws(as.character(dt4$OM_TM))==trimws("-9999.9")),NA,dt4$OM_TM)               
suppressWarnings(dt4$OM_TM <- ifelse(!is.na(as.numeric("-9999.9")) & (trimws(as.character(dt4$OM_TM))==as.character(as.numeric("-9999.9"))),NA,dt4$OM_TM))
dt4$OM_TM <- ifelse((trimws(as.character(dt4$OM_TM))==trimws("-8888.8")),NA,dt4$OM_TM)               
suppressWarnings(dt4$OM_TM <- ifelse(!is.na(as.numeric("-8888.8")) & (trimws(as.character(dt4$OM_TM))==as.character(as.numeric("-8888.8"))),NA,dt4$OM_TM))
dt4$OM_OM <- ifelse((trimws(as.character(dt4$OM_OM))==trimws("-9999.9")),NA,dt4$OM_OM)               
suppressWarnings(dt4$OM_OM <- ifelse(!is.na(as.numeric("-9999.9")) & (trimws(as.character(dt4$OM_OM))==as.character(as.numeric("-9999.9"))),NA,dt4$OM_OM))
dt4$OM_OM <- ifelse((trimws(as.character(dt4$OM_OM))==trimws("-8888.8")),NA,dt4$OM_OM)               
suppressWarnings(dt4$OM_OM <- ifelse(!is.na(as.numeric("-8888.8")) & (trimws(as.character(dt4$OM_OM))==as.character(as.numeric("-8888.8"))),NA,dt4$OM_OM))
dt4$OM_LOI <- ifelse((trimws(as.character(dt4$OM_LOI))==trimws("-9999.9")),NA,dt4$OM_LOI)               
suppressWarnings(dt4$OM_LOI <- ifelse(!is.na(as.numeric("-9999.9")) & (trimws(as.character(dt4$OM_LOI))==as.character(as.numeric("-9999.9"))),NA,dt4$OM_LOI))
dt4$OM_LOI <- ifelse((trimws(as.character(dt4$OM_LOI))==trimws("-8888.8")),NA,dt4$OM_LOI)               
suppressWarnings(dt4$OM_LOI <- ifelse(!is.na(as.numeric("-8888.8")) & (trimws(as.character(dt4$OM_LOI))==as.character(as.numeric("-8888.8"))),NA,dt4$OM_LOI))


############################################################################

#######

############################################################################

# Package ID: knb-lter-hbr.176.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Mass and Chemistry of Organic Horizons and Surface Mineral Soils on Watershed 1 at the Hubbard Brook Experimental Forest 1996-present.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/176/3/0d3c7051d3d86680490227361153bdc7" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site_ID",     
                 "Year",     
                 "Plot",     
                 "Horizon",     
                 "pHw",     
                 "pHs",     
                 "PercentN",     
                 "PercentC",     
                 "ExAcidcmolc_kg",     
                 "ExCacmolc_kg",     
                 "ExMgcmolc_kg",     
                 "ExNacmolc_kg",     
                 "ExKcmolc_kg",     
                 "ExAlcmolc_kg",     
                 "ExSicmolc_kg",     
                 "ExAl_KClcmolc_kg",     
                 "Ca_g_m2",     
                 "Mg_g_m2",     
                 "K_g_m2",     
                 "P_g_m2",     
                 "Mn_g_m2",     
                 "Cu_g_m2",     
                 "Zn_g_m2",     
                 "Pb_g_m2",     
                 "Al_g_m2",     
                 "Fe_g_m2",     
                 "Ca_mg_kg",     
                 "Mg_mg_kg",     
                 "K_mg_kg",     
                 "P_mg_kg",     
                 "Mn_mg_kg",     
                 "Cu_mg_kg",     
                 "Zn_mg_kg",     
                 "Pb_mg_kg",     
                 "Al_mg_kg",     
                 "Fe_mg_kg",     
                 "TM",     
                 "OM_OM",     
                 "LOI"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Site_ID)!="factor") dt1$Site_ID<- as.factor(dt1$Site_ID)
if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Horizon)!="factor") dt1$Horizon<- as.factor(dt1$Horizon)
if (class(dt1$pHw)=="factor") dt1$pHw <-as.numeric(levels(dt1$pHw))[as.integer(dt1$pHw) ]               
if (class(dt1$pHw)=="character") dt1$pHw <-as.numeric(dt1$pHw)
if (class(dt1$pHs)=="factor") dt1$pHs <-as.numeric(levels(dt1$pHs))[as.integer(dt1$pHs) ]               
if (class(dt1$pHs)=="character") dt1$pHs <-as.numeric(dt1$pHs)
if (class(dt1$PercentN)=="factor") dt1$PercentN <-as.numeric(levels(dt1$PercentN))[as.integer(dt1$PercentN) ]               
if (class(dt1$PercentN)=="character") dt1$PercentN <-as.numeric(dt1$PercentN)
if (class(dt1$PercentC)=="factor") dt1$PercentC <-as.numeric(levels(dt1$PercentC))[as.integer(dt1$PercentC) ]               
if (class(dt1$PercentC)=="character") dt1$PercentC <-as.numeric(dt1$PercentC)
if (class(dt1$ExAcidcmolc_kg)=="factor") dt1$ExAcidcmolc_kg <-as.numeric(levels(dt1$ExAcidcmolc_kg))[as.integer(dt1$ExAcidcmolc_kg) ]               
if (class(dt1$ExAcidcmolc_kg)=="character") dt1$ExAcidcmolc_kg <-as.numeric(dt1$ExAcidcmolc_kg)
if (class(dt1$ExCacmolc_kg)=="factor") dt1$ExCacmolc_kg <-as.numeric(levels(dt1$ExCacmolc_kg))[as.integer(dt1$ExCacmolc_kg) ]               
if (class(dt1$ExCacmolc_kg)=="character") dt1$ExCacmolc_kg <-as.numeric(dt1$ExCacmolc_kg)
if (class(dt1$ExMgcmolc_kg)=="factor") dt1$ExMgcmolc_kg <-as.numeric(levels(dt1$ExMgcmolc_kg))[as.integer(dt1$ExMgcmolc_kg) ]               
if (class(dt1$ExMgcmolc_kg)=="character") dt1$ExMgcmolc_kg <-as.numeric(dt1$ExMgcmolc_kg)
if (class(dt1$ExNacmolc_kg)=="factor") dt1$ExNacmolc_kg <-as.numeric(levels(dt1$ExNacmolc_kg))[as.integer(dt1$ExNacmolc_kg) ]               
if (class(dt1$ExNacmolc_kg)=="character") dt1$ExNacmolc_kg <-as.numeric(dt1$ExNacmolc_kg)
if (class(dt1$ExKcmolc_kg)=="factor") dt1$ExKcmolc_kg <-as.numeric(levels(dt1$ExKcmolc_kg))[as.integer(dt1$ExKcmolc_kg) ]               
if (class(dt1$ExKcmolc_kg)=="character") dt1$ExKcmolc_kg <-as.numeric(dt1$ExKcmolc_kg)
if (class(dt1$ExAlcmolc_kg)=="factor") dt1$ExAlcmolc_kg <-as.numeric(levels(dt1$ExAlcmolc_kg))[as.integer(dt1$ExAlcmolc_kg) ]               
if (class(dt1$ExAlcmolc_kg)=="character") dt1$ExAlcmolc_kg <-as.numeric(dt1$ExAlcmolc_kg)
if (class(dt1$ExSicmolc_kg)=="factor") dt1$ExSicmolc_kg <-as.numeric(levels(dt1$ExSicmolc_kg))[as.integer(dt1$ExSicmolc_kg) ]               
if (class(dt1$ExSicmolc_kg)=="character") dt1$ExSicmolc_kg <-as.numeric(dt1$ExSicmolc_kg)
if (class(dt1$ExAl_KClcmolc_kg)=="factor") dt1$ExAl_KClcmolc_kg <-as.numeric(levels(dt1$ExAl_KClcmolc_kg))[as.integer(dt1$ExAl_KClcmolc_kg) ]               
if (class(dt1$ExAl_KClcmolc_kg)=="character") dt1$ExAl_KClcmolc_kg <-as.numeric(dt1$ExAl_KClcmolc_kg)
if (class(dt1$Ca_g_m2)=="factor") dt1$Ca_g_m2 <-as.numeric(levels(dt1$Ca_g_m2))[as.integer(dt1$Ca_g_m2) ]               
if (class(dt1$Ca_g_m2)=="character") dt1$Ca_g_m2 <-as.numeric(dt1$Ca_g_m2)
if (class(dt1$Mg_g_m2)=="factor") dt1$Mg_g_m2 <-as.numeric(levels(dt1$Mg_g_m2))[as.integer(dt1$Mg_g_m2) ]               
if (class(dt1$Mg_g_m2)=="character") dt1$Mg_g_m2 <-as.numeric(dt1$Mg_g_m2)
if (class(dt1$K_g_m2)=="factor") dt1$K_g_m2 <-as.numeric(levels(dt1$K_g_m2))[as.integer(dt1$K_g_m2) ]               
if (class(dt1$K_g_m2)=="character") dt1$K_g_m2 <-as.numeric(dt1$K_g_m2)
if (class(dt1$P_g_m2)=="factor") dt1$P_g_m2 <-as.numeric(levels(dt1$P_g_m2))[as.integer(dt1$P_g_m2) ]               
if (class(dt1$P_g_m2)=="character") dt1$P_g_m2 <-as.numeric(dt1$P_g_m2)
if (class(dt1$Mn_g_m2)=="factor") dt1$Mn_g_m2 <-as.numeric(levels(dt1$Mn_g_m2))[as.integer(dt1$Mn_g_m2) ]               
if (class(dt1$Mn_g_m2)=="character") dt1$Mn_g_m2 <-as.numeric(dt1$Mn_g_m2)
if (class(dt1$Cu_g_m2)=="factor") dt1$Cu_g_m2 <-as.numeric(levels(dt1$Cu_g_m2))[as.integer(dt1$Cu_g_m2) ]               
if (class(dt1$Cu_g_m2)=="character") dt1$Cu_g_m2 <-as.numeric(dt1$Cu_g_m2)
if (class(dt1$Zn_g_m2)=="factor") dt1$Zn_g_m2 <-as.numeric(levels(dt1$Zn_g_m2))[as.integer(dt1$Zn_g_m2) ]               
if (class(dt1$Zn_g_m2)=="character") dt1$Zn_g_m2 <-as.numeric(dt1$Zn_g_m2)
if (class(dt1$Pb_g_m2)=="factor") dt1$Pb_g_m2 <-as.numeric(levels(dt1$Pb_g_m2))[as.integer(dt1$Pb_g_m2) ]               
if (class(dt1$Pb_g_m2)=="character") dt1$Pb_g_m2 <-as.numeric(dt1$Pb_g_m2)
if (class(dt1$Al_g_m2)=="factor") dt1$Al_g_m2 <-as.numeric(levels(dt1$Al_g_m2))[as.integer(dt1$Al_g_m2) ]               
if (class(dt1$Al_g_m2)=="character") dt1$Al_g_m2 <-as.numeric(dt1$Al_g_m2)
if (class(dt1$Fe_g_m2)=="factor") dt1$Fe_g_m2 <-as.numeric(levels(dt1$Fe_g_m2))[as.integer(dt1$Fe_g_m2) ]               
if (class(dt1$Fe_g_m2)=="character") dt1$Fe_g_m2 <-as.numeric(dt1$Fe_g_m2)
if (class(dt1$Ca_mg_kg)=="factor") dt1$Ca_mg_kg <-as.numeric(levels(dt1$Ca_mg_kg))[as.integer(dt1$Ca_mg_kg) ]               
if (class(dt1$Ca_mg_kg)=="character") dt1$Ca_mg_kg <-as.numeric(dt1$Ca_mg_kg)
if (class(dt1$Mg_mg_kg)=="factor") dt1$Mg_mg_kg <-as.numeric(levels(dt1$Mg_mg_kg))[as.integer(dt1$Mg_mg_kg) ]               
if (class(dt1$Mg_mg_kg)=="character") dt1$Mg_mg_kg <-as.numeric(dt1$Mg_mg_kg)
if (class(dt1$K_mg_kg)=="factor") dt1$K_mg_kg <-as.numeric(levels(dt1$K_mg_kg))[as.integer(dt1$K_mg_kg) ]               
if (class(dt1$K_mg_kg)=="character") dt1$K_mg_kg <-as.numeric(dt1$K_mg_kg)
if (class(dt1$P_mg_kg)=="factor") dt1$P_mg_kg <-as.numeric(levels(dt1$P_mg_kg))[as.integer(dt1$P_mg_kg) ]               
if (class(dt1$P_mg_kg)=="character") dt1$P_mg_kg <-as.numeric(dt1$P_mg_kg)
if (class(dt1$Mn_mg_kg)=="factor") dt1$Mn_mg_kg <-as.numeric(levels(dt1$Mn_mg_kg))[as.integer(dt1$Mn_mg_kg) ]               
if (class(dt1$Mn_mg_kg)=="character") dt1$Mn_mg_kg <-as.numeric(dt1$Mn_mg_kg)
if (class(dt1$Cu_mg_kg)=="factor") dt1$Cu_mg_kg <-as.numeric(levels(dt1$Cu_mg_kg))[as.integer(dt1$Cu_mg_kg) ]               
if (class(dt1$Cu_mg_kg)=="character") dt1$Cu_mg_kg <-as.numeric(dt1$Cu_mg_kg)
if (class(dt1$Zn_mg_kg)=="factor") dt1$Zn_mg_kg <-as.numeric(levels(dt1$Zn_mg_kg))[as.integer(dt1$Zn_mg_kg) ]               
if (class(dt1$Zn_mg_kg)=="character") dt1$Zn_mg_kg <-as.numeric(dt1$Zn_mg_kg)
if (class(dt1$Pb_mg_kg)=="factor") dt1$Pb_mg_kg <-as.numeric(levels(dt1$Pb_mg_kg))[as.integer(dt1$Pb_mg_kg) ]               
if (class(dt1$Pb_mg_kg)=="character") dt1$Pb_mg_kg <-as.numeric(dt1$Pb_mg_kg)
if (class(dt1$Al_mg_kg)=="factor") dt1$Al_mg_kg <-as.numeric(levels(dt1$Al_mg_kg))[as.integer(dt1$Al_mg_kg) ]               
if (class(dt1$Al_mg_kg)=="character") dt1$Al_mg_kg <-as.numeric(dt1$Al_mg_kg)
if (class(dt1$Fe_mg_kg)=="factor") dt1$Fe_mg_kg <-as.numeric(levels(dt1$Fe_mg_kg))[as.integer(dt1$Fe_mg_kg) ]               
if (class(dt1$Fe_mg_kg)=="character") dt1$Fe_mg_kg <-as.numeric(dt1$Fe_mg_kg)
if (class(dt1$TM)=="factor") dt1$TM <-as.numeric(levels(dt1$TM))[as.integer(dt1$TM) ]               
if (class(dt1$TM)=="character") dt1$TM <-as.numeric(dt1$TM)
if (class(dt1$OM_OM)=="factor") dt1$OM_OM <-as.numeric(levels(dt1$OM_OM))[as.integer(dt1$OM_OM) ]               
if (class(dt1$OM_OM)=="character") dt1$OM_OM <-as.numeric(dt1$OM_OM)
if (class(dt1$LOI)=="factor") dt1$LOI <-as.numeric(levels(dt1$LOI))[as.integer(dt1$LOI) ]               
if (class(dt1$LOI)=="character") dt1$LOI <-as.numeric(dt1$LOI)

# Convert Missing Values to NA for non-dates

dt1$pHw <- ifelse((trimws(as.character(dt1$pHw))==trimws("-999.99")),NA,dt1$pHw)               
suppressWarnings(dt1$pHw <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$pHw))==as.character(as.numeric("-999.99"))),NA,dt1$pHw))
dt1$pHw <- ifelse((trimws(as.character(dt1$pHw))==trimws("-888.88")),NA,dt1$pHw)               
suppressWarnings(dt1$pHw <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pHw))==as.character(as.numeric("-888.88"))),NA,dt1$pHw))
dt1$pHs <- ifelse((trimws(as.character(dt1$pHs))==trimws("-999.99")),NA,dt1$pHs)               
suppressWarnings(dt1$pHs <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$pHs))==as.character(as.numeric("-999.99"))),NA,dt1$pHs))
dt1$pHs <- ifelse((trimws(as.character(dt1$pHs))==trimws("-888.88")),NA,dt1$pHs)               
suppressWarnings(dt1$pHs <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$pHs))==as.character(as.numeric("-888.88"))),NA,dt1$pHs))
dt1$PercentN <- ifelse((trimws(as.character(dt1$PercentN))==trimws("-999.99")),NA,dt1$PercentN)               
suppressWarnings(dt1$PercentN <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$PercentN))==as.character(as.numeric("-999.99"))),NA,dt1$PercentN))
dt1$PercentN <- ifelse((trimws(as.character(dt1$PercentN))==trimws("-888.88")),NA,dt1$PercentN)               
suppressWarnings(dt1$PercentN <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PercentN))==as.character(as.numeric("-888.88"))),NA,dt1$PercentN))
dt1$PercentC <- ifelse((trimws(as.character(dt1$PercentC))==trimws("-999.99")),NA,dt1$PercentC)               
suppressWarnings(dt1$PercentC <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$PercentC))==as.character(as.numeric("-999.99"))),NA,dt1$PercentC))
dt1$PercentC <- ifelse((trimws(as.character(dt1$PercentC))==trimws("-888.88")),NA,dt1$PercentC)               
suppressWarnings(dt1$PercentC <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$PercentC))==as.character(as.numeric("-888.88"))),NA,dt1$PercentC))
dt1$ExAcidcmolc_kg <- ifelse((trimws(as.character(dt1$ExAcidcmolc_kg))==trimws("-999.99")),NA,dt1$ExAcidcmolc_kg)               
suppressWarnings(dt1$ExAcidcmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExAcidcmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExAcidcmolc_kg))
dt1$ExAcidcmolc_kg <- ifelse((trimws(as.character(dt1$ExAcidcmolc_kg))==trimws("-888.88")),NA,dt1$ExAcidcmolc_kg)               
suppressWarnings(dt1$ExAcidcmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExAcidcmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExAcidcmolc_kg))
dt1$ExCacmolc_kg <- ifelse((trimws(as.character(dt1$ExCacmolc_kg))==trimws("-999.99")),NA,dt1$ExCacmolc_kg)               
suppressWarnings(dt1$ExCacmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExCacmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExCacmolc_kg))
dt1$ExCacmolc_kg <- ifelse((trimws(as.character(dt1$ExCacmolc_kg))==trimws("-888.88")),NA,dt1$ExCacmolc_kg)               
suppressWarnings(dt1$ExCacmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExCacmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExCacmolc_kg))
dt1$ExMgcmolc_kg <- ifelse((trimws(as.character(dt1$ExMgcmolc_kg))==trimws("-999.99")),NA,dt1$ExMgcmolc_kg)               
suppressWarnings(dt1$ExMgcmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExMgcmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExMgcmolc_kg))
dt1$ExMgcmolc_kg <- ifelse((trimws(as.character(dt1$ExMgcmolc_kg))==trimws("-888.88")),NA,dt1$ExMgcmolc_kg)               
suppressWarnings(dt1$ExMgcmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExMgcmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExMgcmolc_kg))
dt1$ExNacmolc_kg <- ifelse((trimws(as.character(dt1$ExNacmolc_kg))==trimws("-999.99")),NA,dt1$ExNacmolc_kg)               
suppressWarnings(dt1$ExNacmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExNacmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExNacmolc_kg))
dt1$ExNacmolc_kg <- ifelse((trimws(as.character(dt1$ExNacmolc_kg))==trimws("-888.88")),NA,dt1$ExNacmolc_kg)               
suppressWarnings(dt1$ExNacmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExNacmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExNacmolc_kg))
dt1$ExKcmolc_kg <- ifelse((trimws(as.character(dt1$ExKcmolc_kg))==trimws("-999.99")),NA,dt1$ExKcmolc_kg)               
suppressWarnings(dt1$ExKcmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExKcmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExKcmolc_kg))
dt1$ExKcmolc_kg <- ifelse((trimws(as.character(dt1$ExKcmolc_kg))==trimws("-888.88")),NA,dt1$ExKcmolc_kg)               
suppressWarnings(dt1$ExKcmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExKcmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExKcmolc_kg))
dt1$ExAlcmolc_kg <- ifelse((trimws(as.character(dt1$ExAlcmolc_kg))==trimws("-999.99")),NA,dt1$ExAlcmolc_kg)               
suppressWarnings(dt1$ExAlcmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExAlcmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExAlcmolc_kg))
dt1$ExAlcmolc_kg <- ifelse((trimws(as.character(dt1$ExAlcmolc_kg))==trimws("-888.88")),NA,dt1$ExAlcmolc_kg)               
suppressWarnings(dt1$ExAlcmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExAlcmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExAlcmolc_kg))
dt1$ExSicmolc_kg <- ifelse((trimws(as.character(dt1$ExSicmolc_kg))==trimws("-999.99")),NA,dt1$ExSicmolc_kg)               
suppressWarnings(dt1$ExSicmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExSicmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExSicmolc_kg))
dt1$ExSicmolc_kg <- ifelse((trimws(as.character(dt1$ExSicmolc_kg))==trimws("-888.88")),NA,dt1$ExSicmolc_kg)               
suppressWarnings(dt1$ExSicmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExSicmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExSicmolc_kg))
dt1$ExAl_KClcmolc_kg <- ifelse((trimws(as.character(dt1$ExAl_KClcmolc_kg))==trimws("-999.99")),NA,dt1$ExAl_KClcmolc_kg)               
suppressWarnings(dt1$ExAl_KClcmolc_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$ExAl_KClcmolc_kg))==as.character(as.numeric("-999.99"))),NA,dt1$ExAl_KClcmolc_kg))
dt1$ExAl_KClcmolc_kg <- ifelse((trimws(as.character(dt1$ExAl_KClcmolc_kg))==trimws("-888.88")),NA,dt1$ExAl_KClcmolc_kg)               
suppressWarnings(dt1$ExAl_KClcmolc_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$ExAl_KClcmolc_kg))==as.character(as.numeric("-888.88"))),NA,dt1$ExAl_KClcmolc_kg))
dt1$Ca_g_m2 <- ifelse((trimws(as.character(dt1$Ca_g_m2))==trimws("-999.99")),NA,dt1$Ca_g_m2)               
suppressWarnings(dt1$Ca_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Ca_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Ca_g_m2))
dt1$Ca_g_m2 <- ifelse((trimws(as.character(dt1$Ca_g_m2))==trimws("-888.88")),NA,dt1$Ca_g_m2)               
suppressWarnings(dt1$Ca_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_g_m2))
dt1$Mg_g_m2 <- ifelse((trimws(as.character(dt1$Mg_g_m2))==trimws("-999.99")),NA,dt1$Mg_g_m2)               
suppressWarnings(dt1$Mg_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Mg_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Mg_g_m2))
dt1$Mg_g_m2 <- ifelse((trimws(as.character(dt1$Mg_g_m2))==trimws("-888.88")),NA,dt1$Mg_g_m2)               
suppressWarnings(dt1$Mg_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_g_m2))
dt1$K_g_m2 <- ifelse((trimws(as.character(dt1$K_g_m2))==trimws("-999.99")),NA,dt1$K_g_m2)               
suppressWarnings(dt1$K_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$K_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$K_g_m2))
dt1$K_g_m2 <- ifelse((trimws(as.character(dt1$K_g_m2))==trimws("-888.88")),NA,dt1$K_g_m2)               
suppressWarnings(dt1$K_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$K_g_m2))
dt1$P_g_m2 <- ifelse((trimws(as.character(dt1$P_g_m2))==trimws("-999.99")),NA,dt1$P_g_m2)               
suppressWarnings(dt1$P_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$P_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$P_g_m2))
dt1$P_g_m2 <- ifelse((trimws(as.character(dt1$P_g_m2))==trimws("-888.88")),NA,dt1$P_g_m2)               
suppressWarnings(dt1$P_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$P_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$P_g_m2))
dt1$Mn_g_m2 <- ifelse((trimws(as.character(dt1$Mn_g_m2))==trimws("-999.99")),NA,dt1$Mn_g_m2)               
suppressWarnings(dt1$Mn_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Mn_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Mn_g_m2))
dt1$Mn_g_m2 <- ifelse((trimws(as.character(dt1$Mn_g_m2))==trimws("-888.88")),NA,dt1$Mn_g_m2)               
suppressWarnings(dt1$Mn_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_g_m2))
dt1$Cu_g_m2 <- ifelse((trimws(as.character(dt1$Cu_g_m2))==trimws("-999.99")),NA,dt1$Cu_g_m2)               
suppressWarnings(dt1$Cu_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Cu_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Cu_g_m2))
dt1$Cu_g_m2 <- ifelse((trimws(as.character(dt1$Cu_g_m2))==trimws("-888.88")),NA,dt1$Cu_g_m2)               
suppressWarnings(dt1$Cu_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cu_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Cu_g_m2))
dt1$Zn_g_m2 <- ifelse((trimws(as.character(dt1$Zn_g_m2))==trimws("-999.99")),NA,dt1$Zn_g_m2)               
suppressWarnings(dt1$Zn_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Zn_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Zn_g_m2))
dt1$Zn_g_m2 <- ifelse((trimws(as.character(dt1$Zn_g_m2))==trimws("-888.88")),NA,dt1$Zn_g_m2)               
suppressWarnings(dt1$Zn_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Zn_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Zn_g_m2))
dt1$Pb_g_m2 <- ifelse((trimws(as.character(dt1$Pb_g_m2))==trimws("-999.99")),NA,dt1$Pb_g_m2)               
suppressWarnings(dt1$Pb_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Pb_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Pb_g_m2))
dt1$Pb_g_m2 <- ifelse((trimws(as.character(dt1$Pb_g_m2))==trimws("-888.88")),NA,dt1$Pb_g_m2)               
suppressWarnings(dt1$Pb_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Pb_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Pb_g_m2))
dt1$Al_g_m2 <- ifelse((trimws(as.character(dt1$Al_g_m2))==trimws("-999.99")),NA,dt1$Al_g_m2)               
suppressWarnings(dt1$Al_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Al_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Al_g_m2))
dt1$Al_g_m2 <- ifelse((trimws(as.character(dt1$Al_g_m2))==trimws("-888.88")),NA,dt1$Al_g_m2)               
suppressWarnings(dt1$Al_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Al_g_m2))
dt1$Fe_g_m2 <- ifelse((trimws(as.character(dt1$Fe_g_m2))==trimws("-999.99")),NA,dt1$Fe_g_m2)               
suppressWarnings(dt1$Fe_g_m2 <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Fe_g_m2))==as.character(as.numeric("-999.99"))),NA,dt1$Fe_g_m2))
dt1$Fe_g_m2 <- ifelse((trimws(as.character(dt1$Fe_g_m2))==trimws("-888.88")),NA,dt1$Fe_g_m2)               
suppressWarnings(dt1$Fe_g_m2 <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_g_m2))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_g_m2))
dt1$Ca_mg_kg <- ifelse((trimws(as.character(dt1$Ca_mg_kg))==trimws("-999.99")),NA,dt1$Ca_mg_kg)               
suppressWarnings(dt1$Ca_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Ca_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Ca_mg_kg))
dt1$Ca_mg_kg <- ifelse((trimws(as.character(dt1$Ca_mg_kg))==trimws("-888.88")),NA,dt1$Ca_mg_kg)               
suppressWarnings(dt1$Ca_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Ca_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Ca_mg_kg))
dt1$Mg_mg_kg <- ifelse((trimws(as.character(dt1$Mg_mg_kg))==trimws("-999.99")),NA,dt1$Mg_mg_kg)               
suppressWarnings(dt1$Mg_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Mg_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Mg_mg_kg))
dt1$Mg_mg_kg <- ifelse((trimws(as.character(dt1$Mg_mg_kg))==trimws("-888.88")),NA,dt1$Mg_mg_kg)               
suppressWarnings(dt1$Mg_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mg_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Mg_mg_kg))
dt1$K_mg_kg <- ifelse((trimws(as.character(dt1$K_mg_kg))==trimws("-999.99")),NA,dt1$K_mg_kg)               
suppressWarnings(dt1$K_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$K_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$K_mg_kg))
dt1$K_mg_kg <- ifelse((trimws(as.character(dt1$K_mg_kg))==trimws("-888.88")),NA,dt1$K_mg_kg)               
suppressWarnings(dt1$K_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$K_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$K_mg_kg))
dt1$P_mg_kg <- ifelse((trimws(as.character(dt1$P_mg_kg))==trimws("-999.99")),NA,dt1$P_mg_kg)               
suppressWarnings(dt1$P_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$P_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$P_mg_kg))
dt1$P_mg_kg <- ifelse((trimws(as.character(dt1$P_mg_kg))==trimws("-888.88")),NA,dt1$P_mg_kg)               
suppressWarnings(dt1$P_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$P_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$P_mg_kg))
dt1$Mn_mg_kg <- ifelse((trimws(as.character(dt1$Mn_mg_kg))==trimws("-999.99")),NA,dt1$Mn_mg_kg)               
suppressWarnings(dt1$Mn_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Mn_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Mn_mg_kg))
dt1$Mn_mg_kg <- ifelse((trimws(as.character(dt1$Mn_mg_kg))==trimws("-888.88")),NA,dt1$Mn_mg_kg)               
suppressWarnings(dt1$Mn_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Mn_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Mn_mg_kg))
dt1$Cu_mg_kg <- ifelse((trimws(as.character(dt1$Cu_mg_kg))==trimws("-999.99")),NA,dt1$Cu_mg_kg)               
suppressWarnings(dt1$Cu_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Cu_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Cu_mg_kg))
dt1$Cu_mg_kg <- ifelse((trimws(as.character(dt1$Cu_mg_kg))==trimws("-888.88")),NA,dt1$Cu_mg_kg)               
suppressWarnings(dt1$Cu_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Cu_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Cu_mg_kg))
dt1$Zn_mg_kg <- ifelse((trimws(as.character(dt1$Zn_mg_kg))==trimws("-999.99")),NA,dt1$Zn_mg_kg)               
suppressWarnings(dt1$Zn_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Zn_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Zn_mg_kg))
dt1$Zn_mg_kg <- ifelse((trimws(as.character(dt1$Zn_mg_kg))==trimws("-888.88")),NA,dt1$Zn_mg_kg)               
suppressWarnings(dt1$Zn_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Zn_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Zn_mg_kg))
dt1$Pb_mg_kg <- ifelse((trimws(as.character(dt1$Pb_mg_kg))==trimws("-999.99")),NA,dt1$Pb_mg_kg)               
suppressWarnings(dt1$Pb_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Pb_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Pb_mg_kg))
dt1$Pb_mg_kg <- ifelse((trimws(as.character(dt1$Pb_mg_kg))==trimws("-888.88")),NA,dt1$Pb_mg_kg)               
suppressWarnings(dt1$Pb_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Pb_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Pb_mg_kg))
dt1$Al_mg_kg <- ifelse((trimws(as.character(dt1$Al_mg_kg))==trimws("-999.99")),NA,dt1$Al_mg_kg)               
suppressWarnings(dt1$Al_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Al_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Al_mg_kg))
dt1$Al_mg_kg <- ifelse((trimws(as.character(dt1$Al_mg_kg))==trimws("-888.88")),NA,dt1$Al_mg_kg)               
suppressWarnings(dt1$Al_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Al_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Al_mg_kg))
dt1$Fe_mg_kg <- ifelse((trimws(as.character(dt1$Fe_mg_kg))==trimws("-999.99")),NA,dt1$Fe_mg_kg)               
suppressWarnings(dt1$Fe_mg_kg <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$Fe_mg_kg))==as.character(as.numeric("-999.99"))),NA,dt1$Fe_mg_kg))
dt1$Fe_mg_kg <- ifelse((trimws(as.character(dt1$Fe_mg_kg))==trimws("-888.88")),NA,dt1$Fe_mg_kg)               
suppressWarnings(dt1$Fe_mg_kg <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$Fe_mg_kg))==as.character(as.numeric("-888.88"))),NA,dt1$Fe_mg_kg))
dt1$TM <- ifelse((trimws(as.character(dt1$TM))==trimws("-999.99")),NA,dt1$TM)               
suppressWarnings(dt1$TM <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$TM))==as.character(as.numeric("-999.99"))),NA,dt1$TM))
dt1$TM <- ifelse((trimws(as.character(dt1$TM))==trimws("-888.88")),NA,dt1$TM)               
suppressWarnings(dt1$TM <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$TM))==as.character(as.numeric("-888.88"))),NA,dt1$TM))
dt1$OM_OM <- ifelse((trimws(as.character(dt1$OM_OM))==trimws("-999.99")),NA,dt1$OM_OM)               
suppressWarnings(dt1$OM_OM <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OM_OM))==as.character(as.numeric("-999.99"))),NA,dt1$OM_OM))
dt1$OM_OM <- ifelse((trimws(as.character(dt1$OM_OM))==trimws("-888.88")),NA,dt1$OM_OM)               
suppressWarnings(dt1$OM_OM <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$OM_OM))==as.character(as.numeric("-888.88"))),NA,dt1$OM_OM))
dt1$LOI <- ifelse((trimws(as.character(dt1$LOI))==trimws("-999.99")),NA,dt1$LOI)               
suppressWarnings(dt1$LOI <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$LOI))==as.character(as.numeric("-999.99"))),NA,dt1$LOI))
dt1$LOI <- ifelse((trimws(as.character(dt1$LOI))==trimws("-888.88")),NA,dt1$LOI)               
suppressWarnings(dt1$LOI <- ifelse(!is.na(as.numeric("-888.88")) & (trimws(as.character(dt1$LOI))==as.character(as.numeric("-888.88"))),NA,dt1$LOI))
##############################################################################

#################

##############################################################################


head(dt1)
head(dt4)

names(dt1)
names(dt4)
# combine watershed, years, horizons, sampleID, and OM_OM
w1<-dt1[ , c(2,3,4,38)]
w1$Watershed<-"1"
w6<-dt4[ , c(2,4,5,7,3)]

head(w1)
head(w6)

om<-rbind(w1,w6)
str(om)


library(lubridate)
library(ggplot2)

# this shows all the samples.
g1<-ggplot(om, aes(x=Year, y=OM_OM, col=Watershed))+geom_point()+
  ylab("Organic matter (kg/m2)")+facet_wrap(~Horizon)
g1

table(om$Year, om$Watershed)

oa<-om[om$Horizon=="Oa",]
oie<-om[om$Horizon=="Oie",]

fom<-rbind(oa, oie)
table(fom$Horizon, fom$Watershed)



yom<-aggregate(list(OM=fom$OM_OM), by=list(Year=fom$Year, Watershed=fom$Watershed, Horizon=fom$Horizon), FUN="mean", na.rm=T)
yom


# calculate SE
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x, na.rm=T)/sqrt(length(x))}

yse<-aggregate(list(OMSE=fom$OM_OM), by=list(Year=fom$Year, Watershed=fom$Watershed, Horizon=fom$Horizon), FUN=st.err)


# add in se to yom
yom$se<-yse$OMSE
  


# just plot the 2 horizons
yom$Watershed<-factor(yom$Watershed, levels=c(6,1))
yom$Horizon<-factor(yom$Horizon, levels=c("Oie","Oa"))

head(yom, 100)
 yo<-yom[ -c(1,16),]
 head(yo, 100)
 str(yo)
 

 # annotations
 a <- list(
   text = "Oi+Oe Horizon",
   xref = "paper",
   yref = "paper",
   yanchor = "bottom",
   xanchor = "center",
   align = "center",
   x = 0.5,
   y = 1,
   showarrow = FALSE
 )
 
 b <- list(
   text = "Oa Horizon",
   xref = "paper",
   yref = "paper",
   yanchor = "bottom",
   xanchor = "center",
   align = "center",
   x = 0.5,
   y = 1,
   showarrow = FALSE
 )
 
 
 
 

 g2<-ggplot(yo[yo$Horizon=="Oie",], aes(x=Year, y=OM, col=Watershed))+geom_point()+
  scale_y_continuous(limits=c(0,4), breaks=)+
   scale_x_continuous(expand = c(0, 0), limits = c(1995, 2020)) +
   scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  ylab("Organic matter (kg/m2)")+ggtitle("")+
  geom_errorbar(aes(ymin=OM-se, ymax=OM+se, width=.4))+
  scale_color_manual(values=c("black","green"))+geom_line()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_text(vjust = +2),
                   plot.title = element_text(hjust = 0.5), text=element_text(size=24))
g2
 
g3<-ggplot(yo[yo$Horizon=="Oa",], aes(x=Year, y=OM, col=Watershed))+geom_point()+
   scale_y_continuous(expand = c(0, 0), limits=c(0,9), breaks=c(0,2,4,6,8))+
  scale_x_continuous(expand = c(0, 0), limits = c(1995, 2020)) +
  ylab("Organic matter (kg/m2)")+ggtitle("")+
   geom_errorbar(aes(ymin=OM-se, ymax=OM+se, width=.4))+
   scale_color_manual(values=c("black","green"))+geom_line()+
   theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    plot.title = element_text(hjust = 0.5),text=element_text(size=24))
g3


library(plotly)

m <- list(
  l = 200,
  r = 50,
  b = 100,
  t = 100,
  pad = 10
)


p2<-ggplotly(g2)%>%
  layout(annotations = a, margin=m)

p2
p3<-ggplotly(g3)%>%
  layout(annotations = b, margin=m)

p3

p23<-subplot(p2, p3,shareX = TRUE, titleY=TRUE,margin=.1)

htmlwidgets::saveWidget(as_widget(p23), "decompCarbon/Fig5_organicMatter.html")

####################################################################################################

#####################

####################################################################################################

#Figure 4, you add the Oie and Oa data together for each plot and multiply by 10 to convert from kg/m2 to Mg/ha. 


#Figure 4, you add the Oie and Oa data together for each plot and multiply by 10 to convert from kg/m2 to Mg/ha. 


table(fom$Watershed)

ref<-om[om$Watershed=="6",]
head(ref)


head(fom)
rom<-aggregate(list(OM=ref$OM_OM), by=list(Year=ref$Year, Plot=ref$Plot), FUN="sum", na.rm=T)
rom$MG.ha<-rom$OM*10

oom<-aggregate(list(OM=rom$MG.ha), by=list(Year=rom$Year), FUN="mean", na.rm=T)
head(oom)


st.err <- function(x) {
  sd(x, na.rm=T)/sqrt(length(x))}


head(oom, 200)
library(tidyr)
ose<-aggregate(rom$MG.ha, by=list(Year=rom$Year), FUN=st.err)
ose

# add in se to yom
oom$se<-ose$x

head(oom)

y2<-ggplot(oom, aes(x=Year, y=OM))+geom_point(size=4)+geom_errorbar(aes(ymin=OM-se, ymax=OM+se))+
  scale_y_continuous(expand = c(0, 0), limits=c(0,100), breaks=c(0,20,40,60,80,100))+
  scale_x_continuous(expand = c(0, 0), limits = c(1970, 2022))+
  ylab("Forest floor OM mass (Mg/ha)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    text=element_text(size=24))



yp2<-ggplotly(y2)

yp2  

htmlwidgets::saveWidget(as_widget(yp2), "decompCarbon/Fig4_organicMatter_W6.html")
