#Campbell Water Balance Graph

#setwd("D:/Fahey/living graphs")

#Install packages
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("ggplotify")
#install.packages("cowplot")

#Use the libraries
library("plyr")
library("ggplot2")
library("ggplotify")
library("cowplot")
library("grid")
library("gridExtra")
library(plotly)

#Download streamflow data by watershed
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/2/14/8c8494334113ddab7ba6591cc6fcd8d3" 
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

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$WS)!="factor") dt1$WS<- as.factor(dt1$WS)
if (class(dt1$Streamflow)=="factor") dt1$Streamflow <-as.numeric(levels(dt1$Streamflow))[as.integer(dt1$Streamflow) ]               
if (class(dt1$Streamflow)=="character") dt1$Streamflow <-as.numeric(dt1$Streamflow)
if (class(dt1$Flag)=="factor") dt1$Flag <-as.numeric(levels(dt1$Flag))[as.integer(dt1$Flag) ]               
if (class(dt1$Flag)=="character") dt1$Flag <-as.numeric(dt1$Flag)

# Convert Missing Values to NA for non-dates
dt1$Flag <- ifelse((trimws(as.character(dt1$Flag))==trimws("NA")),NA,dt1$Flag)               
suppressWarnings(dt1$Flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Flag))==as.character(as.numeric("NA"))),NA,dt1$Flag))

streamflow <- dt1

#Download precipitation data by watershed
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/14/18/c606bfe2f2deb3fa3eabf692ae15f02d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "DATE",     
                 "watershed",     
                 "Precip"    ), check.names=TRUE)

unlink(infile1)

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$watershed)!="factor") dt1$watershed<- as.factor(dt1$watershed)
if (class(dt1$Precip)=="factor") dt1$Precip <-as.numeric(levels(dt1$Precip))[as.integer(dt1$Precip) ]               
if (class(dt1$Precip)=="character") dt1$Precip <-as.numeric(dt1$Precip)

precipitation <- dt1

colnames(precipitation) <- c("date","watershed","precip_mm")
streamflow$Flag <- NULL
colnames(streamflow) <- c("date","watershed","strflow_mm")
precipitation$watershed <- as.character(precipitation$watershed)
precipitation$watershed[(precipitation$watershed == "W1")] <- 1
precipitation$watershed[(precipitation$watershed == "W2")] <- 2
precipitation$watershed[(precipitation$watershed == "W3")] <- 3
precipitation$watershed[(precipitation$watershed == "W4")] <- 4
precipitation$watershed[(precipitation$watershed == "W5")] <- 5
precipitation$watershed[(precipitation$watershed == "W6")] <- 6
precipitation$watershed[(precipitation$watershed == "W7")] <- 7
precipitation$watershed[(precipitation$watershed == "W8")] <- 8
precipitation$watershed[(precipitation$watershed == "W9")] <- 9

waterbudget <- merge(streamflow,precipitation, by = c("date", "watershed"), all = FALSE)

temp_date <- waterbudget$date
year <- substr(temp_date,1,4)
year <- as.matrix(year)
month <-  substr(temp_date,6,7)
month <- as.matrix(month)
year_month <- paste(year,month,sep="-")
year_month <- as.matrix(year_month)

# water year June 1st - May 31st
# assign water year depending if month < 6
water_year <- function(year,month)
{
  tYear <- as.numeric(year)
  tMon <- as.numeric(month)
  WYear <- ifelse(tMon<6,tYear-1,tYear)
  return (WYear)
}

wy <- water_year(year,month)
wy <- as.matrix(wy)

waterbudget <- cbind(waterbudget,wy)

waterbudget_W1 <- subset(waterbudget,(watershed==1))
waterbudget_W2 <- subset(waterbudget,(watershed==2))
waterbudget_W3 <- subset(waterbudget,(watershed==3))
waterbudget_W4 <- subset(waterbudget,(watershed==4))
waterbudget_W5 <- subset(waterbudget,(watershed==5))
waterbudget_W6 <- subset(waterbudget,(watershed==6))
waterbudget_W7 <- subset(waterbudget,(watershed==7))
waterbudget_W8 <- subset(waterbudget,(watershed==8))
waterbudget_W9 <- subset(waterbudget,(watershed==9))

waterbudget_W1$watershed <- NULL
waterbudget_W2$watershed <- NULL
waterbudget_W3$watershed <- NULL
waterbudget_W4$watershed <- NULL
waterbudget_W5$watershed <- NULL
waterbudget_W6$watershed <- NULL
waterbudget_W7$watershed <- NULL
waterbudget_W8$watershed <- NULL
waterbudget_W9$watershed <- NULL

colnames(waterbudget_W1) <- c("date","strflow_mm_W1", "precip_mm_W1","wy")
colnames(waterbudget_W2) <- c("date","strflow_mm_W2", "precip_mm_W2","wy")
colnames(waterbudget_W3) <- c("date","strflow_mm_W3", "precip_mm_W3","wy")
colnames(waterbudget_W4) <- c("date","strflow_mm_W4", "precip_mm_W4","wy")
colnames(waterbudget_W5) <- c("date","strflow_mm_W5", "precip_mm_W5","wy")
colnames(waterbudget_W6) <- c("date","strflow_mm_W6", "precip_mm_W6","wy")
colnames(waterbudget_W7) <- c("date","strflow_mm_W7", "precip_mm_W7","wy")
colnames(waterbudget_W8) <- c("date","strflow_mm_W8", "precip_mm_W8","wy")
colnames(waterbudget_W9) <- c("date","strflow_mm_W9", "precip_mm_W9","wy")

WY_day_count_W1 <- count(waterbudget_W1, "wy")
keep_wy_W1 <- subset(WY_day_count_W1,(freq>=365))
waterbudget_W1_merge <- merge(waterbudget_W1,keep_wy_W1, by = c("wy"))
waterbudget_W1_merge$freq <- NULL

WY_day_count_W2 <- count(waterbudget_W2, "wy")
keep_wy_W2 <- subset(WY_day_count_W2,(freq>=365))
waterbudget_W2_merge <- merge(waterbudget_W2,keep_wy_W2, by = c("wy"))
waterbudget_W2_merge$freq <- NULL 

WY_day_count_W3 <- count(waterbudget_W3, "wy")
keep_wy_W3 <- subset(WY_day_count_W3,(freq>=365))
waterbudget_W3_merge <- merge(waterbudget_W3,keep_wy_W3, by = c("wy"))
waterbudget_W3_merge$freq <- NULL 

WY_day_count_W4 <- count(waterbudget_W4, "wy")
keep_wy_W4 <- subset(WY_day_count_W4,(freq>=365))
waterbudget_W4_merge <- merge(waterbudget_W4,keep_wy_W4, by = c("wy"))
waterbudget_W4_merge$freq <- NULL 

WY_day_count_W5 <- count(waterbudget_W5, "wy")
keep_wy_W5 <- subset(WY_day_count_W5,(freq>=365))
waterbudget_W5_merge <- merge(waterbudget_W5,keep_wy_W5, by = c("wy"))
waterbudget_W5_merge$freq <- NULL 

WY_day_count_W6 <- count(waterbudget_W6, "wy")
keep_wy_W6 <- subset(WY_day_count_W6,(freq>=365))
waterbudget_W6_merge <- merge(waterbudget_W6,keep_wy_W6, by = c("wy"))
waterbudget_W6_merge$freq <- NULL 

WY_day_count_W7 <- count(waterbudget_W7, "wy")
keep_wy_W7 <- subset(WY_day_count_W7,(freq>=365))
waterbudget_W7_merge <- merge(waterbudget_W7,keep_wy_W7, by = c("wy"))
waterbudget_W7_merge$freq <- NULL 

WY_day_count_W8 <- count(waterbudget_W8, "wy")
keep_wy_W8 <- subset(WY_day_count_W8,(freq>=365))
waterbudget_W8_merge <- merge(waterbudget_W8,keep_wy_W8, by = c("wy"))
waterbudget_W8_merge$freq <- NULL 

WY_day_count_W9 <- count(waterbudget_W9, "wy")
keep_wy_W9 <- subset(WY_day_count_W9,(freq>=365))
waterbudget_W9_merge <- merge(waterbudget_W9,keep_wy_W9, by = c("wy"))
waterbudget_W9_merge$freq <- NULL

wb <- merge(waterbudget_W1_merge,waterbudget_W2_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W3_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W4_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W5_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W6_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W7_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W8_merge, by = c("date","wy"), all = TRUE)
wb <- merge(wb,waterbudget_W9_merge, by = c("date","wy"), all = TRUE)

wb$date <- NULL

wb_wy <- aggregate(wb, by=list(wb$wy), FUN = sum)
wb_wy$wy <- NULL
names(wb_wy)[names(wb_wy) == "Group.1"] <- "wy"
wb_wy$et_mm_W1 <- wb_wy$precip_mm_W1 - wb_wy$strflow_mm_W1
wb_wy$et_mm_W2 <- wb_wy$precip_mm_W2 - wb_wy$strflow_mm_W2
wb_wy$et_mm_W3 <- wb_wy$precip_mm_W3 - wb_wy$strflow_mm_W3
wb_wy$et_mm_W4 <- wb_wy$precip_mm_W4 - wb_wy$strflow_mm_W4
wb_wy$et_mm_W5 <- wb_wy$precip_mm_W5 - wb_wy$strflow_mm_W5
wb_wy$et_mm_W6 <- wb_wy$precip_mm_W6 - wb_wy$strflow_mm_W6
wb_wy$et_mm_W7 <- wb_wy$precip_mm_W7 - wb_wy$strflow_mm_W7
wb_wy$et_mm_W8 <- wb_wy$precip_mm_W8 - wb_wy$strflow_mm_W8
wb_wy$et_mm_W9 <- wb_wy$precip_mm_W9 - wb_wy$strflow_mm_W9

wb_wy <- 
  wb_wy[c("wy","precip_mm_W1","strflow_mm_W1","et_mm_W1",
          "precip_mm_W2","strflow_mm_W2","et_mm_W2",
          "precip_mm_W3","strflow_mm_W3","et_mm_W3",
          "precip_mm_W4","strflow_mm_W4","et_mm_W4",
          "precip_mm_W5","strflow_mm_W5","et_mm_W5",
          "precip_mm_W6","strflow_mm_W6","et_mm_W6",
          "precip_mm_W7","strflow_mm_W7","et_mm_W7",
          "precip_mm_W8","strflow_mm_W8","et_mm_W8",
          "precip_mm_W9","strflow_mm_W9","et_mm_W9")]

head(wb_wy,5)




#########  Alex Y stepped in here to re-structure the dataframe

# gather the data frame to make each watershed be identified in a column
library(tidyr)

head(wb_wy)
names(wb_wy)
wb<-gather(wb_wy, "Var_ID","value", 2:28)
head(wb)

table(wb$Var_ID,wb$wy)

dim(wb)

# After gathering by the columns of et, precip, and strflow,Extract the variable ID and Watershed ID from the string
wb$vars <- substr(wb$Var_ID, 1, 5)  # extract the first three characters

wb[wb$vars=="preci","Variable"]<-"Precipitation"   # clean up the column after parsing it
wb[wb$vars=="et_mm","Variable"]<-"Evapotranspiration"
wb[wb$vars=="strfl","Variable"]<-"Streamflow"


head(wb)
n_last <- 2    # Specify number of characters to extract
wb$Watershed<-substr(wb$Var_ID, nchar(wb$Var_ID) - n_last + 1, nchar(wb$Var_ID)) # Extract last three characters

table(wb$Watershed, wb$wy)

table(wb$wy, wb$Watershed)

####################################################

## set your max and min values so the axes will continually scale
maxval <- max(c(wb$value), na.rm=TRUE)
minval <- min(c(wb$value), na.rm=TRUE)
maxval
minval

# order the variables in the way they show on the graph
table(wb$Variable)

table(wb$wy)


wb$Variable <- factor(wb$Variable, levels=c("Precipitation","Streamflow","Evapotranspiration"))

table(wb$Watershed, wb$wy)

head(wb)
# if you just want to show 4 panels

str(wb)

wb$Watershed<-as.factor(wb$Watershed)
wa3<-wb[wb$Watershed==c("W3"),]
wa6<-wb[wb$Watershed==c("W6"),]
wa7<-wb[wb$Watershed==c("W7"),]
wa8<-wb[wb$Watershed==c("W8"),]



wbj<-rbind(wa3, wa6, wa7, wa8)

W <- ggplot(data = wbj ,aes(x = wy, y= value, col= Variable)) +
  geom_line() +
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=F)+
  scale_color_manual(values=c("dark grey","blue","forest green"))+
  xlab("Water year (June 1)")+  ylab("Water (mm)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #scale_y_continuous(limits = c(floor(minval/250)*250, ceiling(maxval/250)*250), expand = c(0,0), breaks=seq(floor(minval/250)*250,ceiling(maxval/250)*250,250)) +
  #scale_x_continuous(limits = c(1955, round(max(wb_wy$wy),-1)+5), expand = c(0,0), breaks=seq(1955,round(max(wb_wy$wy),-1)+5,10))+
  facet_wrap(~Watershed)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1), legend.position = "bottom", legend.title = element_blank())

W


W <- ggplotly(W,  margin=0.05, nrows=3, shareY=TRUE, shareX=TRUE) %>%
  layout(legend = list(orientation = 'h', x = -.05, y = -.2)) 
      
      
W


# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(W), "Hydrology/fig2_precip_stream_evapotranspiration.html")



