##
##
## Historic air temperature at Hubbard Brook
##    Alex Young 7/5/2022

library(ggplot2) # graphing
library(tidyr) # dataframe manipulation
library(lubridate) # for handling date times
library(plotly)

# Package ID: knb-lter-hbr.59.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Daily Temperature Record, 1955 - present.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/59/10/9723086870f14b48409869f6c06d6aa8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


air <-read.csv(infile1,header=F 
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

head(air)

# We have a data column, but its not formatted as a date
air$DATE<-ymd(air$date) # change how R interprets Date to be a date
air$Year<-year(air$DATE)

# add in water year
w.year <- as.numeric(format(air$DATE, "%Y"))
june.july.sept <- as.numeric(format(air$DATE, "%m")) < 6
w.year[june.july.sept] <- w.year[june.july.sept] - 1
air$wyear<-w.year # add water year as a column to precip dataset

# add in water year-station
air$wys<-paste(air$wyear, air$STA)


#visualize to become familiar with data
ggplot(air, aes(x=wyear, y=AVE))+geom_line()+
  geom_line(aes(x=wyear, y=MAX))+
  facet_wrap(~STA)
  

#subset to just HQ
hq<-air[air$STA=="HQ",]

# make sure you are only using complete years for the record
pre.obs<-as.data.frame(table(hq$STA , hq$wyear))
pre.obs
pre.obs$wys<-paste(pre.obs$Var2, pre.obs$Var1)
pre.obs[pre.obs$Freq<350, "Use"]<-"incomplete wyear" # complete is 350 or more days
pre.obs[is.na(pre.obs$Use),"Use"]<-"complete"
pre.obs

hq$Use<-pre.obs$Use[match(hq$wys, pre.obs$wys)]
hq.complete<-hq[hq$Use=="complete",]

# aggregate to annual average
hqa<-aggregate(list(MAX=hq.complete$MAX, MIN=hq.complete$MIN, AVE=hq.complete$AVE), by=list(wyear=hq.complete$wyear), FUN="mean")
names(hqa)
hq_g<-gather(hqa, "type","value", 2:4)


# st.err
st.err <- function(x) {  sd(x)/sqrt(length(x))}
SE <- aggregate(list(MAX=hq.complete$MAX, MIN=hq.complete$MIN, AVE=hq.complete$AVE), by=list(wyear=hq.complete$wyear), st.err)
hqSE<-gather(SE, "type","value", 2:4)

hqSE
hq_g

hq_g$SE<-hqSE$value

# order temp types
hq_g$type<-factor(hq_g$type, levels=c("MAX","AVE","MIN"))



a1<-ggplot(hq_g, aes(x=wyear, y=value, col=type))+geom_point()+ geom_line()+
 # geom_errorbar(aes(ymax=value+SE, ymin=value-SE))+
  ylab("Air temperature (C)")+xlab("Water year (June 1st)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(col='')
p1<-ggplotly(a1)
p1

htmlwidgets::saveWidget(as_widget(p1), "climateChange/air_temp.html")


#  Plot weekly values?
hqweek<-aggregate(list(MAX=hq.complete$MAX, MIN=hq.complete$MIN, AVE=hq.complete$AVE), by=list(wyear=hq.complete$wyear), FUN="mean")
names(hqweek)
hq_w<-gather(hqweek, "type","value", 2:4)

head(hq_w)




ggplot(hq_w, aes(x=wyear, y=value, col=type))+geom_point()+ geom_line()+
  ylab("Air temperature (C)")+xlab("Water year (June 1st)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(col='Annual temperature')


