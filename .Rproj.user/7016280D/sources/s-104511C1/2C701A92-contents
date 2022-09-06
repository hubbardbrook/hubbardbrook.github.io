
library(daymetr)
install.packages("daymetr")
df <- download_daymet(site = "Hubbard Brook",
                      lat = 43.940330,
                      lon = -71.718497,
                      start = 1990,
                      end = 2020,
                      internal = TRUE,
                      simplify = TRUE) # return tidy data !!

table(df$altitude)
head(df)



###########   Air temp record
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


######################

# carry on with growing degree days
library(pollen)
library(ggplot2)
library(tidyr)


data("gdd_data", package = "pollen")
head(gdd_data)

df_plot1 <- pivot_longer(gdd_data, tmax:tmin)
p1 <- ggplot(df_plot1) +
  geom_line(aes(day, value, color = name))
p1



gdd_data$type_b <- gdd(tmax = gdd_data$tmax, tmin = gdd_data$tmin, 
                       tbase = 5, type = "B")
gdd_data$type_c <- gdd(tmax = gdd_data$tmax, tmin = gdd_data$tmin, 
                       tbase = 5, tbase_max = 20, type = "C")
gdd_data$type_d <- gdd(tmax = gdd_data$tmax, tmin = gdd_data$tmin, 
                       tbase = 5, tbase_max = 20, type = "D")
head(gdd_data)

df_plot2 <- pivot_longer(gdd_data, type_b:type_d)
p2 <- ggplot(df_plot2) +
  geom_line(aes(day, value, color = name))
p2
