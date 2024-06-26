# Snow trends
# Campbell 26 Nov 2021
# 'plotlified' by Alex Young 15 June 2022

rm(list = ls())
#setwd("C:/Fahey/living graphs/snowpack")

#Use the libraries
library("tidyr")
library("lubridate")
library("dplyr")
library("ggplot2")
library("trend")
library("zoo")
library("ggplotify")
library("egg")
library("cowplot")
library("grid")
library("gridExtra")
library("ggpmisc")
library(plotly)

# load data from EDI:

# Package ID: knb-lter-hbr.27.16 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Weekly Snow and Frost Measurements, 1955 - present.
# Data set creator:    - USDA Forest Service, Northern Research Station 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/27/16/e9dc11e1518fb5820e614dc512a2517d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dat <-read.csv(infile1, stringsAsFactors=F, na.strings = -99)

unlink(infile1)

#format date
dat$Date <- as.Date(dat$Date)

#take a peek at the df
str(dat)
summary(dat)

#select only current snowcourses and drop rows with NA snow_depth
current_sc <- c("STA19","STA2","STAHQ","STA17","STA9")

depthDat <- dat %>% 
  filter(Site %in% current_sc) %>%
  select(c(WINTER, Date, Site, snow_depth, swe)) %>%
  drop_na() %>%
  dplyr::distinct()

#Calculate snowpack duration and max snow depth in a nested loop: 

#set up vectors to hold the output:
winter <- vector(length=0)
site <- vector(length=0)
first_date <- vector(length=0)
last_date <- vector(length=0)
first_depth <- vector(length=0)
last_depth <- vector(length=0)
max_depth <- vector(length=0)
max_swe <- vector(length=0)

sites <- unique(depthDat$Site)

for (i in seq_along(sites)){
  
  temp <- subset(depthDat, Site == sites[i])
  winters <- unique(temp$WINTER)
  
  for(j in seq_along(winters)){
    
    temp2 <- subset(temp, WINTER == winters[j])
    
    #code depth as binary (0/1 = no/yes)
    cutoff <- 0
    temp2$snow_depth_binary <- ifelse(temp2$snow_depth <= cutoff, 0, 1)
    
    #run length encoding, change everything but the longest run to 0, then expand the edited rle back to a vector
    RLE <- rle(temp2$snow_depth_binary)
    RLE$values[RLE$lengths!=max(RLE$lengths[RLE$values==1])] <- 0
    temp2$test <- inverse.rle(RLE)
    
    #select only the dates of snowpack duration:
    temp3 <- subset(temp2, temp2$test == 1)
    
    #append info to output vectors
    winter <- append(winter, winters[j])
    site <- append(site, sites[i])
    first_date <- append(first_date,as.character(temp3$Date[1]))
    first_depth <- append(first_depth, temp3$snow_depth[1])
    last_date <- append(last_date,as.character(temp3$Date[length(temp3$Date)]))
    last_depth <- append(last_depth, temp3$snow_depth[length(temp3$Date)])
    max_depth <- append(max_depth, max(temp3$snow_depth))
    max_swe <- append(max_swe, max(temp3$swe))
  }
}

#create dataframe of output
out1 <- as.data.frame(cbind(winter, site, max_depth, max_swe, first_date, first_depth, last_date, last_depth), stringsAsFactors=F)
out1$winter <- as.numeric(out1$winter)
out1$max_depth <- as.numeric(out1$max_depth)
out1$max_swe <- as.numeric(out1$max_swe)
out1$first_date <- as.Date(out1$first_date)
out1$first_depth <- as.numeric(out1$first_depth)
out1$last_date <- as.Date(out1$last_date)
out1$last_depth <- as.numeric(out1$last_depth)

#calculate duration
out1$duration <- out1$last_date-out1$first_date + 7 #add 7 days to match how John defined duration in 2015 paper
out1$duration <- as.numeric(out1$duration)

#convert to cm
out1$max_depth <- out1$max_depth/10
out1$max_swe <- out1$max_swe/10

out <- subset(out1, site == "STA2", select = c("winter","site","max_depth","max_swe","duration"))

max_depth_sen <- sens.slope(out$max_depth, conf.level = 0.95)
max_depth_sen
out$max_depth_slp <- NA
out$max_depth_slp[1] <- median(out$max_depth)-
  (((nrow(out)-1)/2)*max_depth_sen$estimate)
out$max_depth_slp[nrow(out)] <- median(out$max_depth)+
  (((nrow(out)-1)/2)*max_depth_sen$estimate)
out$max_depth_slp <- 
  c(na.approx(out$max_depth_slp))
length(out$max_depth_slp)*max_depth_sen$estimate

max_swe_sen <- sens.slope(out$max_swe, conf.level = 0.95)
max_swe_sen
out$max_swe_slp <- NA
out$max_swe_slp[1] <- median(out$max_swe)-
  (((nrow(out)-1)/2)*max_swe_sen$estimate)
out$max_swe_slp[nrow(out)] <- median(out$max_swe)+
  (((nrow(out)-1)/2)*max_swe_sen$estimate)
out$max_swe_slp <- 
  c(na.approx(out$max_swe_slp))
length(out$max_swe_slp)*max_swe_sen$estimate

duration_sen <- sens.slope(out$duration, conf.level = 0.95)
duration_sen
out$duration_slp <- NA
out$duration_slp[1] <- median(out$duration)-
  (((nrow(out)-1)/2)*duration_sen$estimate)
out$duration_slp[nrow(out)] <- median(out$duration)+
  (((nrow(out)-1)/2)*duration_sen$estimate)
out$duration_slp <- 
  c(na.approx(out$duration_slp))
length(out$duration_slp)*duration_sen$estimate

snow_depth_yr_count <- sum(sapply(out$max_depth, function(x) sum(!is.na(x))))
snow_depth_slp_time <- max_depth_sen$estimates * snow_depth_yr_count
snow_depth_sen_slp <- round(snow_depth_slp_time, digits=0)
snow_depth_sen_p <- round(max_depth_sen$p.value, digits=3)

swe_yr_count <- sum(sapply(out$max_swe, function(x) sum(!is.na(x))))
swe_slp_time <- max_swe_sen$estimates * swe_yr_count
swe_sen_slp <- round(swe_slp_time, digits=0)
swe_sen_p <- round(max_swe_sen$p.value, digits=3)

snow_days_yr_count <- sum(sapply(out$duration, function(x) sum(!is.na(x))))
duration_slp_time <- duration_sen$estimates * snow_days_yr_count
duration_sen_slp <- round(duration_slp_time, digits=1)
duration_sen_p <- round(duration_sen$p.value, digits=3)

#create the plot
snow_depth <- ggplot() +
  geom_line(data = out,aes(x = winter, y= max_depth), colour="gold3", lwd=0.6) +
  geom_point(data = out,aes(x = winter, y= max_depth), colour="gold3", size=2) +
  geom_line(data = out,aes(x = winter, y= max_depth_slp), colour="gold3", lwd=0.6) + xlab("")+
  ylab("Max. snow depth (cm)")+
  theme_bw()+
 scale_y_continuous(limits = c(floor((min(out$max_depth)-10)/20)*20, ceiling((max(out$max_depth)+10)/20)*20), expand = c(0,0), breaks=seq(floor((min(out$max_depth)-10)/20)*20,ceiling(max((out$max_depth)+10)/20)*20,20)) +
  scale_x_continuous(limits = c(1955, round(max(out$winter),-1)+5), expand = c(0,0), breaks=seq(1955,round(max(out$winter),-1)+5,10)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjus=0.04, vjus=-9), 
        plot.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0,1,0,1), "mm"),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.title.y=element_text(size=17, colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=1, linetype="solid"),
        panel.background = element_rect(fill = "transparent",colour = NA)) +  
        theme(axis.line = element_blank(), axis.ticks = element_blank())+
  geom_text(aes(x=2000, y=150, label=paste0("Snow depth has decreased by ", snow_depth_sen_slp , " cm over ",snow_depth_yr_count," years p = ",snow_depth_sen_p)), color="gold3",size=4.5, col="gold3") 

snow_depth  

swe_depth <- ggplot() +
  geom_line(data = out,aes(x = winter, y= max_swe), colour="darkblue", lwd=0.6) +
  geom_point(data = out,aes(x = winter, y= max_swe), colour="darkblue", size=2) +
  geom_line(data = out,aes(x = winter, y= max_swe_slp), colour="darkblue", lwd=0.6) + xlab("")+
  ylab("Max. SWE (cm)")+
  theme_bw()+
  scale_y_continuous(limits = c(floor((min(out$max_swe)-2)/5)*5, ceiling((max(out$max_swe)+2)/5)*5), expand = c(0,0), breaks=seq(floor((min(out$max_swe)-2)/5)*5,ceiling((max(out$max_swe)+2)/5)*5,5)) +
  scale_x_continuous(limits = c(1955, round(max(out$winter),-1)+5), expand = c(0,0), breaks=seq(1955,round(max(out$winter),-1)+5,10)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjus=0.04, vjus=-9), 
        plot.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0,1,0,1), "mm"),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.title.y=element_text(size=17, colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=1, linetype="solid"),
        panel.background = element_rect(fill = "transparent",colour = NA)) +
  geom_text(aes(x=2000, y=38, label=paste0("Snow water has decreased by ", swe_sen_slp , " cm over ",swe_yr_count," years p = ",swe_sen_p)),size=4.5, col="darkblue") 
    
swe_depth

snow_days <- ggplot() +
  geom_line(data = out,aes(x = winter, y= duration), colour="darkgray", lwd=0.6) +
  geom_point(data = out,aes(x = winter, y= duration), colour="darkgray", size=2) +
  geom_line(data = out,aes(x = winter, y= duration_slp), colour="darkgray", lwd=0.6) + xlab("")+
  ylab("Snow cover (days)")+
  theme_bw()+
  scale_y_continuous(limits = c(floor((min(out$duration)-5)/20)*20, ceiling((max(out$duration)+5)/20)*20), expand = c(0,0), breaks=seq(floor((min(out$duration)-5)/20)*20,ceiling((max(out$duration)+5)/20)*20,20)) +
  scale_x_continuous(limits = c(1955, round(max(out$winter),-1)+5), expand = c(0,0), breaks=seq(1955,round(max(out$winter),-1)+5,10)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjus=0.04, vjus=-9), 
        plot.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0,1,0,1), "mm"),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.y=element_text(size=15, colour = "black"),
        axis.title.y=element_text(size=17, colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x=element_text(size=15, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=1, linetype="solid"),
        panel.background = element_rect(fill = "transparent",colour = NA) ) +
  geom_text(aes(x=2000, y=175, label=paste0("Snow cover has decreased by ", duration_sen_slp , " days over ",snow_days_yr_count," years p = ",duration_sen_p)),size=4.5, col="darkgray") 


  snow_days 


# snow_depth_fixed <- set_panel_size(snow_depth, width  = unit(5, "in"), height = unit(2, "in"))
# swe_depth_fixed <- set_panel_size(swe_depth, width  = unit(5, "in"), height = unit(2, "in"))
# snow_days_fixed <- set_panel_size(snow_days, width  = unit(5, "in"), height = unit(2, "in"))
# 
# p1 <- as.grob(snow_depth_fixed)
# p2 <- as.grob(swe_depth_fixed)
# p3 <- as.grob(snow_days_fixed)
# 
# top_row <- plot_grid(p1, NULL, p2, NULL, p3, ncol = 1, rel_heights = c(1, -0.3, 1, -0.3, 1), align="hv") 
# 
# svg("snow_trends.svg", height = 8)
# top_row
# dev.off()


plot1 <- ggplotly(snow_depth)
plot2 <- ggplotly(swe_depth)
plot3 <- ggplotly(snow_days)


# create single plot with 3 panels
plotfinal <- subplot(plot1, plot2,  plot3,
                     margin=0.05, nrows=3, 
                     shareY=TRUE, shareX=TRUE)
plotfinal

# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(plotfinal), "climateChange/fig_3_snow_depth_cover_swe.html")
