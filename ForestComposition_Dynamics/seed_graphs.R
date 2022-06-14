# Alex Young
# June 10, 2022
# Hubbard Brook Online Book
# Seed Production Figure - Ch 2 Fig 7
# Transforming from static to interactive figures

library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggplotify)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/298/2/cf084305a5437e0fd56aa1fa7cf930a9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=T,quot='"',na.strings="-9999" )
unlink(infile1)

# Create tibble grouped by year with summarized annual mean of ACSA seed production 

names(dt1)
seeds<-aggregate(list(ACSAannmean=dt1$ACSA.seed, ACSAannm2mean=dt1$ACSA.seed.m2,
                    FAGRannmean=dt1$FAGR.seed, FAGRannm2mean=dt1$FAGR.seed.m2,
                    FRAMannmean=dt1$FRAM.seed, FRAMannm2mean=dt1$FRAM.seed.m2), by=list(YEAR=dt1$YEAR), FUN="mean", na.rm=T)



st.err <- function(x) {sd(x, na.rm=T)/sqrt(length(x))}
sese<-aggregate(list(ACSAannmean=dt1$ACSA.seed, ACSAannm2mean=dt1$ACSA.seed.m2,
                      FAGRannmean=dt1$FAGR.seed, FAGRannm2mean=dt1$FAGR.seed.m2,
                      FRAMannmean=dt1$FRAM.seed, FRAMannm2mean=dt1$FRAM.seed.m2), by=list(YEAR=dt1$YEAR), st.err)

seeds$FAGRsesem2<-sese$FAGRannm2mean
seeds$ACSAsesem2<-sese$ACSAannm2mean



# Create plot 1 - annual mean of ACSA seeds
ggplot(seeds, aes(x = YEAR, y = ACSAannm2mean)) + 
  geom_bar(stat = "identity")+
  geom_errorbar(seeds, mapping=aes(x=YEAR, ymin=ACSAannm2mean - ACSAsesem2, ymax=ACSAannm2mean + ACSAsesem2))+
  geom_hline(yintercept = mean(dt1$ACSA.seed.m2, na.rm=TRUE), size=1)+
  labs(x = "Year", y = expression(paste("ACSA (seeds ", " ", m^2, ")")))+
  theme_classic()


#test finding the long term mean of seed production
FAGRlongtermmeanm2 <- (mean(dt1$FAGR.seed.m2, na.rm = TRUE))

FAGRlongtermsdm2 <- (sd(dt1$FAGR.seed.m2, na.rm = TRUE))

FAGRlongtermmean <- (mean(dt1$FAGR.seed, na.rm = TRUE))

ACSAlongtermmeanm2 <- (mean(dt1$ACSA.seed.m2, na.rm = TRUE))

ACSAlongtermmean <- (mean(dt1$ACSA.seed, na.rm = TRUE))
ACSAlongtermsd <- (sd(dt1$ACSA.seed, na.rm = TRUE))

ACSAlongtermsd <- (sd(dt1$ACSA.seed, na.rm = TRUE))





#############################################################
#############################################################



#Save plot 1 - annual mean of FAGR seeds
plot1 <- ggplot(seeds, aes(x = YEAR, y = FAGRannm2mean)) + 
  geom_bar(stat = "identity",  fill="gray", col="black") +
  geom_hline(yintercept = mean(dt1$FAGR.seed.m2, na.rm=TRUE))+
  geom_errorbar(seeds, mapping=aes(x=YEAR, ymin=FAGRannm2mean - FAGRsesem2, ymax=FAGRannm2mean + FAGRsesem2, width=0.5))+
    annotate("text", x = 2002, y = 62, label = "*", size=10) +
  annotate("text", x = 2006, y = 68, label = "*", size=10) +
  annotate("text", x = 2011, y = 56, label = "*", size=10) +
  annotate("text", x = 2013, y = 56, label = "*", size=10) +
  annotate("text", x = 2017, y = 74, label = "*", size=10) +
  labs(x='Year', y="FAGR (seeds m2)")+
  theme_classic()+ylim(0,80)
plot1

#Save plot 2 - annual mean of FAGR seeds
plot2 <- ggplot(seeds, aes(x = YEAR, y = ACSAannm2mean)) + 
  geom_bar(stat = "identity", fill="black")+
  geom_hline(yintercept = mean(dt1$ACSA.seed.m2, na.rm=TRUE), size=1)+
  geom_errorbar(seeds, mapping=aes(x=YEAR, ymin=ACSAannm2mean - ACSAsesem2, ymax=ACSAannm2mean + ACSAsesem2, width=0.5))+
  xlab("Year")+ylab("ACSA (seeds m2)")+
  annotate("text", x = 1994, y = 205, label = "*", size=10) +
  annotate("text", x = 1996, y = 245, label = "*", size=10) +
  annotate("text", x = 2002, y = 200, label = "*", size=10) +
  annotate("text", x = 2006, y = 295, label = "*", size=10) +
  annotate("text", x = 2017, y = 185, label = "*", size=10) +
  annotate("text", x = 2019, y = 293, label = "*", size=10) +
  theme_classic()+ylim(0,325)
plot2

#Save plot 2 - annual mean of FRAM seeds
plot3 <- ggplot(seeds, aes(x = YEAR, y = FRAMannm2mean)) + 
  geom_bar(stat = "identity")+
  geom_hline(yintercept = mean(dt1$FRAM.seed.m2, na.rm=TRUE), size=1)+
  xlab("Year")+ylab("FRAM (seeds m2)")+
  theme_classic()
plot3

##############################################
##############################################
### Part B of figure ### Standard deviation from annual seed production 


### generate the difference between longterm mean from the annual mean.
# 
seeds$ACSA_diff<-((seeds$ACSAannm2mean - mean(seeds$ACSAannm2mean, na.rm=T)))
seeds$FAGR_diff<-((seeds$FAGRannm2mean - mean(seeds$FAGRannm2mean, na.rm=T)))
seeds$FRAM_diff<-((seeds$FRAMannm2mean - mean(seeds$FRAMannm2mean, na.rm=T)))




library(tidyr)

head(see)
see<-gather( seeds, "Type","Value",2:10)




# Save plot 4 
plot4 <- ggplot(seeds, aes(x=YEAR, y=FAGR_diff)) +
  geom_bar(stat="identity",  fill="gray", col="black") +
  geom_hline(yintercept = sd(seeds$FAGR_diff,  na.rm=TRUE), linetype="dashed")+
  labs(x = "Year", y = ("FAGR ASD"))+
  theme_classic()
plot4


# Save plot 5 
plot5 <- ggplot(seeds, aes(x=YEAR, y=ACSA_diff)) +
  geom_bar(stat="identity",fill="black") +
  geom_hline(yintercept = sd(seeds$ACSA_diff, na.rm=T), linetype="dashed")+
  labs(x = "Year", y = ("ACSA ASD"))+
  theme_classic()
plot5

# Save plot 6
plot6 <- ggplot(seeds, aes(x=YEAR, y=FRAM_diff)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = sd(seeds$FRAM_diff, na.rm=TRUE), linetype="dashed")+
  labs(x = "Year", y = ("FRAM deviation"))+
  theme_classic()
plot6


## form the plotly graphs
p1<-ggplotly(plot1)
p2<-ggplotly(plot2)
p4<-ggplotly(plot4)
p5<-ggplotly(plot5) 

# %>%
#   layout(title = list(text = paste0('Fig. 1. Seed production for sugar maple (ACSA) and American beech
# (FAGR) at Hubbard Brook Experimental Forest, New Hampshire',
#                                     '<br>',
#                                     '<sup>',
#                                     'Hover over figure to view values, and to access zoom, pan, download, and other controls."',
#                                     '</sup>')))

p5



plotfinal<-subplot(p2, p1, p5, p4, nrows=4,  
         shareX = FALSE, titleY=TRUE,margin=0.01)

plotfinal
# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(plotfinal), "Chapter2_fig7_seed_production_Cleavitt/fig7_ACSA_FAGR_seeds.html")



################################################################################
################################################################################
################################################################################
## Attempting to add superscript in y axis title

#### Error message #########
# Error in unique.default(x) : 
#   unimplemented type 'expression' in 'HashTableSetup'

# t <- ggplotly(t)
# 
# fig <- plotly_build(t)
# str(fig)
# 
# fig$layout$xaxis$title$text <- 'Updated title'
# 
# figtest <- fig  %>% 
# layout(xaxis = list(text ="YEAR"), yaxis = list(text ="ACSAannm2mean"))
#        
#        figtest
#        
# xaxis = list(title = ""),
# yaxis = list(title = "Games Played"
# fig


###########################################
