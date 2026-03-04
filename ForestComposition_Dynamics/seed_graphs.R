# Seed graph figure for forest dynamics chapter of the Hubbard Brook online book
# Mary Martin (original by A. Young)

library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggplotify)
library(EDIutils)

# Repository scope 
scope <- "knb-lter-hbr"

# specify the package identifier
identifier <- 298 

# Step 1: Get the newest revision
newest_revision <- list_data_package_revisions(scope = scope, 
                                               identifier = identifier, 
                                               filter = "newest")

# Create the full package Id
packageId=paste0(scope,".",identifier,".",newest_revision)

# Step 2: Get the entity names (data table identifiers)
entity_names <- read_data_entity_names(packageId)

# Step 3: Construct download URLs for each data table
base_url <- "https://portal.edirepository.org/nis/dataviewer"

# For each entity, create the download URL
data_urls <- sapply(entity_names, function(entity) {
  paste0("https://pasta.lternet.edu/package/data/eml/",
         scope, "/", identifier, "/", newest_revision, "/", entity)
})

print(data_urls)

# read in the data table
dt=read.csv(data_urls[1])

# filter out all the records where site=TF, those should not be included
dt1 <- dt[dt$SITE != "TF", ]

# Create tibble grouped by year with summarized annual mean of ACSA seed production 

names(dt1)

#calculate the mean seed number for each species within each year as count and number per m^2
seeds<-aggregate(list(ACSAannmean=dt1$ACSA.seed, ACSAannm2mean=dt1$ACSA.m2,
                    FAGRannmean=dt1$FAGR.seed, FAGRannm2mean=dt1$FAGR.m2,
                    FRAMannmean=dt1$FRAM.seed, FRAMannm2mean=dt1$FRAM.m2), by=list(YEAR=dt1$YEAR), FUN="mean", na.rm=T)

# define the standard error
st.err <- function(x) {sd(x, na.rm=T)/sqrt(length(x))}

# calculate the standard error for each species within each year as count and number^m2
sese<-aggregate(list(ACSAannmean=dt1$ACSA.seed, ACSAannm2mean=dt1$ACSA.m2,
                      FAGRannmean=dt1$FAGR.seed, FAGRannm2mean=dt1$FAGR.m2,
                      FRAMannmean=dt1$FRAM.seed, FRAMannm2mean=dt1$FRAM.m2), by=list(YEAR=dt1$YEAR), st.err)

# put the std error values in the dataftame
seeds$FAGRsesem2<-sese$FAGRannm2mean
seeds$ACSAsesem2<-sese$ACSAannm2mean

# calculate the number of years in the dataset. used to plot deviation from dataset mean for each year in plots C/D
numyears=dim(seeds)[1]

# generate the difference between longterm mean from the annual mean.
# divide by the number of years in the datset this value is used to flag the mast years
seeds$ACSA_diff<-((seeds$ACSAannm2mean - mean(seeds$ACSAannm2mean, na.rm=T)))/numyears
seeds$FAGR_diff<-((seeds$FAGRannm2mean - mean(seeds$FAGRannm2mean, na.rm=T)))/numyears

# find mast years as diff between std error of the diffs and each year's diff 
seeds$mastACSA=seeds$ACSA_diff >=  sd(seeds$ACSA_diff, na.rm=T) 
seeds$mastFAGR=seeds$FAGR_diff >= sd(seeds$FAGR_diff, na.rm=T)

# write out the mast years as a vector to be used in plots 1 and 2 to place the mast stars above bars
ACSAmastYears=as.numeric(seeds$YEAR[seeds$mastACSA])
FAGRmastYears=as.numeric(seeds$YEAR[seeds$mastFAGR])

#############################################################
#############################################################
# set colors for the two species
ACSAcolor = "#1B5E20"
FAGRcolor = "#7FBF7B"

# plot ACSA mean values for count/m^2 
plot1 <- ggplot(seeds, aes(x = YEAR, y = ACSAannm2mean)) + 
  geom_bar(stat = "identity", fill = ACSAcolor, col = "black") +
  geom_hline(yintercept = mean(dt1$ACSA.m2, na.rm = TRUE), size = .5) +
  geom_errorbar(
    aes(
      ymin = ACSAannm2mean - ACSAsesem2,
      ymax = ACSAannm2mean + ACSAsesem2
    ),
    width = 0.5
  ) +
  geom_text(
	# use array of mast years for star annotation
    data = subset(seeds, YEAR %in% ACSAmastYears),
    aes(
      y = ACSAannm2mean + ACSAsesem2 + 5,
      label = "*"
    ),
    size = 10
  ) +
  annotate("text", x = 1993, y = 350, label = "A.", size = 6) +
scale_x_continuous(
    breaks = seq(
      from = floor(min(seeds$YEAR) / 5) * 5,
      to   = ceiling(max(seeds$YEAR) / 5) * 5,
      by   = 5
    )
  ) +
  xlab("Year") +
  ylab("ACSA (seeds m2)") +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 13)
  ) +
  ylim(0, 400)

# plot ACSA mean values for count/m^2 
plot2 <- ggplot(seeds, aes(x = YEAR, y = FAGRannm2mean)) + 
  geom_bar(stat = "identity", fill = FAGRcolor, col = "black") +
  geom_hline(yintercept = mean(dt1$FAGR.m2, na.rm = TRUE), size = .5) +
  geom_errorbar(
    aes(
      ymin = FAGRannm2mean - FAGRsesem2,
      ymax = FAGRannm2mean + FAGRsesem2
    ),
    width = 0.5
  ) +
  geom_text(
	# use array of mast years for star annotation
    data = subset(seeds, YEAR %in% FAGRmastYears),
    aes(
      y = FAGRannm2mean + FAGRsesem2 + 5,
      label = "*"
    ),
    size = 10
  ) +
  annotate("text", x = 1993, y = 80, label = "B.", size = 6) +
  xlab("Year") +
  ylab("FAGR (seeds m2)") +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 13)
  ) +
  ylim(0, 100)


#  plot ACSA differences between lonterm mean and each year as bars   
plot4 <- ggplot(seeds, aes(x=YEAR, y=ACSA_diff)) +
  geom_bar(stat="identity",fill=ACSAcolor, col="black") +
  geom_hline(yintercept = sd(seeds$ACSA_diff, na.rm=T), linetype="dashed")+
  labs(x = "Year", y = ("ACSA ASD"))+
  annotate("text", x = 1993, y = 6.2, label = "C.", size = 6) +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 13)
  ) 
plot4

#  plot FAGR differences between lonterm mean and each year as bars   
plot5 <- ggplot(seeds, aes(x=YEAR, y=FAGR_diff)) +
  geom_bar(stat="identity",  fill=FAGRcolor, col="black") +
  geom_hline(yintercept = sd(seeds$FAGR_diff,  na.rm=TRUE), linetype="dashed")+
  labs(x = "Year", y = ("FAGR ASD"))+
  scale_x_continuous(
    breaks = seq(
      from = floor(min(seeds$YEAR) / 5) * 5,
      to   = ceiling(max(seeds$YEAR) / 5) * 5,
      by   = 5
    )
  ) +
  annotate("text", x = 1993, y = 1.0, label = "D.", size = 6) +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 13)
  ) 
plot5 <- plot5 %>%
  layout(
    modebar = list(
      bgcolor = "grey",
      color = "black",
      activecolor = "#1B5E20"
    )
  )

plot5

## form the plotly graphs
p1<-ggplotly(plot1)
p2<-ggplotly(plot2)
p4<-ggplotly(plot4)
p5<-ggplotly(plot5) 

p5 <- p5 %>%
  layout(
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"

    )
  )
# combine the plots into a grid
plotfinal<-subplot(p1, p2, p4, p5, nrows=4,  
         shareX = TRUE, titleY=TRUE,margin=0.01)

# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(plotfinal), "fig7_ACSA_FAGR_seeds.html")


