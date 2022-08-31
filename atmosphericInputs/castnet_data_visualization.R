

cast<-read.csv("atmosphericInputs/castnet_data/Total Deposition by Pollutant - Annual.csv")

head(cast)
library(ggplot2)
library(plotly)
library(tidyr)

names(cast)

ca<-cast[ ,c("SITE_ID","YEAR","PRECIP", "S_DRY_SO4","S_DRY_SO2","N_DRY_HNO3", "N_DRY_NH4", "N_DRY_NO3")]
head(ca)

# 
# S_DRY_SO2
# N_DRY_HNO3
# S_DRY_SO4
# N_DRY_NH4
# N_DRY_NO3


head(ca)

c<-gather(ca, "Pollutant","value", 4:8)

head(c)
c$Pollutant <-factor(c$Pollutant , levels=c("S_DRY_SO2","N_DRY_HNO3","S_DRY_SO4", "N_DRY_NH4", "N_DRY_NO3"))
c$sipo<-paste(c$SITE_ID, c$Pollutant)




cas1<-ggplot(c, aes(x=YEAR, y=value, col=Pollutant, group=sipo ))+
  geom_point(size=5)+geom_line(size=1.8)+ylab("Units  kg N or kg S per hectare")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values=c("light blue","orange","grey","tan","dark blue"))+
  scale_x_continuous(expand = c(0, 0), limits=c(1980,2020), breaks=c(1990,2000,2010,2020))+
  xlab("")+theme(text=element_text(size=24))

pas1<-ggplotly(cas1)

pas1
# this line writes the html file to create interactive graphs for the online book
htmlwidgets::saveWidget(as_widget(po), "phosphorus/P_exportW5_W6_Driscoll.html")




########## Now for individual S wet and dry

sulf<-cast[ ,c("SITE_ID","YEAR","PRECIP", "S_DRY","S_WET")]
head(sulf)

su<-gather(sulf, "Pollutant","value", 4:5)

head(su)
su$Pollutant <-factor(su$Pollutant , levels=c("S_DRY","S_WET"))
su$sipo<-paste(su$SITE_ID, su$Pollutant)




sul1<-ggplot(su, aes(x=YEAR, y=value, col=Pollutant, group=sipo , shape=Pollutant))+
  scale_shape_manual(values=c("diamond","square"))+
  geom_point(size=3)+geom_line(size=1.8)+ylab("Deposition (Kg S / hectare)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values=c("blue","dark red"))+
  scale_x_continuous(expand = c(0, 0), limits=c(1980,2020), breaks=c(1990,2000,2010,2020))+
  xlab("")+theme(text=element_text(size=24))
sul1

## add annotation for ggplotly title
# annotations
a <- list(
  text = "Wet and Dry S Deposition",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

## make margins
m <- list(
  l = 100,
  r = 50,
  b = 100,
  t = 100,
  pad = 10
)


pul1<-ggplotly(sul1)%>%
  layout(annotations = a, margin=m)

pul1


########## Now for individual N wet and dry

Nfig<-cast[ ,c("SITE_ID","YEAR","PRECIP", "N_DRY","N_WET")]
head(Nfig)

nf<-gather(Nfig, "Pollutant","value", 4:5)

head(nf)
nf$Pollutant <-factor(nf$Pollutant , levels=c("S_DRY","S_WET"))
nf$sipo<-paste(nf$SITE_ID, nf$Pollutant)




nfl1<-ggplot(nf, aes(x=YEAR, y=value, col=Pollutant, group=sipo , shape=Pollutant))+
  scale_shape_manual(values=c("diamond","square"))+
  geom_point(size=3)+geom_line(size=1.8)+ylab("Deposition (Kg N / hectare)")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values=c("blue","dark red"))+
  scale_x_continuous(expand = c(0, 0), limits=c(1980,2020), breaks=c(1990,2000,2010,2020))+
  xlab("")+theme(text=element_text(size=24))
nfl1

## add annotation for ggplotly title
# annotations
a <- list(
  text = "Wet and Dry S Deposition",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

## make margins
m <- list(
  l = 100,
  r = 50,
  b = 100,
  t = 100,
  pad = 10
)


pul1<-ggplotly(sul1)%>%
  layout(annotations = a, margin=m)

pul1





