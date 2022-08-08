

cast<-read.csv("C:\\Users\\bears\\Downloads\\castnet_HB\\Total Deposition by Pollutant - Annual .csv")

head(cast)
library(ggplot2)
library(plotly)
library(tidyr)

names(cast)

ca<-cast[ ,c(1,2,3, 7,8,10,22,23)]

head(ca)

c<-gather(ca, "Pollutant","value", 4:8)

ggplot(c, aes(x=YEAR, y=value, col=Pollutant ))+
  geom_point()+geom_line()+ylab("Concentration")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())












