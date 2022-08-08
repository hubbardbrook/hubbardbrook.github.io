## R code for data analysis for sugar maple crown N*P.
## Alex Young   10/7/2021
## alexyoung.116@gmail.com
library(data.table)
library(tidyr)



samp<-read.csv("better_melnhe_sugar_maple_crown_depth.csv", header=T)
head(samp)

#samp$scaled<-log((samp$scaled+1.00001))


# second<-read.csv("Data/second_collection_MELNHE_SugarMapleCrownDepth.csv")
# second$br<-paste(second$Tree.ID, second$dfromtop)
# 
# second$moisture<-(second$wetmass2_g-second$mass2_g)/second$wetmass2_g
# head(second)
# 
# samp$moisture<-second$moisture[match(samp$br, second$br)]
#write.csv(samp[,c(1:53,55)], file="better_melnhe_sugar_maple_crown_depth.csv")


# IF you want to assess the traits per unit Area rather than Mass
#samp<-samp[,c(14:27)]/samp$SLA
samp$Treatment <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$trmt <- factor(samp$trmt,levels=c("Control","N treatment","P treatment","N+P treatment"))
samp$Tree_ID<-as.numeric(samp$Tree_ID)


tree<-read.csv("Data/Tree_info_MELNHE_sugar_maple_crown.csv")
tree<-tree[, c(1:6)]



##3 create output for each leaf characteristic
## this gets the coefficients for the fixed effects
aov.lme <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  fm2<- lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude)
  emm2<-fm2$coefficients$fixed
  em<- as.data.frame(emm2)
      return(em)}

# this is for the coefficients
output.lme<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.lme[[i-11]] <- aov.lme(y, Stand, Ntrmt, Ptrmt, Tree_ID)}
d.em<- as.data.frame(rbindlist(output.lme))


# carry on with d.em for the coefficients!
d.em$`Fixed effect`<-rep(c("intercept","Depth","Ntrmt","Ptrmt","height*N","height*P","N:P","height*N*P"), 44)

rep(names(samp)[c(12:55)], each=8)
d.em$variable<-rep(names(samp)[c(12:55)], each=8)



int<-d.em[d.em$`Fixed effect`=="intercept",]
d.em$int<-int$emm2[match(d.em$variable, int$variable)]
d.em$coef<-d.em$emm2+d.em$int

table(d.em$`Fixed effect`)
d.em$`Fixed effect`<-factor(d.em$`Fixed effect`, levels=c("intercept","Depth","Ntrmt","Ptrmt","height*N","height*P","N:P","height*N*P"))




### add in leaf characteristics


d.em$Group[d.em$variable=="area_cm2"]<-"Physical characteristics"
d.em$Group[d.em$variable=="mass_g"]<-"Physical characteristics"
d.em$Group[d.em$variable=="SLA"]<-"Physical characteristics"

#d.em$Group[d.em$variable=="C"]<-"Elements"
d.em$Group[d.em$variable=="N"]<-"Elements"
d.em$Group[d.em$variable=="Mg"]<-"Elements"
d.em$Group[d.em$variable=="Ca"]<-"Elements"
d.em$Group[d.em$variable=="Al"]<-"Elements"
d.em$Group[d.em$variable=="B"]<-"Elements"
d.em$Group[d.em$variable=="Mn"]<-"Elements"
d.em$Group[d.em$variable=="P"]<-"Elements"
d.em$Group[d.em$variable=="Zn"]<-"Elements"
d.em$Group[d.em$variable=="Fe"]<-"Elements"

#d.em$Group[d.em$variable=="N_P"]<-"Elements"
#d.em$Group[d.em$variable=="S"]<-"Elements"


table(d.em$variable)

d.em$Group[d.em$variable=="total_chl"]<-"Photosynthetic pigments"
d.em$Group[d.em$variable=="carot"]<-"Photosynthetic pigments"
#d.em$Group[d.em$variable=="Chl_R"]<-"Photosynthetic pigments"

d.em$Group[d.em$variable=="Ala"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="GABA"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Arg"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Glu"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="Val"]<-"Amino acids and soluble protein"
d.em$Group[d.em$variable=="protein"]<-"Amino acids and soluble protein"

d.em$Group[d.em$variable=="Put"]<-"Polyamines"
d.em$Group[d.em$variable=="Spd"]<-"Polyamines"
d.em$Group[d.em$variable=="Spm"]<-"Polyamines"




library(ggplot2)
d.em$variable<-factor(d.em$variable, levels=c( "SLA","area_cm2","mass_g","Zn","B","Fe","Al","Mn","Mg","Ca","P","N","Spd","Spm","Put",
                                               "protein","Lys","Ile","Val","GABA","Pro","Ala","Arg","Glu",
                                               "carot","total_chl"))

d.em$Group<-factor(d.em$Group, levels=c("Physical characteristics","Elements","Polyamines","Amino acids and soluble protein","Photosynthetic pigments"))

d.em$norm<-d.em$emm2/ d.em$int






dsca<-subset(d.em, `Fixed effect` == "Depth")
dntr<-subset(d.em, `Fixed effect` == "Ntrmt")
dptr<-subset(d.em, `Fixed effect` == "Ptrmt")


dnp<-rbind(dsca, dntr, dptr)
dnp<-dnp[!is.na(dnp$Group),]



library(ggplot2)
# couple of clean up vaariable names
ggplot(dnp, aes(x=variable, y=norm, col=`Fixed effect`))+geom_point()+facet_wrap(~Group, scales="free_y", ncol=2)+coord_flip()+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')  +scale_color_manual(values=c("black","blue","red"))+
  xlab("Leaf characteristics")+ylab("Direction and effect size from top to bottom of the crown")+theme(legend.position = "bottom")





############################################################

##3 create output for each leaf characteristic
aov.mixed <- function(y, Stand, Ntrmt, Ptrmt, Tree_ID){
  #1 aov for linear mixed effect model
  library(nlme)
  mixed1<-anova( lme(y ~ scaled*Ntrmt*Ptrmt, random=~1|Stand/Tree_ID, data=samp, na.action=na.exclude))
  return(mixed1)}



 output.mixed<-list()
for(i in c(12:55)){ 
  y = samp[,i]
  Stand= samp$Stand
  Ntrmt=samp$Ntrmt
  Ptrmt=samp$Ptrmt
  output.mixed[[i-11]] <- aov.mixed(y, Stand, Ntrmt, Ptrmt, Tree_ID)}

output.mixed

# so some formatting
d.int<- as.data.frame(rbindlist(output.mixed))
d.int
d.int$Source<-rep(c("intercept","depth","N","P","depth*N","depth*P","N*P","depth*N*P"))
dim(d.int)

d.int


rep(names(samp)[c(12:55)], each=8)
d.int$variable<-rep(names(samp)[c(12:55)], each=8)
names(d.int)

names(d.int)
d.int<-d.int[ ,c(6,5, 1, 2, 3, 4)]
names(d.int)
# for an easier time of formatting names for tables
pism<-d.int
pism

## add leaf characteristic
pism$Group[pism$variable=="area_cm2"]<-"Physical characteristics"
pism$Group[pism$variable=="mass_g"]<-"Physical characteristics"
pism$Group[pism$variable=="SLA"]<-"Physical characteristics"

pism$Group[pism$variable=="C"]<-"Elements"
pism$Group[pism$variable=="Mg"]<-"Elements"
pism$Group[pism$variable=="Ca"]<-"Elements"
pism$Group[pism$variable=="Al"]<-"Elements"
pism$Group[pism$variable=="B"]<-"Elements"
pism$Group[pism$variable=="Mn"]<-"Elements"
pism$Group[pism$variable=="N"]<-"Elements"
pism$Group[pism$variable=="Fe"]<-"Elements"
pism$Group[pism$variable=="P"]<-"Elements"
pism$Group[pism$variable=="S"]<-"Elements"
pism$Group[pism$variable=="Zn"]<-"Elements"

pism$Group[pism$variable=="total_chl"]<-"Photosynthetic pigments"
pism$Group[pism$variable=="carot"]<-"Photosynthetic pigments"
pism$Group[pism$variable=="Chl_R"]<-"Photosynthetic pigments"

pism$Group[pism$variable=="Ala"]<-"Amino acids and soluble protein"
pism$Group[pism$variable=="GABA"]<-"Amino acids and soluble protein"
pism$Group[pism$variable=="Arg"]<-"Amino acids and soluble protein"
pism$Group[pism$variable=="Glu"]<-"Amino acids and soluble protein"
pism$Group[pism$variable=="Val"]<-"Amino acids and soluble protein"


pism$Group[pism$variable=="Put"]<-"Polyamines"
pism$Group[pism$variable=="Spd"]<-"Polyamines"
pism$Group[pism$variable=="Spm"]<-"Polyamines"
head(pism)

##Carry on with fp
## order fp by the order of variables. 

order<-seq(1:44)
name<-c("mass_g",	"area_cm2","SLA", "Put",	"Spd"	,"Spm", "Glu","Arg","Ala","GABA","Val","protein","N","P","Ca","Mg","Mn","Al","Fe","B",	"Zn","total_chl",	"carot"	,
        "s_Al"	,"s_Ca"	,"s_K",	"s_Mn",	"s_P"	,"s_Mg"	,"s_Zn",	"Chl_A",	"Chl_B"	,"Chl_R", "C", "S"	,"K"	,"Sr","N_P","Ile","Lys","Asp","Leu"	,	"Pro"	,"moisture"	)





length(name)

pism$variable %in% name 

tail(pism)
vord<-as.data.frame(order, name)
vord$name<-rownames(vord)
vord$order<-as.numeric(vord$order)


names(pism)
#order the variables
fp<-spread(pism[ , c(1,2,6)], "Source","p-value")

fp$vord<-vord$order [match(fp$variable, vord$name)]
fp$Group <- pism$Group[match(fp$variable, pism$variable)]

fp<-fp[order(fp$vord),]

# order the colums, add some as well.
fp$leaf.char<-pism$leaf.char[match(fp$variable, pism$variable)]
str(fp)

fp<-fp[ ,c(11,10,1,2,7,9,3,5,8,4)]
head(fp)




# fp$height.adj<-p.adjust(fp$height,method="hochberg")
# fp$N.adj<-p.adjust(fp$N,method="hochberg")
# fp$P.adj<-p.adjust(fp$P,method="hochberg")
# fp$heightN.adj<-p.adjust(fp$`height*N`,method="hochberg")
# fp$heightP.adj<-p.adjust(fp$`height*P`,method="hochberg")
# fp$NP.adj<-p.adjust(fp$`N*P`,method="hochberg")
# fp$height.adj<-p.adjust(fp$`height*N*P`,method="hochberg")

write.csv(fp, file="thesis_p_values_4_9_2022.csv")




