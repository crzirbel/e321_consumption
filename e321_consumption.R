#e321 consumption 2018
#by Chad Zirbel
#12/14/2018

##To Do##
#power analysis

##load packages
library(tidyverse)
library(reshape2)
#library(dplyr)
#library(ggplot2)
library(lme4)
library(car)

##load data
consump1<-read.csv("e321_consump_1_2018.csv")
consump2<-read.csv("e321_consump_2_2018.csv")
consump3<-read.csv("e321_consump_3_2018.csv")
consump4<-read.csv("e321_consump_4_2018.csv")
sp_biomass<-read.csv("sp_biomass_data_2018.csv")
site_data<-read.csv("e321_site_data.csv")
light<-read.csv("e321_light_data_2018.csv")
cam.data<-read.csv("camera_trap_data_clean_2018.csv")
root<-read.csv("e321_root_igc_2019.csv")
sp_cover<-read.csv("e321_sp_cover_2018_clean.csv")

## Format site data -------------------------------------------------------
#add time since fire variable
site_data$fire.time<-2018-site_data$last.burn
#add whether or not plot was burned this year
site_data$this.year.burn<-ifelse(site_data$last.burn==2018,"yes","no")
#add light data
site_data<-merge(site_data,light[1:2],by="plot")
  
##Consumption data formatting##
#merge consumption data
consump.lst<-list(consump1,consump2,consump3,consump4)
consump<-Reduce(function(x, y) merge(x, y, all=TRUE), consump.lst)

#drop unneeded columns
keep<-c("sample_period","plot_pair","subplot","species","mass")
consump<-consump[,keep]

##Create woody, grass, litter, and forb:grass variables##
biomass.vars<-sp_biomass%>%
  group_by(plot, type)%>%
  summarise(mass=sum(mass))%>%
  spread(type, value=mass, fill=0)

#rename variables
names(biomass.vars)<- c("plot", "forb.biomass", "grass.biomass", "litter.biomass", "woody.biomass")
#forb:grass
biomass.vars$forb.grass.ratio<- biomass.vars$forb.biomass/biomass.vars$grass.biomass

biomass.vars[is.na(biomass.vars)]<-0

#Do the same for the cover data
cover.vars<- sp_cover %>%
  group_by(plot, type) %>%
  summarise(cover=sum(cover)) %>%
  spread(type, value=cover, fill=0)

#rename plot variable levels
cover.vars$plot<- gsub("plot_", "", cover.vars$plot)

#rename variables
names(cover.vars)<- c("plot", "bare.cover","forb.cover", "grass.cover", "litter.cover", "woody.cover")
#forb:grass
cover.vars$forb.grass.ratio.cover<- cover.vars$forb.cover/cover.vars$grass.cover

#merge biomass and cover data frames
biomass.cover.vars<-merge(biomass.vars, cover.vars, by="plot")

#merge with site data
#site_data<- merge(site_data, biomass.vars, by="plot")

#create plot pair average values data frame
biomass.cover.vars.avg<-merge(biomass.cover.vars, site_data[,1:2], by="plot")

biomass.cover.vars.avg<- biomass.cover.vars.avg%>%
  group_by(plot_pair) %>% 
  summarise_at(2:12,mean,na.rm=T)

## All species productivity ---------------------------------------------------------------
#only keep this year's growth
graze.all<-consump[consump$species%in% c("Green matter (alive)", "1st Year Woody"),]

#aggregate within plot biomass measurements
graze.all<-aggregate(mass~sample_period+plot_pair+subplot,data=graze.all,sum)

#Make all of the open plots for graze.all 4 = 0 (we don't need them to calculate graze.all)
graze.all[(graze.all$sample_period%in% "C4" & graze.all$subplot%in% "open"),"mass"]<-0

graze.all<-graze.all %>%
           dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0) %>%
           mutate(consump= fence-open)

#aggregate by plot
graze.all<-aggregate(consump~plot_pair,data=graze.all,sum)

##Fenced plot data clean##
sp_biomass<-sp_biomass[,c("plot","species","mass","type")]

#merge site data and biomass from ungrazed plots
fence_data<-merge(sp_biomass,site_data,by= "plot", all=T)

#drop plots outside the bison fence
fence_data<-fence_data[fence_data$bison_fence%in%"yes",]

#only keep ungrazed plots
fence_data<-fence_data[fence_data$grazed%in%"no",]

#drop woody species/debris
#fence_data<-fence_data[fence_data$type!="wood",]
fence_data<-fence_data[fence_data$species!="Woody debris",]

#drop litter
fence_data<-fence_data[fence_data$species!="Miscellaneous litter",]

#keep only woody species
#fence_data<-fence_data[fence_data$type%in% "wood",]

#sum mass across species within the plot
fence_ag<-aggregate(mass~plot_pair,data=fence_data,sum)

#divide by sample area to standardized biomass measurements (0.4 m^2)
fence_ag$mass_stnd<-fence_ag$mass/0.4
#drop unstandardized mass
fence_ag$mass<-NULL

##Combining datasets##
biomass.data<-merge(graze.all,fence_ag,by="plot_pair",all=T)

#make data long
biomass.data.melt<-melt(biomass.data,id.vars = "plot_pair", value.name = "mass")

#drop all of the grazed plot types from site.data to not cause duplication when merging
site_data_cut<-site_data[site_data$grazed%in%"no",]

#merge with site.data
full.data<-merge(biomass.data.melt,site_data_cut,by="plot_pair",all.x=T)

#add biomass and cover summary variables
full.data<-merge(full.data, biomass.cover.vars.avg, by="plot_pair")

#drop forested plots for t-test
biomass.data.cut<-biomass.data[!(biomass.data$plot_pair%in% "13//14" | biomass.data$plot_pair%in% "15//16"),]

#drop forested plots from full.data
full.data.cut<-full.data[full.data$cover.type!="forest",]

#calculate % change in productivity
mean((biomass.data.cut$consump-biomass.data.cut$mass_stnd)/biomass.data.cut$mass_stnd)

mean(biomass.data.cut$mass_stnd)/mean(biomass.data.cut$consump)
mean(biomass.data.cut$consump)-mean(biomass.data.cut$mass_stnd)

(mean(biomass.data.cut$consump)-mean(biomass.data.cut$mass_stnd))/mean(biomass.data.cut$mass_stnd)

##build models##
t.test(biomass.data.cut$consump, biomass.data.cut$mass_stnd, paired=TRUE, conf.level=0.95)

mod1<-lmer(mass~variable+(1|plot_pair),data=full.data.cut)
summary(mod1)
Anova(mod1, type=3)

data.super.cut<-full.data.cut[!(full.data.cut$plot_pair %in% c("19//20","21//22")),]

mod1.cut<-lmer(mass~variable+(1|plot_pair),data=data.super.cut)
summary(mod1.cut)
Anova(mod1.cut, type=3)

mod1.1<-lmer(mass~variable*this.year.burn+(1|plot_pair),data=full.data.cut)
summary(mod1.1)
Anova(mod1.1, type=3)

mod1.1.1<-lmer(mass~variable*fire.frequency+(1|plot_pair),data=full.data.cut)
summary(mod1.1.1)
Anova(mod1.1.1, type=3)

mod1.2<-lmer(mass~variable*woody.cover+(1|plot_pair), data=full.data.cut)
summary(mod1.2)
Anova(mod1.2, type=3)

##plots##
plot(biomass.data.cut$consump-biomass.data.cut$mass_stnd,
     pch = 16,ylim=c(-250,700), cex=1.2,
     ylab="Productivity (g/m^2)")
abline(0,0, col="grey", lwd=2)
points(biomass.data.cut$consump, col="red", cex=1.2)
points(biomass.data.cut$mass_stnd, col="blue", cex=1.2)
legend(1, 600, legend=c("Difference", "Grazed","Fenced"),
       col=c("black","red", "blue"), pch=c(16,1,1), cex=0.8)

##
var.label<-c("consump"="Grazed", "mass_stnd"="Ungrazed")
tiff("graze_prod.tiff",res=300,height=4,width=6, units= "in")
ggplot(full.data.cut, aes(x=variable, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitter(width = 0.1))+
  labs(x="", y=bquote('Productivity (g/ '*m^2~'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  #geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c("none"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

ggplot(full.data.cut, aes(x=variable, y=mass))+
  geom_point(size=2)+
  geom_line(aes(group=plot_pair),size=1.24,color="grey")+
  labs(x="", y=bquote('Productivity (g/ '*m^2~'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c("none"),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

data.sum<-full.data.cut %>% 
  group_by(variable) %>% 
  summarise(mean=mean(mass),sd=sd(mass),
            se=(sd(mass)/sqrt(length(mass))),
            ci=(sd(mass)/sqrt(length(mass))) * qt((0.95/2 +0.5), (length(mass)-1)))

ggplot(data.sum, aes(variable, mean, color=variable))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('Productivity (g/ '*m^2*'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#cover type
ggplot(full.data.cut, aes(x=cover.type, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Plant cover type", y=bquote('Productivity (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(full.data.cut, aes(woody.cover, y=mass, color=variable))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
labs(x="Woody cover (%)", y=bquote('Productivity (g/ '*m^2~'/yr)'))+
  scale_color_manual(name = "",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#fire frequency
tiff("graze_prod_fire.tiff",res=300,height=6,width=8.5, units= "in")
ggplot(full.data.cut, aes(x=as.factor(fire.frequency), y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Fire frequency", y=bquote('Productivity (g/ '*m^2~'/year)'))+
  scale_fill_manual(name = "",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(0.9,0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

#### non-woody species productivity -------------------------------------------------------
#only keep this year's growth
graze.herb<-consump[consump$species%in% "Green matter (alive)",]

#aggregate within plot biomass measurements
graze.herb<-aggregate(mass~sample_period+plot_pair+subplot,data=graze.herb,sum)

#Make all of the open plots for graze.alltion 4 = 0 (we don't need them to calculate graze.alltion)
graze.herb[(graze.herb$sample_period%in% "C4" & graze.herb$subplot%in% "open"),"mass"]<-0

graze.herb<-graze.herb %>%
  dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0) %>%
  mutate(consump= fence-open)

#aggregate by plot
graze.herb<-aggregate(consump~plot_pair,data=graze.herb,sum)

##fence_data (herb only)##
#drop woody species/debris
fence_data_herb<-fence_data[fence_data$type!="wood",]
fence_data_herb<-fence_data_herb[fence_data_herb$species!="Woody debris",]

#drop litter
fence_data_herb<-fence_data_herb[fence_data_herb$species!="Miscellaneous litter",]

#sum mass across species within the plot
fence_ag_herb<-aggregate(mass~plot_pair,data=fence_data_herb,sum)

#divide by sample area to standardized biomass measurements (0.4 m^2)
fence_ag_herb$mass_stnd<-fence_ag_herb$mass/0.4
#drop unstandardized mass
fence_ag_herb$mass<-NULL

##Combining datasets##
biomass.data.herb<-merge(graze.herb,fence_ag_herb,by="plot_pair",all=T)

#make data long
biomass.data.herb.melt<-melt(biomass.data.herb,id.vars = "plot_pair", value.name = "mass")

#merge with site.data
full.data.herb<-merge(biomass.data.herb.melt,site_data_cut,by="plot_pair",all.x=T)

#drop the forest plots
full.data.herb.cut<-full.data.herb[full.data.herb$cover.type!="forest",]

##fit model##
mod2<-lmer(mass~variable+(1|plot_pair),data=full.data.herb.cut)
summary(mod2)
Anova(mod2, type=3)

##
ggplot(full.data.herb.cut, aes(x=variable, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Grazed?", y=bquote('Productivity (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#cover type
ggplot(full.data.herb.cut, aes(x=cover.type, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Plant cover type", y=bquote('Productivity (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#### woody species productivity --------------------------------------------------------
#only keep this year's growth
graze.wood<-consump[consump$species%in% "1st Year Woody",]

#aggregate within plot biomass measurements
graze.wood<-aggregate(mass~sample_period+plot_pair+subplot,data=graze.wood,sum)

#Make all of the open plots for graze.alltion 4 = 0 (we don't need them to calculate graze.alltion)
graze.wood[(graze.wood$sample_period%in% "C4" & graze.wood$subplot%in% "open"),"mass"]<-0

graze.wood<-graze.wood %>%
  dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0) %>%
  mutate(consump= fence-open)

#aggregate by plot
graze.wood<-aggregate(consump~plot_pair,data=graze.wood,sum)

##fence_data (wood only)##
#drop litter/debris and keep woody species
fence_data_wood<-fence_data[fence_data$type%in% "wood",]
fence_data_wood<-fence_data_wood[fence_data_wood$species!="Woody debris",]

#drop litter
fence_data_wood<-fence_data_wood[fence_data_wood$species!="Miscellaneous litter",]

#sum mass across species within the plot
fence_ag_wood<-aggregate(mass~plot_pair,data=fence_data_wood,sum)

#divide by sample area to standardized biomass measurements (0.4 m^2)
fence_ag_wood$mass_stnd<-fence_ag_wood$mass/0.4
#drop unstandardized mass
fence_ag_wood$mass<-NULL

##Combining datasets##
biomass.data.wood<-merge(graze.wood,fence_ag_wood,by="plot_pair",all=T)

#add zero mass for plots without woody species
biomass.data.wood$mass_stnd[is.na(biomass.data.wood$mass_stnd)]<-0

#make data long
biomass.data.wood.melt<-melt(biomass.data.wood,id.vars = "plot_pair", value.name = "mass")

#merge with site.data
full.data.wood<-merge(biomass.data.wood.melt,site_data_cut,by="plot_pair",all.x=T)

#drop forest plots
full.data.wood.cut<-full.data.wood[!(full.data.wood$cover.type%in%"forest"),]

##fit model##
mod3<-lmer(mass~variable+(1|plot_pair),data=full.data.wood.cut)
summary(mod3)
Anova(mod3,type=3)

##
ggplot(full.data.wood.cut, aes(x=variable, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Grazed?", y=bquote('Productivity (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#cover type
ggplot(full.data.wood, aes(x=cover.type, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2,position=position_jitterdodge(jitter.width=.25))+
  labs(x="Plant cover type", y=bquote('Productivity (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#### Consumption ---------------------------------------------------------------
#only keep this year's growth
consumption<-consump[consump$species%in% c("Green matter (alive)", "1st Year Woody"),]
#consumption<-consump[consump$species%in% "Green matter (alive)",]

#aggregate within plot biomass measurements
consumption<-aggregate(mass~sample_period+plot_pair+subplot,data=consumption,sum)

#subtract open from fence to measure consumption at each time point
consumption<-consumption %>%
  dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0) %>%
  mutate(consump= fence-open)

#aggregate by plot
consumption<-aggregate(consump~plot_pair,data=consumption,sum)

#merge data
data.consump<-merge(consumption,site_data_cut, by="plot_pair", all.x=T)

#drop forest plots
data.consump.cut<-data.consump[data.consump$cover.type!="forest",]

#clean camera data
#add sample period factor
cam.data$sample_period<-ifelse(cam.data$sample.interval=="2018-06-26", "C1",
                               ifelse(cam.data$sample.interval=="2018-07-03", "C2",
                                      ifelse(cam.data$sample.interval=="2018-07-24", "C3", "C4")))
#create plot_pair variable
cam.data$plot_pair<- gsub("plot_",'',cam.data$Directory)
cam.data$plot_pair<- gsub("-",'//',cam.data$plot_pair)
#cam.data<- cam.data[,c("plot_pair", "sample_period", "grazing.den")] #clean data frame

#add camera data
cam.data.ag<-cam.data%>%
  group_by(plot_pair)%>%
  summarise(bison_hdown=sum(bison_hdown))

cam.data.ag$graz.den<-cam.data.ag$bison_hdown/70 #70 days

#merge
data.consump.cut<-merge(data.consump.cut, cam.data.ag, by="plot_pair", all.x=T)

#model
mod.consmp.gden2<-lm(consump~grazing.den, data=data.consump.cut)
summary(mod.consmp.gden2)

#add biomass variables
data.consump.cut<-merge(data.consump.cut, biomass.cover.vars.avg, by="plot_pair")

#plot
ggplot(data.consump.cut, aes(grazing.den, consump))+
  geom_point(size=1.75)+
  geom_smooth(method="lm",se=F, size=1.5, color="black")+
  labs(x="Bison grazing density", y=bquote('Consumption (g/ '*m^2~')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
ggplot(data.consump.cut, aes(x=plot_pair, y=consump))+
  geom_point(size=2, aes(color=cover.type))+
  labs(x="Plot", y=bquote('Consumption (g/ '*m^2~')'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=16),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##fit model##
mod4<-aov(consump~cover.type,data=data.consump)
summary(mod4)
Anova(mod4,Type=3)
TukeyHSD(mod4, 'cover.type', conf.level=0.95)

#cover type
ggplot(data.consump, aes(x=cover.type, y=consump))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2)+
  labs(x="Plant cover type", y=bquote('Consumption (g/ '*m^2*')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(data.consump.cut, aes(woody.biomass, y=consump))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
  labs(x=bquote('Woody biomass (g/ '*m^2*')'), y=bquote('Consumption (g/ '*m^2*')'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))


mod5<-aov(consump~this.year.burn,data=data.consump)
summary(mod5)
Anova(mod5,Type=3)
#TukeyHSD(mod5, 'cover.type', conf.level=0.95)

#Fire
ggplot(data.consump, aes(x=as.factor(this.year.burn), y=consump))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2)+
  labs(x="Spring 2018 burn", y=bquote('Consumption (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

summary(aov(consump~as.factor(fire.time),data=data.consump))

ggplot(data.consump, aes(x=fire.frequency, y=consump))+
  geom_point(size=2)+
  labs(x="Fire frequency", y=bquote('Consumption (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

#data.consump.cut<-data.consump[data.consump$cover.type!="forest",]

ggplot(data.consump, aes(x=as.factor(fire.time), y=consump))+
  geom_point(size=2)+
  labs(x="Spring 2018 burn", y=bquote('Consumption (g/ '*m^2~')'))+
  scale_fill_manual(name = "Biomass",labels = var.label,values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

## Belowground productivity --------------------------------------
#merge site data and root data
root<-merge(root, site_data, by="plot")

#exclude plots outside the fence
root<-root[!(root$fence_type%in%"open_out"),]

#model
t.test(root_mass~grazed, data=root, paired=T)

#plot
ggplot(root, aes(x=as.factor(grazed), y=root_mass))+
  geom_boxplot(lwd=.75)+
  geom_point(size=2)+
  labs(x="Grazed?", y=bquote('Belowground productivity (g/405'*cm^3*'/yr)'))+
  scale_fill_manual(name = "Biomass",values=c("#DC143C","#00BFFF"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(root, aes(x=as.factor(grazed), y=root_mass))+
  geom_point(size=2)+
  geom_line(aes(group=plot_pair), size=1.25, color="grey")+
  labs(x="Grazed?", y=bquote('Belowground productivity (g/405'*cm^3*'/yr)'))+
  scale_fill_manual(name = "Biomass",values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))


## Consumption through time---------------------------------------

#aggregate across species
#only keep this year's growth
consump.ag<-consump[consump$species%in% c("Green matter (alive)", "1st Year Woody"),]
#consumption<-consump[consump$species%in% "Green matter (alive)",]

#aggregate within plot biomass measurements
consump.ag<-aggregate(mass~sample_period+plot_pair+subplot,data=consump.ag,sum)

#subtract open from fence to measure consumption at each time point
consump.ag<-consump.ag %>%
  dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0) %>%
  mutate(consump= fence-open)

#drop forest plots
consump.ag.drop<-consump.ag[!consump.ag$plot_pair%in%c("13//14", "15//16"),]

#mean and sd
consump.ag.drop%>%
  group_by(sample_period)%>%
  summarise(avg=mean(consump),sd=sd(consump))

#merge camera and consumption data
consump.ag.drop<-merge(consump.ag.drop, cam.data, by=c("plot_pair", "sample_period"), all.x=T)

#replace missing camera data with zeros
consump.ag.drop$grazing.den[is.na(consump.ag.drop$grazing.den)]<- 0

#model
mod.consmp.gden<-lmer(consump~grazing.den+(1|plot_pair)+(1|sample_period),
                      data=consump.ag.drop)
summary(mod.consmp.gden)
Anova(mod.consmp.gden, type=3)

#plot
ggplot(consump.ag.drop, aes(grazing.den, consump))+
  geom_point(size=1.75, aes(color=plot_pair))+
  geom_smooth(method="lm",se=F, size=1.5, color="black")+
  labs(x="Bison grazing density", y=bquote('Consumption (g/ '*m^2*')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
ggplot(consump.ag.drop, aes(x=as.factor(sample_period), y=consump))+
  geom_point(size=2)+
  facet_wrap(~plot_pair, scales="free_y")+
  labs(x="Sample period", y=bquote('Consumption (g/ '*m^2~')'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
ggplot(consump.ag.drop, aes(x=as.factor(sample_period), y=consump))+
  geom_boxplot()+
  labs(x="Sample period", y=bquote('Consumption (g/ '*m^2~')'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

mod.consump.time<-lmer(consump~sample_period+(1|plot_pair), data=consump.ag.drop)
summary(mod.consump.time)
Anova(mod.consump.time, type=3)

consump.sum<-consump.ag.drop %>% 
  group_by(sample_period) %>% 
  summarise(mean=mean(consump),sd=sd(consump),
            se=(sd(consump)/sqrt(length(consump))),
            ci=(sd(consump)/sqrt(length(consump))) * qt((0.95/2 +0.5), (length(consump)-1)))

ggplot(consump.sum, aes(sample_period, mean))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Sample period", y=bquote('Consumption (g/ '*m^2*')'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

## Main plot biomass comparison ----------------------------------------
#merge site data and biomass from ungrazed plots
plot_data<-merge(sp_biomass,site_data,by= "plot", all=T)

#drop plots outside the bison fence
plot_data<-plot_data[plot_data$bison_fence%in%"yes",]

#drop woody species/debris
#fence_data<-fence_data[fence_data$type!="wood",]
plot_data<-plot_data[plot_data$species!="Woody debris",]

#drop litter
plot_data<-plot_data[plot_data$species!="Miscellaneous litter",]

#keep only woody species
#fence_data<-fence_data[fence_data$type%in% "wood",]

#sum mass across species within the plot
plot_mass_ag<-aggregate(mass~plot,data=plot_data,sum)

#divide by sample area to standardized biomass measurements (0.4 m^2)
plot_mass_ag$mass_stnd<-plot_mass_ag$mass/0.4
#drop unstandardized mass
plot_mass_ag$mass<-NULL

#merge with site data
plot_mass_ag<-merge(plot_mass_ag, site_data, by="plot",all.x=T)

#add biomass variables
plot_mass_ag<-merge(plot_mass_ag, biomass.cover.vars, by="plot")

#drop forest plots
plot_mass_ag_drop<-plot_mass_ag[!plot_mass_ag$plot_pair%in%c("13//14", "15//16"),]

#mean and sd
plot_mass_ag_drop%>%
  group_by(grazed)%>%
  summarise(avg=mean(mass_stnd),sd=sd(mass_stnd))

#plot
ggplot(plot_mass_ag_drop, aes(x=grazed, y=mass_stnd))+
  geom_boxplot(lwd=0.75, )+
  geom_point(size=2, aes(color=cover.type))+
  labs(x="Grazing?", y=bquote('End of Season Biomass (g/ '*m^2~')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(plot_mass_ag_drop, aes(x=grazed, y=mass_stnd))+
  geom_point(size=2, aes(color=cover.type))+
  facet_wrap(~plot_pair)+
  labs(x="Grazing?", y=bquote('End of Season Biomass (g/ '*m^2~')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "bottom",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
var.label2<-c("no"="Ungrazed", "yes"="grazed")
plot_mass_ag_grass<-plot_mass_ag_drop[plot_mass_ag_drop$cover.type%in%"grass",]

mean(plot_mass_ag_grass$mass_stnd[plot_mass_ag_grass$grazed%in%"yes"])
mean(plot_mass_ag_grass$mass_stnd[plot_mass_ag_grass$grazed%in%"no"])

tiff("graze_end_season.tiff",res=300,height=5,width=6, units= "in")
ggplot(plot_mass_ag_grass, aes(x=grazed, y=mass_stnd, fill=grazed))+
  geom_boxplot(lwd=0.75)+
  labs(x="", y=bquote('End of Season Biomass (g/ '*m^2~')'))+
  scale_x_discrete(labels=var.label2)+
  scale_fill_manual(name = "",values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

cover.labs <- c("Grass dominated plots", "Shrub dominated plots")
names(cover.labs) <- c("grass", "shrub")

tiff("graze_end_season_cover.tiff",res=300,height=5,width=6, units= "in")
ggplot(plot_mass_ag_drop, aes(x=grazed, y=mass_stnd, fill=grazed))+
  geom_boxplot(lwd=0.75)+
  facet_grid(~cover.type, labeller = labeller(cover.type = cover.labs))+
  labs(x="", y=bquote('End of Season Biomass (g/ '*m^2~')'))+
  scale_x_discrete(labels=var.label2)+
  scale_fill_manual(name = "Grazed?",values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),strip.background = element_rect(
    color="white", fill="white", size=1.5, linetype="solid"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

##woody cover
ggplot(plot_mass_ag_drop, aes(x=woody.cover, y=mass_stnd, color=grazed))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
  labs(x="Woody cover (%)", y=bquote('End of Season Biomass (g/ '*m^2*')'))+
  scale_color_manual(name = "Grazed?",values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=20),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position =c(0.8,0.85),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

mod.cover.bio<-lmer(mass_stnd~woody.cover*grazed+(1|plot_pair),data=plot_mass_ag_drop)
summary(mod.cover.bio)
Anova(mod.cover.bio, type=3)

## Power analysis ----------------------------------------------------------
library(simr)
simrOptions(progress=FALSE)

##base model
mod.prod.sim<-lmer(mass~variable+(1|plot_pair),data=full.data.cut)

#simulate power for grazing variable
powerSim(mod.prod.sim, nsim=100)

#power by changing reps
#model_ext_base <- extend(mod.prod.sim, within="variable", n=12)
model_ext_base <- extend(mod.prod.sim, along="plot_pair", n=24)

powerSim(model_ext_base, nsim=100)

p_curve_base <- powerCurve(model_ext_base, along="plot_pair")
plot(p_curve_base)

##fire frequency
mod.prod.fire.sim<-lmer(mass~variable*fire.frequency+(1|plot_pair),data=full.data.cut)

powerSim(mod.prod.fire.sim, nsim=100, test=fcompare(mass~variable))

mod.ext.fire<- extend(mod.prod.fire.sim, along= "plot_pair", n=24)

powerSim(mod.ext.fire, nsim=100, test=fcompare(mass~fire.frequency))

##woody cover