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
#consumption
consump1<-read.csv("e321_consump_1_2018.csv")
consump2<-read.csv("e321_consump_2_2018.csv")
consump3<-read.csv("e321_consump_3_2018.csv")
consump4<-read.csv("e321_consump_4_2018.csv")
consump.2020<-read.csv("e321_consumption_2020.csv")

#plot biomass
sp_biomass<-read.csv("sp_biomass_data_2018.csv")
sp.biomass.20<-read.csv("e321_sp_biomass_2020.csv")

#site conditions
site_data<-read.csv("e321_site_data.csv")
light<-read.csv("e321_light_final_2018.csv")
cam.data<-read.csv("camera_trap_data_clean_2018.csv")
sp_cover<-read.csv("e321_sp_cover_2018_final.csv")
n.min<-read.csv("e321_N_mineralization_2019.csv")

#roots
root.19<-read.csv("e321_root_igc_2019.csv")
root.20<-read.csv("e321_root_igc_2020.csv")

## Format site data -------------------------------------------------------
#add time since fire variable
site_data$fire.time<-2018-site_data$last.burn
#add whether or not plot was burned this year
site_data$this.year.burn<-ifelse(site_data$last.burn==2018,"yes","no")
#add light data
site_data<-merge(site_data,light[-2:-9],by="plot")

##Format N mineralization data
#remove blanks
n.min<-n.min[n.min$blank%in%"no",]
n.min$sample<-NULL
n.min$blank<-NULL

n.min<-n.min %>% 
  gather(n.type, value, nitrate.nitrite:ammonium) %>% 
  unite(variable, n.type, initial.final, sep=".") %>% 
  spread(variable, value)

n.min$ammonium.total<-n.min$ammonium.final-n.min$ammonium.initial
n.min$nitrate.nitrite.total<-n.min$nitrate.nitrite.final-n.min$nitrate.nitrite.initial
n.min$n.min.total<-n.min$ammonium.total+n.min$nitrate.nitrite.total
n.min$n.min.rate<-n.min$n.min.total/30 #30 day incubation

n.min<-n.min[c("plot", "ammonium.total", "nitrate.nitrite.total", "n.min.total", "n.min.rate")]

site_data<-merge(site_data, n.min, by="plot")

##Consumption data formatting##
#merge consumption data
consump.lst<-list(consump1,consump2,consump3,consump4)
consump<-Reduce(function(x, y) merge(x, y, all=TRUE), consump.lst)

#drop unneeded columns
keep<-c("sample_period","plot_pair","subplot","species","mass")
consump<-consump[,keep]

#clean 2020 consumption data
consump.2020<-consump.2020[c("sample_period","plot_pair","subplot","rep","species","mass")]

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

#Do the same for the cover data (cover not relative cover)
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
biomass.cover.vars2<-merge(biomass.cover.vars, site_data[,c(1:2,8)], by="plot")

biomass.cover.vars.graze<- biomass.cover.vars2 %>%
  filter(grazed%in%"yes") %>% 
  select(-grazed)

biomass.cover.vars.ungraze<- biomass.cover.vars2 %>%
  filter(grazed%in%"no") %>% 
  select(-grazed)

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
graze.all$plot<-gsub("/.*","",graze.all$plot_pair)
graze.all$year<-rep("2018",nrow(graze.all))

##add 2020 consumption data
#only keep this year's growth
graze.all.20<-consump.2020[consump.2020$species%in% c("Forbs","Miscellaneous grasses", "1st Year Woody","Graminoids", "Woody 1st Year"),]

#aggregate within plot biomass measurements
graze.all.20<-aggregate(mass~sample_period+plot_pair+subplot+rep,data=graze.all.20,sum)

#Make all of the open plots for graze.all 4 = 0 (we don't need them to calculate graze.all.20)
graze.all.20[(graze.all.20$sample_period%in% "C4" & graze.all.20$subplot%in% "open"),"mass"]<-0

#drop plot 31//32 A for sampling C4 because a bison smashed it
graze.all.20<-graze.all.20[!(graze.all.20$plot_pair %in% "31//32" & graze.all.20$rep %in% "A" & graze.all.20$sample_period %in% "C4"),]

#Make all of the open plots for graze.all 4 = 0 (we don't need them to calculate graze.all.20)
graze.all.20<-graze.all.20 %>%
  dcast(sample_period+plot_pair~subplot,value.var = "mass", fill=0, mean) %>%
  mutate(consump= fence-open)

#aggregate by plot
graze.all.20.ag<-aggregate(consump~plot_pair,data=graze.all.20,sum)
#graze.all.20.ag<-aggregate(consump~plot_pair, data=graze.all.20, mean)
graze.all.20.ag$plot<-gsub("/.*","",graze.all.20.ag$plot_pair)
graze.all.20.ag$year<-rep("2020",nrow(graze.all.20.ag))

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
fence_ag<-aggregate(mass~plot_pair+plot,data=fence_data,sum)

#divide by sample area to standardized biomass measurements (0.4 m^2)
fence_ag$mass_stnd<-fence_ag$mass/0.4
#drop unstandardized mass
fence_ag$mass<-NULL

##add 2020 control biomass data
sp.biomass.20<-sp.biomass.20[,c("plot","species","mass","type", "drop")]

#merge site data and biomass from ungrazed plots
fence_data.20<-merge(sp.biomass.20,site_data,by= "plot", all=T)

#drop plots outside the bison fence
fence_data.20<-fence_data.20[fence_data.20$bison_fence%in%"yes",]

#only keep ungrazed plots
fence_data.20<-fence_data.20[fence_data.20$grazed%in%"no",]

#drop woody debris, litter, previous year's growth, and non-plants
#fence_data<-fence_data[fence_data$type!="wood",]
fence_data.20<-fence_data.20[!(fence_data.20$type %in% "litter" | fence_data.20$drop %in% "drop"),]

#drop litter
#fence_data<-fence_data[fence_data$species!="Miscellaneous litter",]

#keep only woody species
#fence_data<-fence_data[fence_data$type%in% "wood",]

#sum mass across species within the plot
fence_ag.20<-aggregate(mass~plot_pair+plot,data=fence_data.20,sum)

#divide by sample area to standardized biomass measurements (0.48 m^2)
fence_ag.20$mass_stnd<-fence_ag.20$mass/0.48

#drop unstandardized mass and the drop column
fence_ag.20$mass<-NULL
fence_ag.20$drop<-NULL

##Combining datasets##
biomass.data<-merge(graze.all,fence_ag,by=c("plot_pair", "plot"),all=T)

#make data long
biomass.data.melt<-melt(biomass.data,id.vars = c("plot_pair", "plot"), value.name = "mass")

#drop all of the grazed plot types from site.data to not cause duplication when merging
site_data_cut<-site_data[site_data$grazed%in%"no",]
site_data_cut2<-site_data[site_data$grazed%in%"yes",] #for consumption plots

#merge with site.data
full.data<-merge(biomass.data.melt,site_data,by=c("plot_pair", "plot"),all.x=T)

#add biomass and cover summary variables
full.data<-merge(full.data, biomass.cover.vars2, by=c("plot_pair", "plot", "grazed"))

#drop forested plots for t-test
biomass.data.cut<-biomass.data[!(biomass.data$plot_pair%in% "13//14" | biomass.data$plot_pair%in% "15//16"),]

#drop forested plots from full.data
full.data.cut<-full.data[full.data$cover.type!="forest",]

#calculate % change in productivity
mean((biomass.data.cut$consump-biomass.data.cut$mass_stnd)/biomass.data.cut$mass_stnd)

mean(biomass.data.cut$mass_stnd, na.rm=T)/mean(biomass.data.cut$consump, na.rm=T)
mean(biomass.data.cut$consump)-mean(biomass.data.cut$mass_stnd)

(mean(biomass.data.cut$consump)-mean(biomass.data.cut$mass_stnd))/mean(biomass.data.cut$mass_stnd)

#add minimum to biomass data to allow for log transformation
full.data$mass.add<-full.data$mass+(abs(min(full.data$mass, na.rm=T))+1)
#full.data.cut$mass.add<-full.data.cut$mass+(abs(min(full.data.cut$mass, na.rm=T))+0.001)

##build models##
t.test(biomass.data.cut$consump, biomass.data.cut$mass_stnd, paired=TRUE, conf.level=0.95)

#mod.prod<-lmer(log(mass)~grazed+fire.frequency+I(fire.frequency^2)+variable*year+woody.cover+(1|plot_pair),data=full.data.cut[!is.na(full.data.cut$mass),])

mod.prod<-lmer(log(mass)~grazed+fire.frequency+I(fire.frequency^2)+woody.cover+(1|plot_pair),data=full.data.cut)
summary(mod.prod)
Anova(mod.prod, type=3)

mod1<-lmer(mass~variable+(1|plot_pair),data=full.data.cut)
summary(mod1)
Anova(mod1, type=3)

data.super.cut<-full.data.cut[!(full.data.cut$plot_pair %in% c("19//20","21//22")),]

mod1.cut<-lmer(mass.add~variable+(1|plot_pair),data=full.data.cut)
summary(mod1.cut)
Anova(mod1.cut, type=3)

mod1.1<-lmer(log(mass.add)~variable*this.year.burn+(1|plot_pair),data=full.data)
summary(mod1.1)
Anova(mod1.1, type=3)

mod1.1.1<-lmer(log(mass.add)~variable*as.factor(fire.frequency)+(1|plot_pair),data=full.data)
summary(mod1.1.1)
Anova(mod1.1.1, type=3)

mod1.2<-lmer(log(mass.add)~variable*fire.frequency+(1|plot_pair), data=full.data)
summary(mod1.2)
Anova(mod1.2, type=3)

mod1.2.1<-lmer(mass~variable+fire.frequency+I(fire.frequency^2)+(1|plot_pair), data=full.data)
summary(mod1.2.1)
Anova(mod1.2.1, type=3)
AIC(mod1.2, mod1.2.1)

##plots##
biomass.data.cut$dif<-biomass.data.cut$consump-biomass.data.cut$mass_stnd

plot(biomass.data.cut$consump-biomass.data.cut$mass_stnd,
     pch = 16,ylim=c(-250,700), cex=1.2,
     ylab="Productivity (g/m^2)")
abline(0,0, col="grey", lwd=2)
points(biomass.data.cut$consump, col="red", cex=1.2)
points(biomass.data.cut$mass_stnd, col="blue", cex=1.2)
legend(1, 600, legend=c("Difference", "Grazed","Fenced"),
       col=c("black","red", "blue"), pch=c(16,1,1), cex=0.8)
##
biomass.data.melt2<-melt(biomass.data.cut, id.vars = "plot_pair", value.name = "mass")

var.label3<-c("consump"="Grazed", "mass_stnd"="Ungrazed", "dif"="Difference")
ggplot(biomass.data.melt2, aes(plot_pair, mass, color=variable))+
  geom_point(size=2)+
  labs(x="Plot pair", y=bquote('Productivity (g/ '*m^2~'/year)'))+
  #scale_x_discrete(labels=var.label3)+
  scale_color_manual(name = "",labels = var.label3,values=c("#DC143C","#00BFFF","black"))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(0.8,0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
###VAR.LABEL###
var.label<-c("consump"="Grazed", "mass_stnd"="Ungrazed")

tiff("graze_prod.tiff",res=300,height=4,width=6, units= "in")
ggplot(full.data.cut, aes(x=variable, y=mass, fill=variable))+
  geom_boxplot(lwd=.75)+
  #geom_point(size=2,position=position_jitter(width = 0.1))+
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
  drop_na(mass) %>% 
  summarise(mean=mean(mass),sd=sd(mass),
            se=(sd(mass)/sqrt(length(mass))),
            ci=(sd(mass)/sqrt(length(mass))) * qt((0.95/2 +0.5), (length(mass)-1)))

tiff("graze_prod3.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum, aes(variable, mean, color=variable))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('Productivity (g/ '*m^2*'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  ylim(0,NA)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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
fire.pred<-ggpredict(mod1.2.1, c("fire.frequency [all]","variable"))

tiff("graze_prod_fire.tiff",res=300,height=6,width=8.5, units= "in")
ggplot(full.data.cut, aes(x=fire.frequency, y=log(mass), color=variable))+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),se=F, size = 1.5)+
  geom_point(size=2)+
  labs(x="Fire frequency", y=bquote('log Productivity (g/ '*m^2~'/year)'))+
  scale_color_manual(name = "",labels = var.label,values=c("#e66101","#5e3c99"))+
  #ylim(0,NA)+
  #geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(0.9,0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

ggplot(full.data.cut, aes(x=fire.frequency, y=log(mass), color=variable))+
  stat_function(fun=function(x)(fixef(mod1.2.1)[1] + I(fixef(mod1.2.1)[3]*x^2)),size=1.5)+
  labs(x="Fire frequency", y=bquote('log Productivity (g/ '*m^2~'/year)'))+
  scale_color_manual(name = "",labels = var.label,values=c("#e66101","#5e3c99"))+
  #ylim(0,NA)+
  #geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(0.9,0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

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

data.sum.herb<-full.data.herb.cut %>% 
  group_by(variable) %>% 
  summarise(mean=mean(mass),sd=sd(mass),
            se=(sd(mass)/sqrt(length(mass))),
            ci=(sd(mass)/sqrt(length(mass))) * qt((0.95/2 +0.5), (length(mass)-1)))

tiff("graze_prod_herb.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum.herb, aes(variable, mean, color=variable))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('Herbaceous Productivity (g/ '*m^2*'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  ylim(0,350)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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

data.sum.wood<-full.data.wood.cut %>% 
  group_by(variable) %>% 
  summarise(mean=mean(mass),sd=sd(mass),
            se=(sd(mass)/sqrt(length(mass))),
            ci=(sd(mass)/sqrt(length(mass))) * qt((0.95/2 +0.5), (length(mass)-1)))

tiff("graze_prod_wood.tiff",res=300,height=4,width=6, units= "in")
ggplot(data.sum.wood, aes(variable, mean, color=variable))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('Woody Productivity (g/ '*m^2*'/year)'))+
  scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  ylim(0,350)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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
data.consump<-merge(consumption,site_data_cut2, by="plot_pair", all.x=T)

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
  summarise(bison_hdown=sum(bison_hdown), bison_count=sum(bison_count), num_bison=sum(num_bison))

cam.data.ag$graz.den<-cam.data.ag$bison_hdown/70 #70 days
cam.data.ag$bison.den<-cam.data.ag$num_bison/70 #70 days
cam.data.ag$b.count.den<-cam.data.ag$bison_count/70 #70 days

#merge
data.consump.cut<-merge(data.consump.cut, cam.data.ag, by="plot_pair", all.x=T)

#add biomass variables
data.consump.cut<-merge(data.consump.cut, biomass.cover.vars.ungraze[-1], by="plot_pair") #ungrazed plots for relative cover. Removing plot colum to merge
data.consump<-merge(data.consump, biomass.cover.vars.ungraze[-1], by="plot_pair") #ungrazed plots for relative cover. Removing plot colum to merge

#model
mod.consmp.gden2<-lm(consump~b.count.den+this.year.burn, data=data.consump.cut)
summary(mod.consmp.gden2)

mod.consmp.grass<-lm(consump~grass.cover+I(grass.cover^2), data=data.consump.cut)
summary(mod.consmp.grass)

mod.graz.den<- lm(graz.den~this.year.burn, data=data.consump.cut)
summary(mod.graz.den)
boxCox(mod.graz.den)

#plot
ggplot(data.consump.cut, aes(graz.den, consump))+
  geom_point(size=1.75)+
  geom_smooth(method="lm",se=F, size=1.5, color="black")+
  labs(x="Bison grazing density", y=bquote('Consumption (g/ '*m^2~')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
ggplot(data.consump.cut, aes(x=plot_pair, y=consump))+
  geom_point(size=2, aes(color=this.year.burn))+
  labs(x="Plot", y=bquote('Consumption (g/ '*m^2~')'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=16),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(data.consump.cut, aes(x=woody.cover, y=consump, color=this.year.burn))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  labs(x="Woody cover (%)", y=bquote('Consumption (g/ '*m^2~')'))+
  theme(text = element_text(size=16),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

ggplot(data.consump.cut, aes(x=litter.cover, y=consump))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
  labs(x="Litter cover (%)", y=bquote('Consumption (g/ '*m^2~')'))+
  theme(text = element_text(size=16),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))


##fit model##
mod4<-lm(consump~litter.cover,data=data.consump.cut)
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

fire.sum<-data.consump.cut %>% 
  group_by(this.year.burn) %>% 
  summarise(mean=mean(consump),sd=sd(consump),
            se=(sd(consump)/sqrt(length(consump))),
            ci=(sd(consump)/sqrt(length(consump))) * qt((0.95/2 +0.5), (length(consump)-1)))

tiff("fire_consump.tiff",res=300,height=5,width=6, units= "in")
ggplot(fire.sum, aes(this.year.burn, mean, color=this.year.burn))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('Consumption (g/'*m^2*'/yr)'))+
  scale_x_discrete(labels=c("Unburned", "Burned"))+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  #ylim(0,0.6)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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

##Consumption by light and Nitrate
mod.consum.light<-lm(consump~light.prop, data=data.consump.cut)
summary(mod.consum.light)

mod.consum.nitrate<-lm(consump~nitrate.nitrite.total, data=data.consump.cut)
summary(mod.consum.nitrate)

##Consumption by grass
tiff("consump_grass.tiff",res=300,height=5,width=6, units= "in")
ggplot(data.consump.cut, aes(grass.cover, y=consump))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5, formula = y ~ x + I(x^2))+
  labs(x="Grasss cover (%)", y=bquote('log Consumption (g/ '*m^2*'/yr)'))+
  geom_hline(yintercept=0, linetype="dotted")+
  annotate("text", x=55, y=3, label="p=0.42", size=6)+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

data.consump$consump.add<-data.consump$consump+(abs(min(data.consump$consump, na.rm=T))+1)

mod.consump.grass<-lm(consump~grass.cover, data=data.consump)
summary(mod.consump.grass)
Anova(mod.consump.grass, type=3)

mod.consump.grass2<-lm(consump~I(grass.cover^2), data=data.consump.cut)
summary(mod.consump.grass2)
Anova(mod.consump.grass2, type=3)
AIC(mod.consump.grass, mod.consump.grass2)

## Belowground productivity --------------------------------------
#merge site data and root data
root<-rbind(root.19, root.20)
root<-merge(root, site_data, by="plot")

#add biomass and cover data
root<-merge(root, biomass.cover.vars, by="plot")

#exclude plots outside the fence
root<-root[!(root$fence_type%in%"open_out"),]

#scale root data
root$mass_scale<-root$root_mass/405
root$mass_scale<-root$mass_scale*1000 #convert to mg

#model
t.test(mass_scale~grazed, data=root, paired=T)

root.mod<-lmer(log(mass_scale)~grazed*as.factor(year)+fire.frequency+I(fire.frequency^2)+woody.cover+(1|plot_pair)+(1|plot), data=root)
summary(root.mod)
Anova(root.mod, type=3)

#plot
ggplot(root, aes(x=as.factor(grazed), y=log(root_mass)))+
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

root.sum<-root %>% 
  group_by(grazed, year) %>% 
  summarise(mean=mean(mass_scale),sd=sd(mass_scale),
            se=(sd(mass_scale)/sqrt(length(mass_scale))),
            ci=(sd(mass_scale)/sqrt(length(mass_scale))) * qt((0.95/2 +0.5), (length(mass_scale)-1)))

root.sum$grazed<-relevel(root.sum$grazed, "yes")

tiff("root_prod.tiff",res=300,height=5,width=6, units= "in")
ggplot(root.sum, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  facet_grid(cols=vars(year))+
  labs(x="", y=bquote('Belowground productivity (mg/'*cm^3*'/yr)'))+
  scale_x_discrete(labels=var.label2)+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  #ylim(0,0.6)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

#fire
ggplot(root, aes(x=fire.frequency, y=log(mass_scale)))+
  geom_point(size=2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),se=F, size = 1.5)+
  #stat_function(fun=function(x)(fixef(root.mod)[1] + fixef(root.mod)[3]*x),size=1.5)+
  labs(x="Fire frequency", y=bquote('Belowground productivity (mg/ '*cm^3*'/yr)'))+
  geom_hline(yintercept=0, linetype="dotted")+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
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
consump.ag<-merge(consump.ag, cam.data, by=c("plot_pair", "sample_period"), all.x=T)

#replace missing camera data with zeros
consump.ag.drop$grazing.den[is.na(consump.ag.drop$grazing.den)]<- 0
consump.ag$grazing.den[is.na(consump.ag$grazing.den)]<- 0

#consumption per day
#add sampling period lengths
#6, 20, 20, 24
consump.ag.drop$graze.days<- ifelse(consump.ag.drop$sample.interval=="2018-06-26", 21,
                       ifelse(consump.ag.drop$sample.interval=="2018-08-14", 24, 20))

consump.ag$graze.days<- ifelse(consump.ag$sample.interval=="2018-06-26", 21,
                                    ifelse(consump.ag$sample.interval=="2018-08-14", 24, 20))

consump.ag.drop$consump.day<-consump.ag.drop$consump/consump.ag.drop$graze.days
consump.ag$consump.day<-consump.ag$consump/consump.ag$graze.days

#add min value to log transform
consump.ag$consump.day.add<-consump.ag$consump.day+(abs(min(consump.ag$consump.day, na.rm=T))+1)

#drop plot 23/24 for sample time 3 and 4
#consump.ag.drop<-consump.ag.drop[!(consump.ag.drop$plot_pair%in%"23//24" & consump.ag.drop$sample_period%in%c("C3", "C4")),]

#model
mod.consmp.gden<-lmer(consump~b.count.den+(1|plot_pair)+(1|sample_period),
                      data=consump.ag.drop)
summary(mod.consmp.gden)
Anova(mod.consmp.gden, type=3)

#add biomass and cover data
consump.ag.drop<-merge(consump.ag.drop, biomass.cover.vars.ungraze[-1], by="plot_pair") #ungrazed plots for relative cover. Removing plot colum to merge

mod.consmp.grass2<-lmer(consump~grass.cover+(1|plot_pair)+(1|sample_period),
                      data=consump.ag.drop)
Anova(mod.consmp.grass2, type=3)

#add fire data
consump.ag.drop<-consump.ag.drop<-merge(consump.ag.drop, site_data_cut, buy="plot_pair")

mod.consmp.fire<-lmer(consump~this.year.burn+grass.cover+I(grass.cover^2)+log(samp.period.num)+(1|plot_pair),
                        data=consump.ag.drop)
Anova(mod.consmp.fire, type=3)

#plot
ggplot(consump.ag.drop, aes(grass.cover, consump))+
  geom_point(size=1.75)+
  geom_smooth(method="lm",se=F, size=1.5, color="black")+
  labs(x="Grass cover (%)", y=bquote('Consumption (g/ '*m^2*')'))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

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

##
consump.ag.drop$samp.period.num<-as.numeric(gsub("C", "", consump.ag.drop$sample_period))
consump.ag$samp.period.num<-as.numeric(gsub("C", "", consump.ag$sample_period))

mod.consump.time<-lmer(consump.day~log(samp.period.num)+(1|plot_pair), data=consump.ag.drop)
summary(mod.consump.time)
Anova(mod.consump.time, type=3)

consump.sum<-consump.ag.drop %>% 
  group_by(sample_period) %>% 
  summarise(mean=mean(consump.day),sd=sd(consump.day),
            se=(sd(consump.day)/sqrt(length(consump.day))),
            ci=(sd(consump.day)/sqrt(length(consump.day))) * qt((0.95/2 +0.5), (length(consump.day)-1)))

ggplot(consump.sum, aes(sample_period, mean))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Sample period", y=bquote('Consumption (g/ '*m^2*'/day)'))+
  #scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

tiff("consump_time.tiff",res=300,height=5,width=6, units= "in")
ggplot(consump.ag.drop, aes(samp.period.num, consump.day))+
  geom_point(size=2.5)+
  geom_smooth(method="lm",se=F, size=1.5, color="black", formula = y ~ log(x))+
  labs(x="Sample period", y=bquote('Consumption (g/ '*m^2*'/day)'))+
  #annotate("text", x=3.5, y=2, label="p=0.26", size=6)+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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

##2020 plot data
#merge site data and biomass from ungrazed plots
plot_data.20<-merge(sp.biomass.20,site_data,by= "plot", all=T)

#drop plots outside the bison fence
plot_data.20<-plot_data.20[plot_data.20$bison_fence%in%"yes",]

#drop woody species/debris
plot_data.20<-plot_data.20[!(plot_data.20$type %in% "litter" | plot_data.20$drop %in% "drop"),]
#fence_data<-fence_data[fence_data$type!="wood",]
#plot_data<-plot_data[plot_data$species!="Woody debris",]

#drop litter
#plot_data<-plot_data[plot_data$species!="Miscellaneous litter",]

#keep only woody species
#fence_data<-fence_data[fence_data$type%in% "wood",]

#sum mass across species within the plot
plot_mass_ag.20<-aggregate(mass~plot,data=plot_data.20,sum)

#divide by sample area to standardized biomass measurements (0.48 m^2)
plot_mass_ag.20$mass_stnd<-plot_mass_ag.20$mass/0.48
#drop unstandardized mass
plot_mass_ag.20$mass<-NULL

#merge with site data
plot_mass_ag.20<-merge(plot_mass_ag.20, site_data, by="plot",all.x=T)

#drop forest plots
plot_mass_ag_drop.20<-plot_mass_ag.20[!plot_mass_ag.20$plot_pair%in%c("13//14", "15//16"),]

##
#merge with site data
plot_mass_ag<-merge(plot_mass_ag, site_data, by="plot",all.x=T)

#add biomass variables (average values between plots)
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
  geom_line(aes(group=plot_pair), size=1.25, color="grey")+
  geom_point(size=2.25, aes(color=cover.type))+
  facet_wrap(~plot_pair)+
  labs(x="Grazing?", y=bquote('End of Season Biomass (g/ '*m^2~')'))+
  scale_color_manual(name = "Dominant cover type",values=c("#e66101","#5e3c99"))+
  theme(text = element_text(size=24),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "bottom",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##
###VAR.LABEL###
var.label2<-c("no"="Ungrazed", "yes"="Grazed")

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
tiff("graze_end_season_woody_cover.tiff",res=300,height=5,width=6, units= "in")
ggplot(plot_mass_ag, aes(x=woody.cover, y=mass_stnd, color=grazed))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
  labs(x="Woody cover (%)", y=bquote('End of Season Biomass (g/'*m^2*')'))+
  scale_color_manual(name = "",labels=var.label2, values=c("#00BFFF", "#DC143C"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position =c(0.8,0.85),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

mod.cover.bio<-lmer(mass_stnd~woody.cover*grazed+(1|plot_pair),data=plot_mass_ag)
summary(mod.cover.bio)
Anova(mod.cover.bio, type=3)

end.sum<-plot_mass_ag_drop %>% 
  group_by(grazed) %>% 
  drop_na(mass_stnd) %>% 
  summarise(mean=mean(mass_stnd),sd=sd(mass_stnd),
            se=(sd(mass_stnd)/sqrt(length(mass_stnd))),
            ci=(sd(mass_stnd)/sqrt(length(mass_stnd))) * qt((0.95/2 +0.5), (length(mass_stnd)-1)))

end.sum$grazed<-factor(end.sum$grazed, levels= c("yes", "no"))

tiff("end_season2.tiff",res=300,height=5,width=6, units= "in")
ggplot(end.sum, aes(grazed, mean, color=grazed))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="", y=bquote('End of Season Biomass (g/ '*m^2*'/year)'))+
  scale_x_discrete(labels=var.label2)+
  scale_color_manual(values=c("#e66101","#5e3c99"))+
  #ylim(0,NA)+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
dev.off()

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

##Litter
consump.litter<-consump[consump$species%in% "Miscellaneous litter",]

#
consump.litter<-consump.litter %>%
  group_by(sample_period, plot_pair) %>% 
  summarise(litter.mass=mean(mass))

consump.litter.drop<-consump.litter[!consump.litter$plot_pair%in%c("13//14", "15//16"),]

consump.litter.drop<-merge(consump.litter.drop, consump.ag.drop, by=c("sample_period", "plot_pair")) #from consumption through time

ggplot(consump.litter.drop, aes(x=litter.mass, y=consump))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, size=1.5)+
  labs(x=bquote('Litter biomass (g/ '*m^2~')'), y=bquote('Consumption (g/ '*m^2~')'))+
  theme(text = element_text(size=16),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = c(.9, 0.8),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

mod.litter<-lmer(consump~litter.mass+ (1|plot_pair)+(1|sample_period),data=consump.litter.drop)
summary(mod.litter)
Anova(mod.litter, type=3)

#Nitrate
site_data_drop<-site_data[!(site_data$cover.type%in%"forest") & site_data$bison_fence%in%"yes",]

mod.nitrate<-lmer(nitrate.nitrite.total~grazed+(1|plot_pair),data=site_data_drop)
summary(mod.nitrate)
Anova(mod.nitrate, type=3)

nitrate.sum<-site_data_drop %>% 
  group_by(grazed) %>% 
  summarise(mean=mean(nitrate.nitrite.total),sd=sd(nitrate.nitrite.total),
            se=(sd(nitrate.nitrite.total)/sqrt(length(nitrate.nitrite.total))),
            ci=(sd(nitrate.nitrite.total)/sqrt(length(nitrate.nitrite.total))) * qt((0.95/2 +0.5), (length(nitrate.nitrite.total)-1)))

ggplot(nitrate.sum, aes(grazed, mean))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Grazed?", y="Nitrate/Nitrite (mg/L)")+
  #scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))

##light
mod.light<-lmer(light.prop~grazed+(1|plot_pair),data=site_data_drop)
summary(mod.light)
Anova(mod.light, type=3)

light.sum<-site_data_drop %>% 
  group_by(grazed) %>% 
  summarise(mean=mean(light.prop),sd=sd(light.prop),
            se=(sd(light.prop)/sqrt(length(light.prop))),
            ci=(sd(light.prop)/sqrt(length(light.prop))) * qt((0.95/2 +0.5), (length(light.prop)-1)))

ggplot(light.sum, aes(grazed, mean))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=0.1)+
  labs(x="Grazed?", y="proportion of PAR reaching soil surface")+
  #scale_x_discrete(labels=var.label)+
  scale_color_manual(values=c("#DC143C","#00BFFF"))+
  theme(text = element_text(size=18),axis.text=element_text(colour="black"),
        panel.background=element_blank(),legend.key=element_blank(),legend.position = "none",
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line = element_line(size=.7, color="black"))
