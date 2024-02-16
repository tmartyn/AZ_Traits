library( Cairo)
library(tidyverse)

traits<-read.csv("data/Traits.GPS.Comp.15.02.2024.csv")

traits$above.below.dry.ratio<-as.numeric(paste0(traits$total.below.dry.mass.g))/(traits$total.above.dry.mass.g+as.numeric(paste0(traits$total.below.dry.mass.g)))
traits$cover.area<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)
traits$above.volume<-pi*(traits$Width.90.cm.y/2)*(traits$Width.long.cm.y/2)*traits$Height.cm.y*(4/3)
traits$below.volume<-pi*(as.numeric(paste0(traits$Root.lateral.cm))/2)*
  (as.numeric(paste0(traits$Root.lateral.cm))/2)*as.numeric(paste0(traits$Root.depth.cm))*(4/3)

#### SLA ########
CairoPNG("SLA.png",height=800,width=1200)
mean.vals<-aggregate(as.numeric(paste0(traits$SLA)),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(as.numeric(paste0(SLA)), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("SLA (",cm^{2},"/g)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Specific Leaf Area")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()
#### LDMC ########
CairoPNG("LDMC.png",height=800,width=1200)
mean.vals<-aggregate(traits$LDMC.dry.wet,by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(LDMC.dry.wet, fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity", sides="b")+
  xlim(0,2)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  ylab("Density of data points")+
  xlab(expression(paste("LDMC (g dry/ g wet)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Leaf Dry Matter Content")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()
#### Above:Below Ratio ########
CairoPNG("Root.mass.fraction.png",height=800,width=1200)
mean.vals<-aggregate(traits$above.below.dry.ratio,by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(above.below.dry.ratio, fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity", sides="b")+
  #xlim(0,2)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  ylab("Density of data points")+
  xlab(expression(paste("Root-mass Fraction")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Root-mass Fraction")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()
#### root.dry.density.g.mm3 ########
CairoPNG("root.dry.density.png",height=800,width=1200)
mean.vals<-aggregate(traits$root.dry.density.g.mm3,by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(root.dry.density.g.mm3, fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity", sides="b")+
  #xlim(0,2)+
  ylab("Density of data points")+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Root-specific density (g/mm3)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Root-specific Density")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()
#### stem.dry.density.g.mm3 ########
CairoPNG("stem.dry.density.png",height=800,width=1200)
mean.vals<-aggregate(traits$stem.dry.density.g.mm3,by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(stem.dry.density.g.mm3, fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity", sides="b")+
  #xlim(0,2)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  ylab("Density of data points")+
  xlab(expression(paste("Stem-specific density (g/mm3)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Stem-specific Density")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()
#### Height ########
CairoPNG("Height.png",height=800,width=1200)
mean.vals<-aggregate(traits$Height.cm.y,by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(Height.cm.y, fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Height (cm)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Plant Height (cm)")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()

#### Volume ########
CairoPNG("Plant.above.volume.png",height=800,width=1200)
mean.vals<-aggregate(log(traits$above.volume),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(log(above.volume), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Aboveground volume log(cm3)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Aboveground plant volume (log)")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()



CairoPNG("Plant.below.volume.png",height=800,width=1200)
mean.vals<-aggregate(log(traits$below.volume),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(log(below.volume), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Belowground volume log(cm3)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Belowground plant volume (log)")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()


#### Leaf Area ########
CairoPNG("Leaf.area.png",height=800,width=1200)
mean.vals<-aggregate(as.numeric(paste0(traits$Leaf.area.mm2)),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(as.numeric(paste0(traits$Leaf.area.mm2)), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Leaf area (mm2)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Leaf area (mm2)")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()

#### Leaf Thickness ########
CairoPNG("Leaf.thickness.png",height=800,width=1200)
mean.vals<-aggregate(as.numeric(paste0(traits$Leaf.thickness.mm)),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(as.numeric(paste0(Leaf.thickness.mm)), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Leaf thickness (mm)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Leaf thickness")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()


#### Leaf drymass ########
CairoPNG("Leaf.Drymass.png",height=800,width=1200)
mean.vals<-aggregate(as.numeric(paste0(traits$Leaf.dry.mass.g)),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(as.numeric(paste0(Leaf.dry.mass.g)), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Leaf dry mass (g)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Leaf dry mass")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()

#### Total reproduction ########
CairoPNG("TOTAL.repo.png",height=800,width=1200)
mean.vals<-aggregate(log(as.numeric(paste0(traits$TOTAL_Reproduction))+1),by=list(traits$Year,traits$Native.Exotic,traits$Life.form),median,na.rm=T)
names(mean.vals)<-c("Year","Native.Exotic","Life.form","mean")
ggplot(traits, aes(log(as.numeric(paste0(TOTAL_Reproduction))+1), fill=Native.Exotic,col=Native.Exotic))+
  geom_density(position = "identity",alpha=0.5,col="black")+
  geom_rug(position="identity",sides="b",aes(col=Native.Exotic))+
  ylab("Density of data points")+
  #xlim(0,400)+
  geom_vline(data=mean.vals,(aes(xintercept=mean,col=Native.Exotic)))+
  xlab(expression(paste("Total Reproduction log(seeds)")))+
  facet_grid(Year~Life.form)+
  labs(fill="Native status")+
  scale_colour_manual(values=c("coral","slateblue4"),guide=F)+
  scale_fill_manual(values=c("coral","slateblue4"))+
  ggtitle("Total Reproduction")+theme_minimal(base_size = 22)+
  theme(legend.position = "bottom")
dev.off()